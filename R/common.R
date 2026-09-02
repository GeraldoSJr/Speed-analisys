## Shared loading / normalisation helpers for the KLUE speed-sweep analysis (R port).
##
## The sweep replayed one synthetic 300-minute trace five times at --speed-by 20, 15, 10, 5 and 1.
## Each run left a run.log under ~/klue-speed-sweep-results/speed-N/ and a directory of Prometheus
## CSVs. The run.log names its own zip, which is what maps a speed to its metrics directory; file
## mtimes are not trusted for that.

suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
  library(stringr)
})

HOME          <- path.expand("~")
RESULTS_ROOT  <- file.path(HOME, "klue-speed-sweep-results")
KLUE_REPO     <- Sys.getenv("KLUE_REPO", unset = file.path(HOME, "klue"))
TRACE_DIR     <- file.path(HOME, "klue-trace-varying-load-300min")
OUT_DIR       <- file.path(HOME, "klue-speed-analysis-r", "results")

## The three Deployments the trace drives, and their namespaces.
SERVICES       <- c(frontend = "frontend-ns", backend = "backend-ns", `batch-worker` = "batch-ns")
EMU_NAMESPACES <- unname(SERVICES)

## Trace geometry (see ~/klue-trace-varying-load-300min/generate_trace.py).
TICK_SECONDS         <- 300
NUM_TICKS            <- 61
TRACE_SPAN_SECONDS   <- (NUM_TICKS - 1) * TICK_SECONDS

## Collector step, fixed in src/manager.py as Collector(step=30) regardless of speed.
COLLECTOR_STEP_SECONDS <- 30

## Per-run memo store, keyed by "<label>::<what>".
.cache <- new.env(parent = emptyenv())

cache_get <- function(key, producer) {
  if (!exists(key, envir = .cache, inherits = FALSE)) {
    assign(key, producer(), envir = .cache)
  }
  get(key, envir = .cache, inherits = FALSE)
}

## ---------------------------------------------------------------------------
## Run discovery
## ---------------------------------------------------------------------------

discover_runs <- function() {
  if (!dir.exists(RESULTS_ROOT)) stop("No sweep results at ", RESULTS_ROOT)

  entries <- list.files(RESULTS_ROOT, pattern = "^speed-[0-9.]+$")
  if (!length(entries)) stop("No speed-* runs under ", RESULTS_ROOT)

  runs <- lapply(entries, function(entry) {
    speed    <- as.numeric(sub("^speed-", "", entry))
    run_dir  <- file.path(RESULTS_ROOT, entry)
    log_path <- file.path(run_dir, "run.log")
    if (!file.exists(log_path)) stop(entry, ": no run.log")

    txt  <- paste(readLines(log_path, warn = FALSE), collapse = "\n")
    zips <- str_extract_all(txt, "output_csv_[0-9\\-]+:[0-9:]+(?=\\.zip)")[[1]]
    if (!length(zips)) stop(entry, ": run.log names no output_csv zip")
    stem <- tail(zips, 1)

    ## The sweep script moves results next to the log; when it does not they stay in the repo.
    candidates  <- c(file.path(run_dir, stem), file.path(KLUE_REPO, stem))
    metrics_dir <- candidates[dir.exists(candidates)][1]
    if (is.na(metrics_dir)) stop(entry, ": metrics dir ", stem, " not found")

    list(speed = speed, label = paste0(format(speed, trim = TRUE), "x"),
         metrics_dir = metrics_dir, log_path = log_path)
  })

  runs[order(-vapply(runs, `[[`, numeric(1), "speed"))]
}

load_metric <- function(run, metric) {
  cache_get(paste0(run$label, "::m::", metric), function() {
    path <- file.path(run$metrics_dir, paste0(metric, ".csv"))
    if (!file.exists(path)) return(data.table())
    dt <- fread(path, showProgress = FALSE)
    if ("timestamp" %in% names(dt)) dt[, timestamp := as.numeric(timestamp)]
    if ("value" %in% names(dt))     dt[, value := as.numeric(value)]
    if ("timestamp" %in% names(dt)) dt <- dt[!is.na(timestamp)]
    dt
  })
}

read_log <- function(run) {
  cache_get(paste0(run$label, "::log"), function() {
    paste(readLines(run$log_path, warn = FALSE), collapse = "\n")
  })
}

## ---------------------------------------------------------------------------
## Ground truth, read from the trace rather than re-derived
## ---------------------------------------------------------------------------

load_ground_truth <- function() {
  cache_get("gt", function() {
    workload <- fromJSON(file.path(TRACE_DIR, "workload_description.json"),
                         simplifyVector = FALSE)
    infra    <- fromJSON(file.path(TRACE_DIR, "infrastructure_description.json"),
                         simplifyVector = FALSE)

    replicas <- setNames(rep(NA_real_, length(SERVICES)), names(SERVICES))
    rows <- vector("list", length(workload$emulation))
    for (i in seq_along(workload$emulation)) {
      entry <- workload$emulation[[i]]
      for (objs in entry$applied_objects) {
        for (obj in objs) replicas[[obj$metadata$name]] <- obj$spec$replicas
      }
      for (sc in entry$scaled_replicasets) replicas[[sc$name]] <- sc$pods
      rows[[i]] <- c(list(trace_time = entry$timestamp), as.list(replicas))
    }
    desired <- rbindlist(rows)

    alive <- 0
    nrows <- vector("list", length(infra$emulation))
    for (i in seq_along(infra$emulation)) {
      entry <- infra$emulation[[i]]
      alive <- alive + length(entry$applied_objects) - length(entry$deleted_objects)
      nrows[[i]] <- list(trace_time = entry$timestamp, nodes = alive)
    }
    nodes <- rbindlist(nrows)

    node_applies <- sum(vapply(infra$emulation,
                               function(e) length(e$applied_objects), numeric(1)))

    list(desired = desired, nodes = nodes, node_applies = node_applies)
  })
}

gt_total_desired <- function(gt) {
  d <- gt$desired
  data.table(trace_time = d$trace_time,
             total = rowSums(as.matrix(d[, names(SERVICES), with = FALSE])))
}

## ---------------------------------------------------------------------------
## Anchoring
## ---------------------------------------------------------------------------

raw_desired_replicas <- function(run) {
  cache_get(paste0(run$label, "::raw_desired"), function() {
    dt <- load_metric(run, "kube_deployment_spec_replicas")
    if (!nrow(dt)) return(data.table())
    dt <- dt[deployment %in% names(SERVICES)]
    w  <- dcast(dt, timestamp ~ deployment, value.var = "value", fun.aggregate = max)
    setorder(w, timestamp)
    w
  })
}

## Wall-clock time of trace t=0.
##
## This cannot be the first deployment sample. The collector queries a window opening `overlap`
## (300s) before the emulation, and the previous run's Deployments survive until this run's reset,
## so every run except the first opens with samples still showing the previous run's final state.
## The anchor is the first sample whose desired-replica vector equals the trace's tick-0 state.
run_anchor <- function(run) {
  cache_get(paste0(run$label, "::anchor"), function() {
    obs <- raw_desired_replicas(run)
    if (!nrow(obs)) stop(run$label, ": no deployment samples, cannot anchor")

    gt   <- load_ground_truth()
    cols <- setdiff(names(obs), "timestamp")
    tick0 <- as.numeric(gt$desired[1, cols, with = FALSE])

    mat     <- as.matrix(obs[, cols, with = FALSE])
    matches <- which(apply(mat, 1, function(r) all(r == tick0)))

    if (length(matches)) {
      anchor <- obs$timestamp[matches[1]]
      method <- "tick0-state"
    } else {
      ## Fallback: the leftover state is whatever the window opens with; the emulation starts at
      ## the first sample that differs from it.
      first   <- mat[1, ]
      differs <- which(apply(mat, 1, function(r) !all(r == first)))
      anchor  <- if (length(differs)) obs$timestamp[differs[1]] else min(obs$timestamp)
      method  <- "first-change (tick-0 state never sampled)"
    }
    list(anchor = anchor, method = method, discarded = sum(obs$timestamp < anchor))
  })
}

anchor_time <- function(run) run_anchor(run)$anchor

align_to_trace_time <- function(run, ts) (as.numeric(ts) - anchor_time(run)) * run$speed

## ---------------------------------------------------------------------------
## Observed series (all trimmed to the run's own emulation)
## ---------------------------------------------------------------------------

observed_desired_replicas <- function(run) {
  obs <- raw_desired_replicas(run)
  if (!nrow(obs)) return(obs)
  obs[timestamp >= anchor_time(run)]
}

observed_pod_phases <- function(run) {
  dt <- load_metric(run, "kube_pod_status_phase")
  if (!nrow(dt)) return(data.table())
  dt <- dt[namespace %in% EMU_NAMESPACES & timestamp >= anchor_time(run)]
  if (!nrow(dt)) return(data.table())
  w <- dcast(dt, timestamp ~ phase, value.var = "value", fun.aggregate = sum, fill = 0)
  setorder(w, timestamp)
  w
}

## `want_phase` deliberately does not share a name with the `phase` column, so the filter below is
## unambiguous inside data.table's scoping.
observed_pods_per_service <- function(run, want_phase = "Running") {
  dt <- load_metric(run, "kube_pod_status_phase")
  if (!nrow(dt)) return(data.table())
  ns2svc <- setNames(names(SERVICES), unname(SERVICES))
  dt <- dt[namespace %in% EMU_NAMESPACES & phase == want_phase & timestamp >= anchor_time(run)]
  if (!nrow(dt)) return(data.table())
  dt[, service := ns2svc[namespace]]
  w <- dcast(dt, timestamp ~ service, value.var = "value", fun.aggregate = sum, fill = 0)
  setorder(w, timestamp)
  w
}

## Distinct KWOK nodes hosting at least one emulated pod. The sweep collected no kube_node_*
## metric, so total node count is not observable; this is the occupied subset.
observed_node_occupancy <- function(run) {
  dt <- load_metric(run, "kube_pod_info")
  if (!nrow(dt)) return(data.table())
  dt <- dt[namespace %in% EMU_NAMESPACES & timestamp >= anchor_time(run)]
  dt <- dt[!is.na(node) & startsWith(as.character(node), "kwok-node-")]
  if (!nrow(dt)) return(data.table())
  out <- dt[, .(nodes = uniqueN(node)), by = timestamp]
  setorder(out, timestamp)
  out
}

## ---------------------------------------------------------------------------
## Small numeric helpers
## ---------------------------------------------------------------------------

## Zero-order-hold lookup of a step function at arbitrary points.
step_interp <- function(x, y, at) {
  ord <- order(x); x <- x[ord]; y <- y[ord]
  idx <- findInterval(at, x)
  out <- rep(NA_real_, length(at))
  ok  <- idx >= 1
  out[ok] <- y[idx[ok]]
  out
}

## Turns a Prometheus counter into a per-second rate. Counter resets (negative deltas) are dropped
## rather than counted as large negative rates.
counter_rate <- function(dt, by = character()) {
  if (!nrow(dt)) return(data.table())
  d <- copy(dt)
  setorderv(d, c(by, "timestamp"))
  if (length(by)) {
    d[, `:=`(dv = value - shift(value), dt_s = timestamp - shift(timestamp)), by = by]
  } else {
    d[, `:=`(dv = value - shift(value), dt_s = timestamp - shift(timestamp))]
  }
  d[, rate := fifelse(!is.na(dv) & !is.na(dt_s) & dv >= 0 & dt_s > 0, dv / dt_s, NA_real_)]
  d
}

ensure_out_dir <- function() {
  dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
  OUT_DIR
}
