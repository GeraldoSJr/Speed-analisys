## Per-run measurements. Every function takes a run and returns plain values; nothing prints.
##
## The distinction that runs through all of this: "emulation-side" effects are about whether KLUE
## issued the trace on time, "cluster-side" effects are about whether Kubernetes kept up with what
## was issued.

## ---------------------------------------------------------------------------
## Emulation-side: did the trace play at the requested speed?
## ---------------------------------------------------------------------------

## Effective replay speed, by regressing when each desired-state change was observed against the
## trace time the trace asks for it. Measuring the raw first-to-last span instead would understate
## every run by exactly one tick, because the collection window closes before kube-state-metrics
## scrapes the final change. Each desired-replica vector is a near-unique fingerprint of a tick,
## which is what makes observed samples matchable back to trace time without assuming they arrived
## on schedule.
m_timing <- function(run, gt) {
  obs <- observed_desired_replicas(run)
  if (!nrow(obs)) return(list())

  cols <- setdiff(names(obs), "timestamp")
  gtm  <- as.matrix(gt$desired[, cols, with = FALSE])
  keys <- apply(gtm, 1, paste, collapse = "|")

  ## States that occur more than once cannot anchor a time, so drop them entirely.
  dup       <- keys %in% keys[duplicated(keys)]
  state2tt  <- setNames(gt$desired$trace_time[!dup], keys[!dup])

  mat     <- as.matrix(obs[, cols, with = FALSE])
  changed <- c(TRUE, rowSums(abs(diff(mat))) > 0)
  okeys   <- apply(mat, 1, paste, collapse = "|")

  sel   <- changed & okeys %in% names(state2tt)
  wall  <- obs$timestamp[sel]
  trace <- unname(state2tt[okeys[sel]])

  out <- list(requested_speed = run$speed,
              observed_change_samples = sum(changed),
              matched_change_samples  = length(wall))
  if (length(wall) < 3) return(out)

  wall <- wall - wall[1]
  fit  <- lm(wall ~ trace)
  ## R^2 is 1.0 for a run that kept perfect time; lm warns about that, which is not a problem.
  r2    <- suppressWarnings(summary(fit)$r.squared)
  slope <- unname(coef(fit)[2])
  resid <- unname(residuals(fit))

  effective <- if (slope > 0) 1 / slope else NA_real_
  c(out, list(
    effective_speed          = effective,
    speed_error_pct          = 100 * (effective / run$speed - 1),
    fit_r2                   = r2,
    timing_resid_std_wall_s  = sd(resid),
    timing_resid_max_wall_s  = max(abs(resid)),
    timing_resid_std_trace_s = sd(resid) * run$speed,
    wall_span_s              = max(wall),
    trace_span_covered_s     = max(trace) - min(trace),
    sample_quantum_trace_s   = COLLECTOR_STEP_SECONDS * run$speed))
}

## How much of the trace the fixed 30s collector step can see at this speed. Below two samples per
## tick the sampling is at or past the Nyquist limit for a one-tick feature.
m_resolution <- function(run, gt) {
  wall_per_tick    <- TICK_SECONDS / run$speed
  samples_per_tick <- wall_per_tick / COLLECTOR_STEP_SECONDS

  obs  <- observed_desired_replicas(run)
  cols <- setdiff(names(obs), "timestamp")
  gt_states  <- unique(apply(as.matrix(gt$desired[, cols, with = FALSE]), 1, paste, collapse = "|"))
  obs_states <- unique(apply(as.matrix(obs[, cols, with = FALSE]), 1, paste, collapse = "|"))
  seen <- length(intersect(gt_states, obs_states))

  list(wall_seconds_per_tick    = wall_per_tick,
       samples_per_tick         = samples_per_tick,
       trace_seconds_per_sample = COLLECTOR_STEP_SECONDS * run$speed,
       samples_observed         = nrow(obs),
       gt_tick_states           = length(gt_states),
       gt_tick_states_observed  = seen,
       tick_states_missed_pct   = 100 * (1 - seen / length(gt_states)),
       nyquist_ok               = samples_per_tick >= 2)
}

## ---------------------------------------------------------------------------
## Cluster-side: did Kubernetes keep up with what was issued?
## ---------------------------------------------------------------------------

## Gap between what KLUE asked for (spec.replicas) and what was actually Running, on the run's own
## samples. Both series come from the same run, so this is pure cluster-side lag, independent of
## how the trace was paced.
m_convergence <- function(run) {
  desired <- observed_desired_replicas(run)
  running <- observed_pods_per_service(run, "Running")
  if (!nrow(desired) || !nrow(running)) return(list(summary = list(), detail = data.table()))

  cols <- intersect(setdiff(names(desired), "timestamp"), setdiff(names(running), "timestamp"))
  ts   <- intersect(desired$timestamp, running$timestamp)
  ## The anchor sample can still hold pods of the previous run that were terminating when this one
  ## started, so it is not evidence about this run's convergence.
  if (length(ts) > 2) ts <- ts[-1]

  d <- desired[timestamp %in% ts]; setorder(d, timestamp)
  r <- running[timestamp %in% ts]; setorder(r, timestamp)

  d_tot <- rowSums(as.matrix(d[, cols, with = FALSE]))
  r_tot <- rowSums(as.matrix(r[, cols, with = FALSE]))
  gap   <- r_tot - d_tot

  detail <- data.table(timestamp = ts, trace_time = align_to_trace_time(run, ts),
                       desired_total = d_tot, running_total = r_tot, gap = gap)

  tol       <- pmax(1, 0.02 * d_tot)
  converged <- abs(gap) <= tol
  list(summary = list(
         convergence_mae_pods         = mean(abs(gap)),
         convergence_nmae_pct         = 100 * mean(abs(gap)) / mean(d_tot),
         convergence_max_deficit_pods = max(-gap),
         convergence_max_surplus_pods = max(gap),
         converged_samples_pct        = 100 * mean(converged),
         mean_desired_pods            = mean(d_tot),
         mean_running_pods            = mean(r_tot)),
       detail = detail)
}

## Spike fidelity: does the run ever actually run the pods the trace's peaks ask for? A spike the
## emulation issues but the cluster never materialises - or the collector never samples - is
## invisible to anyone reading the collected metrics afterwards.
m_peaks <- function(run, gt) {
  tt      <- gt_total_desired(gt)
  gt_peak <- max(tt$total)

  desired <- observed_desired_replicas(run)
  running <- observed_pods_per_service(run, "Running")
  dcols   <- setdiff(names(desired), "timestamp")
  rcols   <- setdiff(names(running), "timestamp")

  obs_d_peak <- if (nrow(desired)) max(rowSums(as.matrix(desired[, dcols, with = FALSE]))) else NA_real_
  obs_r_peak <- if (nrow(running)) max(rowSums(as.matrix(running[, rcols, with = FALSE]))) else NA_real_

  ## The trace's three sharp frontend spikes, as (start, end) in trace seconds.
  windows <- list(c(13, 15), c(31, 32), c(49, 50))
  rows <- list()
  if (nrow(running)) {
    tau <- align_to_trace_time(run, running$timestamp)
    rt  <- rowSums(as.matrix(running[, rcols, with = FALSE]))
    dt_ <- if (nrow(desired)) rowSums(as.matrix(desired[, dcols, with = FALSE])) else NULL
    for (i in seq_along(windows)) {
      a <- windows[[i]][1] * TICK_SECONDS; b <- windows[[i]][2] * TICK_SECONDS
      m <- tau >= (a - TICK_SECONDS) & tau <= (b + TICK_SECONDS)
      gt_win <- max(tt$total[tt$trace_time >= a & tt$trace_time <= b])
      rows[[i]] <- data.table(
        spike = i, window_trace_s = paste0(a, "-", b), gt_peak_pods = gt_win,
        samples_in_window = sum(m),
        observed_desired_peak = if (!is.null(dt_) && any(m)) max(dt_[m]) else NA_real_,
        observed_running_peak = if (any(m)) max(rt[m]) else NA_real_,
        capture_pct = if (any(m)) 100 * max(rt[m]) / gt_win else NA_real_)
    }
  }

  list(summary = list(
         gt_peak_pods               = gt_peak,
         observed_desired_peak_pods = obs_d_peak,
         observed_running_peak_pods = obs_r_peak,
         running_peak_capture_pct   = 100 * obs_r_peak / gt_peak),
       detail = if (length(rows)) rbindlist(rows) else data.table())
}

m_pod_health <- function(run) {
  ph <- observed_pod_phases(run)
  if (!nrow(ph)) return(list())
  out <- list()
  for (phase in c("Pending", "Failed", "Unknown", "Succeeded")) {
    v <- if (phase %in% names(ph)) ph[[phase]] else 0
    out[[paste0("pods_", tolower(phase), "_max")]]  <- max(v)
    out[[paste0("pods_", tolower(phase), "_mean")]] <- mean(v)
  }
  out
}

m_node_occupancy <- function(run, gt) {
  occ <- observed_node_occupancy(run)
  if (!nrow(occ)) return(list())
  list(occupied_nodes_max       = max(occ$nodes),
       occupied_nodes_mean      = mean(occ$nodes),
       gt_nodes_max             = max(gt$nodes$nodes),
       node_occupancy_peak_pct  = 100 * max(occ$nodes) / max(gt$nodes$nodes))
}

## ---------------------------------------------------------------------------
## Scheduler
## ---------------------------------------------------------------------------

## Rates are per wall-clock second: acceleration compresses the same trace into less real time, so
## the real load on the scheduler is what changes.
m_scheduler <- function(run) {
  out <- list()

  att <- load_metric(run, "scheduler_schedule_attempts_total")
  if (nrow(att)) {
    rated <- counter_rate(att[!is.na(value)], by = "result")
    agg   <- rated[!is.na(rate), .(mean_r = mean(rate), max_r = max(rate)), by = result]
    for (i in seq_len(nrow(agg))) {
      out[[paste0("sched_", agg$result[i], "_per_s_mean")]] <- agg$mean_r[i]
      out[[paste0("sched_", agg$result[i], "_per_s_max")]]  <- agg$max_r[i]
    }
    tot <- att[, .(n = max(value) - min(value)), by = result]
    for (i in seq_len(nrow(tot))) out[[paste0("sched_", tot$result[i], "_total")]] <- tot$n[i]
  }

  pend <- load_metric(run, "scheduler_pending_pods")
  if (nrow(pend)) {
    per_ts <- pend[, .(v = sum(value)), by = timestamp]
    out$sched_pending_max  <- max(per_ts$v)
    out$sched_pending_mean <- mean(per_ts$v)
  }

  s_sum <- load_metric(run, "scheduler_scheduling_attempt_duration_seconds_sum")
  s_cnt <- load_metric(run, "scheduler_scheduling_attempt_duration_seconds_count")
  if (nrow(s_sum) && nrow(s_cnt)) {
    a <- s_sum[result == "scheduled", .(s = sum(value)), by = timestamp][order(timestamp)]
    b <- s_cnt[result == "scheduled", .(n = sum(value)), by = timestamp][order(timestamp)]
    m <- merge(a, b, by = "timestamp")
    m[, `:=`(ds = s - shift(s), dn = n - shift(n))]
    lat <- m[!is.na(dn) & dn > 0 & ds >= 0, ds / dn]
    if (length(lat)) {
      out$sched_latency_mean_ms <- 1000 * mean(lat)
      out$sched_latency_p95_ms  <- 1000 * unname(quantile(lat, 0.95))
      out$sched_latency_max_ms  <- 1000 * max(lat)
    }
    if (nrow(m) > 1 && (max(m$n) - min(m$n)) > 0) {
      out$sched_latency_overall_ms <- 1000 * (max(m$s) - min(m$s)) / (max(m$n) - min(m$n))
    }
  }

  inc <- load_metric(run, "scheduler_queue_incoming_pods_total")
  if (nrow(inc)) {
    by <- if ("event" %in% names(inc)) "event" else character()
    rated <- counter_rate(inc[!is.na(value)], by = by)
    if (nrow(rated) && any(!is.na(rated$rate))) {
      out$sched_incoming_per_s_mean <- mean(rated$rate, na.rm = TRUE)
      out$sched_incoming_per_s_max  <- max(rated$rate, na.rm = TRUE)
    }
  }

  pre <- load_metric(run, "scheduler_preemption_attempts_total")
  if (nrow(pre)) out$sched_preemption_total <- max(pre$value) - min(pre$value)

  out
}

## ---------------------------------------------------------------------------
## Control-plane cost
## ---------------------------------------------------------------------------

CONTROL_PLANE <- c(`kwok-controller`    = "^kwok-controller-",
                   `kube-state-metrics` = "^kube-state-metrics-",
                   prometheus           = "^prometheus-k8s-",
                   coredns              = "^coredns-")

## CPU (cores) of the components that carry the emulation. cAdvisor exports one series per named
## container plus unnamed cgroup rollups for the pod sandbox; keeping the named containers and
## summing them counts each pod exactly once. Differencing across mixed series instead inflates
## the rate by orders of magnitude, since consecutive rows come from different counters.
m_control_plane_cpu <- function(run) {
  dt <- load_metric(run, "container_cpu_usage_seconds_total")
  if (!nrow(dt)) return(list(summary = list(), detail = data.table()))

  dt <- dt[!is.na(value) & !is.na(pod)]
  if ("container" %in% names(dt)) {
    dt <- dt[!is.na(container) & trimws(as.character(container)) != "" &
               as.character(container) != "POD"]
  }
  if (!nrow(dt)) return(list(summary = list(), detail = data.table()))

  out <- list(); frames <- list()
  for (nm in names(CONTROL_PLANE)) {
    sub <- dt[grepl(CONTROL_PLANE[[nm]], as.character(pod))]
    if (!nrow(sub)) next
    rated  <- counter_rate(sub, by = c("pod", "container"))
    per_ts <- rated[!is.na(rate), .(cores = sum(rate)), by = timestamp][order(timestamp)]
    if (!nrow(per_ts)) next
    out[[paste0("cpu_", nm, "_mean_cores")]] <- mean(per_ts$cores)
    out[[paste0("cpu_", nm, "_max_cores")]]  <- max(per_ts$cores)
    frames[[nm]] <- data.table(component = nm, timestamp = per_ts$timestamp,
                               trace_time = align_to_trace_time(run, per_ts$timestamp),
                               cores = per_ts$cores)
  }
  if (length(out)) {
    means <- unlist(out[grepl("_mean_cores$", names(out))])
    out$cpu_control_plane_total_mean_cores <- sum(means)
  }
  list(summary = out, detail = if (length(frames)) rbindlist(frames) else data.table())
}

## ---------------------------------------------------------------------------
## Log-derived signals
## ---------------------------------------------------------------------------

m_log_signals <- function(run, gt_node_applies) {
  txt <- read_log(run)
  cnt <- function(p) length(str_extract_all(txt, p)[[1]])
  errors <- str_extract_all(txt, "\\[ERROR\\][^\\n]*")[[1]]
  list(log_entries_processed = cnt("Processing entry at timestamp"),
       log_scale_commands    = cnt("Scaled deployment"),
       log_nodes_created     = cnt("Node \\S+ created"),
       log_nodes_updated     = cnt("Node \\S+ updated"),
       log_nodes_deleted     = cnt("Node \\S+ deleted"),
       log_node_applies_seen = cnt("Node \\S+ created") + cnt("Node \\S+ updated"),
       log_gt_node_applies   = gt_node_applies,
       log_errors            = length(errors),
       log_apply_failures    = sum(grepl("Failed to apply", errors)),
       log_delete_failures   = sum(grepl("Failed to delete", errors)),
       log_error_text        = paste(substr(trimws(errors), 1, 150), collapse = " | "))
}
