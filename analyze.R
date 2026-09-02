#!/usr/bin/env Rscript
##
## Analyses the KLUE speed sweep: what changes when the same trace is replayed faster.
##
## Reads the five runs under ~/klue-speed-sweep-results (pairing each with the metrics directory
## its run.log names), compares them against the trace JSONs, and writes tables, figures and a
## full report into ~/klue-speed-analysis-r/results/.
##
## Usage: ./analyze.R

suppressPackageStartupMessages({
  here <- dirname(normalizePath(sub("^--file=", "",
    grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1])))
  if (is.na(here) || !nzchar(here)) here <- getwd()
  for (f in c("common.R", "metrics.R", "summarise.R", "plots.R", "report.R")) {
    source(file.path(here, "R", f))
  }
})

main <- function() {
  ## arrangeGrob() draws to the default device, which would otherwise leave a stray Rplots.pdf.
  pdf(NULL)
  on.exit(invisible(grDevices::dev.off()), add = TRUE)

  out_dir <- ensure_out_dir()

  runs <- discover_runs()
  cat(sprintf("Found %d runs:\n", length(runs)))
  for (r in runs) {
    a <- run_anchor(r)
    cat(sprintf("  %4s  %-32s (anchor: %s, discarded %d pre-anchor samples)\n",
                r$label, basename(r$metrics_dir), a$method, a$discarded))
  }

  gt <- load_ground_truth()
  tt <- gt_total_desired(gt)
  cat(sprintf("\nGround truth: %d ticks, %d-%d pods, %d-%d nodes, %d node applies\n",
              nrow(gt$desired), min(tt$total), max(tt$total),
              min(gt$nodes$nodes), max(gt$nodes$nodes), gt$node_applies))

  cat("\nMeasuring...\n")
  res <- build_summary(runs, gt)

  cat("Writing tables...\n")
  write_tables(res, out_dir)

  cat("Rendering figures...\n")
  files <- make_all_plots(runs, res$summary, gt, out_dir)
  plot_files <- list(
    workload = files[1], infra = files[2], dynamics = files[3], timelines = files[4],
    degradation = files[5], unaffected = files[6], gap = files[7], sampling = files[8],
    scheduler = files[9], cpu = files[10])

  cat("Writing report...\n")
  write_report(res, gt, out_dir, plot_files)

  cols <- c("label", "effective_speed", "converged_samples_pct", "convergence_nmae_pct",
            "pod_surplus_pct", "samples_per_tick", "tick_states_missed_pct",
            "running_peak_capture_pct", "sched_pending_max")
  cat("\n")
  print(res$summary[, ..cols])

  cat(sprintf("\nWrote results to %s\n", out_dir))
  for (f in sort(list.files(out_dir))) cat("  ", f, "\n", sep = "")
}

main()
