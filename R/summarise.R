## Collapses the per-run measurements into one row per speed.

suppressPackageStartupMessages({ library(data.table); library(dplyr) })

build_summary <- function(runs, gt) {
  rows <- list(); details <- list()

  for (r in runs) {
    a  <- run_anchor(r)
    cv <- m_convergence(r)
    pk <- m_peaks(r, gt)
    cp <- m_control_plane_cpu(r)

    row <- c(list(speed = r$speed, label = r$label,
                  anchor_method = a$method, discarded_pre_anchor_samples = a$discarded),
             m_timing(r, gt), m_resolution(r, gt), cv$summary, pk$summary,
             m_pod_health(r), m_node_occupancy(r, gt), m_scheduler(r), cp$summary,
             m_log_signals(r, gt$node_applies))

    ## Cross-metric derivations.
    if (!is.null(row$mean_desired_pods) && row$mean_desired_pods > 0) {
      row$pod_surplus_pct <- 100 * (row$mean_running_pods / row$mean_desired_pods - 1)
    }
    if (!is.null(row$wall_span_s) && row$wall_span_s > 0) {
      tt <- gt_total_desired(gt)
      row$pod_churn_per_wall_s <- sum(abs(diff(tt$total))) / row$wall_span_s
    }

    rows[[r$label]] <- as.data.table(row)
    details[[r$label]] <- list(convergence = cv$detail, spikes = pk$detail, cpu = cp$detail)
  }

  summary_df <- rbindlist(rows, fill = TRUE)
  setorder(summary_df, -speed)
  list(summary = summary_df, details = details)
}

write_tables <- function(res, out_dir) {
  fwrite(res$summary, file.path(out_dir, "summary_by_speed.csv"))

  spikes <- rbindlist(lapply(names(res$details), function(l) {
    d <- res$details[[l]]$spikes
    if (!nrow(d)) return(NULL)
    cbind(label = l, d)
  }), fill = TRUE)
  if (nrow(spikes)) fwrite(spikes, file.path(out_dir, "spike_capture.csv"))

  conv <- rbindlist(lapply(names(res$details), function(l) {
    d <- res$details[[l]]$convergence
    if (!nrow(d)) return(NULL)
    cbind(label = l, d)
  }), fill = TRUE)
  if (nrow(conv)) fwrite(conv, file.path(out_dir, "convergence_timeseries.csv"))

  invisible(NULL)
}
