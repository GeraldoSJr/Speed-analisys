## Figures for the speed-sweep report. Every function writes a PNG into out_dir and returns its
## filename.

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(scales)
  library(gridExtra)
})

SPEED_COLORS <- c(`20x` = "#e34948", `15x` = "#eb6834", `10x` = "#eda100",
                  `5x`  = "#1baf7a", `1x`  = "#2a78d6")
SERVICE_COLORS <- c(frontend = "#2a78d6", backend = "#eb6834", `batch-worker` = "#1baf7a")

theme_klue <- function(base = 11) {
  theme_minimal(base_size = base) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "#e1e0d9", linewidth = 0.35),
          plot.title    = element_text(size = base + 1, colour = "#0b0b0b", face = "plain"),
          plot.subtitle = element_text(size = base - 1, colour = "#52514e"),
          axis.title    = element_text(size = base - 2, colour = "#52514e"),
          axis.text     = element_text(size = base - 3, colour = "#52514e"),
          legend.title  = element_blank(),
          legend.text   = element_text(size = base - 3, colour = "#52514e"),
          legend.position = "top",
          strip.text    = element_text(size = base - 2, colour = "#0b0b0b"),
          plot.background = element_rect(fill = "white", colour = NA))
}

save_plot <- function(p, out_dir, name, w = 11, h = 5) {
  path <- file.path(out_dir, name)
  ggsave(path, p, width = w, height = h, dpi = 140, bg = "white")
  name
}

speed_levels <- function(labels) factor(labels, levels = c("1x", "5x", "10x", "15x", "20x"))

## ---------------------------------------------------------------------------
## Part 1 - the input trace
## ---------------------------------------------------------------------------

plot_trace_workload <- function(gt, out_dir) {
  long <- gt$desired %>%
    pivot_longer(-trace_time, names_to = "service", values_to = "pods") %>%
    mutate(minute = trace_time / 60)
  total <- gt_total_desired(gt) %>% mutate(minute = trace_time / 60)

  p <- ggplot() +
    geom_step(data = total, aes(minute, total, linetype = "total"), colour = "#52514e",
              linewidth = 0.7) +
    geom_step(data = long, aes(minute, pods, colour = service), linewidth = 0.8) +
    scale_colour_manual(values = SERVICE_COLORS) +
    scale_linetype_manual(values = c(total = "dashed")) +
    scale_x_continuous(breaks = seq(0, 300, 30)) +
    labs(title = "Input trace: desired pods per Deployment over 300 minutes",
         subtitle = "Three services with distinct shapes; dashed line is the cluster-wide total",
         x = "trace time (minutes)", y = "desired replicas") +
    theme_klue()
  save_plot(p, out_dir, "10_input_workload.png", 11, 5)
}

plot_trace_infra <- function(gt, out_dir) {
  total <- gt_total_desired(gt) %>% mutate(minute = trace_time / 60)
  nodes <- gt$nodes %>% mutate(minute = trace_time / 60)
  joined <- total %>% inner_join(nodes, by = "trace_time") %>%
    mutate(pods_per_node = total / nodes)

  p1 <- ggplot(joined, aes(minute.x, nodes)) +
    geom_step(colour = "#4a3aa7", linewidth = 0.8) +
    scale_x_continuous(breaks = seq(0, 300, 60)) +
    labs(title = "Nodes provisioned by the trace", x = "trace time (minutes)",
         y = "KWOK nodes alive") + theme_klue()

  p2 <- ggplot(joined, aes(minute.x, pods_per_node)) +
    geom_step(colour = "#1baf7a", linewidth = 0.8) +
    geom_hline(yintercept = 8, linetype = "dashed", colour = "#898781") +
    scale_x_continuous(breaks = seq(0, 300, 60)) +
    labs(title = "Pod density (dashed = the 8 pods/node design target)",
         x = "trace time (minutes)", y = "pods per node") + theme_klue()

  g <- arrangeGrob(p1, p2, ncol = 2)
  ggsave(file.path(out_dir, "11_input_infrastructure.png"), g, width = 11, height = 4,
         dpi = 140, bg = "white")
  "11_input_infrastructure.png"
}

plot_trace_dynamics <- function(gt, out_dir) {
  total <- gt_total_desired(gt)
  d <- data.frame(trace_time = total$trace_time[-1],
                  delta = diff(total$total)) %>% mutate(minute = trace_time / 60)

  p1 <- ggplot(d, aes(minute, delta)) +
    geom_col(aes(fill = delta > 0), width = 4) +
    scale_fill_manual(values = c(`TRUE` = "#1baf7a", `FALSE` = "#e34948"),
                      labels = c(`TRUE` = "scale up", `FALSE` = "scale down")) +
    scale_x_continuous(breaks = seq(0, 300, 60)) +
    labs(title = "Per-tick change in total pods", x = "trace time (minutes)",
         y = "delta pods vs previous tick") + theme_klue()

  p2 <- ggplot(d, aes(x = delta)) +
    geom_histogram(bins = 30, fill = "#4a3aa7", colour = "white", linewidth = 0.2) +
    labs(title = "Distribution of per-tick changes",
         subtitle = "Long tails are the spikes and their collapses",
         x = "delta pods", y = "ticks") + theme_klue()

  g <- arrangeGrob(p1, p2, ncol = 2, widths = c(1.5, 1))
  ggsave(file.path(out_dir, "12_input_dynamics.png"), g, width = 11, height = 4,
         dpi = 140, bg = "white")
  "12_input_dynamics.png"
}

## ---------------------------------------------------------------------------
## Part 2 - results
## ---------------------------------------------------------------------------

plot_convergence_timelines <- function(runs, gt, out_dir) {
  total <- gt_total_desired(gt) %>% mutate(minute = trace_time / 60)

  frames <- lapply(runs, function(r) {
    d <- m_convergence(r)$detail
    if (!nrow(d)) return(NULL)
    data.frame(label = r$label, minute = d$trace_time / 60,
               running = d$running_total, desired = d$desired_total)
  })
  df <- bind_rows(frames) %>% mutate(label = speed_levels(label))

  gtrep <- do.call(rbind, lapply(levels(df$label), function(l)
    transform(total, label = l)))
  gtrep$label <- speed_levels(gtrep$label)

  p <- ggplot() +
    geom_ribbon(data = df, aes(minute, ymin = pmin(running, desired),
                               ymax = pmax(running, desired), fill = label), alpha = 0.25) +
    geom_step(data = gtrep, aes(minute, total), colour = "#898781", linewidth = 0.5) +
    geom_line(data = df, aes(minute, running, colour = label), linewidth = 0.7) +
    facet_wrap(~label, ncol = 1, strip.position = "right") +
    scale_colour_manual(values = SPEED_COLORS, guide = "none") +
    scale_fill_manual(values = SPEED_COLORS, guide = "none") +
    scale_x_continuous(breaks = seq(0, 300, 30)) +
    labs(title = "Requested vs achieved pod count, aligned to trace time",
         subtitle = "Grey step = the trace. Coloured line = pods actually Running. Shaded = the gap.",
         x = "trace time (minutes)", y = "pods") +
    theme_klue()
  save_plot(p, out_dir, "20_convergence_timelines.png", 11, 10)
}

plot_degradation <- function(summary_df, out_dir) {
  s <- summary_df %>% arrange(speed)
  base <- function(y, title, ylab, colour) {
    ggplot(s, aes(speed, .data[[y]])) +
      geom_line(colour = colour, linewidth = 0.8) +
      geom_point(colour = colour, size = 2.2) +
      scale_x_continuous(breaks = c(1, 5, 10, 15, 20)) +
      labs(title = title, x = "requested speed (x)", y = ylab) + theme_klue()
  }
  p1 <- base("converged_samples_pct", "Cluster matches the requested state less often",
             "samples converged (%)", "#2a78d6") + ylim(0, 100)
  p2 <- base("convergence_nmae_pct", "Mean deviation from the requested state",
             "|Running - desired| (% of load)", "#e34948") + ylim(0, NA)
  p3 <- base("samples_per_tick", "Metric resolution collapses", "samples per trace tick",
             "#1baf7a") +
    geom_hline(yintercept = 2, linetype = "dashed", colour = "#898781") +
    scale_y_log10()
  p4 <- base("pod_surplus_pct", "Pods linger after scale-down", "mean pod surplus (%)",
             "#eb6834") + geom_hline(yintercept = 0, colour = "#898781")

  g <- arrangeGrob(p1, p2, p3, p4, ncol = 2,
                   top = grid::textGrob("What acceleration costs",
                                        gp = grid::gpar(fontsize = 13, col = "#0b0b0b")))
  ggsave(file.path(out_dir, "21_degradation_vs_speed.png"), g, width = 11, height = 7.5,
         dpi = 140, bg = "white")
  "21_degradation_vs_speed.png"
}

plot_unaffected <- function(summary_df, out_dir) {
  s <- summary_df %>% arrange(speed)

  p1 <- ggplot(s, aes(speed, effective_speed)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "#898781") +
    geom_line(colour = "#2a78d6", linewidth = 0.8) + geom_point(colour = "#2a78d6", size = 2.2) +
    scale_x_continuous(breaks = c(1, 5, 10, 15, 20)) +
    labs(title = "Pacing is exact (dashed = ideal)", x = "requested speed (x)",
         y = "effective speed (x)") + theme_klue()

  p2 <- ggplot(s, aes(speed, running_peak_capture_pct)) +
    geom_hline(yintercept = 100, linetype = "dashed", colour = "#898781") +
    geom_line(colour = "#1baf7a", linewidth = 0.8) + geom_point(colour = "#1baf7a", size = 2.2) +
    scale_x_continuous(breaks = c(1, 5, 10, 15, 20)) + ylim(90, 110) +
    labs(title = "Spike amplitude survives", x = "requested speed (x)",
         y = "peak captured (%)") + theme_klue()

  lat <- s %>% select(speed, mean = sched_latency_overall_ms, p95 = sched_latency_p95_ms) %>%
    pivot_longer(-speed, names_to = "stat", values_to = "ms")
  p3 <- ggplot(lat, aes(speed, ms, colour = stat)) +
    geom_line(linewidth = 0.8) + geom_point(size = 2.2) +
    scale_colour_manual(values = c(mean = "#4a3aa7", p95 = "#9085e9")) +
    scale_x_continuous(breaks = c(1, 5, 10, 15, 20)) + ylim(0, NA) +
    labs(title = "Scheduling latency stays low", x = "requested speed (x)",
         y = "latency (ms)") + theme_klue()

  cpu_cols <- grep("^cpu_.*_mean_cores$", names(s), value = TRUE)
  cpu_cols <- setdiff(cpu_cols, "cpu_control_plane_total_mean_cores")
  cpu <- s %>% select(speed, all_of(cpu_cols)) %>%
    pivot_longer(-speed, names_to = "component", values_to = "cores") %>%
    mutate(component = gsub("^cpu_|_mean_cores$", "", component), millicores = 1000 * cores)
  p4 <- ggplot(cpu, aes(speed, millicores, colour = component)) +
    geom_line(linewidth = 0.8) + geom_point(size = 2) +
    scale_colour_manual(values = c(`kwok-controller` = "#e34948",
                                   `kube-state-metrics` = "#eda100",
                                   prometheus = "#2a78d6", coredns = "#1baf7a")) +
    scale_x_continuous(breaks = c(1, 5, 10, 15, 20)) +
    labs(title = "Control-plane CPU has headroom", x = "requested speed (x)",
         y = "mean CPU (millicores)") + theme_klue()

  g <- arrangeGrob(p1, p2, p3, p4, ncol = 2,
                   top = grid::textGrob("What acceleration did not break",
                                        gp = grid::gpar(fontsize = 13, col = "#0b0b0b")))
  ggsave(file.path(out_dir, "22_unaffected_vs_speed.png"), g, width = 11, height = 7.5,
         dpi = 140, bg = "white")
  "22_unaffected_vs_speed.png"
}

plot_gap_distribution <- function(runs, out_dir) {
  df <- bind_rows(lapply(runs, function(r) {
    d <- m_convergence(r)$detail
    if (!nrow(d)) return(NULL)
    data.frame(label = r$label, gap = d$gap)
  })) %>% mutate(label = speed_levels(label))

  p <- ggplot(df, aes(label, gap, fill = label)) +
    geom_hline(yintercept = 0, colour = "#898781") +
    geom_boxplot(alpha = 0.35, outlier.size = 0.5, outlier.colour = "#898781", width = 0.55) +
    scale_fill_manual(values = SPEED_COLORS, guide = "none") +
    labs(title = "Distribution of (Running - desired) per sample",
         subtitle = "Above zero means pods that the trace has already scaled away but are still Running",
         x = "requested speed", y = "pod gap") + theme_klue()
  save_plot(p, out_dir, "23_gap_distribution.png", 11, 4.5)
}

plot_sampling_coverage <- function(runs, gt, out_dir) {
  total <- gt_total_desired(gt) %>% mutate(minute = trace_time / 60)
  pts <- bind_rows(lapply(runs, function(r) {
    obs <- observed_desired_replicas(r)
    if (!nrow(obs)) return(NULL)
    cols <- setdiff(names(obs), "timestamp")
    data.frame(label = r$label,
               minute = align_to_trace_time(r, obs$timestamp) / 60,
               pods = rowSums(as.matrix(obs[, cols, with = FALSE])))
  })) %>% mutate(label = speed_levels(label))

  p <- ggplot() +
    geom_step(data = total, aes(minute, total), colour = "#c3c2b7", linewidth = 0.5) +
    geom_point(data = pts, aes(minute, pods, colour = label), size = 0.9, alpha = 0.9) +
    facet_wrap(~label, ncol = 1, strip.position = "right") +
    scale_colour_manual(values = SPEED_COLORS, guide = "none") +
    scale_x_continuous(breaks = seq(0, 300, 30)) +
    labs(title = "Where each speed actually sampled the trace",
         subtitle = "Grey = the trace. Dots = collector samples. Fewer dots means more of the trace was never recorded.",
         x = "trace time (minutes)", y = "desired pods") + theme_klue()
  save_plot(p, out_dir, "24_sampling_coverage.png", 11, 9)
}

plot_scheduler <- function(summary_df, out_dir) {
  s <- summary_df %>% arrange(speed)
  thr <- s %>% select(speed, mean = sched_scheduled_per_s_mean,
                      peak = sched_scheduled_per_s_max) %>%
    pivot_longer(-speed, names_to = "stat", values_to = "pods_per_s")

  p1 <- ggplot(thr, aes(speed, pods_per_s, colour = stat)) +
    geom_line(linewidth = 0.8) + geom_point(size = 2.2) +
    scale_colour_manual(values = c(mean = "#2a78d6", peak = "#e34948")) +
    scale_x_continuous(breaks = c(1, 5, 10, 15, 20)) +
    labs(title = "Scheduling throughput (per wall-clock second)",
         x = "requested speed (x)", y = "pods scheduled / s") + theme_klue()

  q <- s %>% select(speed, pending = sched_pending_max,
                    unschedulable = sched_unschedulable_total,
                    preemptions = sched_preemption_total) %>%
    pivot_longer(-speed, names_to = "signal", values_to = "count")
  p2 <- ggplot(q, aes(speed, count, colour = signal)) +
    geom_line(linewidth = 0.8) + geom_point(size = 2.2) +
    scale_x_continuous(breaks = c(1, 5, 10, 15, 20)) +
    coord_cartesian(ylim = c(-0.5, 1)) +
    labs(title = "Queue pressure: flat zero at every speed",
         x = "requested speed (x)", y = "count") + theme_klue()

  g <- arrangeGrob(p1, p2, ncol = 2)
  ggsave(file.path(out_dir, "25_scheduler.png"), g, width = 11, height = 4, dpi = 140,
         bg = "white")
  "25_scheduler.png"
}

plot_cpu_timeline <- function(runs, out_dir) {
  df <- bind_rows(lapply(runs, function(r) {
    d <- m_control_plane_cpu(r)$detail
    if (!nrow(d)) return(NULL)
    data.frame(label = r$label, minute = d$trace_time / 60,
               component = d$component, millicores = 1000 * d$cores)
  })) %>% mutate(label = speed_levels(label))

  p <- ggplot(df, aes(minute, millicores, colour = component)) +
    geom_line(linewidth = 0.45, alpha = 0.9) +
    facet_wrap(~label, ncol = 1, scales = "free_y", strip.position = "right") +
    scale_colour_manual(values = c(`kwok-controller` = "#e34948",
                                   `kube-state-metrics` = "#eda100",
                                   prometheus = "#2a78d6", coredns = "#1baf7a")) +
    scale_x_continuous(breaks = seq(0, 300, 30)) +
    labs(title = "Control-plane CPU through the run, aligned to trace time",
         x = "trace time (minutes)", y = "CPU (millicores)") + theme_klue()
  save_plot(p, out_dir, "26_cpu_timeline.png", 11, 9)
}

make_all_plots <- function(runs, summary_df, gt, out_dir) {
  c(plot_trace_workload(gt, out_dir),
    plot_trace_infra(gt, out_dir),
    plot_trace_dynamics(gt, out_dir),
    plot_convergence_timelines(runs, gt, out_dir),
    plot_degradation(summary_df, out_dir),
    plot_unaffected(summary_df, out_dir),
    plot_gap_distribution(runs, out_dir),
    plot_sampling_coverage(runs, gt, out_dir),
    plot_scheduler(summary_df, out_dir),
    plot_cpu_timeline(runs, out_dir))
}
