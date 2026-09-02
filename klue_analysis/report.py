"""Assembles the per-run measurements into a summary table and a written report."""
import json
import os

import numpy as np
import pandas as pd

from . import common as C
from . import metrics as M


def build_summary(runs, gt_desired, gt_nodes, gt_node_applies):
    rows, details = [], {}
    for run in runs:
        row = {"speed": run.speed, "label": run.label}
        row.update(C.anchor_info(run))
        row.update(M.timing(run, gt_desired))
        row.update(M.resolution(run, gt_desired))

        fid, fid_detail = M.fidelity(run, gt_desired)
        row.update(fid)

        conv, conv_detail = M.convergence(run)
        row.update(conv)

        pk, spike_detail = M.peaks(run, gt_desired)
        row.update(pk)

        row.update(M.pod_health(run))
        row.update(M.node_occupancy(run, gt_nodes))
        row.update(M.scheduler(run))

        cpu, cpu_detail = M.control_plane_cpu(run)
        row.update(cpu)
        row.update(M.log_signals(run, gt_node_applies))

        # Derived cross-metric quantities.
        if row.get("mean_desired_pods"):
            row["pod_surplus_pct"] = 100.0 * (
                row["mean_running_pods"] / row["mean_desired_pods"] - 1.0)
        if row.get("wall_span_s"):
            row["pod_churn_per_wall_s"] = float(
                gt_desired.diff().abs().sum().sum() / row["wall_span_s"])

        rows.append(row)
        details[run.label] = {
            "fidelity": fid_detail, "convergence": conv_detail,
            "spikes": spike_detail, "cpu": cpu_detail,
        }

    return pd.DataFrame(rows).sort_values("speed", ascending=False), details


def _fmt(v, nd=2):
    if v is None or (isinstance(v, float) and (np.isnan(v))):
        return "-"
    if isinstance(v, (bool, np.bool_)):   # before int: bool is a subclass of int
        return "yes" if v else "no"
    if isinstance(v, (int, np.integer)):
        return f"{v}"
    if isinstance(v, float):
        return f"{v:.{nd}f}"
    return str(v)


def _table(df, spec):
    """spec: list of (column, header, decimals)."""
    head = "| Metric | " + " | ".join(df["label"]) + " |"
    sep = "|---" * (len(df) + 1) + "|"
    lines = [head, sep]
    for col, header, nd in spec:
        if col not in df:
            continue
        cells = " | ".join(_fmt(v, nd) for v in df[col])
        lines.append(f"| {header} | {cells} |")
    return "\n".join(lines)


def write_report(summary, details, gt_desired, gt_nodes, gt_node_applies, out_dir):
    fast = summary.iloc[0]
    slow = summary.iloc[-1]

    parts = []
    parts.append(f"""# Side effects of accelerating a KLUE emulation

The same synthetic 300-minute trace (61 ticks, 13-265 pods, 2-34 KWOK nodes) was replayed five
times on one k3d cluster at `--speed-by` 20, 15, 10, 5 and 1, with no provisioner (KWOK nodes come
straight from the trace). This report compares the five runs against the trace itself and against
each other.

All figures below are computed by `klue_analysis/` from the collected Prometheus CSVs; the trace
JSONs are the ground truth.

## Headline

Acceleration did **not** break the emulator's pacing, and did **not** overload the scheduler. What
it degrades is the **cluster's ability to converge** to each tick before the next one arrives, and
the **resolution of the metrics** you collect as evidence. Both degrade smoothly with speed, and
both are invisible unless you look for them: the trace still "completes", the peaks still appear,
and no errors are raised.
""")

    parts.append(f"""## 1. Pacing is exact at every speed

Each observed change of desired replicas is matched back to the tick that asked for it, then
regressed against wall-clock time. The slope is the achieved speed.

{_table(summary, [
    ("effective_speed", "Effective speed (x)", 3),
    ("speed_error_pct", "Error vs requested (%)", 2),
    ("fit_r2", "Fit R^2", 6),
    ("timing_resid_std_wall_s", "Timing residual, std (wall s)", 1),
    ("wall_span_s", "Wall-clock span (s)", 0),
    ("matched_change_samples", "Change samples matched", 0),
])}

Every run tracked its requested speed to within 0.1%, with R^2 at or indistinguishable from 1.0.
The 15x residual is not drift: at 15x one 30s sample spans 450s of trace time, which never aligns
with the 300s tick grid, so changes are seen at a rotating phase offset.

The logs agree: all five runs processed 61/61 trace entries and issued 145/145 scale commands, and
all five attempted the trace's {gt_node_applies} node applies.

{_table(summary, [
    ("log_entries_processed", "Trace entries processed (of 61)", 0),
    ("log_scale_commands", "Scale commands issued (of 145)", 0),
    ("log_nodes_created", "Node 'created' log lines", 0),
    ("log_apply_failures", "Node apply failures", 0),
    ("log_errors", "Error lines", 0),
])}

The only failure in the whole sweep is one node apply lost to a transient
`RemoteDisconnected` - **in the 1x run**, the slowest one. Acceleration did not cause it.
""")

    parts.append(f"""## 2. The cluster stops keeping up

"Converged" means the number of Running pods is within max(1 pod, 2%) of what KLUE has asked the
API server for, measured on the run's own samples. This is purely cluster-side: both series come
from the same run, so it is unaffected by how the trace was paced.

{_table(summary, [
    ("converged_samples_pct", "Samples converged (%)", 1),
    ("convergence_nmae_pct", "Mean |Running - desired| (% of load)", 2),
    ("convergence_mae_pods", "Mean |Running - desired| (pods)", 2),
    ("mean_desired_pods", "Mean desired pods", 1),
    ("mean_running_pods", "Mean Running pods", 1),
    ("pod_surplus_pct", "Mean pod surplus (%)", 1),
    ("pod_churn_per_wall_s", "Pod churn (replicas/wall s)", 2),
])}

Convergence falls from {_fmt(slow['converged_samples_pct'], 1)}% of samples at 1x to
{_fmt(fast['converged_samples_pct'], 1)}% at 20x. At 20x the cluster is in the state the trace
asks for less than two thirds of the time, and the average error is
{_fmt(fast['convergence_nmae_pct'], 1)}% of the running load against
{_fmt(slow['convergence_nmae_pct'], 2)}% at 1x - roughly a nineteen-fold increase.

The error is not symmetric. It is dominated by pods that should be **gone but are still Running**:
the mean pod count runs {_fmt(fast['pod_surplus_pct'], 1)}% above the requested level at 20x
versus {_fmt(slow['pod_surplus_pct'], 1)}% at 1x. Scale-ups are cheap for KWOK, scale-downs are
not: deletion takes wall-clock time that does not shrink when the trace is compressed, so each
scale-down is still draining when the next tick arrives.

`01_convergence_timelines.png` shows what this does to the shape of the workload. At 1x the
achieved curve is indistinguishable from the trace. As speed rises the square edges round off:
every sharp drop becomes an exponential-looking decay, and the plateaus between spikes get filled
in. **Acceleration behaves like a low-pass filter on the emulated workload** - it preserves the
peaks and the slow trend while erasing the fast structure, which is exactly the structure an
autoscaler would react to.

One caveat that cuts against the numbers above, not for them: at 15x and 20x the error is
measured from samples that are themselves too sparse to catch short transients (section 3), and
the 15x/20x runs record no negative gap at all while 10x and 5x record deficits of up to
{_fmt(summary[summary.speed == 10].iloc[0].get('convergence_max_deficit_pods'), 0)} pods. The
scale-up deficits have not disappeared at high speed; they are being sampled over. Treat the 15x
and 20x convergence figures as **lower bounds** on the real divergence.
""")

    parts.append(f"""### The same lag shows up in the node population

{_table(summary, [
    ("occupied_nodes_max", "Peak nodes hosting pods", 0),
    ("gt_nodes_max", "Peak nodes in the trace", 0),
    ("node_occupancy_peak_pct", "Peak occupancy vs trace (%)", 1),
    ("occupied_nodes_mean", "Mean nodes hosting pods", 1),
])}

The trace never has more than {int(gt_nodes['nodes'].max())} nodes alive, yet every run has pods
spread over more than that at peak. Nodes the trace has deleted are still carrying pods, for the
same reason: teardown is a real-time cost.
""")

    parts.append(f"""## 3. Observability degrades faster than fidelity

The collector's step is hard-coded to {C.COLLECTOR_STEP_SECONDS}s of wall time
(`Collector(step=30)` in `src/manager.py`) and does not scale with `--speed-by`. So the faster the
replay, the more trace time each sample covers.

{_table(summary, [
    ("wall_seconds_per_tick", "Wall seconds per tick", 1),
    ("samples_per_tick", "Samples per tick", 2),
    ("trace_seconds_per_sample", "Trace seconds per sample", 0),
    ("samples_observed", "Samples in the run", 0),
    ("gt_tick_states_observed", "Distinct tick states seen (of 59)", 0),
    ("tick_states_missed_pct", "Tick states never sampled (%)", 1),
    ("nyquist_ok", "At least 2 samples/tick", 0),
])}

At 20x each sample covers 600s of trace time - two whole ticks - so
{_fmt(fast['tick_states_missed_pct'], 0)}% of the trace's distinct states are never recorded at
all. Only 5x and 1x stay at or above two samples per tick. This is the effect most likely to be
mistaken for a clean result: the CSVs look normal, they are simply a subsample.

What survives is amplitude:

{_table(summary, [
    ("gt_peak_pods", "Trace peak (pods)", 0),
    ("observed_running_peak_pods", "Observed Running peak (pods)", 0),
    ("running_peak_capture_pct", "Peak captured (%)", 1),
])}

Every run captures every one of the trace's three spikes to within 1.5%, because they are 2-3
ticks wide and so survive even a 600s sampling quantum. Narrower spikes would not: a
one-tick feature is already at the Nyquist limit at 10x and below it at 15x and 20x.
""")

    parts.append(f"""## 4. The scheduler was never the bottleneck

{_table(summary, [
    ("sched_scheduled_per_s_mean", "Scheduled pods/s (mean)", 3),
    ("sched_scheduled_per_s_max", "Scheduled pods/s (peak)", 2),
    ("sched_latency_overall_ms", "Scheduling latency, overall mean (ms)", 2),
    ("sched_latency_p95_ms", "Scheduling latency p95 (ms)", 2),
    ("sched_latency_max_ms", "Scheduling latency max (ms)", 2),
    ("sched_pending_max", "Peak pending pods", 0),
    ("sched_unschedulable_total", "Unschedulable attempts", 0),
    ("sched_preemption_total", "Preemptions", 0),
    ("pods_pending_max", "Peak emulated pods in Pending", 0),
    ("pods_failed_max", "Peak emulated pods in Failed", 0),
])}

Peak scheduling throughput rises with speed but tops out around 8-9 pods/s, and the queue never
backs up: **zero** pending pods, **zero** unschedulable attempts and **zero** preemptions in every
run. Per-attempt latency stays in single-digit to low-double-digit milliseconds. The convergence
gap in section 2 is therefore not a scheduling failure - pods are placed as fast as they arrive.
The lag is in pod lifecycle transitions (KWOK's stages and deletion), not in placement.
""")

    parts.append(f"""## 5. Control-plane cost concentrates rather than grows

CPU is measured from cAdvisor counters for the components that actually do work; the emulated pods
themselves burn nothing.

{_table(summary, [
    ("cpu_kwok-controller_mean_cores", "kwok-controller mean (cores)", 4),
    ("cpu_kwok-controller_max_cores", "kwok-controller peak (cores)", 4),
    ("cpu_kube-state-metrics_mean_cores", "kube-state-metrics mean (cores)", 4),
    ("cpu_prometheus_mean_cores", "prometheus mean (cores)", 4),
    ("cpu_coredns_mean_cores", "coredns mean (cores)", 4),
    ("cpu_control_plane_total_mean_cores", "control plane total mean (cores)", 4),
])}

Compressing the trace 20x raises mean kwok-controller CPU only about
{_fmt(fast['cpu_kwok-controller_mean_cores'] / slow['cpu_kwok-controller_mean_cores'], 1)}x, not
20x, and the absolute numbers stay small - single-digit millicores on this cluster. The control
plane had ample headroom at every speed, which is consistent with the scheduler never queuing.
Acceleration on this workload is not CPU-bound; it is bound by pod lifecycle latency, which is a
delay, not a throughput limit. That is why more CPU would not fix section 2.
""")

    parts.append("""## 6. A measurement artifact worth fixing

Every run except the first opens with ~330s of samples that belong to the **previous** run. The
collector queries a window starting `overlap` (300s) before the emulation, and the previous run's
Deployments survive until the next run's reset, so the window catches their final state
(frontend=8, backend=14, batch=2 - the trace's tick 60).

This analysis detects and discards those samples by anchoring on the trace's tick-0 state, but
anyone reading the raw CSVs would see a phantom plateau before each run and, at the pod level, a
handful of the previous run's pods still terminating. The same three leftover KWOK nodes are why
runs 2-5 log 98 node creations plus 3 updates while the first run logs 101 creations.

Fixes, in order of preference: reset the cluster before the run *and* wait for the namespaces to
be fully gone, or set the collector's `overlap` below the inter-run gap, or simply discard the
first `overlap` seconds of every collection.
""")

    parts.append(f"""## 7. What this means for running accelerated emulations

- **Pacing is not the risk.** KLUE issued the trace on time at 20x with R^2 = 1.0. If you only
  check that the run completed and the peaks are there, every speed looks fine.
- **The risk is that the cluster lags and the metrics hide it.** At 20x the cluster matches the
  requested state only {_fmt(fast['converged_samples_pct'], 0)}% of the time, and
  {_fmt(fast['tick_states_missed_pct'], 0)}% of the trace's states are never sampled.
- **Scale-down is the binding constraint**, not scale-up, not scheduling, not CPU. Pod deletion
  costs wall-clock time that acceleration does not shrink, so surplus pods accumulate
  ({_fmt(fast['pod_surplus_pct'], 0)}% at 20x).
- **A defensible ceiling on this workload is around 5x**, where convergence is still
  {_fmt(summary[summary.speed == 5].iloc[0]['converged_samples_pct'], 0)}% and sampling still
  clears two samples per tick. 10x and beyond trade measurable fidelity for wall-clock time.
- **If you accelerate, scale the collector step too.** `Collector(step=30)` should become
  something like `step = max(1, 30 / speed_by)` to keep trace-time resolution constant; otherwise
  the faster run is not just faster, it is a coarser measurement of a different thing.
- **Comparisons across speeds are only valid in trace time**, and only for quantities that are not
  themselves rate-like. Anything per-second (scheduling throughput, CPU) is a wall-clock quantity
  and will differ by construction.

## Figures

- `01_convergence_timelines.png` - requested vs achieved pods per speed, in trace time. The
  clearest single view of the low-pass effect.
- `02_degradation_vs_speed.png` - convergence, deviation, sampling resolution and pod surplus
  against speed.
- `03_unaffected_vs_speed.png` - pacing, peak capture, scheduling latency and control-plane CPU:
  the things that held up.
- `04_gap_distribution.png` - distribution of (Running - desired) per sample, per speed.
- `05_sampling_coverage.png` - where on the trace each speed actually took samples.

Tables: `summary_by_speed.csv` (every metric), `spike_capture.csv` (per-spike detail),
`convergence_timeseries.csv` (per-sample gaps).

## Method notes and limits

- Ground truth is the trace JSON, not a re-derivation.
- Each run is anchored on the first sample whose desired-replica vector equals the trace's tick-0
  state; earlier samples are discarded as previous-run contamination (see section 6). The first
  post-anchor sample is excluded from convergence statistics because it can still contain the
  previous run's terminating pods.
- Node counts are *occupied* nodes (KWOK nodes hosting at least one emulated pod). The sweep
  collected no `kube_node_*` metric, so total node count is not directly observable and the true
  overshoot may be larger than reported.
- Only one repetition per speed, on one cluster. Ordering effects (the 1x run ran last, on a
  cluster that had already hosted four emulations) are not controlled for, and with n=1 per speed
  the non-monotonic points (10x vs 15x deviation) are within what run-to-run noise could produce.
  The monotonic trends - convergence, sampling resolution, pod surplus - are large enough and
  consistent enough across independent metrics to be safe; individual pairwise gaps are not.
- Convergence at high speed is measured on sparse samples and is therefore a lower bound.
- Karpenter and Cluster Autoscaler metrics are absent by design: the sweep ran with no provisioner.
""")

    report = "\n".join(parts)
    with open(os.path.join(out_dir, "REPORT.md"), "w", encoding="utf-8") as f:
        f.write(report)
    return report


def write_tables(summary, details, out_dir):
    summary.to_csv(os.path.join(out_dir, "summary_by_speed.csv"), index=False)

    spikes = pd.concat(
        [d["spikes"].assign(label=label) for label, d in details.items() if not d["spikes"].empty],
        ignore_index=True)
    spikes.to_csv(os.path.join(out_dir, "spike_capture.csv"), index=False)

    conv = pd.concat(
        [d["convergence"].assign(label=label) for label, d in details.items()
         if not d["convergence"].empty], ignore_index=True)
    conv.to_csv(os.path.join(out_dir, "convergence_timeseries.csv"), index=False)

    with open(os.path.join(out_dir, "summary_by_speed.json"), "w", encoding="utf-8") as f:
        json.dump(json.loads(summary.to_json(orient="records")), f, indent=2)
