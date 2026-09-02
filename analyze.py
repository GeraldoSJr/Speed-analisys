#!/usr/bin/env python3
"""
Analyses the KLUE speed sweep: what changes when the same trace is replayed faster.

Reads the five runs under ~/klue-speed-sweep-results (pairing each with the metrics directory its
run.log names), compares them against the trace JSONs, and writes tables, figures and a written
report into ~/klue-speed-analysis/results/.

Usage: ./analyze.py
"""
import json
import os
import sys
import warnings

warnings.filterwarnings("ignore", category=FutureWarning)
warnings.filterwarnings("ignore", category=UserWarning)

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from klue_analysis import common as C          # noqa: E402
from klue_analysis import plots, report        # noqa: E402


def main():
    out_dir = C.ensure_out_dir()

    runs = C.discover_runs()
    print(f"Found {len(runs)} runs:")
    for r in runs:
        info = C.anchor_info(r)
        print(f"  {r.label:>4}  {os.path.basename(r.metrics_dir)}"
              f"  (anchor: {info['anchor_method']}, "
              f"discarded {info['discarded_pre_anchor_samples']} pre-anchor samples)")

    gt_desired, gt_nodes = C.load_ground_truth()
    with open(os.path.join(C.TRACE_DIR, "infrastructure_description.json"), encoding="utf-8") as f:
        gt_node_applies = sum(len(e.get("applied_objects", [])) for e in json.load(f)["emulation"])

    print(f"\nGround truth: {len(gt_desired)} ticks, "
          f"{int(gt_desired.sum(axis=1).min())}-{int(gt_desired.sum(axis=1).max())} pods, "
          f"{int(gt_nodes['nodes'].min())}-{int(gt_nodes['nodes'].max())} nodes, "
          f"{gt_node_applies} node applies")

    print("\nMeasuring...")
    summary, details = report.build_summary(runs, gt_desired, gt_nodes, gt_node_applies)

    print("Writing tables...")
    report.write_tables(summary, details, out_dir)

    print("Rendering figures...")
    plots.make_all(runs, summary, gt_desired, out_dir)

    print("Writing report...")
    report.write_report(summary, details, gt_desired, gt_nodes, gt_node_applies, out_dir)

    cols = ["label", "effective_speed", "converged_samples_pct", "convergence_nmae_pct",
            "pod_surplus_pct", "samples_per_tick", "tick_states_missed_pct",
            "running_peak_capture_pct", "sched_pending_max"]
    print("\n" + summary[[c for c in cols if c in summary]].to_string(index=False))
    print(f"\nWrote results to {out_dir}")
    for name in sorted(os.listdir(out_dir)):
        print(f"  {name}")


if __name__ == "__main__":
    main()
