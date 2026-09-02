"""Figures for the speed-sweep analysis. Writes PNGs, returns nothing."""
import os

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np

from . import common as C
from . import metrics as M

SPEED_COLORS = {20.0: "#e34948", 15.0: "#eb6834", 10.0: "#eda100", 5.0: "#1baf7a", 1.0: "#2a78d6"}
GRID = dict(color="#e1e0d9", linewidth=0.8)


def _style(ax, title, xlabel, ylabel):
    ax.set_title(title, fontsize=11, color="#0b0b0b")
    ax.set_xlabel(xlabel, fontsize=9, color="#52514e")
    ax.set_ylabel(ylabel, fontsize=9, color="#52514e")
    ax.grid(True, **GRID)
    ax.set_axisbelow(True)
    for s in ("top", "right"):
        ax.spines[s].set_visible(False)
    ax.tick_params(labelsize=8, colors="#52514e")


def plot_convergence_timelines(runs, gt_desired, out_dir):
    """Desired vs actually-Running pods, in trace time, one panel per speed."""
    fig, axes = plt.subplots(len(runs), 1, figsize=(11, 2.1 * len(runs)), sharex=True)
    gt_total = gt_desired.sum(axis=1)

    for ax, run in zip(np.atleast_1d(axes), runs):
        _, conv = M.convergence(run)
        ax.step(gt_total.index, gt_total.values, where="post", color="#898781",
                linewidth=1.2, label="trace (ground truth)")
        if not conv.empty:
            ax.plot(conv["trace_time"], conv["running_total"], color=SPEED_COLORS.get(run.speed, "#2a78d6"),
                    linewidth=1.6, label=f"Running @ {run.label}")
            ax.fill_between(conv["trace_time"], conv["desired_total"], conv["running_total"],
                            color=SPEED_COLORS.get(run.speed, "#2a78d6"), alpha=0.18, linewidth=0)
        _style(ax, "", "", "pods")
        ax.legend(fontsize=8, frameon=False, loc="upper left", ncol=2)
        ax.set_ylabel(f"{run.label}\npods", fontsize=9, color="#52514e")

    np.atleast_1d(axes)[-1].set_xlabel("trace time (s)", fontsize=9, color="#52514e")
    fig.suptitle("Requested vs achieved pod count, aligned to trace time",
                 fontsize=12, color="#0b0b0b")
    fig.tight_layout(rect=[0, 0, 1, 0.98])
    fig.savefig(os.path.join(out_dir, "01_convergence_timelines.png"), dpi=140,
                facecolor="white")
    plt.close(fig)


def plot_degradation(summary, out_dir):
    """The headline curves: what gets worse as speed rises."""
    fig, axes = plt.subplots(2, 2, figsize=(11, 7))
    s = summary.sort_values("speed")
    x = s["speed"]

    ax = axes[0][0]
    ax.plot(x, s["converged_samples_pct"], "o-", color="#2a78d6", linewidth=2)
    _style(ax, "Cluster matches the requested state less often", "speed (x)", "samples converged (%)")
    ax.set_ylim(0, 105)

    ax = axes[0][1]
    ax.plot(x, s["convergence_nmae_pct"], "o-", color="#e34948", linewidth=2)
    _style(ax, "Mean deviation from the requested state", "speed (x)", "|Running - desired| (% of load)")
    ax.set_ylim(bottom=0)

    ax = axes[1][0]
    ax.plot(x, s["samples_per_tick"], "o-", color="#1baf7a", linewidth=2, label="samples per tick")
    ax.axhline(2.0, color="#898781", linestyle="--", linewidth=1.2, label="Nyquist floor (2/tick)")
    _style(ax, "Metric resolution collapses", "speed (x)", "samples per trace tick")
    ax.set_yscale("log")
    ax.legend(fontsize=8, frameon=False)

    ax = axes[1][1]
    ax.plot(x, s["pod_surplus_pct"], "o-", color="#eb6834", linewidth=2)
    ax.axhline(0, color="#898781", linewidth=1)
    _style(ax, "Pods linger after scale-down", "speed (x)", "mean pod surplus (%)")

    fig.suptitle("What acceleration costs", fontsize=12, color="#0b0b0b")
    fig.tight_layout(rect=[0, 0, 1, 0.96])
    fig.savefig(os.path.join(out_dir, "02_degradation_vs_speed.png"), dpi=140, facecolor="white")
    plt.close(fig)


def plot_unaffected(summary, out_dir):
    """The controls: what acceleration did not break."""
    fig, axes = plt.subplots(2, 2, figsize=(11, 7))
    s = summary.sort_values("speed")
    x = s["speed"]

    ax = axes[0][0]
    ax.plot(x, s["effective_speed"], "o-", color="#2a78d6", linewidth=2, label="achieved")
    ax.plot(x, x, "--", color="#898781", linewidth=1.2, label="requested")
    _style(ax, "Pacing is exact", "requested speed (x)", "effective speed (x)")
    ax.legend(fontsize=8, frameon=False)

    ax = axes[0][1]
    ax.plot(x, s["running_peak_capture_pct"], "o-", color="#1baf7a", linewidth=2)
    ax.axhline(100, color="#898781", linestyle="--", linewidth=1.2)
    _style(ax, "Spike amplitude survives", "speed (x)", "peak captured (%)")
    ax.set_ylim(90, 110)

    ax = axes[1][0]
    if "sched_latency_overall_ms" in s:
        ax.plot(x, s["sched_latency_overall_ms"], "o-", color="#4a3aa7", linewidth=2, label="mean")
    if "sched_latency_p95_ms" in s:
        ax.plot(x, s["sched_latency_p95_ms"], "s--", color="#9085e9", linewidth=1.5, label="p95")
    _style(ax, "Scheduling latency stays low", "speed (x)", "latency (ms)")
    ax.set_ylim(bottom=0)
    ax.legend(fontsize=8, frameon=False)

    ax = axes[1][1]
    for comp, color in [("kwok-controller", "#e34948"), ("kube-state-metrics", "#eda100"),
                        ("prometheus", "#2a78d6"), ("coredns", "#1baf7a")]:
        col = f"cpu_{comp}_mean_cores"
        if col in s:
            ax.plot(x, 1000 * s[col], "o-", linewidth=1.8, color=color, label=comp)
    _style(ax, "Control-plane CPU has headroom", "speed (x)", "mean CPU (millicores)")
    ax.legend(fontsize=8, frameon=False)

    fig.suptitle("What acceleration did not break", fontsize=12, color="#0b0b0b")
    fig.tight_layout(rect=[0, 0, 1, 0.96])
    fig.savefig(os.path.join(out_dir, "03_unaffected_vs_speed.png"), dpi=140, facecolor="white")
    plt.close(fig)


def plot_gap_distribution(runs, out_dir):
    """Where the convergence error lives: surplus (positive) vs deficit (negative)."""
    fig, ax = plt.subplots(figsize=(11, 4.2))
    data, labels, colors = [], [], []
    for run in sorted(runs, key=lambda r: r.speed):
        _, conv = M.convergence(run)
        if conv.empty:
            continue
        data.append(conv["gap"].to_numpy())
        labels.append(run.label)
        colors.append(SPEED_COLORS.get(run.speed, "#2a78d6"))

    bp = ax.boxplot(data, tick_labels=labels, patch_artist=True, widths=0.55, showfliers=True,
                    flierprops=dict(marker=".", markersize=3, markerfacecolor="#898781",
                                    markeredgecolor="none"))
    for patch, color in zip(bp["boxes"], colors):
        patch.set_facecolor(color)
        patch.set_alpha(0.35)
        patch.set_edgecolor(color)
    for med in bp["medians"]:
        med.set_color("#0b0b0b")
    ax.axhline(0, color="#898781", linewidth=1.2)
    _style(ax, "Distribution of (Running - desired) per sample: above zero means pods that should be gone",
           "speed", "pod gap")
    fig.tight_layout()
    fig.savefig(os.path.join(out_dir, "04_gap_distribution.png"), dpi=140, facecolor="white")
    plt.close(fig)


def plot_sampling(runs, gt_desired, out_dir):
    """Shows literally which trace states each speed managed to record."""
    fig, ax = plt.subplots(figsize=(11, 3.6))
    gt_total = gt_desired.sum(axis=1)
    ax.step(gt_total.index, gt_total.values, where="post", color="#c3c2b7", linewidth=1.4,
            label="trace")

    for run in sorted(runs, key=lambda r: -r.speed):
        obs = C.observed_desired_replicas(run)
        if obs.empty:
            continue
        tau = C.align_to_trace_time(run, obs.index)
        ax.plot(tau, obs.sum(axis=1).to_numpy(), ".", markersize=6,
                color=SPEED_COLORS.get(run.speed, "#2a78d6"), label=f"{run.label} samples", alpha=0.85)

    _style(ax, "Where each speed actually sampled the trace", "trace time (s)", "desired pods")
    ax.legend(fontsize=8, frameon=False, ncol=6)
    fig.tight_layout()
    fig.savefig(os.path.join(out_dir, "05_sampling_coverage.png"), dpi=140, facecolor="white")
    plt.close(fig)


def make_all(runs, summary, gt_desired, out_dir):
    plot_convergence_timelines(runs, gt_desired, out_dir)
    plot_degradation(summary, out_dir)
    plot_unaffected(summary, out_dir)
    plot_gap_distribution(runs, out_dir)
    plot_sampling(runs, gt_desired, out_dir)
