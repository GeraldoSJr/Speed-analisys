"""
Shared loading / normalization helpers for the KLUE speed-sweep analysis.

The sweep ran the same 300-minute trace five times at --speed-by 20, 15, 10, 5 and 1. Each run
produced a run.log (in ~/klue-speed-sweep-results/speed-N/) and a directory of Prometheus CSVs
(left in the KLUE repo as output_csv_<timestamp>/). The run.log names its own zip, which is what
maps a speed to its metrics directory - no guessing from mtimes.
"""
import json
import os
import re
from dataclasses import dataclass, field

import numpy as np
import pandas as pd

HOME = os.path.expanduser("~")
RESULTS_ROOT = os.path.join(HOME, "klue-speed-sweep-results")
KLUE_REPO = os.environ.get("KLUE_REPO", os.path.join(HOME, "klue"))
TRACE_DIR = os.path.join(HOME, "klue-trace-varying-load-300min")
OUT_DIR = os.path.join(HOME, "klue-speed-analysis", "results")

# The three Deployments the synthetic trace drives, and the namespaces they live in.
SERVICES = {"frontend": "frontend-ns", "backend": "backend-ns", "batch-worker": "batch-ns"}
EMU_NAMESPACES = sorted(set(SERVICES.values()))

# Trace geometry (see ~/klue-trace-varying-load-300min/generate_trace.py).
TICK_SECONDS = 300
NUM_TICKS = 61
TRACE_SPAN_SECONDS = (NUM_TICKS - 1) * TICK_SECONDS  # 18000s = 300 minutes

# Collector sampling step, fixed in src/manager.py as Collector(step=30) regardless of speed.
COLLECTOR_STEP_SECONDS = 30


@dataclass
class Run:
    speed: float
    metrics_dir: str
    log_path: str
    _cache: dict = field(default_factory=dict, repr=False)

    @property
    def label(self):
        return f"{self.speed:g}x"

    def has(self, metric):
        return os.path.exists(os.path.join(self.metrics_dir, f"{metric}.csv"))

    def load(self, metric):
        """Loads one metric CSV, memoized. Returns an empty frame when the metric was not scraped."""
        if metric in self._cache:
            return self._cache[metric]
        path = os.path.join(self.metrics_dir, f"{metric}.csv")
        if not os.path.exists(path):
            df = pd.DataFrame()
        else:
            df = pd.read_csv(path, low_memory=False)
            if "timestamp" in df:
                df["timestamp"] = pd.to_numeric(df["timestamp"], errors="coerce")
            if "value" in df:
                df["value"] = pd.to_numeric(df["value"], errors="coerce")
            df = df.dropna(subset=["timestamp"])
        self._cache[metric] = df
        return df

    def log_text(self):
        if "__log" not in self._cache:
            with open(self.log_path, encoding="utf-8", errors="replace") as f:
                self._cache["__log"] = f.read()
        return self._cache["__log"]


def discover_runs():
    """
    Finds every speed-N result directory and pairs it with the metrics directory its log names.

    Raises RuntimeError if a run's metrics directory cannot be located, rather than silently
    analysing a subset.
    """
    runs = []
    if not os.path.isdir(RESULTS_ROOT):
        raise RuntimeError(f"No sweep results at {RESULTS_ROOT}")

    for entry in sorted(os.listdir(RESULTS_ROOT)):
        m = re.fullmatch(r"speed-(\d+(?:\.\d+)?)", entry)
        if not m:
            continue
        speed = float(m.group(1))
        run_dir = os.path.join(RESULTS_ROOT, entry)
        log_path = os.path.join(run_dir, "run.log")
        if not os.path.exists(log_path):
            raise RuntimeError(f"{entry}: no run.log")

        with open(log_path, encoding="utf-8", errors="replace") as f:
            text = f.read()
        zips = re.findall(r"output_csv_[\d\-]+:[\d:]+(?=\.zip)", text)
        if not zips:
            raise RuntimeError(f"{entry}: run.log names no output_csv zip")
        stem = zips[-1]

        # The sweep script moves results next to the log; when it does not, they stay in the repo.
        candidates = [os.path.join(run_dir, stem), os.path.join(KLUE_REPO, stem)]
        metrics_dir = next((c for c in candidates if os.path.isdir(c)), None)
        if metrics_dir is None:
            raise RuntimeError(f"{entry}: metrics dir {stem} not found in {candidates}")

        runs.append(Run(speed=speed, metrics_dir=metrics_dir, log_path=log_path))

    if not runs:
        raise RuntimeError(f"No speed-* runs under {RESULTS_ROOT}")
    return sorted(runs, key=lambda r: -r.speed)


# ---------------------------------------------------------------------------
# Ground truth, read from the trace itself rather than re-derived
# ---------------------------------------------------------------------------

_GT_CACHE = {}


def load_ground_truth():
    """
    Returns (desired_df, nodes_df) describing what the trace asks for at each tick.

    desired_df: index=trace_time, columns=service, value=desired replicas (step function).
    nodes_df:   index=trace_time, column 'nodes' = number of KWOK nodes alive after that tick.
    """
    if "gt" in _GT_CACHE:
        return _GT_CACHE["gt"]

    with open(os.path.join(TRACE_DIR, "workload_description.json"), encoding="utf-8") as f:
        workload = json.load(f)
    with open(os.path.join(TRACE_DIR, "infrastructure_description.json"), encoding="utf-8") as f:
        infra = json.load(f)

    replicas = {s: None for s in SERVICES}
    rows = []
    for entry in workload["emulation"]:
        for objs in entry.get("applied_objects", {}).values():
            for obj in objs:
                replicas[obj["metadata"]["name"]] = obj["spec"]["replicas"]
        for scaled in entry.get("scaled_replicasets", []):
            replicas[scaled["name"]] = scaled["pods"]
        rows.append({"trace_time": entry["timestamp"], **replicas})
    desired = pd.DataFrame(rows).set_index("trace_time")

    alive, node_rows = 0, []
    for entry in infra["emulation"]:
        alive += len(entry.get("applied_objects", [])) - len(entry.get("deleted_objects", []))
        node_rows.append({"trace_time": entry["timestamp"], "nodes": alive})
    nodes = pd.DataFrame(node_rows).set_index("trace_time")

    _GT_CACHE["gt"] = (desired, nodes)
    return desired, nodes


# ---------------------------------------------------------------------------
# Observed series
# ---------------------------------------------------------------------------

def _raw_desired_replicas(run):
    """spec.replicas per service over the whole collection window, contamination included."""
    df = run.load("kube_deployment_spec_replicas")
    if df.empty:
        return pd.DataFrame()
    df = df[df["deployment"].isin(SERVICES)]
    wide = df.pivot_table(index="timestamp", columns="deployment", values="value", aggfunc="max")
    return wide.sort_index()


def observed_desired_replicas(run):
    """Wall-clock series of spec.replicas per service, trimmed to this run's emulation."""
    wide = _raw_desired_replicas(run)
    if wide.empty:
        return wide
    return wide[wide.index >= run_anchor(run)]


def observed_pod_phases(run):
    """Wall-clock counts of emulated pods per phase (Running/Pending/Failed/...)."""
    df = run.load("kube_pod_status_phase")
    if df.empty:
        return pd.DataFrame()
    df = df[(df["namespace"].isin(EMU_NAMESPACES)) & (df["timestamp"] >= run_anchor(run))]
    counts = df.groupby(["timestamp", "phase"])["value"].sum().unstack(fill_value=0.0)
    return counts.sort_index()


def observed_pods_per_service(run, phase="Running"):
    """Wall-clock count of pods in `phase`, split by the service that owns them."""
    df = run.load("kube_pod_status_phase")
    if df.empty:
        return pd.DataFrame()
    df = df[(df["namespace"].isin(EMU_NAMESPACES)) & (df["phase"] == phase)
            & (df["timestamp"] >= run_anchor(run))]
    ns_to_service = {v: k for k, v in SERVICES.items()}
    df = df.assign(service=df["namespace"].map(ns_to_service))
    wide = df.pivot_table(index="timestamp", columns="service", values="value", aggfunc="sum")
    return wide.sort_index()


def observed_node_occupancy(run):
    """
    Wall-clock count of distinct KWOK nodes that are hosting at least one emulated pod.

    The sweep collected no kube_node_* metric, so total node count is not directly observable;
    this is the occupied subset, which is what the scheduler actually had to place onto.
    """
    df = run.load("kube_pod_info")
    if df.empty:
        return pd.Series(dtype=float)
    df = df[(df["namespace"].isin(EMU_NAMESPACES)) & (df["timestamp"] >= run_anchor(run))]
    df = df.dropna(subset=["node"])
    df = df[df["node"].astype(str).str.startswith("kwok-node-")]
    return df.groupby("timestamp")["node"].nunique().sort_index()


def align_to_trace_time(run, index_values):
    """Maps wall-clock timestamps to nominal trace time using the run's own anchor and speed."""
    t0 = run_anchor(run)
    return (np.asarray(index_values, dtype=float) - t0) * run.speed


def run_anchor(run):
    """
    Wall-clock time of trace t=0 for a run.

    This cannot simply be the first deployment sample. The collector queries a window that starts
    `overlap` (300s) before the emulation, and the previous run's Deployments survive until this
    run's reset, so every run except the first opens with several samples still showing the
    previous run's final state. The anchor is therefore the first sample whose desired-replica
    vector equals the trace's tick-0 state; samples before it belong to the previous run and are
    discarded by the observed_* helpers.
    """
    if "__anchor" in run._cache:
        return run._cache["__anchor"]

    obs = _raw_desired_replicas(run)
    if obs.empty:
        raise RuntimeError(f"{run.label}: no deployment samples, cannot anchor")

    gt_desired, _ = load_ground_truth()
    tick0 = tuple(gt_desired.iloc[0][list(obs.columns)])
    matches = [i for i, row in enumerate(obs.to_numpy()) if tuple(row) == tick0]

    if matches:
        anchor = float(obs.index[matches[0]])
        run._cache["__anchor_method"] = "tick0-state"
    else:
        # Fallback: the leftover state is whatever the window opens with; the emulation starts at
        # the first sample that differs from it.
        first = tuple(obs.iloc[0])
        differs = [i for i, row in enumerate(obs.to_numpy()) if tuple(row) != first]
        anchor = float(obs.index[differs[0]]) if differs else float(obs.index.min())
        run._cache["__anchor_method"] = "first-change (tick-0 state never sampled)"

    run._cache["__anchor"] = anchor
    run._cache["__pre_anchor_samples"] = int((obs.index < anchor).sum())
    return anchor


def anchor_info(run):
    run_anchor(run)
    return {
        "anchor_method": run._cache.get("__anchor_method"),
        "discarded_pre_anchor_samples": run._cache.get("__pre_anchor_samples", 0),
    }


def counter_rate(df, value_col="value", group_cols=None):
    """
    Turns a Prometheus counter into a per-second rate between consecutive samples.

    Counter resets (a restarted scheduler) show up as negative deltas and are dropped rather than
    counted as huge negative rates.
    """
    group_cols = group_cols or []
    out = []
    grouper = df.groupby(group_cols) if group_cols else [((), df)]
    for key, g in grouper:
        g = g.sort_values("timestamp")
        dt = g["timestamp"].diff()
        dv = g[value_col].diff()
        rate = dv / dt
        rate[(dv < 0) | (dt <= 0)] = np.nan
        piece = g.assign(rate=rate)
        if group_cols:
            keys = key if isinstance(key, tuple) else (key,)
            for col, val in zip(group_cols, keys):
                piece[col] = val
        out.append(piece)
    return pd.concat(out) if out else pd.DataFrame()


def step_interp(series, at):
    """Zero-order-hold lookup of a step-function series at the given points."""
    if series.empty:
        return np.full(len(at), np.nan)
    idx = np.asarray(series.index, dtype=float)
    vals = np.asarray(series.values, dtype=float)
    pos = np.searchsorted(idx, np.asarray(at, dtype=float), side="right") - 1
    out = np.full(len(at), np.nan)
    ok = pos >= 0
    out[ok] = vals[pos[ok]]
    return out


def ensure_out_dir():
    os.makedirs(OUT_DIR, exist_ok=True)
    return OUT_DIR
