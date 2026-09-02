"""
Per-run measurements for the KLUE speed-sweep analysis.

Every function here takes a Run and returns plain numbers/frames; nothing prints or plots. The
distinction that matters throughout: "emulation-side" effects are about whether KLUE issued the
trace on time, "cluster-side" effects are about whether Kubernetes kept up with what was issued.
"""
import re

import numpy as np
import pandas as pd

from . import common as C


# ---------------------------------------------------------------------------
# Emulation-side: did the trace play at the requested speed?
# ---------------------------------------------------------------------------

def timing(run, gt_desired):
    """
    Effective replay speed, by regressing when each desired-state change was observed against the
    trace time the trace asks for it.

    Measuring the raw first-to-last span instead would understate every run by exactly one tick,
    because the collection window closes before kube-state-metrics scrapes the final change. The
    regression uses every matched change, so a missing endpoint costs precision, not accuracy.
    Each desired-replica vector is a near-unique fingerprint of a tick, which is what makes the
    observed samples matchable back to trace time without assuming they arrived on schedule.
    """
    obs = C.observed_desired_replicas(run)
    if obs.empty:
        return {}

    cols = list(obs.columns)
    state_to_tick, seen = {}, set()
    for trace_time, row in gt_desired[cols].iterrows():
        key = tuple(row)
        if key in seen:
            state_to_tick.pop(key, None)   # ambiguous state, unusable as an anchor
        else:
            seen.add(key)
            state_to_tick[key] = float(trace_time)

    changed = obs.diff().abs().sum(axis=1) > 0
    changed.iloc[0] = True
    pairs = [(float(ts), state_to_tick[tuple(row)])
             for ts, row in obs[changed].iterrows() if tuple(row) in state_to_tick]

    out = {
        "requested_speed": run.speed,
        "observed_change_samples": int(changed.sum()),
        "matched_change_samples": len(pairs),
    }
    if len(pairs) < 3:
        return out

    wall = np.array([p[0] for p in pairs]) - pairs[0][0]
    trace = np.array([p[1] for p in pairs])
    slope, intercept = np.polyfit(trace, wall, 1)
    pred = slope * trace + intercept
    resid = wall - pred
    ss_res = float(np.sum(resid ** 2))
    ss_tot = float(np.sum((wall - wall.mean()) ** 2))

    effective = 1.0 / slope if slope > 0 else np.nan
    out.update({
        "effective_speed": float(effective),
        "speed_error_pct": float(100.0 * (effective / run.speed - 1.0)),
        "fit_r2": float(1 - ss_res / ss_tot) if ss_tot else np.nan,
        "timing_resid_std_wall_s": float(resid.std()),
        "timing_resid_max_wall_s": float(np.abs(resid).max()),
        "timing_resid_std_trace_s": float(resid.std() * run.speed),
        "timing_resid_max_trace_s": float(np.abs(resid).max() * run.speed),
        "wall_span_s": float(wall.max()),
        "trace_span_covered_s": float(trace.max() - trace.min()),
        "sample_quantum_trace_s": C.COLLECTOR_STEP_SECONDS * run.speed,
    })
    return out


def resolution(run, gt_desired):
    """
    How much of the trace the fixed 30s collector step can actually see at this speed.

    A tick lasts TICK_SECONDS of trace time, i.e. TICK_SECONDS/speed of wall time. Below two
    samples per tick the sampling is at or past the Nyquist limit for a one-tick feature, so
    single-tick spikes may vanish entirely.
    """
    wall_per_tick = C.TICK_SECONDS / run.speed
    samples_per_tick = wall_per_tick / C.COLLECTOR_STEP_SECONDS
    trace_s_per_sample = C.COLLECTOR_STEP_SECONDS * run.speed

    obs = C.observed_desired_replicas(run)
    n_samples = len(obs)
    gt_values_seen = 0
    if not obs.empty:
        gt_tuples = {tuple(r) for r in gt_desired[list(obs.columns)].to_numpy()}
        obs_tuples = {tuple(r) for r in obs.to_numpy()}
        gt_values_seen = len(gt_tuples & obs_tuples)

    return {
        "wall_seconds_per_tick": wall_per_tick,
        "samples_per_tick": samples_per_tick,
        "trace_seconds_per_sample": trace_s_per_sample,
        "samples_observed": n_samples,
        "gt_tick_states": int(len({tuple(r) for r in gt_desired.to_numpy()})),
        "gt_tick_states_observed": gt_values_seen,
        "tick_states_missed_pct": 100.0 * (1 - gt_values_seen / max(1, len({tuple(r) for r in gt_desired.to_numpy()}))),
        "nyquist_ok": samples_per_tick >= 2.0,
    }


def fidelity(run, gt_desired):
    """
    Emulation-side fidelity: at each sample's nominal trace time, is the desired replica count
    the one the trace asks for?

    Deviation here means the trace is playing late (or the sample landed between ticks), not that
    the cluster failed - desired replicas are whatever KLUE last told the API server.
    """
    obs = C.observed_desired_replicas(run)
    if obs.empty:
        return {}, pd.DataFrame()

    tau = C.align_to_trace_time(run, obs.index)
    rows = []
    for svc in obs.columns:
        expected = C.step_interp(gt_desired[svc], tau)
        rows.append(pd.DataFrame({
            "trace_time": tau, "service": svc,
            "observed": obs[svc].to_numpy(dtype=float), "expected": expected,
        }))
    detail = pd.concat(rows, ignore_index=True).dropna(subset=["expected"])
    detail["abs_err"] = (detail["observed"] - detail["expected"]).abs()

    total_obs = obs.sum(axis=1).to_numpy(dtype=float)
    total_exp = np.nansum(
        np.column_stack([C.step_interp(gt_desired[s], tau) for s in obs.columns]), axis=1)
    ok = ~np.isnan(total_exp)

    denom = np.mean(total_exp[ok]) if ok.any() else np.nan
    return {
        "desired_mae_pods": float(detail["abs_err"].mean()),
        "desired_total_mae_pods": float(np.mean(np.abs(total_obs[ok] - total_exp[ok]))),
        "desired_total_nmae_pct": float(100.0 * np.mean(np.abs(total_obs[ok] - total_exp[ok])) / denom),
        "desired_exact_match_pct": float(100.0 * (detail["abs_err"] == 0).mean()),
        "desired_max_err_pods": float(detail["abs_err"].max()),
    }, detail


# ---------------------------------------------------------------------------
# Cluster-side: did Kubernetes keep up with what was issued?
# ---------------------------------------------------------------------------

def convergence(run):
    """
    Gap between what KLUE asked for (spec.replicas) and what was actually Running, on the run's
    own wall-clock samples. This is pure cluster-side lag: both series come from the same run.
    """
    desired = C.observed_desired_replicas(run)
    running = C.observed_pods_per_service(run, phase="Running")
    if desired.empty or running.empty:
        return {}, pd.DataFrame()

    common_cols = [c for c in desired.columns if c in running.columns]
    idx = desired.index.intersection(running.index)
    # The anchor sample can still hold pods of the previous run that were terminating when this
    # one started, so it is not evidence about this run's convergence.
    idx = idx[1:] if len(idx) > 2 else idx
    d = desired.loc[idx, common_cols]
    r = running.loc[idx, common_cols].reindex(columns=common_cols).fillna(0.0)

    d_tot, r_tot = d.sum(axis=1), r.sum(axis=1)
    gap = (r_tot - d_tot)
    detail = pd.DataFrame({
        "timestamp": idx, "trace_time": C.align_to_trace_time(run, idx),
        "desired_total": d_tot.to_numpy(), "running_total": r_tot.to_numpy(),
        "gap": gap.to_numpy(),
    })

    denom = d_tot.mean()
    converged = (gap.abs() <= np.maximum(1.0, 0.02 * d_tot))
    return {
        "convergence_mae_pods": float(gap.abs().mean()),
        "convergence_nmae_pct": float(100.0 * gap.abs().mean() / denom) if denom else np.nan,
        "convergence_max_deficit_pods": float((-gap).max()),
        "converged_samples_pct": float(100.0 * converged.mean()),
        "mean_desired_pods": float(denom),
        "mean_running_pods": float(r_tot.mean()),
    }, detail


def peaks(run, gt_desired):
    """
    Spike fidelity: does the run ever actually run the pods the trace's peaks ask for?

    A spike that the emulation issues but the cluster never materialises - or that the collector
    never samples - is invisible downstream, which is the failure mode that matters for anyone
    reading the collected metrics as if they described the original trace.
    """
    gt_total = gt_desired.sum(axis=1)
    gt_peak = float(gt_total.max())

    desired = C.observed_desired_replicas(run)
    running = C.observed_pods_per_service(run, phase="Running")
    obs_desired_peak = float(desired.sum(axis=1).max()) if not desired.empty else np.nan
    obs_running_peak = float(running.sum(axis=1).max()) if not running.empty else np.nan

    # The three sharp frontend spikes of the trace, as (start_tick, end_tick) in trace seconds.
    windows = [(13 * 300, 15 * 300), (31 * 300, 32 * 300), (49 * 300, 50 * 300)]
    per_spike = []
    if not running.empty:
        tau = C.align_to_trace_time(run, running.index)
        rt = running.sum(axis=1).to_numpy(dtype=float)
        dt_ = desired.sum(axis=1).to_numpy(dtype=float) if not desired.empty else None
        for i, (a, b) in enumerate(windows, 1):
            m = (tau >= a - 300) & (tau <= b + 300)
            gt_win = float(gt_total[(gt_total.index >= a) & (gt_total.index <= b)].max())
            per_spike.append({
                "spike": i, "window_trace_s": f"{a}-{b}", "gt_peak_pods": gt_win,
                "samples_in_window": int(m.sum()),
                "observed_desired_peak": float(dt_[m].max()) if dt_ is not None and m.any() else np.nan,
                "observed_running_peak": float(rt[m].max()) if m.any() else np.nan,
                "capture_pct": float(100.0 * rt[m].max() / gt_win) if m.any() and gt_win else np.nan,
            })

    return {
        "gt_peak_pods": gt_peak,
        "observed_desired_peak_pods": obs_desired_peak,
        "observed_running_peak_pods": obs_running_peak,
        "desired_peak_capture_pct": 100.0 * obs_desired_peak / gt_peak if gt_peak else np.nan,
        "running_peak_capture_pct": 100.0 * obs_running_peak / gt_peak if gt_peak else np.nan,
    }, pd.DataFrame(per_spike)


def pod_health(run):
    """Non-Running pod phases, which is where pressure on the cluster surfaces."""
    ph = C.observed_pod_phases(run)
    if ph.empty:
        return {}
    out = {}
    for phase in ["Pending", "Failed", "Unknown", "Succeeded"]:
        col = ph[phase] if phase in ph else pd.Series([0.0])
        out[f"pods_{phase.lower()}_max"] = float(col.max())
        out[f"pods_{phase.lower()}_mean"] = float(col.mean())
    tot = ph.sum(axis=1).replace(0, np.nan)
    out["pods_pending_pct_mean"] = float(100.0 * (ph.get("Pending", 0) / tot).mean())
    return out


def node_occupancy(run, gt_nodes):
    """Distinct KWOK nodes hosting emulated pods, against the node count the trace provisions."""
    occ = C.observed_node_occupancy(run)
    if occ.empty:
        return {}
    return {
        "occupied_nodes_max": float(occ.max()),
        "occupied_nodes_mean": float(occ.mean()),
        "gt_nodes_max": float(gt_nodes["nodes"].max()),
        "node_occupancy_peak_pct": float(100.0 * occ.max() / gt_nodes["nodes"].max()),
    }


# ---------------------------------------------------------------------------
# Scheduler
# ---------------------------------------------------------------------------

def scheduler(run):
    """
    Scheduler throughput, latency and backlog.

    Rates are per wall-clock second: acceleration compresses the same trace into less real time,
    so the real load on the scheduler is what changes.
    """
    out = {}

    att = run.load("scheduler_schedule_attempts_total")
    if not att.empty:
        att = att.dropna(subset=["value"])
        rated = C.counter_rate(att, group_cols=["result"])
        by_result = rated.groupby("result")["rate"]
        for result, s in by_result:
            out[f"sched_{result}_per_s_mean"] = float(s.mean())
            out[f"sched_{result}_per_s_max"] = float(s.max())
        totals = att.groupby("result")["value"].agg(["min", "max"])
        for result, row in totals.iterrows():
            out[f"sched_{result}_total"] = float(row["max"] - row["min"])

    pend = run.load("scheduler_pending_pods")
    if not pend.empty:
        per_ts = pend.groupby("timestamp")["value"].sum()
        out["sched_pending_max"] = float(per_ts.max())
        out["sched_pending_mean"] = float(per_ts.mean())
        by_queue = pend.groupby("queue")["value"].max()
        for q, v in by_queue.items():
            out[f"sched_pending_{q}_max"] = float(v)

    s_sum = run.load("scheduler_scheduling_attempt_duration_seconds_sum")
    s_cnt = run.load("scheduler_scheduling_attempt_duration_seconds_count")
    if not s_sum.empty and not s_cnt.empty:
        a = s_sum[s_sum["result"] == "scheduled"].groupby("timestamp")["value"].sum()
        b = s_cnt[s_cnt["result"] == "scheduled"].groupby("timestamp")["value"].sum()
        idx = a.index.intersection(b.index)
        da, db = a.loc[idx].diff(), b.loc[idx].diff()
        valid = (db > 0) & (da >= 0)
        lat = (da[valid] / db[valid])
        if len(lat):
            out["sched_latency_mean_ms"] = float(1000 * lat.mean())
            out["sched_latency_p95_ms"] = float(1000 * lat.quantile(0.95))
            out["sched_latency_max_ms"] = float(1000 * lat.max())
        cum = (a.max() - a.min()) / (b.max() - b.min()) if (b.max() - b.min()) > 0 else np.nan
        out["sched_latency_overall_ms"] = float(1000 * cum)

    inc = run.load("scheduler_queue_incoming_pods_total")
    if not inc.empty:
        rated = C.counter_rate(inc.dropna(subset=["value"]), group_cols=["event"] if "event" in inc else [])
        if "rate" in rated:
            out["sched_incoming_per_s_mean"] = float(rated["rate"].mean())
            out["sched_incoming_per_s_max"] = float(rated["rate"].max())

    pre = run.load("scheduler_preemption_attempts_total")
    if not pre.empty:
        out["sched_preemption_total"] = float(pre["value"].max() - pre["value"].min())

    return out


# ---------------------------------------------------------------------------
# Control plane cost
# ---------------------------------------------------------------------------

CONTROL_PLANE = {
    "kwok-controller": r"^kwok-controller-",
    "kube-state-metrics": r"^kube-state-metrics-",
    "prometheus": r"^prometheus-k8s-",
    "coredns": r"^coredns-",
}


def control_plane_cpu(run):
    """
    Mean and peak CPU (cores) of the components that carry the emulation, from the cAdvisor
    counter. Fake pods burn no CPU, so this is entirely real control-plane cost.
    """
    df = run.load("container_cpu_usage_seconds_total")
    if df.empty:
        return {}, pd.DataFrame()

    df = df.dropna(subset=["value", "pod"]).copy()
    df["pod"] = df["pod"].astype(str)

    # cAdvisor exports one series per named container plus unnamed cgroup rollups for the pod
    # sandbox. Keeping the named containers and summing them gives per-pod CPU exactly once;
    # mixing in the rollups (or differencing across series) inflates the rate by orders of
    # magnitude, since consecutive rows would come from different counters.
    if "container" in df:
        df = df[df["container"].notna() & (df["container"].astype(str).str.strip() != "")]
        df = df[df["container"].astype(str) != "POD"]

    out, frames = {}, []
    for name, pattern in CONTROL_PLANE.items():
        sub = df[df["pod"].str.match(pattern)]
        if sub.empty:
            continue
        rated = C.counter_rate(sub, group_cols=["pod", "container"])
        per_ts = rated.dropna(subset=["rate"]).groupby("timestamp")["rate"].sum()
        if per_ts.empty:
            continue
        out[f"cpu_{name}_mean_cores"] = float(per_ts.mean())
        out[f"cpu_{name}_max_cores"] = float(per_ts.max())
        frames.append(pd.DataFrame({
            "component": name, "timestamp": per_ts.index,
            "trace_time": C.align_to_trace_time(run, per_ts.index), "cores": per_ts.to_numpy(),
        }))

    detail = pd.concat(frames, ignore_index=True) if frames else pd.DataFrame()
    if out:
        out["cpu_control_plane_total_mean_cores"] = float(
            sum(v for k, v in out.items() if k.endswith("_mean_cores")))
    return out, detail


# ---------------------------------------------------------------------------
# Log-derived signals
# ---------------------------------------------------------------------------

def log_signals(run, gt_node_creates):
    """Errors and retry pressure that only show up in the emulator's own output."""
    text = run.log_text()
    created = len(re.findall(r"Node \S+ created", text))
    deleted = len(re.findall(r"Node \S+ deleted", text))
    entries = len(re.findall(r"Processing entry at timestamp", text))
    errors = re.findall(r"\[ERROR\][^\n]*", text)
    apply_fail = len([e for e in errors if "Failed to apply" in e])
    delete_fail = len([e for e in errors if "Failed to delete" in e])
    return {
        "log_entries_processed": entries,
        "log_scale_commands": len(re.findall(r"Scaled deployment", text)),
        "log_nodes_created": created,
        "log_nodes_deleted": deleted,
        "log_node_create_excess": created - gt_node_creates,
        "log_errors": len(errors),
        "log_apply_failures": apply_fail,
        "log_delete_failures": delete_fail,
        "log_error_lines": " | ".join(e.strip()[:160] for e in errors[:5]),
    }
