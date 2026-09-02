# KLUE speed-sweep analysis

Measures what changes when the same KLUE trace is replayed at different `--speed-by` factors.

Reads the five runs produced by `~/klue-trace-varying-load-300min/run_speed_sweep.sh` and compares
them against the trace JSONs (the ground truth), then writes tables, figures and a written report.

## Run it

```bash
./analyze.py
```

Everything lands in `results/`. Re-running is safe and idempotent.

## What it expects

- `~/klue-speed-sweep-results/speed-<N>/run.log` for each speed.
- The metrics directory each `run.log` names (`output_csv_<timestamp>/`), either next to the log
  or still in the KLUE repo. Set `KLUE_REPO` if the repo is not at `~/klue`.
- `~/klue-trace-varying-load-300min/{workload,infrastructure}_description.json` as ground truth.

The speed-to-metrics mapping comes from the zip name inside each `run.log`, not from file
timestamps, so it stays correct if the directories are moved or re-dated.

## Layout

| File | Purpose |
|---|---|
| `analyze.py` | Entry point: discover runs, measure, write everything |
| `klue_analysis/common.py` | Loading, run discovery, ground truth, trace-time alignment |
| `klue_analysis/metrics.py` | The measurements (timing, resolution, convergence, scheduler, CPU) |
| `klue_analysis/report.py` | Summary table + `REPORT.md` |
| `klue_analysis/plots.py` | Figures |

## Two things worth knowing about the data

**Cross-run contamination.** The collector queries a window starting 300s before the emulation,
and the previous run's Deployments survive until the next run's reset, so every run except the
first opens with ~330s of samples showing the *previous* run's final state. `common.run_anchor()`
detects this by locking onto the trace's tick-0 state and discards the earlier samples. Raw CSVs
read without that correction will show a phantom plateau before each run.

**Sampling resolution is not constant across runs.** `Collector(step=30)` is fixed in wall-clock
seconds, so one sample covers 30s of trace time at 1x but 600s at 20x. Any comparison across
speeds has to be made in trace time, and quantities measured at 15x/20x are subsampled enough that
short transients are missed entirely - which is itself one of the findings.
