# KLUE speed-sweep analysis (R)

Measures what changes when the same KLUE trace is replayed at different `--speed-by` factors, and
writes a complete experimental report covering the input, the environment, the procedure and every
analysis.

This is an R rewrite of the earlier Python implementation in `~/klue-speed-analysis/`. Both produce
identical numbers; this one additionally characterises the input trace and documents the emulation
environment.

## Run it

```bash
./analyze.R
```

Everything lands in `results/`. Re-running is safe and idempotent. Takes about a minute, most of it
reading the 1x run's 146 MB of CSVs.

## Requirements

R 4.x with `data.table`, `jsonlite`, `stringr`, `ggplot2`, `dplyr`, `tidyr`, `scales`, `gridExtra`
and `glue`. No pandoc needed - the report is written as Markdown directly.

## What it expects

- `~/klue-speed-sweep-results/speed-<N>/run.log` for each speed.
- The metrics directory each `run.log` names (`output_csv_<timestamp>/`), either beside the log or
  still in the KLUE repo. Set `KLUE_REPO` if the repo is not at `~/klue`.
- `~/klue-trace-varying-load-300min/{workload,infrastructure}_description.json` as ground truth.

The speed-to-metrics mapping is recovered from the zip name inside each `run.log`, not from file
timestamps, so it stays correct if directories are moved or re-dated.

## Layout

| File | Role |
|---|---|
| `analyze.R` | Entry point: discover runs, measure, write tables, figures and report |
| `R/common.R` | Run discovery, CSV loading, ground truth, anchoring, trace-time alignment |
| `R/metrics.R` | All measurements (timing, resolution, convergence, peaks, scheduler, CPU, logs) |
| `R/summarise.R` | Collapses per-run measurements into one row per speed |
| `R/plots.R` | Figures: 3 for the input trace, 7 for the results |
| `R/report.R` | `REPORT.md`, including the recorded environment facts |

Environment facts (cluster, KWOK, scheduler, monitoring, collector settings) are constants in
`R/report.R`, each carrying the file and line it was read from. They are configuration, not
measurements - if the setup changes, update them there.

## Two things worth knowing about the data

**Cross-run contamination.** The collector queries a window opening 300s before the emulation, and
the previous run's Deployments survive until the next run's reset, so every run except the first
opens with ~330s of samples showing the *previous* run's final state. `run_anchor()` detects this by
locking onto the trace's tick-0 state and discards the earlier samples. Raw CSVs read without that
correction show a phantom plateau before four of the five runs.

**Sampling resolution is not constant across runs.** `Collector(step=30)` is fixed in wall-clock
seconds, so one sample covers 30s of trace time at 1x but 600s at 20x. Comparisons across speeds
must be made in trace time, and quantities measured at 15x/20x are subsampled enough to miss short
transients - which is itself one of the findings.
