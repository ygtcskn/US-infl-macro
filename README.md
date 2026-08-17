# US-infl-macro

Forecasting US CPI inflation (YoY, `CPIAUCSL_PC1`) from a monthly macro panel of 28
predictors, Jan 1994 – Sept 2025. Compares LASSO, Random Forest, XGBoost and ARIMAX on a
chronological 70/30 split, with SHAP-based interpretation.

Coursework for the Experimental Economics seminar.

<img src="https://raw.githubusercontent.com/ygtcskn/US-infl-macro/main/output/figures/final/model_grid.png" alt="Actual vs predicted inflation, four models" width="100%">

## Layout

```
├── run_all.R                  entry point
├── data/
│   ├── raw/                   CPI, gold, BCOM — immutable inputs
│   ├── temp/                  half-built data, safe to delete
│   └── final/infl_macro.csv   model-ready panel
├── R/
│   ├── 00_config.R            paths and constants
│   ├── 01_prepare_data.R      FRED pull, transforms, ADF, differencing
│   ├── 02_benchmark_models.R  LASSO / RF / XGBoost x {no lag, AR1} + ARIMAX
│   ├── 03_xgboost_shap.R      XGBoost, feature importance, SHAP
│   ├── 04_final_figures.R     paper figures, LASSO coefficients
│   └── 05_model_grid.R        2x2 actual vs predicted panel
└── output/                    figures, tables, models, logs (all regenerated)
```

## The scripts

`00_config.R` holds every path and constant in the project, so no script contains a
hardcoded directory. The rest are numbered in the order they must run.

`01_prepare_data.R` downloads 28 macro series from FRED, merges them with the CPI, gold
and commodity files in `data/raw`, applies log and difference transforms, runs ADF tests
and differences whatever is non-stationary. It writes the model-ready panel that
everything downstream reads.

`02_benchmark_models.R` fits LASSO, Random Forest and XGBoost under two specifications —
with and without the lagged inflation term — plus ARIMAX, and writes the RMSE / MAE /
sMAPE comparison to `results.xlsx`. It produces no figures.

`03_xgboost_shap.R` refits XGBoost on the lagged specification and produces its feature
importance and SHAP plots. `04_final_figures.R` does the same for LASSO, ARIMAX and
Random Forest, and exports the LASSO coefficients. `05_model_grid.R` reads the saved
predictions from both and assembles the four-panel figure above.

`run_all.R` runs the whole chain, checks its inputs first, isolates each stage in its own
environment, stops at the first failure, and writes a transcript plus `sessionInfo()` to
`output/logs/`.

## Running it

```bash
Rscript run_all.R
```

A subset, when you only want to redraw the grid:

```bash
Rscript run_all.R 5
```

From the R console, `source("run_all.R")` runs everything; set `STAGES <- 2:5` first for
part of it.

| # | Script | Writes |
|---|--------|--------|
| 1 | `01_prepare_data.R` | `data/final/infl_macro.csv` |
| 2 | `02_benchmark_models.R` | `output/tables/results.xlsx` |
| 3 | `03_xgboost_shap.R` | XGBoost figures, `table_variable_summary_XGB.xlsx` |
| 4 | `04_final_figures.R` | paper figures, `table_variable_summary_LASSO.xlsx` |
| 5 | `05_model_grid.R` | `output/figures/final/model_grid.*` |

Stage 1 needs an internet connection (`quantmod::getSymbols`). Stages 2–5 read only
`data/final/infl_macro.csv`, so they run offline.

## Results

Out-of-sample performance on the test window (Apr 2016 – Sept 2025, 114 obs):

| Model | RMSE | MAE | sMAPE |
|---|---|---|---|
| LASSO (AR1) | 0.295 | 0.210 | 0.094 |
| XGBoost (AR1) | 1.526 | 0.804 | 0.207 |
| Random Forest (AR1) | 1.772 | 0.981 | 0.271 |
| ARIMAX | 2.171 | 1.352 | 0.406 |

Full table, including the no-lag specifications and in-sample RMSE, in
`output/tables/results.xlsx`.
