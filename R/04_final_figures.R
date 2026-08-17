rm(list = ls())

source("R/00_config.R")

plotdir <- FIG_FINAL
tabdir <- DIR_TABLES

library(zoo)          # as.yearmon
library(caret)        # createTimeSlices
library(xgboost)      # XGBoost modeling
library(kernelshap)   # SHAP values
library(shapviz)      # SHAP visualization
library(ggplot2)      # plotting
library(reshape2)
library(corrplot)
library(scales)
library(forecast)
library(grid)
library(dplyr)
library(zoo)
library(tibble)
library(openxlsx)
library(glmnet)

###############################################################################
###############################################################################
###############################################################################
### TRAIN/TEST PREPARATION

# Loading the Data

df <- read.csv(FILE_MODEL_DATA)
df$YearMon <- as.yearmon(df$YearMon)
df <- na.omit(df)

# Taking lag1 of Inlflation

df <- df %>%
  mutate(
    inflation_lag1 = lag(inflation, 1)
)

df <- na.omit(df)

# Spliting test and train data chronlogically

x <- model.matrix(inflation ~. -YearMon, df)[, -1]
y <- df$inflation

set.seed(1234)

n <- nrow(df)
slices <- createTimeSlices(1:n,
                           initialWindow = floor(0.7 * n),
                           horizon = n - floor(0.7 * n),
                           fixedWindow = TRUE)

train_idx <- slices$train[[1]]
test_idx  <- slices$test[[1]]

X_train <- x[train_idx, ]
X_test  <- x[test_idx, ]
y_train <- y[train_idx]
y_test  <- y[test_idx]

train_means <- colMeans(X_train)
train_sds   <- apply(X_train, 2, sd)

X_train_std <- scale(X_train, center = train_means, scale = train_sds)
X_test_std  <- scale(X_test,  center = train_means, scale = train_sds)

###############################################################################
###############################################################################
###############################################################################
# LASSO

k <- 5
n_train <- length(y_train)
foldid <- cut(seq_len(n_train),
              breaks = k,
              labels = FALSE)

lasso_fit <- cv.glmnet(
  x = X_train_std,
  y = y_train,
  family = "gaussian",
  alpha = 1,
  foldid = foldid
)

plot(lasso_fit)

# Finding the best lambda

best_lambda <- lasso_fit$lambda.min

lasso_model <- glmnet(
  x = X_train_std,
  y = y_train,
  family = "gaussian",
  alpha = 1,
  lambda = best_lambda
)

coef_lasso <- coef(lasso_model)
coef_lasso



###############################################################################
###############################################################################
########################### VISUALIZATION #####################################
###############################################################################
###############################################################################

### INFLATION TIME SERIES PLOT

inflation_plot <- ggplot(df, aes(x = YearMon, y = inflation)) +
  geom_line(aes(color = "Inflation"), linewidth = 1) +
  labs(
    x = NULL,                 # no x-axis title
    y = "Inflation YoY"
  ) +
  scale_color_manual(
    values = c("Inflation" = "#000000"),
    name = ""                 # empty legend title (base style)
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    
    # X-axis: keep labels (YearMon), angled
    axis.text.x  = element_text(size = 11, angle = 45, hjust = 1),
    axis.ticks.x = element_line(color = "black", linewidth = 0.5),
    
    # Y-axis: keep ticks + line
    axis.text.y  = element_text(size = 11),
    axis.ticks.y = element_line(color = "black", linewidth = 0.5),
    axis.line.y  = element_line(color = "black", linewidth = 0.5),
    
    legend.position = c(0.85, 0.15)
    
  )
inflation_plot

ggsave(
  filename = file.path(plotdir,"inflation_timeseries.png"),
  plot = inflation_plot,
  width = 10,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(plotdir, "inflation_timeseries.emf"),
  plot = inflation_plot,
  device = devEMF::emf,
  width = 10,
  height = 5
)

ggsave(
  filename = file.path(plotdir,"inflation_timeseries.pdf"),
  plot = inflation_plot,
  width = 10,
  height = 5
)

###############################################################################
###############################################################################
###############################################################################

### ALL SERIES PLOT

df_melt <- reshape2::melt(
  df %>% dplyr::select(-inflation_lag1),
  id.vars = "YearMon"
)

all_timeseries <- ggplot(df_melt, aes(x = YearMon, y = value)) +
  geom_line(color = "#006400", linewidth = 0.6) +
  facet_wrap(~ variable, scales = "free_y", ncol = 5) +
  labs(x = "", y = "") +
  scale_x_yearmon(
    breaks = as.yearmon(c("2000-01", "2010-01", "2020-01")),
    labels = c("2000", "2010", "2020")
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    panel.spacing   = unit(1.2, "lines"),
    
    axis.text.x  = element_text(size = 9),
    axis.ticks.x = element_line(color = "grey40", linewidth = 0.4),
    
    axis.text.y  = element_text(size = 10),
    axis.ticks.y = element_line(color = "grey40", linewidth = 0.4),
    
    axis.line = element_blank(),
    
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text       = element_text(size = 10, face = "bold"),
    
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  geom_segment(
    aes(x = min(df_melt$YearMon),
        xend = min(df_melt$YearMon),
        y = -Inf,
        yend = Inf),
    inherit.aes = FALSE,
    linewidth = 0.4,
    color = "black"
  ) +
  geom_segment(
    aes(x = -Inf,
        xend = Inf,
        y = -Inf,
        yend = -Inf),
    inherit.aes = FALSE,
    linewidth = 0.4,
    color = "black"
  )

all_timeseries


ggsave(
  filename = file.path(plotdir, "all_timeseries_SCALED.png"),
  plot = all_timeseries,
  width = 10,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(plotdir, "all_timeseries_SCALED.emf"),
  plot = all_timeseries,
  device = devEMF::emf,
  width = 10,
  height = 10
)

ggsave(
  filename = file.path(plotdir, "all_timeseries_SCALED.pdf"),
  plot = all_timeseries,
  width = 10,
  height = 5
)

###############################################################################
###############################################################################
###############################################################################

### CORRELATION PLOT

df_nolags <- df[, !names(df) %in% c("inflation_lag1", "inflation_lag2", "YearMon")]
corr_mat <- cor(df_nolags, use = "pairwise.complete.obs")
corr_df <- reshape2::melt(corr_mat)

corr_plot <- ggplot(corr_df, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient2(
    low      = "white",
    mid      = "#E5F2E5",
    high     = "#006400",
    midpoint = 0,
    name     = "Correlation"
  ) +
  labs(
    x = "",
    y = ""
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.ticks.x = element_line(color = "black", linewidth = 0.5),
    axis.ticks.y = element_line(color = "black", linewidth = 0.5)
  )
corr_plot

ggsave(
  filename = file.path(plotdir, "correlation_plot.png"),
  plot = corr_plot,
  width = 10,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(plotdir, "correlation_plot.emf"),
  plot = corr_plot,
  device = devEMF::emf,
  width = 12,
  height = 10
)

ggsave(
  filename = file.path(plotdir, "correlation_plot.pdf"),
  plot = corr_plot,
  width = 10,
  height = 5
)

###############################################################################
###############################################################################
###############################################################################

### ACF and PACF

acf(df$inflation)
pacf(df$inflation)

###############################################################################
###############################################################################
###############################################################################

### SHAP GLOBAL IMPORTANCE

shp_xgb = kernelshap(lasso_model, X = X_train_std, bg_X = X_test_std)

sv_xgb <- shapviz(shp_xgb)

shap_values <- sv_importance(sv_xgb, kind = "bee") + 
  scale_color_gradientn(
    colors = c("#0000FF", "#AA00FF", "#FF0088", "#FF0000")
  ) +
  
  # Grey background with gridlines (slightly darker)
  theme_bw(base_size = 14) +
  
  theme(
    # Panel background (dark grey)
    panel.background = element_rect(fill = "grey90", color = NA),
    
    # Grid lines — keep light but visible
    panel.grid.major = element_line(color = "grey80", size = 0.3),
    panel.grid.minor = element_line(color = "grey85", size = 0.2),
    
    # Plot background (white)
    plot.background = element_rect(fill = "white", color = NA),
    
    # Axis text — match base graph
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    
    # Titles — match your base graph
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12)
  )
shap_values

ggsave(
  filename = file.path(plotdir, "shap_values_LASSO.png"),
  plot = shap_values,
  width = 10,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(plotdir, "shap_values_LASSO.emf"),
  plot = shap_values,
  device = devEMF::emf,
  width = 10,
  height = 5
)

ggsave(
  filename = file.path(plotdir, "shap_values_LASSO.pdf"),
  plot = shap_values,
  width = 10,
  height = 5
)

###############################################################################
###############################################################################
###############################################################################

### Actual vs Predict: out of sample

dates <- df$YearMon

lassohat <- predict(lasso_model, newx = X_test_std)
lassohat <- as.numeric(lassohat)

plot_df <- data.frame(
  Date   = dates[test_idx],             # or seq_along(test_idx)
  actual = y_test,
  pred   = lassohat
)

actualvpredict <- ggplot(plot_df, aes(x = Date)) +
  geom_line(aes(y = actual,
                linetype = "Actual",
                color    = "Actual"),
            linewidth = 1.2) +
  geom_line(aes(y = pred,
                linetype = "Predicted",
                color    = "Predicted"),
            linewidth = 1.2) +
  labs(
    x = "",
    y = "Inflation YoY"
  ) +
  scale_linetype_manual(
    values = c("Actual" = "solid", "Predicted" = "solid"),
    name   = ""
  ) +
  scale_color_manual(
    values = c("Actual"    = "grey40",
               "Predicted" = "#006400"),
    name = ""
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = c(0.85, 0.15),
    legend.background = element_rect(fill = scales::alpha("white", 0.8), color = NA),
    axis.text.x = element_text(angle = 0)   # keep labels clean
  )
actualvpredict

ggsave(
  filename = file.path(plotdir, "actualvpredict_LASSO.png"),
  plot = actualvpredict,
  width = 10,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(plotdir, "actualvpredict_LASSO.emf"),
  plot = actualvpredict,
  device = devEMF::emf,
  width = 10,
  height = 5
)

ggsave(
  filename = file.path(plotdir, "actualvpredict_LASSO.pdf"),
  plot = actualvpredict,
  width = 10,
  height = 5
)

###############################################################################
###############################################################################
###############################################################################

### Actual vs Predict: LASSO – combined train + test

dates <- df$YearMon

# --- In-sample predictions ---
lassohat_in <- predict(lasso_model, newx = X_train_std)
lassohat_in <- as.numeric(lassohat_in)

# --- Build combined data frame ---
combined_lasso <- rbind(
  data.frame(
    Date   = dates[train_idx],
    actual = y_train,
    pred   = lassohat_in,
    set    = "Train"
  ),
  data.frame(
    Date   = dates[test_idx],
    actual = y_test,
    pred   = lassohat,
    set    = "Test"
  )
)

split_date <- dates[test_idx[1]]

saveRDS(combined_lasso, path_model("preds_LASSO.rds"))

actualvpredict_LASSO_all <- ggplot(combined_lasso, aes(x = Date)) +
  geom_line(aes(y = actual,
                linetype = "Actual",
                color    = "Actual"),
            linewidth = 1.2) +
  geom_line(aes(y = pred,
                linetype = "Predicted",
                color    = "Predicted"),
            linewidth = 1.2) +
  geom_vline(
    xintercept = split_date,
    linetype   = "dashed",
    color      = "red",
    linewidth  = 1
  ) +
  labs(
    x = "",
    y = "Inflation YoY"
  ) +
  scale_linetype_manual(
    values = c("Actual" = "solid", "Predicted" = "solid"),
    name   = ""
  ) +
  scale_color_manual(
    values = c("Actual"    = "grey40",
               "Predicted" = "#006400"),
    name = ""
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(size = 12),
    legend.position  = c(0.85, 0.15),
    legend.background = element_rect(fill = scales::alpha("white", 0.8), color = NA),
    axis.text.x      = element_text(angle = 0)
  )

actualvpredict_LASSO_all

ggsave(
  filename = file.path(plotdir, "actualvpredict_all_LASSO.png"),
  plot = actualvpredict_LASSO_all,
  width = 10,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(plotdir, "actualvpredict_all_LASSO.emf"),
  plot = actualvpredict_LASSO_all,
  device = devEMF::emf,
  width = 10,
  height = 5
)

ggsave(
  filename = file.path(plotdir, "actualvpredict_all_LASSO.pdf"),
  plot = actualvpredict_LASSO_all,
  width = 10,
  height = 5
)


###############################################################################
###############################################################################
################################ TABLES #######################################
###############################################################################
###############################################################################

### DATA SUMMARY TABLE

## Dataset-level summary
data_summary <- tibble(
  Start      = min(df$YearMon),
  End        = max(df$YearMon),
  N_obs      = nrow(df),
  Frequency  = "Monthly",
  Variables  = ncol(df)
)

## Remove lag variables
df_main <- df %>% select(-inflation_lag1)

## Descriptions
desc_list <- list(
  YearMon              = "Month-Year timestamp",
  UNRATE               = "Unemployment Rate (%)",
  UMCSENT              = "Consumer Sentiment Index",
  TOTALSL              = "Total Consumer Credit Outstanding",
  CES3000000008        = "Avg Hourly Earnings, Manufacturing",
  PCE                  = "Personal Consumption Expenditures",
  RRSFS                = "Real Retail Sales",
  FEDFUNDS             = "Federal Funds Rate (%)",
  M2SL                 = "M2 Money Stock",
  GS2                  = "2-Year Treasury Yield",
  BOPGSTB              = "Trade Balance: Goods & Services",
  IR                   = "Import Price Index",
  IQ                   = "Export Price Index",
  DCOILWTICO           = "WTI Crude Oil Price",
  GASREGW              = "Retail Gasoline Price",
  INDPRO               = "Industrial Production Index",
  PPIACO               = "PPI: All Commodities",
  HOUST                = "Housing Starts",
  PERMIT               = "Building Permits",
  EXPINF1YR            = "1-Year Inflation Expectations",
  REAINTRATREARAT1YE   = "1-Year Real Interest Rate Expectations",
  CSUSHPINSA           = "National Home Price Index (Case-Shiller)",
  CUUR0000SEHA         = "CPI: Rent of Primary Residence",
  APU000072610         = "Average Electricity Price (per kWh)",
  PCU335311335311      = "PPI: Electric Power & Transformer Mfg.",
  STLFSI4              = "St. Louis Fed Financial Stress Index",
  BCOM                 = "Bloomberg Commodity Index",
  GOLD                 = "Gold Price",
  inflation            = "CPI Inflation"
)

## Transformation symbols
trans_list <- list(
  YearMon              = "–",
  
  # ΔX — first differences (not logged)
  UNRATE               = "ΔX",
  UMCSENT              = "ΔX",
  FEDFUNDS             = "ΔX",
  GS2                  = "ΔX",
  EXPINF1YR            = "ΔX",
  REAINTRATREARAT1YE   = "ΔX",
  
  # Δ log(X) — log-differences
  TOTALSL              = "Δ log(X)",
  CES3000000008        = "Δ log(X)",
  PCE                  = "Δ log(X)",
  RRSFS                = "Δ log(X)",
  M2SL                 = "Δ log(X)",
  IR                   = "Δ log(X)",
  IQ                   = "Δ log(X)",
  DCOILWTICO           = "Δ log(X)",
  GASREGW              = "Δ log(X)",
  INDPRO               = "Δ log(X)",
  PPIACO               = "Δ log(X)",
  HOUST                = "Δ log(X)",
  PERMIT               = "Δ log(X)",
  CSUSHPINSA           = "Δ log(X)",
  CUUR0000SEHA         = "Δ log(X)",
  APU000072610         = "Δ log(X)",
  PCU335311335311      = "Δ log(X)",
  GOLD                 = "Δ log(X)",
  BCOM                 = "Δ log(X)",
  
  # First difference in levels (trade balance, cannot log)
  BOPGSTB              = "ΔX",
  
  # Level (no transformation)
  STLFSI4              = "X",
  inflation            = "X"
)

## Sources (update BCOM + GOLD)
var_summary <- tibble(
  Variable      = names(df_main),
  Description   = unlist(desc_list[names(df_main)]),
  Frequency     = "Monthly",
  Source        = case_when(
    Variable == "YearMon" ~ "-",
    Variable == "BCOM"    ~ "Bloomberg",
    Variable == "GOLD"    ~ "Financial data provider",
    TRUE                  ~ "FRED"
  ),
  Transformation = unlist(trans_list[names(df_main)])
)

data_summary
var_summary


###############################################################################
###############################################################################
###############################################################################

### DESCRIPTIVE STATISTICS TABLE

vars_num <- df %>% select(-YearMon, -inflation_lag1)

desc_stats <- vars_num %>%
  summarise(across(
    everything(),
    list(
      mean   = ~mean(.x, na.rm = TRUE),
      sd     = ~sd(.x, na.rm = TRUE),
      min    = ~min(.x, na.rm = TRUE),
      max    = ~max(.x, na.rm = TRUE),
      median = ~median(.x, na.rm = TRUE)
    )
  ))

# Make long-format, nicer for table
desc_stats_long <- tidyr::pivot_longer(
  desc_stats,
  cols = everything(),
  names_to = c("Variable", ".value"),
  names_sep = "_"
)

# Round
desc_stats_long <- desc_stats_long %>%
  mutate(across(where(is.numeric), ~round(.x, 3)))

desc_stats_long

###############################################################################
###############################################################################
###############################################################################

### MODEL PERFORMANCE TABLE

model_perf <- read.xlsx(path_table("results.xlsx")) %>%
  mutate(across(where(is.numeric), ~round(.x, 4)))

model_perf

###############################################################################
###############################################################################
###############################################################################

### SHAP SUMMARY TABLE

S_mat <- sv_xgb$S

shap_table <- tibble::tibble(
  Feature       = colnames(S_mat),
  mean_abs_SHAP = apply(abs(S_mat), 2, mean, na.rm = TRUE)
) %>%
  dplyr::arrange(dplyr::desc(mean_abs_SHAP)) %>%
  dplyr::mutate(
    Rank = dplyr::row_number(),
    mean_abs_SHAP = round(mean_abs_SHAP, 4)
  ) %>%
  dplyr::select(Rank, Feature, mean_abs_SHAP)

shap_table

###############################################################################
###############################################################################
###############################################################################

### FORECAST RESULTS TABLE

forecast_table <- plot_df %>%
  dplyr::mutate(
    error      = pred - actual,
    abs_error  = abs(error),
    sq_error   = (error)^2
  )

# Optional rounding
forecast_table_out <- forecast_table %>%
  dplyr::mutate(
    actual    = round(actual, 3),
    pred      = round(pred, 3),
    error     = round(error, 3),
    abs_error = round(abs_error, 3),
    sq_error  = round(sq_error, 4)
  )

forecast_table_out

###############################################################################
###############################################################################
###############################################################################

### LASSO COEFFICIENTS

# Extract coefficients
coef_lasso <- coef(lasso_model)

# Convert to data frame
lasso_coef_table <- data.frame(
  Feature     = rownames(coef_lasso),
  Coefficient = as.numeric(coef_lasso),
  row.names  = NULL
)

# Remove intercept
lasso_coef_table <- lasso_coef_table |>
  dplyr::filter(Feature != "(Intercept)")

# Add absolute value and rank
lasso_coef_table <- lasso_coef_table |>
  dplyr::mutate(
    Abs_Coefficient = abs(Coefficient)
  ) |>
  dplyr::arrange(desc(Abs_Coefficient))

# Optional: keep only selected variables
lasso_coef_table_nonzero <- lasso_coef_table |>
  dplyr::filter(Abs_Coefficient > 0)

# Round values (paper-friendly)
lasso_coef_table_nonzero <- lasso_coef_table_nonzero |>
  dplyr::mutate(
    Coefficient      = round(Coefficient, 4),
    Abs_Coefficient  = round(Abs_Coefficient, 4)
  )

lasso_coef_table_nonzero

write.xlsx(
  x = list(
    "Dataset Summary"  = data_summary,
    "Variable Summary" = var_summary,
    "Descriptive Statistics" = desc_stats_long,
    "Model Performance" = model_perf,
    "Shap Values Table LASSO" = shap_table,
    "Actual vs Predict LASSO" = forecast_table_out,
    "Lasso Coefficients" = lasso_coef_table_nonzero
  ),
  file = file.path(tabdir, "table_variable_summary_LASSO.xlsx"),
  asTable = TRUE
)

###############################################################################
###############################################################################
###############################################################################

# Reload data
df <- read.csv(FILE_MODEL_DATA)
df$YearMon <- as.yearmon(df$YearMon)

# (Here ARIMAX is on "no-lag" data; if you want lags, add them BEFORE na.omit)
df <- na.omit(df)

# Design matrix and target
x <- model.matrix(inflation ~ . - YearMon, df)[, -1]
y <- df$inflation

# Chronological split (same logic as in the loop)
n <- nrow(df)
slices <- createTimeSlices(
  1:n,
  initialWindow = floor(0.7 * n),
  horizon       = n - floor(0.7 * n),
  fixedWindow   = FALSE
)

train_idx <- slices$train[[1]]
test_idx  <- slices$test[[1]]

X_train <- x[train_idx, ]
X_test  <- x[test_idx, ]
y_train <- y[train_idx]
y_test  <- y[test_idx]

## ===== Standardize here (only X, using TRAIN stats) =====
train_means <- colMeans(X_train)
train_sds   <- apply(X_train, 2, sd)

X_train_std <- scale(X_train, center = train_means, scale = train_sds)
X_test_std  <- scale(X_test,  center = train_means, scale = train_sds)

# --- 1. Define target as time series -----------------------------------
y_ts       <- ts(y, frequency = 12)   # 12 for monthly
y_train_ts <- y_ts[train_idx]
y_test_ts  <- y_ts[test_idx]

# --- 2. Fit ARIMAX on training set -------------------------------------
arimax_model <- auto.arima(
  y_train_ts,
  xreg          = X_train_std,
  d             = 0,              # keep inflation in levels
  stepwise      = FALSE,
  approximation = FALSE
)
arimax_model

# --- 3. Forecast using test regressors ---------------------------------
arimax_forecast <- forecast(arimax_model, xreg = X_test_std)

# ======================================================================
#   COMBINED TRAIN + TEST PLOT IN YOUR LASSO FORMAT
# ======================================================================

# In-sample fitted values
arimax_fitted <- fitted(arimax_model)   # length == length(y_train_ts)

dates <- df$YearMon

# Build combined data frame (Train + Test)
combined_arimax <- rbind(
  data.frame(
    Date   = dates[train_idx],
    actual = y_train,
    pred   = as.numeric(arimax_fitted),
    set    = "Train"
  ),
  data.frame(
    Date   = dates[test_idx],
    actual = y_test,
    pred   = as.numeric(arimax_forecast$mean),
    set    = "Test"
  )
)

# Split date (first test observation)
split_date <- dates[test_idx[1]]

saveRDS(combined_arimax, path_model("preds_ARIMAX.rds"))

actualvpredict_ARIMAX_all <- ggplot(combined_arimax, aes(x = Date)) +
  geom_line(aes(y = actual,
                linetype = "Actual",
                color    = "Actual"),
            linewidth = 1.2) +
  geom_line(aes(y = pred,
                linetype = "Predicted",
                color    = "Predicted"),
            linewidth = 1.2) +
  geom_vline(
    xintercept = split_date,
    linetype   = "dashed",
    color      = "red",
    linewidth  = 1
  ) +
  labs(
    x = "",
    y = "Inflation YoY"
  ) +
  scale_linetype_manual(
    values = c("Actual" = "solid", "Predicted" = "solid"),
    name   = ""
  ) +
  scale_color_manual(
    values = c("Actual"    = "grey40",
               "Predicted" = "#1F4E79"),
    name = ""
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title        = element_text(face = "bold"),
    plot.subtitle     = element_text(size = 12),
    legend.position   = c(0.85, 0.15),
    legend.background = element_rect(fill = scales::alpha("white", 0.8), color = NA),
    axis.text.x       = element_text(angle = 0)
  )

actualvpredict_ARIMAX_all

# Save in PNG / EMF / PDF (same as LASSO)
ggsave(
  filename = file.path(plotdir, "actualvpredict_all_ARIMAX.png"),
  plot = actualvpredict_ARIMAX_all,
  width = 10,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(plotdir, "actualvpredict_all_ARIMAX.emf"),
  plot = actualvpredict_ARIMAX_all,
  device = devEMF::emf,
  width = 10,
  height = 5
)

ggsave(
  filename = file.path(plotdir, "actualvpredict_all_ARIMAX.pdf"),
  plot = actualvpredict_ARIMAX_all,
  width = 10,
  height = 5
)


###############################################################################
###############################################################################
###############################################################################

# Reload data
df <- read.csv(FILE_MODEL_DATA)
df$YearMon <- as.yearmon(df$YearMon)

# (Here ARIMAX is on "no-lag" data; if you want lags, add them BEFORE na.omit)
df <- na.omit(df)

df <- df %>%
  mutate(
    inflation_lag1 = lag(inflation, 1)
  )

df <- na.omit(df)

# Design matrix and target
x <- model.matrix(inflation ~ . - YearMon, df)[, -1]
y <- df$inflation

# Chronological split (same logic as in the loop)
n <- nrow(df)
slices <- createTimeSlices(
  1:n,
  initialWindow = floor(0.7 * n),
  horizon       = n - floor(0.7 * n),
  fixedWindow   = FALSE
)

train_idx <- slices$train[[1]]
test_idx  <- slices$test[[1]]

X_train <- x[train_idx, ]
X_test  <- x[test_idx, ]
y_train <- y[train_idx]
y_test  <- y[test_idx]

## ===== Standardize here (only X, using TRAIN stats) =====
train_means <- colMeans(X_train)
train_sds   <- apply(X_train, 2, sd)

X_train_std <- scale(X_train, center = train_means, scale = train_sds)
X_test_std  <- scale(X_test,  center = train_means, scale = train_sds)

# --- Combined RF: Train + Test ---------------------------------------------

dates <- df$YearMon  # same as LASSO

p <- ncol(X_train_std)

rf_model <- randomForest::randomForest(
  x        = X_train_std,
  y        = y_train,
  mtry     = floor(sqrt(p)),
  ntree    = 500,
  nodesize = 1
)

rfhat <- predict(rf_model, newdata = X_test_std)

rfhat_in <- predict(rf_model, newdata = X_train_std)


rf_combined <- rbind(
  data.frame(
    Date   = dates[train_idx],
    actual = y_train,
    pred   = rfhat_in,
    set    = "Train"
  ),
  data.frame(
    Date   = dates[test_idx],
    actual = y_test,
    pred   = rfhat,
    set    = "Test"
  )
)

split_date <- dates[test_idx[1]]

saveRDS(rf_combined, path_model("preds_RF.rds"))

actualvpredict_RF_all <- ggplot(rf_combined, aes(x = Date)) +
  geom_line(aes(y = actual,
                linetype = "Actual",
                color    = "Actual"),
            linewidth = 1.2) +
  geom_line(aes(y = pred,
                linetype = "Predicted",
                color    = "Predicted"),
            linewidth = 1.2) +
  geom_vline(
    xintercept = split_date,
    linetype   = "dashed",
    color      = "red",
    linewidth  = 1
  ) +
  labs(
    x = "",
    y = "Inflation YoY"
  ) +
  scale_linetype_manual(
    values = c("Actual" = "solid", "Predicted" = "solid"),
    name   = ""
  ) +
  scale_color_manual(
    values = c("Actual"    = "grey40",
               "Predicted" = "#E68613"),
    name = ""
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title        = element_text(face = "bold"),
    plot.subtitle     = element_text(size = 12),
    legend.position   = c(0.85, 0.15),
    legend.background = element_rect(fill = scales::alpha("white", 0.8), color = NA),
    axis.text.x       = element_text(angle = 0)
  )

actualvpredict_RF_all

ggsave(
  filename = file.path(plotdir, "actualvpredict_all_RF.png"),
  plot     = actualvpredict_RF_all,
  width    = 10,
  height   = 5,
  dpi      = 300
)

ggsave(
  filename = file.path(plotdir, "actualvpredict_all_RF.emf"),
  plot     = actualvpredict_RF_all,
  device   = devEMF::emf,
  width    = 10,
  height   = 5
)

ggsave(
  filename = file.path(plotdir, "actualvpredict_all_RF.pdf"),
  plot     = actualvpredict_RF_all,
  width    = 10,
  height   = 5
)
