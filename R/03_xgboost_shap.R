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
library(devEMF)

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
### XGBoost Model

# Creating a DMatrix

trainMatrix <- xgb.DMatrix(data = X_train_std, label = y_train)
valMatrix   <- xgb.DMatrix(data = X_test_std,  label = y_test)

params <- list(
  booster = "gbtree",
  objective = "reg:squarederror",
  
  # Learning / model complexity
  eta = 0.07,
  max_depth = 3,
  min_child_weight = 5,
  
  # Regularization
  lambda = 3,
  alpha = 1,
  gamma = 2,
  
  # Randomness
  subsample = 0.7,
  colsample_bytree = 0.7
)


xgboost_model <- xgb.train(
  params = params,
  data = trainMatrix,
  nrounds = 300,
  watchlist = list(val = valMatrix),
  early_stopping_rounds = 15,
  verbose = 1
)
xgboost_model

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
    values = c("Inflation" = "#006400"),
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
  filename = file.path(plotdir,"inflation_timeseries_XGB.png"),
  plot = inflation_plot,
  width = 10,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(plotdir, "inflation_timeseries_XGB.emf"),
  plot = inflation_plot,
  device = devEMF::emf,
  width = 10,
  height = 5
)



###############################################################################
###############################################################################
###############################################################################

### ALL SERIES PLOT

df_melt <- reshape2::melt(df, id.vars = "YearMon")

all_timeseries <- ggplot(df_melt, aes(YearMon, value)) +
  geom_line(color = "#006400", alpha = 0.8, linewidth = 0.7) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(
    x = "",
    y = ""
  ) +
  scale_x_yearmon(labels = NULL) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    
    # x-axis: remove labels and ticks
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    
    # y-axis: keep labels and ticks
    axis.text.y  = element_text(size = 11),
    axis.ticks.y = element_line(color = "black", linewidth = 0.5),
    
    # remove default axis lines (we draw our own)
    axis.line = element_blank(),
    
    # facet strips
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text       = element_text(size = 11, face = "bold"),
    
    # only horizontal gridlines inside panels
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    
    legend.position = "none",
    panel.spacing   = unit(1, "lines")
  ) +
  # left border in each facet
  geom_vline(xintercept = -Inf, color = "black", linewidth = 0.5) +
  # bottom border in each facet
  geom_hline(yintercept = -Inf, color = "black", linewidth = 0.5)
all_timeseries

ggsave(
  filename = file.path(plotdir, "all_timeseries_XGB.png"),
  plot = all_timeseries,
  width = 10,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(plotdir, "all_timeseries_XGB.emf"),
  plot = all_timeseries,
  device = devEMF::emf,
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
  filename = file.path(plotdir, "correlation_plot_XGB.png"),
  plot = corr_plot,
  width = 10,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(plotdir, "correlation_plot_XGB.emf"),
  plot = corr_plot,
  device = devEMF::emf,
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

### FEATURE IMPORTANCE PLOT

importance <- xgb.importance(feature_names = colnames(trainMatrix), model = xgboost_model)

xgb.plot.importance(importance_matrix = importance)

imp_df <- importance[1:16, ]

feature_importance <- ggplot(imp_df,
       aes(x = reorder(Feature, Gain),  # order by importance
           y = Gain)) +
  geom_col(fill = "#6A1B9A") +
  coord_flip() +                        # features on y-axis
  labs(
    x        = "Features",
    y        = ""
  ) +
  theme_classic(base_size = 14) +       # same base as actual vs predicted
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.x   = element_text(size = 11),
    axis.text.y   = element_text(size = 11)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
feature_importance

ggsave(
  filename = file.path(plotdir, "feature_importance_XGB.png"),
  plot = feature_importance,
  width = 10,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(plotdir, "feature_importance_XGB.emf"),
  plot = feature_importance,
  device = devEMF::emf,
  width = 5,
  height = 5
)
###############################################################################
###############################################################################
###############################################################################

### SHAP GLOBAL IMPORTANCE

shp_xgb = kernelshap(xgboost_model, X = X_train_std, bg_X = X_test_std)

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
  filename = file.path(plotdir, "shap_values_XGB.png"),
  plot = shap_values,
  width = 10,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(plotdir, "shap_values_XGB.emf"),
  plot = shap_values,
  device = devEMF::emf,
  width = 10,
  height = 5
)
###############################################################################
###############################################################################
###############################################################################

### Actual vs Predict: out of sample

dates <- df$YearMon

pred_val <- predict(xgboost_model, newdata = valMatrix) # Predict on Val data

plot_df <- data.frame(
  Date   = dates[test_idx],             # or seq_along(test_idx)
  actual = y_test,
  pred   = pred_val
)

# if Date is "Apr 1994" etc. (character) :
plot_df$Date2 <- as.Date(as.yearmon(plot_df$Date, "%b %Y"))

# if Date is already yearmon:
# plot_df$Date2 <- as.Date(plot_df$Date)

actualvpredict <- ggplot(plot_df, aes(x = Date2)) +
  geom_line(aes(y = actual,
                linetype = "Actual",
                color    = "Actual"),
            linewidth = 1.2) +
  geom_line(aes(y = pred,
                linetype = "Predicted",
                color    = "Predicted"),
            linewidth = 1.2) +
  labs(x = "", y = "Inflation YoY") +
  scale_linetype_manual(values = c("Actual" = "solid", "Predicted" = "solid"),
                        name = "") +
  scale_color_manual(values = c("Actual" = "grey40",
                                "Predicted" = "#6A1B9A"),
                     name = "") +
  scale_x_date(
    limits = c(as.Date("2016-01-01"), max(plot_df$Date2)),
    date_labels = "Jan %Y"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = c(0.85, 0.15),
    legend.background = element_rect(fill = scales::alpha("white", 0.8), color = NA),
    axis.text.x = element_text(angle = 0)
  )

actualvpredict


ggsave(
  filename = file.path(plotdir, "actualvpredict_XGB.png"),
  plot = actualvpredict,
  width = 10,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(plotdir, "actualvpredict_XGB.emf"),
  plot = actualvpredict,
  device = devEMF::emf,
  width = 10,
  height = 5
)

###############################################################################
###############################################################################
###############################################################################

pred_in <- predict(xgboost_model, newdata = trainMatrix)

### Combined Train + Test: Actual vs Predicted (XGBoost)

combined_df <- rbind(
  data.frame(
    Date   = dates[train_idx],
    actual = y_train,
    pred   = pred_in,
    set    = "Train"
  ),
  data.frame(
    Date   = dates[test_idx],
    actual = y_test,
    pred   = pred_val,
    set    = "Test"
  )
)

split_date <- dates[test_idx[1]]

saveRDS(combined_df, path_model("preds_XGBoost.rds"))

actualvpredict_all <- ggplot(combined_df, aes(x = Date)) +
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
    linetype = "dashed",
    color = "red",        # <-- RED SPLIT LINE
    linewidth = 1
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
               "Predicted" = "#6A1B9A"),
    name = ""
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = c(0.85, 0.15),
    legend.background = element_rect(fill = scales::alpha("white", 0.8), color = NA),
    axis.text.x = element_text(angle = 0)
  )

actualvpredict_all

ggsave(
  filename = file.path(plotdir, "actualvpredict_XGB_all.png"),
  plot = actualvpredict_all,
  width = 10,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(plotdir, "actualvpredict_XGB_all.emf"),
  plot = actualvpredict_all,
  device = devEMF::emf,
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

### MODEL PERFORMANCE TABLE

model_perf <- tibble::tibble(
  Model = c("LASSO", "Random Forest", "XGBoost", "ARIMAX"),
  
  RMSE  = c(
    0.295985978,   #  rmse_lasso
    1.801025886,   #  rmse_rf
    1.51937668,    #  rmse_xgb
    1.758017101    #  rmse_arimax
  ),
  
  MAE   = c(
    0.210019174,   #  mae_lasso
    1.004985474,   #  mae_rf
    0.783650171,   #  mae_xgb
    1.181758821    #  mae_arimax
  ),
  
  SMAPE  = c(
    0.094771088,   #  smape_lasso
    0.279321625,   #  smape_rf
    0.195024946,   #  smape_xgb
    0.380795541    #  smape_arimax
  )
) %>%
  mutate(across(where(is.numeric), ~round(.x, 4)))

model_perf


###############################################################################
###############################################################################
###############################################################################

### FEATURE IMPORTANCE TABLE

importance <- xgb.importance(feature_names = colnames(trainMatrix),
                             model = xgboost_model)

imp_table <- importance %>%
  dplyr::slice(1:20) %>%                  # top 20
  dplyr::mutate(
    Gain      = round(Gain, 4),
    Cover     = round(Cover, 4),
    Frequency = round(Frequency, 4)
  )

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

write.xlsx(
  x = list(
    "Dataset Summary"  = data_summary,
    "Variable Summary" = var_summary,
    "Descriptive Statistics" = desc_stats_long,
    "Model Performance" = model_perf,
    "Shap Values Table XGB" = shap_table,
    "Act vs Pre Table XGB" = forecast_table_out
  ),
  file = file.path(tabdir, "table_variable_summary_XGB.xlsx"),
  asTable = TRUE
)
