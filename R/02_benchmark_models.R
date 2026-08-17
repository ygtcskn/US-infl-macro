rm(list = ls())

source("R/00_config.R")

# PACKAGES
library(glmnet)
library(caret)
library(ggplot2)
library(dplyr)
library(zoo)
library(nnls)
library(gam)
library(splines)
library(foreach)
library(party)
library(nnet)
library(ipred)
library(kernlab)
library(SuperLearner)
library(AER)
library(xgboost)
library(data.table)
library(shapviz)
library(kernelshap)
library(openxlsx)
library(forecast)

### METHOD SELECTION

for (type in 1:2) {
  
  # 1 = no lag, 2 = AR1
  
  ###############################################################################
  ###############################################################################
  ###############################################################################
  ###############################################################################
  ###############################################################################
  
  ######### PREPARING THE DATA
  
  df <- read.csv(FILE_MODEL_DATA)
  df$YearMon <- as.yearmon(df$YearMon)
  
  ###############################################################################
  
  # Switching model type
  
  if (type == 1) {
    
    # 1) Without Lag
    
    transformation_name <- "Without_lag"
    
    results <- data.frame(
      transformation = character(),
      model  = character(),
      rmse   = numeric(),
      rmse_in = numeric(),
      rmspe  = numeric(),
      mae    = numeric(),
      smape  = numeric()
    )
    
  }
  
  if (type == 2) {
    
    # 2) AR1
    
    df <- df %>%
      mutate(
        inflation_lag1 = lag(inflation, 1)
      )
    transformation_name <- "AR1"
    
    results <- readRDS(path_model("results_table.rds"))
    
  }
  
  df <- na.omit(df)
  
  set.seed(1234)
  
  ###############################################################################
  
  x <- model.matrix(inflation ~ . - YearMon, df)[, -1]
  y <- df$inflation
  
  # Define train and test (CHRONOLOGICAL SEPARATION) with createTimeSlices
  
  n <- nrow(df)
  slices <- createTimeSlices(1:n,
                             initialWindow = floor(0.7 * n),
                             horizon = n - floor(0.7 * n),
                             fixedWindow = FALSE)
  
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
  ## ========================================================
  
  ###############################################################################
  ############################## LASSO ##########################################
  ###############################################################################
  
  # Fitting the Model
  
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
  
  # Finding the best lambda
  
  best_lambda <- lasso_fit$lambda.min
  
  lasso_model <- glmnet(
    x = X_train_std,
    y = y_train,
    family = "gaussian",
    alpha = 1,
    lambda = best_lambda
  )
  
  # Coefficients
  
  coef_lasso <- coef(lasso_model)
  
  lassohat <- predict(lasso_model, newx = X_test_std)
  lassohat <- as.numeric(lassohat)
  
  ###############################################################################
  # In-sample prediction, used by the RMSE below

  pred_in_sample <- predict(lasso_model, newx = X_train_std, s = "lambda.min")
  pred_in_sample <- as.numeric(pred_in_sample)

  ###############################################################################
  # LASSO metrics (out-of-sample + in-sample)
  
  rmse_lasso_out <- sqrt(mean((y_test  - lassohat)^2))
  rmse_lasso_in  <- sqrt(mean((y_train - pred_in_sample)^2))
  
  results <- rbind(results, data.frame(
    transformation = transformation_name,
    model   = "LASSO",
    rmse    = rmse_lasso_out,
    rmse_in = rmse_lasso_in,
    rmspe   = sqrt(mean(((y_test - lassohat) / y_test)[is.finite((y_test - lassohat) / y_test)]^2)),
    mae     = mean(abs(y_test - lassohat)),
    smape   = mean(abs(y_test - lassohat) / ((abs(y_test) + abs(lassohat)) / 2))
  ))
  
  
  ###############################################################################
  ############################## RANDOM FOREST ##################################
  ###############################################################################
  
  p <- ncol(X_train_std)
  
  rf_model <- randomForest::randomForest(
    x        = X_train_std,
    y        = y_train,
    mtry     = floor(sqrt(p)),
    ntree    = 500,
    nodesize = 1
  )
  
  rfhat <- predict(rf_model, newdata = X_test_std)

  ###############################################################################
  # In-sample prediction, used by the RMSE below

  rfhat_in <- predict(rf_model, newdata = X_train_std)

  ###############################################################################
  ### RF metrics
  
  rmse_rf_out <- sqrt(mean((y_test  - rfhat)^2))
  rmse_rf_in  <- sqrt(mean((y_train - rfhat_in)^2))
  
  results <- rbind(results, data.frame(
    transformation = transformation_name,
    model   = "RF",
    rmse    = rmse_rf_out,
    rmse_in = rmse_rf_in,
    rmspe   = sqrt(mean(((y_test - rfhat) / y_test)[is.finite((y_test - rfhat) / y_test)]^2)),
    mae     = mean(abs(y_test - rfhat)),
    smape   = mean(abs(y_test - rfhat) / ((abs(y_test) + abs(rfhat)) / 2))
  ))
  
  
  
  ###############################################################################
  ############################## XGBoost (FAST) ##################################
  ###############################################################################
  
  trainMatrix <- xgb.DMatrix(data = X_train_std, label = y_train)
  valMatrix   <- xgb.DMatrix(data = X_test_std,  label = y_test)
  
  params <- list(
    booster = "gbtree",
    objective = "reg:squarederror",
    
    # Learning / model complexity
    eta = 0.07,              # middle-speed learning
    max_depth = 3,           # moderate flexibility
    min_child_weight = 5,    # avoid tiny leaves
    
    # Regularization
    lambda = 3,              # moderate L2
    alpha = 1,               # light L1
    gamma = 2,               # require some improvement for splitting
    
    # Randomness
    subsample = 0.7,         # not too low, not too high
    colsample_bytree = 0.7   # middle ground
  )
  
  
  xgboost_model <- xgb.train(
    params = params,
    data = trainMatrix,
    nrounds = 300,
    watchlist = list(val = valMatrix),
    early_stopping_rounds = 15,
    verbose = 1
  )
  
  pred_val <- predict(xgboost_model, newdata = valMatrix)

  ###############################################################################
  # In-sample prediction, used by the RMSE below

  xgb_pred_in <- predict(xgboost_model, newdata = trainMatrix)

  ###############################################################################
  ### XGBoost metrics
  
  rmse_xgb_out <- sqrt(mean((y_test  - pred_val)^2))
  rmse_xgb_in  <- sqrt(mean((y_train - xgb_pred_in)^2))
  
  results <- rbind(results, data.frame(
    transformation = transformation_name,
    model   = "XGBoost",
    rmse    = rmse_xgb_out,
    rmse_in = rmse_xgb_in,
    rmspe   = sqrt(mean(((y_test - pred_val) / y_test)[is.finite((y_test - pred_val) / y_test)]^2)),
    mae     = mean(abs(y_test - pred_val)),
    smape   = mean(abs(y_test - pred_val) / ((abs(y_test) + abs(pred_val)) / 2))
  ))
  
  saveRDS(results, path_model("results_table.rds"))
  
} # end for(type)

###############################################################################
################################# ARIMAX #######################################
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

# --- 4. In-sample fit ---------------------------------------------------

arimax_fitted <- fitted(arimax_model)  # length == length(y_train_ts)

# --- 6. Evaluation metrics ---------------------------------------------
arimax_eval <- accuracy(arimax_forecast, y_test_ts)
print(arimax_eval)

# --- Predictions and true values ---
yhat  <- as.numeric(arimax_forecast$mean)
ytrue <- as.numeric(y_test_ts)

###############################################################################
### ARIMAX metrics

rmse_arimax_out <- sqrt(mean((ytrue      - yhat)^2))
rmse_arimax_in  <- sqrt(mean((y_train_ts - arimax_fitted)^2))

results <- rbind(
  results,
  data.frame(
    transformation = "ARIMAX_no_lag",
    model          = "Arimax",
    rmse           = rmse_arimax_out,
    rmse_in        = rmse_arimax_in,
    rmspe          = sqrt(mean(((ytrue - yhat) / ytrue)[is.finite((ytrue - yhat) / ytrue)]^2)),
    mae            = mean(abs(ytrue - yhat)),
    smape          = mean(2 * abs(ytrue - yhat) / (abs(ytrue) + abs(yhat)))
  )
)


# --- 7. Save final results once ----------------------------------------
write.xlsx(results, file = path_table("results.xlsx"), rowNames = FALSE)
