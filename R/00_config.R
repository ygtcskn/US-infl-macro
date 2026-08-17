# Paths and project-wide settings. Every other script starts with:
#
#     source("R/00_config.R")

### PROJECT ROOT

if (requireNamespace("here", quietly = TRUE)) {
  PROJ_ROOT <- here::here()
} else {
  PROJ_ROOT <- "C:/Users/ygtcs/Desktop/R base/inflation and macro"
}

### DIRECTORIES

# data/raw is immutable input; everything else is rebuilt by the pipeline.

DIR_DATA   <- file.path(PROJ_ROOT, "data")
DIR_RAW    <- file.path(DIR_DATA, "raw")
DIR_TEMP   <- file.path(DIR_DATA, "temp")
DIR_FINAL  <- file.path(DIR_DATA, "final")

DIR_OUTPUT <- file.path(PROJ_ROOT, "output")
FIG_FINAL  <- file.path(DIR_OUTPUT, "figures", "final")
DIR_TABLES <- file.path(DIR_OUTPUT, "tables")
DIR_MODELS <- file.path(DIR_OUTPUT, "models")
DIR_LOGS   <- file.path(DIR_OUTPUT, "logs")

for (d in c(DIR_RAW, DIR_TEMP, DIR_FINAL, FIG_FINAL,
            DIR_TABLES, DIR_MODELS, DIR_LOGS)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

### PATH HELPERS

path_raw        <- function(...) file.path(DIR_RAW, ...)
path_temp       <- function(...) file.path(DIR_TEMP, ...)
path_data_final <- function(...) file.path(DIR_FINAL, ...)
path_fig_final  <- function(...) file.path(FIG_FINAL, ...)
path_table      <- function(...) file.path(DIR_TABLES, ...)
path_model      <- function(...) file.path(DIR_MODELS, ...)
path_log        <- function(...) file.path(DIR_LOGS, ...)

### ANALYSIS CONSTANTS

SEED         <- 1234
SAMPLE_START <- "1992-01"   # binding start is ~1994-01 (EXPINF1YR, STLFSI4)
TRAIN_FRAC   <- 0.70
CV_FOLDS     <- 5

### FILES

FILE_CPI  <- path_raw("CPIAUCSL_PC1.csv")
FILE_GOLD <- path_raw("gold.csv")
FILE_BCOM <- path_raw("bcom.csv")

FILE_MODEL_DATA <- path_data_final("infl_macro.csv")
