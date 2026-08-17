# Runs the whole pipeline in order. Must be run from the project root.
#
#   Rscript run_all.R              whole pipeline
#   Rscript run_all.R 2 3          stages 2 and 3 only
#   source("run_all.R")            from the R console
#   STAGES <- 2:4; source("run_all.R")

t_pipeline_start <- Sys.time()

### CONFIG

if (!file.exists(file.path("R", "00_config.R"))) {
  stop(
    "run_all.R must be run from the project root (the folder containing R/ and data/).\n",
    "  Current working directory: ", getwd(),
    call. = FALSE
  )
}

source(file.path("R", "00_config.R"))

### PIPELINE DEFINITION

stages <- data.frame(
  id       = 1:5,
  file     = c(
    "R/01_prepare_data.R",
    "R/02_benchmark_models.R",
    "R/03_xgboost_shap.R",
    "R/04_final_figures.R",
    "R/05_model_grid.R"
  ),
  label    = c(
    "Prepare data      (FRED pull, transforms, ADF, differencing)",
    "Benchmark models  (LASSO / RF / XGBoost x {no lag, AR1} + ARIMAX)",
    "XGBoost + SHAP    (feature importance, SHAP, summary tables)",
    "Final figures     (paper figures, LASSO coefficients, PCR)",
    "Model grid        (2x2 actual vs predicted panel)"
  ),
  online   = c(TRUE, FALSE, FALSE, FALSE, FALSE),
  stringsAsFactors = FALSE
)

### STAGE SELECTION

cli_args <- if (interactive()) character(0) else commandArgs(trailingOnly = TRUE)

if (exists("STAGES", envir = globalenv(), inherits = FALSE)) {
  selected <- as.integer(get("STAGES", envir = globalenv()))
} else if (length(cli_args) > 0) {
  selected <- suppressWarnings(as.integer(cli_args))
} else {
  selected <- stages$id
}

if (anyNA(selected) || !all(selected %in% stages$id)) {
  stop("Stages must be integers in 1:", max(stages$id),
       ". Received: ", paste(cli_args, collapse = " "), call. = FALSE)
}
selected <- sort(unique(selected))

### PREFLIGHT

missing_scripts <- stages$file[!file.exists(stages$file)]
if (length(missing_scripts) > 0) {
  stop("Missing pipeline scripts:\n  ", paste(missing_scripts, collapse = "\n  "),
       call. = FALSE)
}

if (1 %in% selected) {
  raw_needed  <- c(FILE_CPI, FILE_GOLD, FILE_BCOM)
  raw_missing <- raw_needed[!file.exists(raw_needed)]
  if (length(raw_missing) > 0) {
    stop("Missing raw inputs in data/raw:\n  ",
         paste(basename(raw_missing), collapse = "\n  "), call. = FALSE)
  }
} else if (!file.exists(FILE_MODEL_DATA)) {
  stop("data/final/infl_macro.csv not found — run stage 1 first:\n",
       "  Rscript run_all.R 1", call. = FALSE)
}

### LOGGING

run_id  <- format(t_pipeline_start, "%Y%m%d-%H%M%S")
logfile <- path_log(paste0("run-", run_id, ".log"))

log_con <- file(logfile, open = "wt")
sink(log_con, split = TRUE)
on.exit({
  while (sink.number() > 0) sink()
  close(log_con)
}, add = TRUE)

rule <- function(char = "=") cat(strrep(char, 78), "\n", sep = "")

rule()
cat("US-INFL-MACRO -- pipeline run ", run_id, "\n", sep = "")
rule()
cat("R version   : ", R.version.string, "\n", sep = "")
cat("Project root: ", PROJ_ROOT, "\n", sep = "")
cat("Log file    : ", logfile, "\n", sep = "")
cat("Stages      : ", paste(selected, collapse = ", "), " of ",
    paste(stages$id, collapse = ", "), "\n", sep = "")
if (any(stages$online[selected])) {
  cat("Note        : stage 1 downloads from FRED and needs an internet connection\n")
}
cat("\n")

### RUN

# Each stage runs in its own environment: the scripts open with rm(list = ls()),
# which would otherwise wipe this runner's variables, and isolation catches any
# stage that only works because of state left behind by the previous one.

wd_before <- getwd()
on.exit(setwd(wd_before), add = TRUE)

status  <- character(length(selected))
elapsed <- numeric(length(selected))

for (i in seq_along(selected)) {

  s  <- stages[stages$id == selected[i], ]
  t0 <- Sys.time()

  rule("-")
  cat("[", format(Sys.time(), "%H:%M:%S"), "]  STAGE ", s$id, "  ", s$label, "\n", sep = "")
  cat("           ", s$file, "\n", sep = "")
  rule("-")

  result <- tryCatch({
    source(s$file, local = new.env(parent = globalenv()), echo = FALSE)
    "OK"
  }, error = function(e) {
    structure("FAILED", message = conditionMessage(e))
  })

  setwd(wd_before)

  elapsed[i] <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  status[i]  <- as.character(result)

  cat("\n[", format(Sys.time(), "%H:%M:%S"), "]  stage ", s$id, ": ", status[i],
      "  (", sprintf("%.1f", elapsed[i]), "s)\n\n", sep = "")

  if (status[i] == "FAILED") {
    cat("Error in ", s$file, ":\n  ", attr(result, "message"), "\n\n", sep = "")
    cat("Pipeline stopped. Later stages depend on this one, so running them\n")
    cat("now would produce output from stale inputs.\n\n")
    break
  }
}

### SUMMARY

total <- as.numeric(difftime(Sys.time(), t_pipeline_start, units = "secs"))
done  <- seq_along(selected)[status != ""]

rule()
cat("SUMMARY\n")
rule()
for (i in done) {
  s <- stages[stages$id == selected[i], ]
  cat(sprintf("  %-8s stage %d  %-6s %7.1fs  %s\n",
              "", s$id, status[i], elapsed[i], basename(s$file)))
}
if (length(selected) > length(done)) {
  for (j in setdiff(seq_along(selected), done)) {
    cat(sprintf("  %-8s stage %d  %-6s %8s  %s\n",
                "", stages$id[stages$id == selected[j]], "SKIPPED", "-",
                basename(stages$file[stages$id == selected[j]])))
  }
}
cat("\n  total ", sprintf("%.1f", total), "s\n", sep = "")
cat("  log   ", logfile, "\n", sep = "")

si_file <- path_log(paste0("session-info-", run_id, ".txt"))
writeLines(capture.output(sessionInfo()), si_file)
cat("  env   ", si_file, "\n", sep = "")
rule()

if (any(status == "FAILED")) {
  if (!interactive()) quit(status = 1)
}
