rm(list = ls())

source("R/00_config.R")

library(ggplot2)
library(dplyr)
library(zoo)
library(devEMF)

plotdir <- FIG_FINAL

### PANEL SELECTION

# Ranked by out-of-sample RMSE in results.xlsx.
panel_models <- c("ARIMAX", "LASSO", "RF", "XGBoost")

model_colors <- c(
  Actual  = "grey40",
  ARIMAX  = "#1F4E79",
  LASSO   = "#006400",
  RF      = "#E68613",
  XGBoost = "#6A1B9A"
)

###############################################################################
###############################################################################
###############################################################################

### LOADING THE PREDICTIONS

pred_files <- file.path(DIR_MODELS, paste0("preds_", panel_models, ".rds"))
missing <- panel_models[!file.exists(pred_files)]

if (length(missing) > 0) {
  stop("Missing predictions for: ", paste(missing, collapse = ", "),
       "\nRun stages 3 and 4 first.", call. = FALSE)
}

preds <- lapply(seq_along(panel_models), function(i) {
  readRDS(pred_files[i]) %>%
    mutate(Model = panel_models[i])
})

preds <- bind_rows(preds) %>%
  mutate(
    Date  = as.yearmon(Date),
    Model = factor(Model, levels = panel_models)
  )

# One split line per panel: the first test month of that model
splits <- preds %>%
  filter(set == "Test") %>%
  group_by(Model) %>%
  summarise(split = min(Date), .groups = "drop")

###############################################################################
###############################################################################
###############################################################################

### GRID PLOT

model_grid <- ggplot(preds, aes(x = Date)) +
  geom_line(aes(y = actual, colour = "Actual"), linewidth = 0.6) +
  geom_line(aes(y = pred, colour = Model), linewidth = 0.6) +
  geom_vline(
    data = splits,
    aes(xintercept = split),
    linetype  = "dashed",
    color     = "red",
    linewidth = 0.6
  ) +
  facet_wrap(~ Model, ncol = 2) +
  scale_colour_manual(values = model_colors, breaks = c("Actual", panel_models)) +
  scale_x_yearmon(n = 7) +
  labs(x = "", y = "Inflation YoY", colour = "") +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "bottom",

    axis.text.x  = element_text(size = 10),
    axis.text.y  = element_text(size = 10),
    axis.ticks   = element_line(color = "black", linewidth = 0.4),

    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text       = element_text(size = 11, face = "bold", hjust = 0),

    panel.spacing      = unit(1.2, "lines"),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3)
  )

model_grid

ggsave(
  filename = file.path(plotdir, "model_grid.png"),
  plot = model_grid,
  width = 12,
  height = 7,
  dpi = 200
)

ggsave(
  filename = file.path(plotdir, "model_grid.pdf"),
  plot = model_grid,
  width = 12,
  height = 7
)

ggsave(
  filename = file.path(plotdir, "model_grid.emf"),
  plot = model_grid,
  device = devEMF::emf,
  width = 12,
  height = 7
)
