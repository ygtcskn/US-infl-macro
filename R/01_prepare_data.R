rm(list = ls())

source("R/00_config.R")
tabdir <- DIR_TABLES
plotdir <- FIG_FINAL

library(quantmod)
library(zoo)
library(xts)
library(dplyr)
library(reshape2)
library(ggplot2)
library(tseries)
library(tibble)
library(openxlsx)
library(devEMF)

### Getting Inflation data

inflation <- read.csv(FILE_CPI)
colnames(inflation) <- c("YearMon", "inflation")
inflation$YearMon <- as.yearmon(inflation$YearMon)

### Getting Gold data

gold <- read.csv(FILE_GOLD)
colnames(gold) <- c("YearMon", "GOLD")
gold$YearMon <- as.Date(gold$YearMon, format = "%m/%d/%Y")
gold$YearMon <- as.yearmon(gold$YearMon)

### Getting Bloomberg Commodity Index

bcom <- read.csv(FILE_BCOM)
bcom <- select(bcom, c("Date", "Price"))
colnames(bcom) <- c("YearMon", "BCOM")
bcom$YearMon <- as.Date(bcom$YearMon, format = "%m/%d/%Y")
bcom$YearMon <- as.yearmon(bcom$YearMon)

### Getting Macro Variables for US

symbols <- c(
  # --- Consumer block ---
  "UNRATE",          # Unemployment rate
  "UMCSENT",         # Consumer confidence
  "TOTALSL",         # Total consumer credit outstanding
  "CES3000000008",   # Wages: avg hourly earnings, manufacturing
  "PCE",             # Personal consumption expenditures
  "RRSFS",           # Real retail sales
  
  # --- Monetary block ---
  "FEDFUNDS",        # Fed funds rate
  "M2SL",            # M2 money stock
  "GS2",             # 2-year Treasury yield
  
  # --- Global / external block ---
  "BOPGSTB",         # Trade balance: goods & services
  "IR",              # Import price index
  "IQ",              # Export price inde
  "DCOILWTICO",      # WTI crude oil price
  "GASREGW",         # Retail gasoline price
  
  # --- Industry block ---
  "INDPRO",          # Industrial production
  "PPIACO",          # PPI: all commodities
  
  # --- Housing block ---
  "HOUST",           # Housing starts
  "PERMIT",        # Building permits
  
  "EXPINF1YR",     # 1 year Inflatıon expectation
  
  "REAINTRATREARAT1YE", # 1 year Real Interest Rate expectation
  
  "CSUSHPINSA",    # S&P CoreLogic Case-Shiller U.S. National Home Price Index
  "CUUR0000SEHA",  # Consumer Price Index for All Urban Consumers: Rent of Primary Residence in U.S. City Average
  
  "APU000072610",  # Average Price: Electricity per Kilowatt-Hour in U.S. City Average
  "PCU335311335311", # Producer Price Index by Industry: Electric Power and Specialty Transformer Manufacturing
  
  "STLFSI4"         # St. Louis Fed Financial Stress Index
) 


getSymbols(symbols, src = "FRED", auto.assign = TRUE)

### Arranging frequency and creating the Dataset

# Check frequency
freq_table <- sapply(symbols, function(sym) periodicity(get(sym))$scale)
freq_table

to_m_daily <- function(x) {
  name <- colnames(x)
  x <- na.locf(x)
  x <- aggregate(x, as.yearmon(index(x)), tail, 1)
  colnames(x) <- name
  x
}

to_yearmon <- function(x) {
  name <- colnames(x)
  index(x) <- as.yearmon(index(x))
  x <- aggregate(x, index(x), tail, 1)
  colnames(x) <- name
  x
}

dailySymbols <- "DCOILWTICO"

for (sym in dailySymbols) {
  x <- get(sym)
  x_m <- to_m_daily(x)
  assign(sym, x_m)
}

weeklySymbols <- "GASREGW"

for (sym in weeklySymbols) {
  x   <- get(sym)
  x_m <- to_m_daily(x)
  assign(sym, x_m)
}

# Apply yearmon to ALL series (everything monthly now)
ym_list <- lapply(symbols, function(sym) to_yearmon(get(sym)))
names(ym_list) <- symbols

# Merge all series (monthly)
merged_series <- Reduce(function(a, b) merge(a, b, all = TRUE), ym_list)

### Building the data frame

df_m <- data.frame(
  YearMon = as.yearmon(index(merged_series)),
  coredata(merged_series)
)

###############################################################################
###############################################################################
###############################################################################

### Merge inflation and macro series

start_m <- as.yearmon("1992-01")

# macro series from start_m
macro_sub <- df_m %>%
  dplyr::filter(YearMon >= start_m)

# inflation from start_m
inf_sub <- inflation %>%
  dplyr::filter(YearMon >= start_m)

gold_sub <- gold %>%
  dplyr::filter(YearMon >= start_m)

bcom_sub <- bcom %>%
  dplyr::filter(YearMon >= start_m)


# Inner join: only months present in both
df_ml <- macro_sub %>%
  dplyr::inner_join(inf_sub, by = "YearMon") %>% 
  dplyr::inner_join(gold_sub, by = "YearMon") %>% 
  dplyr::inner_join(bcom_sub, by = "YearMon")


head(df_ml)

### Plot all series

### ALL SERIES PLOT

df_melt <- reshape2::melt(df_ml, id.vars = "YearMon")

all_timeseries <- ggplot(df_melt, aes(x = YearMon, y = value)) +
  geom_line(
    color = "#006400",
    linewidth = 0.6
  ) +
  facet_wrap(
    ~ variable,
    scales = "free_y",
    ncol = 5
  ) +
  labs(
    x = "",
    y = ""
  ) +
  scale_x_yearmon(
    labels = scales::date_format("%Y")   # add years back
  ) +
  theme_classic(base_size = 14) +
  theme(
    # ---- overall ----
    legend.position = "none",
    panel.spacing   = unit(1.2, "lines"),
    
    # ---- x-axis: numbers back (small, subtle) ----
    axis.text.x  = element_text(size = 9),
    axis.ticks.x = element_line(color = "grey40", linewidth = 0.4),
    
    # ---- y-axis: keep numbers ----
    axis.text.y  = element_text(size = 10),
    axis.ticks.y = element_line(color = "grey40", linewidth = 0.4),
    
    # remove default axis lines
    axis.line = element_blank(),
    
    # ---- facet strips ----
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text       = element_text(size = 10, face = "bold"),
    
    # ---- grids ----
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  # left border in each facet
  geom_segment(
    aes(x = min(df_melt$YearMon),
        xend = min(df_melt$YearMon),
        y = -Inf,
        yend = Inf),
    inherit.aes = FALSE,
    linewidth = 0.4,
    color = "black"
  ) +
  # bottom border in each facet
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
  filename = file.path(plotdir, "all_timeseries_levels.png"),
  plot = all_timeseries,
  width = 10,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(plotdir, "all_timeseries_levels.emf"),
  plot = all_timeseries,
  device = devEMF::emf,
  width = 30,
  height = 15
)

ggsave(
  filename = file.path(plotdir, "all_timeseries_levels.pdf"),
  plot = all_timeseries,
  width = 10,
  height = 5
)

### DESCRIPTIVE STATISTICS TABLE

vars_num <- df_ml %>% select(-YearMon)

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

write.xlsx(
  x = desc_stats_long,
  file = file.path(tabdir, "desc_nativetable.xlsx"),
  asTable = TRUE
)


# Untransformed levels, kept separate from the model-ready file
write.csv(df_ml, file.path(DIR_TEMP, "infl_macro_levels.csv"), row.names = FALSE)

###############################################################################
###############################################################################
###############################################################################

df_ml <- df_ml %>%
  mutate(
    # ------------------------------
    # LOG TRANSFORMS (safe, positive)
    # ------------------------------
    PCE         = log(PCE),
    RRSFS       = log(RRSFS),
    TOTALSL     = log(TOTALSL),
    CES3000000008 = log(CES3000000008),
    M2SL        = log(M2SL),
    INDPRO      = log(INDPRO),
    PPIACO      = log(PPIACO),
    HOUST       = log(HOUST),
    PERMIT      = log(PERMIT),
    CSUSHPINSA  = log(CSUSHPINSA),
    CUUR0000SEHA = log(CUUR0000SEHA),
    APU000072610 = log(APU000072610),
    PCU335311335311 = log(PCU335311335311),
    GOLD        = log(GOLD),
    BCOM        = log(BCOM),
    DCOILWTICO  = log(DCOILWTICO),
    GASREGW     = log(GASREGW),
    IR          = log(IR),      # import prices
    IQ          = log(IQ),      # export prices
    
    # ---------------------------------------
    # VARIABLES THAT CANNOT BE LOGGED
    # (negative values: e.g., trade balance)
    # ---------------------------------------
    BOPGSTB = BOPGSTB - lag(BOPGSTB),
    
    # ---------------------------------------
    # VARIABLES KEPT IN LEVELS (no log or diff)
    # ---------------------------------------
    UNRATE    = UNRATE,
    FEDFUNDS  = FEDFUNDS,
    GS2      = GS2,
    EXPINF1YR = EXPINF1YR,
    REAINTRATREARAT1YE = REAINTRATREARAT1YE,
    UMCSENT   = UMCSENT,
    STLFSI4   = STLFSI4,
    inflation = inflation
  ) 

###############################################################################
###############################################################################
###############################################################################

### ADF Test

# Ensure no missing values
df_ml <- na.omit(df_ml)

# Select only numeric variables
vars_numeric <- df_ml %>% select(where(is.numeric))

# Function to run ADF and extract components
run_adf <- function(x) {
  out <- tryCatch(adf.test(x), error = function(e) NULL)
  if (is.null(out)) return(c(NA, NA))
  
  c(statistic = as.numeric(out$statistic),
    p_value   = as.numeric(out$p.value))
}

# Loop over variables
adf_matrix <- t(sapply(vars_numeric, run_adf))

# Build tidy table
adf_table <- tibble(
  Variable   = rownames(adf_matrix),
  ADF_stat   = adf_matrix[, "statistic"],
  p_value    = adf_matrix[, "p_value"],
  Decision   = ifelse(p_value < 0.05, "Stationary", "Non-stationary")
)

# Sort: non-stationary first
adf_table <- adf_table %>% arrange(desc(p_value))

# Print final table
adf_table

non_stationary <- adf_table %>% 
  filter(Decision == "Non-stationary") %>% 
  pull(Variable)
non_stationary

stationary <- adf_table %>% 
  filter(Decision == "Stationary") %>% 
  pull(Variable)
stationary

write.xlsx(
  x = list(
    "ADF Results" = adf_table
  ),
  file = file.path(tabdir, "ADF_results.xlsx"),
  asTable = TRUE
)


###############################################################################
###############################################################################
###############################################################################

df_ml <- df_ml %>%
  arrange(YearMon) %>%  
  mutate(
    across(
      .cols  = all_of(non_stationary),
      .fns   = ~ . - lag(.)
    )
  )

write.csv(df_ml, FILE_MODEL_DATA, row.names = FALSE)

###############################################################################
###############################################################################
###############################################################################

### CHECK ACF PACF

acf(df_ml$inflation)
pacf(df_ml$inflation)
# 1 or 2 lag is possible

###############################################################################
###############################################################################
###############################################################################


