# =============================================================================
# TRIP COUNT REGRESSIONS - COUNT MODELS (POISSON/NEGATIVE BINOMIAL)
# =============================================================================
# This script performs count model regression analysis on trip counts using
# monthly aggregation with different lag windows to examine how pirate attacks
# affect the number of trips.
# =============================================================================

library(here)
library(tidyverse)
library(fixest)
library(lubridate)

# =============================================================================
# 1. LOAD AND PREPARE DATA
# =============================================================================

cat("Loading data and preparing...\n")
wdb <- readRDS(here("data", "processed", "voyages.rds")) %>%
  mutate(
    drop = guinea + aden + asia
  ) %>%
  filter(drop <= 1, best_vessel_type_cargo) %>%
  mutate(
    hotspot = ifelse(guinea == 1, "Guinea",
                     ifelse(aden == 1, "Aden",
                            ifelse(asia == 1, "Asia", "None")))
  )

# =============================================================================
# 2. MONTHLY AGGREGATION (COUNTRY-TO-COUNTRY)
# =============================================================================

cat("Aggregating to daily and monthly levels...\n")

# Daily aggregation for 7-day lag
trip_counts_daily <- wdb %>%
  mutate(date = as.Date(departure_date)) %>%
  group_by(country_pair, date, hotspot) %>%
  summarize(
    n_trips = n(),
    avg_attacks_7day = mean(number_previous_attacks_7_days_5_degrees, na.rm = TRUE),
    avg_wind_speed = mean(wind_speed, na.rm = TRUE),
    avg_wind_vector = mean(wind_vector, na.rm = TRUE),
    avg_wave_height = mean(wave_height, na.rm = TRUE),
    year = year(date),
    month = month(date),
    .groups = "drop"
  ) %>%
  arrange(country_pair, date) %>%
  group_by(country_pair) %>%
  mutate(
    lag_7days = lag(avg_attacks_7day, n = 7)
  ) %>%
  ungroup()

# Monthly aggregation
trip_counts_monthly <- wdb %>%
  group_by(country_pair, month, year, hotspot) %>%
  summarize(
    n_trips = n(),
    n_vessels = n_distinct(mmsi),
    avg_attacks_7day = mean(number_previous_attacks_7_days_5_degrees, na.rm = TRUE),
    avg_wind_speed = mean(wind_speed, na.rm = TRUE),
    avg_wind_vector = mean(wind_vector, na.rm = TRUE),
    avg_wave_height = mean(wave_height, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    trips_per_vessel = n_trips / n_vessels,
    has_trip = as.numeric(n_trips > 0)
  ) %>%
  arrange(country_pair, year, month) %>%
  group_by(country_pair) %>%
  mutate(
    lag_1month = lag(avg_attacks_7day, n = 1),
    lag_2months = lag(avg_attacks_7day, n = 2),
    lag_3months = lag(avg_attacks_7day, n = 3),
    lag_4months = lag(avg_attacks_7day, n = 4),
    lag_6months = lag(avg_attacks_7day, n = 6),
    high_attacks_lag1m = as.numeric(lag_1month > median(lag_1month, na.rm = TRUE)),
    high_attacks_lag6m = as.numeric(lag_6months > median(lag_6months, na.rm = TRUE))
  ) %>%
  ungroup()

# =============================================================================
# 3. SET UP CONTROLS
# =============================================================================

setFixest_fml(..wctrl = ~ avg_wind_speed + avg_wind_vector + avg_wave_height)

# =============================================================================
# 4. RUN ALL MODELS (SILENTLY)
# =============================================================================

cat("Running regressions...\n")

# OLS Models (Level Changes) - Monthly
m_ols_lag1m <- feols(n_trips ~ lag_1month + ..wctrl 
                     | country_pair + hotspot + month^year,
                     trip_counts_monthly, 
                     cluster = ~country_pair ^ year, lean = TRUE)

m_ols_lag2m <- feols(n_trips ~ lag_2months + ..wctrl 
                     | country_pair + hotspot + month^year,
                     trip_counts_monthly, 
                     cluster = ~country_pair ^ year, lean = TRUE)

m_ols_lag3m <- feols(n_trips ~ lag_3months + ..wctrl 
                     | country_pair + hotspot + month^year,
                     trip_counts_monthly, 
                     cluster = ~country_pair ^ year, lean = TRUE)

m_ols_lag6m <- feols(n_trips ~ lag_6months + ..wctrl 
                     | country_pair + hotspot + month^year,
                     trip_counts_monthly, 
                     cluster = ~country_pair ^ year, lean = TRUE)

m_ols_compare <- feols(n_trips ~ lag_1month + lag_6months + ..wctrl 
                        | country_pair + hotspot + month^year,
                        trip_counts_monthly, 
                        cluster = ~country_pair ^ year, lean = TRUE)

m_ols_threshold <- feols(n_trips ~ high_attacks_lag1m + ..wctrl 
                          | country_pair + hotspot + month^year,
                          trip_counts_monthly, 
                          cluster = ~country_pair ^ year, lean = TRUE)

# Poisson Models (Elasticities)
m_pois_lag1m <- fepois(n_trips ~ lag_1month + ..wctrl 
                       | country_pair + hotspot + month^year,
                       trip_counts_monthly, 
                       cluster = ~country_pair ^ year, lean = TRUE)

m_pois_lag2m <- fepois(n_trips ~ lag_2months + ..wctrl 
                       | country_pair + hotspot + month^year,
                       trip_counts_monthly, 
                       cluster = ~country_pair ^ year, lean = TRUE)

m_pois_lag3m <- fepois(n_trips ~ lag_3months + ..wctrl 
                       | country_pair + hotspot + month^year,
                       trip_counts_monthly, 
                       cluster = ~country_pair ^ year, lean = TRUE)

m_pois_lag4m <- fepois(n_trips ~ lag_4months + ..wctrl 
                       | country_pair + hotspot + month^year,
                       trip_counts_monthly, 
                       cluster = ~country_pair ^ year, lean = TRUE)

m_pois_all_lags <- fepois(n_trips ~ lag_1month + lag_2months + lag_3months + lag_4months + ..wctrl 
                          | country_pair + hotspot + month^year,
                          trip_counts_monthly, 
                          cluster = ~country_pair ^ year, lean = TRUE)

m_pois_threshold <- fepois(n_trips ~ high_attacks_lag1m + ..wctrl 
                           | country_pair + hotspot + month^year,
                           trip_counts_monthly, 
                           cluster = ~country_pair ^ year, lean = TRUE)

m_pois_lag6m <- fepois(n_trips ~ lag_6months + ..wctrl 
                       | country_pair + hotspot + month^year,
                       trip_counts_monthly, 
                       cluster = ~country_pair ^ year, lean = TRUE)

m_pois_compare <- fepois(n_trips ~ lag_1month + lag_6months + ..wctrl 
                          | country_pair + hotspot + month^year,
                          trip_counts_monthly, 
                          cluster = ~country_pair ^ year, lean = TRUE)

m_pois_aden <- fepois(n_trips ~ lag_1month + ..wctrl 
                      | country_pair + hotspot + month^year,
                      trip_counts_monthly %>% filter(hotspot == "Aden"),
                      cluster = ~country_pair ^ year, lean = TRUE)

m_pois_guinea <- fepois(n_trips ~ lag_1month + ..wctrl 
                        | country_pair + hotspot + month^year,
                        trip_counts_monthly %>% filter(hotspot == "Guinea"),
                        cluster = ~country_pair ^ year, lean = TRUE)

m_pois_asia <- fepois(n_trips ~ lag_1month + ..wctrl 
                      | country_pair + hotspot + month^year,
                      trip_counts_monthly %>% filter(hotspot == "Asia"),
                      cluster = ~country_pair ^ year, lean = TRUE)

# Negative Binomial Models (Elasticities)
m_nb_lag1m <- fenegbin(n_trips ~ lag_1month + ..wctrl 
                       | country_pair + hotspot + month^year,
                       trip_counts_monthly, 
                       cluster = ~country_pair ^ year, lean = TRUE)

m_nb_lag2m <- fenegbin(n_trips ~ lag_2months + ..wctrl 
                       | country_pair + hotspot + month^year,
                       trip_counts_monthly, 
                       cluster = ~country_pair ^ year, lean = TRUE)

m_nb_lag3m <- fenegbin(n_trips ~ lag_3months + ..wctrl 
                       | country_pair + hotspot + month^year,
                       trip_counts_monthly, 
                       cluster = ~country_pair ^ year, lean = TRUE)

m_nb_lag4m <- fenegbin(n_trips ~ lag_4months + ..wctrl 
                       | country_pair + hotspot + month^year,
                       trip_counts_monthly, 
                       cluster = ~country_pair ^ year, lean = TRUE)

m_nb_all_lags <- fenegbin(n_trips ~ lag_1month + lag_2months + lag_3months + lag_4months + ..wctrl 
                          | country_pair + hotspot + month^year,
                          trip_counts_monthly, 
                          cluster = ~country_pair ^ year, lean = TRUE)

m_nb_threshold <- fenegbin(n_trips ~ high_attacks_lag1m + ..wctrl 
                           | country_pair + hotspot + month^year,
                           trip_counts_monthly, 
                           cluster = ~country_pair ^ year, lean = TRUE)

m_nb_lag6m <- fenegbin(n_trips ~ lag_6months + ..wctrl 
                       | country_pair + hotspot + month^year,
                       trip_counts_monthly, 
                       cluster = ~country_pair ^ year, lean = TRUE)

m_nb_compare <- fenegbin(n_trips ~ lag_1month + lag_6months + ..wctrl 
                          | country_pair + hotspot + month^year,
                          trip_counts_monthly, 
                          cluster = ~country_pair ^ year, lean = TRUE)

m_nb_aden <- fenegbin(n_trips ~ lag_1month + ..wctrl 
                       | country_pair + hotspot + month^year,
                       trip_counts_monthly %>% filter(hotspot == "Aden"),
                       cluster = ~country_pair ^ year, lean = TRUE)

m_nb_guinea <- fenegbin(n_trips ~ lag_1month + ..wctrl 
                         | country_pair + hotspot + month^year,
                         trip_counts_monthly %>% filter(hotspot == "Guinea"),
                         cluster = ~country_pair ^ year, lean = TRUE)

m_nb_asia <- fenegbin(n_trips ~ lag_1month + ..wctrl 
                       | country_pair + hotspot + month^year,
                       trip_counts_monthly %>% filter(hotspot == "Asia"),
                       cluster = ~country_pair ^ year, lean = TRUE)

# 7-DAY LAG MODELS (Daily aggregation)
m_nb_lag7d <- fenegbin(n_trips ~ lag_7days + avg_wind_speed + avg_wind_vector + avg_wave_height
                       | country_pair + hotspot + month^year,
                       trip_counts_daily, 
                       cluster = ~country_pair ^ year, lean = TRUE)

m_ols_lag7d <- feols(n_trips ~ lag_7days + avg_wind_speed + avg_wind_vector + avg_wave_height
                     | country_pair + hotspot + month^year,
                     trip_counts_daily, 
                     cluster = ~country_pair ^ year, lean = TRUE)

cat("Done.\n\n")

# =============================================================================
# 5. SUMMARY RESULTS TABLE
# =============================================================================

cat("=", rep("=", 80), "\n", sep = "")
cat("SUMMARY OF RESULTS\n")
cat("=", rep("=", 80), "\n\n", sep = "")

# Helper function to extract coefficient and p-value
extract_coef <- function(model, var_name) {
  tryCatch({
    coef_val <- coef(model)[var_name]
    if (length(coef_val) == 0 || is.null(coef_val)) {
      return(list(coef = NA_real_, pval = NA_real_, n_obs = nobs(model)))
    }
    
    coef_table <- summary(model)$coeftable
    pval <- NA_real_
    
    # Try to get p-value from either t or z test
    if (var_name %in% rownames(coef_table)) {
      if ("Pr(>|t|)" %in% colnames(coef_table)) {
        pval <- as.numeric(coef_table[var_name, "Pr(>|t|)"])
      } else if ("Pr(>|z|)" %in% colnames(coef_table)) {
        pval <- as.numeric(coef_table[var_name, "Pr(>|z|)"])
      }
    }
    
    n_obs <- nobs(model)
    return(list(coef = as.numeric(coef_val), pval = pval, n_obs = n_obs))
  }, error = function(e) {
    return(list(coef = NA_real_, pval = NA_real_, n_obs = nobs(model)))
  })
}

# Helper function to format significance
format_sig <- function(pval) {
  if (length(pval) == 0 || is.null(pval)) return("")
  pval <- pval[1]  # Take first element if vector
  if (is.na(pval)) return("")
  if (pval < 0.001) return("***")
  if (pval < 0.01) return("**")
  if (pval < 0.05) return("*")
  if (pval < 0.1) return(".")
  return("")
}

# =============================================================================
# 7-DAY LAG RESULTS
# =============================================================================

cat("--- 7-DAY LAG (DAILY AGGREGATION) ---\n")
cat("Effect of attacks 7 days ago on current day's trips\n\n")

lag7d_results <- tibble(
  Model = c("OLS (level change)", "Negative Binomial (elasticity)"),
  Coefficient = c(
    extract_coef(m_ols_lag7d, "lag_7days")$coef,
    extract_coef(m_nb_lag7d, "lag_7days")$coef
  ),
  P_value = c(
    extract_coef(m_ols_lag7d, "lag_7days")$pval,
    extract_coef(m_nb_lag7d, "lag_7days")$pval
  ),
  N_obs = c(
    extract_coef(m_ols_lag7d, "lag_7days")$n_obs,
    extract_coef(m_nb_lag7d, "lag_7days")$n_obs
  )
) %>%
  mutate(
    Result = paste0(round(Coefficient, 4), format_sig(P_value)),
    Interpretation = case_when(
      Model == "OLS (level change)" ~ paste0("Change in number of trips: ", round(Coefficient, 2)),
      Model == "Negative Binomial (elasticity)" ~ paste0("Percentage change: ", round((exp(Coefficient) - 1) * 100, 2), "%")
    )
  ) %>%
  select(Model, Result, Interpretation, P_value, N_obs)

print(lag7d_results, n = Inf)
cat("\n")

# =============================================================================
# MONTHLY LAG RESULTS
# =============================================================================

# OLS Results (Level Changes)
cat("--- OLS MODELS (LEVEL CHANGES) ---\n")
cat("Coefficient = change in NUMBER of trips per 1-unit increase in attacks\n\n")

ols_results <- tibble(
  Model = c("Lag 1 month", "Lag 2 months", "Lag 3 months", "Lag 6 months", 
            "Threshold (high attacks)", "Lag 1m + Lag 6m (both)"),
  `Lag 1m Coef` = c(
    extract_coef(m_ols_lag1m, "lag_1month")$coef,
    NA, NA, NA, NA,
    extract_coef(m_ols_compare, "lag_1month")$coef
  ),
  `Lag 1m P-val` = c(
    extract_coef(m_ols_lag1m, "lag_1month")$pval,
    NA, NA, NA, NA,
    extract_coef(m_ols_compare, "lag_1month")$pval
  ),
  `Lag 6m Coef` = c(
    NA, NA, NA,
    extract_coef(m_ols_lag6m, "lag_6months")$coef,
    NA,
    extract_coef(m_ols_compare, "lag_6months")$coef
  ),
  `Lag 6m P-val` = c(
    NA, NA, NA,
    extract_coef(m_ols_lag6m, "lag_6months")$pval,
    NA,
    extract_coef(m_ols_compare, "lag_6months")$pval
  ),
  `Threshold Coef` = c(
    NA, NA, NA, NA,
    extract_coef(m_ols_threshold, "high_attacks_lag1m")$coef,
    NA
  ),
  `Threshold P-val` = c(
    NA, NA, NA, NA,
    extract_coef(m_ols_threshold, "high_attacks_lag1m")$pval,
    NA
  ),
  N_obs = c(
    extract_coef(m_ols_lag1m, "lag_1month")$n_obs,
    extract_coef(m_ols_lag2m, "lag_2months")$n_obs,
    extract_coef(m_ols_lag3m, "lag_3months")$n_obs,
    extract_coef(m_ols_lag6m, "lag_6months")$n_obs,
    extract_coef(m_ols_threshold, "high_attacks_lag1m")$n_obs,
    extract_coef(m_ols_compare, "lag_1month")$n_obs
  )
) %>%
  mutate(
    `Lag 1m` = if_else(is.na(`Lag 1m Coef`), "", 
                       paste0(round(`Lag 1m Coef`, 4), format_sig(`Lag 1m P-val`))),
    `Lag 6m` = if_else(is.na(`Lag 6m Coef`), "", 
                       paste0(round(`Lag 6m Coef`, 4), format_sig(`Lag 6m P-val`))),
    `Threshold` = if_else(is.na(`Threshold Coef`), "", 
                          paste0(round(`Threshold Coef`, 4), format_sig(`Threshold P-val`)))
  ) %>%
  select(Model, `Lag 1m`, `Lag 6m`, Threshold, N_obs)

print(ols_results, n = Inf)
cat("\n")

# Poisson Results (Elasticities)
cat("--- POISSON MODELS (ELASTICITIES) ---\n")
cat("Coefficient = percentage change in trips per 1-unit increase in attacks\n")
cat("(For small coefficients, exp(β) - 1 ≈ β)\n\n")

pois_results <- tibble(
  Model = c("Lag 1 month", "Lag 2 months", "Lag 3 months", "Lag 4 months", 
            "Lag 6 months", "Threshold (high attacks)", "Lag 1m + Lag 6m (both)"),
  `Lag 1m Coef` = c(
    extract_coef(m_pois_lag1m, "lag_1month")$coef,
    NA, NA, NA, NA, NA,
    extract_coef(m_pois_compare, "lag_1month")$coef
  ),
  `Lag 1m P-val` = c(
    extract_coef(m_pois_lag1m, "lag_1month")$pval,
    NA, NA, NA, NA, NA,
    extract_coef(m_pois_compare, "lag_1month")$pval
  ),
  `Lag 6m Coef` = c(
    NA, NA, NA, NA,
    extract_coef(m_pois_lag6m, "lag_6months")$coef,
    NA,
    extract_coef(m_pois_compare, "lag_6months")$coef
  ),
  `Lag 6m P-val` = c(
    NA, NA, NA, NA,
    extract_coef(m_pois_lag6m, "lag_6months")$pval,
    NA,
    extract_coef(m_pois_compare, "lag_6months")$pval
  ),
  `Threshold Coef` = c(
    NA, NA, NA, NA, NA,
    extract_coef(m_pois_threshold, "high_attacks_lag1m")$coef,
    NA
  ),
  `Threshold P-val` = c(
    NA, NA, NA, NA, NA,
    extract_coef(m_pois_threshold, "high_attacks_lag1m")$pval,
    NA
  ),
  N_obs = c(
    extract_coef(m_pois_lag1m, "lag_1month")$n_obs,
    extract_coef(m_pois_lag2m, "lag_2months")$n_obs,
    extract_coef(m_pois_lag3m, "lag_3months")$n_obs,
    extract_coef(m_pois_lag4m, "lag_4months")$n_obs,
    extract_coef(m_pois_lag6m, "lag_6months")$n_obs,
    extract_coef(m_pois_threshold, "high_attacks_lag1m")$n_obs,
    extract_coef(m_pois_compare, "lag_1month")$n_obs
  )
) %>%
  mutate(
    `Lag 1m` = if_else(is.na(`Lag 1m Coef`), "", 
                       paste0(round(`Lag 1m Coef`, 4), format_sig(`Lag 1m P-val`))),
    `Lag 6m` = if_else(is.na(`Lag 6m Coef`), "", 
                       paste0(round(`Lag 6m Coef`, 4), format_sig(`Lag 6m P-val`))),
    `Threshold` = if_else(is.na(`Threshold Coef`), "", 
                          paste0(round(`Threshold Coef`, 4), format_sig(`Threshold P-val`)))
  ) %>%
  select(Model, `Lag 1m`, `Lag 6m`, Threshold, N_obs)

print(pois_results, n = Inf)
cat("\n")

# Negative Binomial Results (Elasticities)
cat("--- NEGATIVE BINOMIAL MODELS (ELASTICITIES) ---\n")
cat("Coefficient = percentage change in trips per 1-unit increase in attacks\n")
cat("(For small coefficients, exp(β) - 1 ≈ β)\n\n")

nb_results <- tibble(
  Model = c("Lag 1 month", "Lag 2 months", "Lag 3 months", "Lag 4 months", 
            "Lag 6 months", "Threshold (high attacks)", "Lag 1m + Lag 6m (both)"),
  `Lag 1m Coef` = c(
    extract_coef(m_nb_lag1m, "lag_1month")$coef,
    NA, NA, NA, NA, NA,
    extract_coef(m_nb_compare, "lag_1month")$coef
  ),
  `Lag 1m P-val` = c(
    extract_coef(m_nb_lag1m, "lag_1month")$pval,
    NA, NA, NA, NA, NA,
    extract_coef(m_nb_compare, "lag_1month")$pval
  ),
  `Lag 6m Coef` = c(
    NA, NA, NA, NA,
    extract_coef(m_nb_lag6m, "lag_6months")$coef,
    NA,
    extract_coef(m_nb_compare, "lag_6months")$coef
  ),
  `Lag 6m P-val` = c(
    NA, NA, NA, NA,
    extract_coef(m_nb_lag6m, "lag_6months")$pval,
    NA,
    extract_coef(m_nb_compare, "lag_6months")$pval
  ),
  `Threshold Coef` = c(
    NA, NA, NA, NA, NA,
    extract_coef(m_nb_threshold, "high_attacks_lag1m")$coef,
    NA
  ),
  `Threshold P-val` = c(
    NA, NA, NA, NA, NA,
    extract_coef(m_nb_threshold, "high_attacks_lag1m")$pval,
    NA
  ),
  N_obs = c(
    extract_coef(m_nb_lag1m, "lag_1month")$n_obs,
    extract_coef(m_nb_lag2m, "lag_2months")$n_obs,
    extract_coef(m_nb_lag3m, "lag_3months")$n_obs,
    extract_coef(m_nb_lag4m, "lag_4months")$n_obs,
    extract_coef(m_nb_lag6m, "lag_6months")$n_obs,
    extract_coef(m_nb_threshold, "high_attacks_lag1m")$n_obs,
    extract_coef(m_nb_compare, "lag_1month")$n_obs
  )
) %>%
  mutate(
    `Lag 1m` = if_else(is.na(`Lag 1m Coef`), "", 
                       paste0(round(`Lag 1m Coef`, 4), format_sig(`Lag 1m P-val`))),
    `Lag 6m` = if_else(is.na(`Lag 6m Coef`), "", 
                       paste0(round(`Lag 6m Coef`, 4), format_sig(`Lag 6m P-val`))),
    `Threshold` = if_else(is.na(`Threshold Coef`), "", 
                          paste0(round(`Threshold Coef`, 4), format_sig(`Threshold P-val`)))
  ) %>%
  select(Model, `Lag 1m`, `Lag 6m`, Threshold, N_obs)

print(nb_results, n = Inf)
cat("\n")

# All lags together
cat("--- ALL LAGS TOGETHER (NEGATIVE BINOMIAL) ---\n")
all_lags_coefs <- coef(m_nb_all_lags)
all_lags_pvals <- summary(m_nb_all_lags)$coeftable[, "Pr(>|z|)"]
all_lags_table <- tibble(
  Lag = c("Lag 1 month", "Lag 2 months", "Lag 3 months", "Lag 4 months"),
  Coefficient = c(all_lags_coefs["lag_1month"], all_lags_coefs["lag_2months"], 
                  all_lags_coefs["lag_3months"], all_lags_coefs["lag_4months"]),
  P_value = c(all_lags_pvals["lag_1month"], all_lags_pvals["lag_2months"],
              all_lags_pvals["lag_3months"], all_lags_pvals["lag_4months"]),
  Significance = c(format_sig(all_lags_pvals["lag_1month"]), 
                   format_sig(all_lags_pvals["lag_2months"]),
                   format_sig(all_lags_pvals["lag_3months"]), 
                   format_sig(all_lags_pvals["lag_4months"]))
) %>%
  mutate(Result = paste0(round(Coefficient, 4), Significance))
print(select(all_lags_table, Lag, Result, P_value), n = Inf)
cat("\n")

# Hotspot-specific
cat("--- HOTSPOT-SPECIFIC (NEGATIVE BINOMIAL, LAG 1 MONTH) ---\n")
hotspot_results <- tibble(
  Hotspot = c("Gulf of Aden", "Gulf of Guinea", "S.E. Asia"),
  Coefficient = c(
    extract_coef(m_nb_aden, "lag_1month")$coef,
    extract_coef(m_nb_guinea, "lag_1month")$coef,
    extract_coef(m_nb_asia, "lag_1month")$coef
  ),
  P_value = c(
    extract_coef(m_nb_aden, "lag_1month")$pval,
    extract_coef(m_nb_guinea, "lag_1month")$pval,
    extract_coef(m_nb_asia, "lag_1month")$pval
  ),
  N_obs = c(
    extract_coef(m_nb_aden, "lag_1month")$n_obs,
    extract_coef(m_nb_guinea, "lag_1month")$n_obs,
    extract_coef(m_nb_asia, "lag_1month")$n_obs
  )
) %>%
  mutate(
    Result = paste0(round(Coefficient, 4), format_sig(P_value)),
    `% Change` = paste0(round((exp(Coefficient) - 1) * 100, 2), "%")
  ) %>%
  select(Hotspot, Result, `% Change`, P_value, N_obs)
print(hotspot_results, n = Inf)
cat("\n")

# =============================================================================
# 6. SANITY CHECK SUMMARY
# =============================================================================

cat("--- SANITY CHECK: 6 MONTHS AGO (PLACEBO TEST) ---\n")
cat("If significant, suggests spurious correlation; if not, supports causal interpretation\n\n")

sanity_check <- tibble(
  Model = c("Poisson (alone)", "Poisson (with lag 1m)", 
            "Negative Binomial (alone)", "Negative Binomial (with lag 1m)"),
  `Lag 6m Coef` = c(
    extract_coef(m_pois_lag6m, "lag_6months")$coef,
    extract_coef(m_pois_compare, "lag_6months")$coef,
    extract_coef(m_nb_lag6m, "lag_6months")$coef,
    extract_coef(m_nb_compare, "lag_6months")$coef
  ),
  `Lag 6m P-val` = c(
    extract_coef(m_pois_lag6m, "lag_6months")$pval,
    extract_coef(m_pois_compare, "lag_6months")$pval,
    extract_coef(m_nb_lag6m, "lag_6months")$pval,
    extract_coef(m_nb_compare, "lag_6months")$pval
  )
) %>%
  mutate(
    Result = paste0(round(`Lag 6m Coef`, 4), format_sig(`Lag 6m P-val`)),
    Interpretation = ifelse(`Lag 6m P-val` < 0.05, 
                           "⚠️ SIGNIFICANT (concerning)", 
                           "✓ Not significant (good)")
  ) %>%
  select(Model, Result, Interpretation, `Lag 6m P-val`)
print(sanity_check, n = Inf)
cat("\n")

# =============================================================================
# 7. DATA SUMMARY
# =============================================================================

cat("--- DATA SUMMARY ---\n")
cat("  Total observations:", nrow(trip_counts_monthly), "\n")
cat("  Mean trips per month:", round(mean(trip_counts_monthly$n_trips, na.rm = TRUE), 2), "\n")
cat("  Median trips per month:", median(trip_counts_monthly$n_trips, na.rm = TRUE), "\n")
cat("  Variance:", round(var(trip_counts_monthly$n_trips, na.rm = TRUE), 2), "\n")
cat("  Variance/Mean ratio:", round(var(trip_counts_monthly$n_trips, na.rm = TRUE) / 
                                    mean(trip_counts_monthly$n_trips, na.rm = TRUE), 2), "\n")
cat("  (If ratio >> 1, Negative Binomial is preferred; if ≈ 1, Poisson is OK)\n")
cat("  Mean lag_1month:", round(mean(trip_counts_monthly$lag_1month, na.rm = TRUE), 4), "\n")
cat("  Mean lag_6months:", round(mean(trip_counts_monthly$lag_6months, na.rm = TRUE), 4), "\n")

cat("\n", rep("=", 80), "\n", sep = "")
cat("Significance codes: *** p<0.001, ** p<0.01, * p<0.05, . p<0.1\n")
cat(rep("=", 80), "\n", sep = "")
