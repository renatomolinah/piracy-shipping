# Additional margins of adjustment: trip frequency (NegBin) and Suez/Cape route choice (logit)

library(here)
library(tidyverse)
library(fixest)
library(modelsummary)
library(lubridate)

source(here("code", "table_helpers.R"))

options("modelsummary_format_numeric_latex" = "plain")

# Force kableExtra backend so modelsummary v2+ outputs threeparttable (not tabularray)
options(modelsummary_factory_latex = "kableExtra")

output_dir <- here("results", "figures_and_tables")

roll_sum <- function(x, k) as.numeric(stats::filter(x, rep(1, k), sides = 1))

# --- Shared data ---

wdb <- readRDS(here("data", "processed", "voyages.rds")) %>%
  mutate(drop = guinea + aden + asia) %>%
  filter(drop <= 1, best_vessel_type_cargo) %>%
  mutate(
    hotspot = ifelse(guinea == 1, "Guinea",
                     ifelse(aden == 1, "Aden",
                            ifelse(asia == 1, "Asia", "None")))
  )

# --- Part 1: Trip frequency (extensive margin) ---

cat("--- Part 1: Trip frequency ---\n")

# Aggregate to weekly: prior 7-day encounter window -> this 7-day window
trip_counts_weekly <- wdb %>%
  mutate(
    date       = as.Date(departure_date),
    week_start = floor_date(date, "week"),
    year       = year(week_start),
    month      = month(week_start)
  ) %>%
  group_by(country_pair, week_start, hotspot, year, month) %>%
  summarize(
    n_trips          = n(),
    avg_attacks_7day = mean(number_previous_attacks_7_days_5_degrees, na.rm = TRUE),
    avg_wind_speed   = mean(wind_speed, na.rm = TRUE),
    avg_wind_vector  = mean(wind_vector, na.rm = TRUE),
    avg_wave_height  = mean(wave_height, na.rm = TRUE),
    .groups = "drop"
) %>%
  arrange(country_pair, week_start) %>%
  group_by(country_pair) %>%
  mutate(
    lag_1week = lag(avg_attacks_7day, n = 1)
  ) %>%
  ungroup()


setFixest_fml(..wctrl = ~ avg_wind_speed + avg_wind_vector + avg_wave_height)

cat("Running NegBin models...\n")

m_nb_global <- fenegbin(n_trips ~ lag_1week + ..wctrl
                        | country_pair + hotspot + month^year,
                        trip_counts_weekly,
                        cluster = ~country_pair ^ year, lean = TRUE)

m_nb_aden   <- fenegbin(n_trips ~ lag_1week + ..wctrl
                        | country_pair + hotspot + month^year,
                        trip_counts_weekly %>% filter(hotspot == "Aden"),
                        cluster = ~country_pair ^ year, lean = TRUE)

m_nb_guinea <- fenegbin(n_trips ~ lag_1week + ..wctrl
                        | country_pair + hotspot + month^year,
                        trip_counts_weekly %>% filter(hotspot == "Guinea"),
                        cluster = ~country_pair ^ year, lean = TRUE)

m_nb_asia   <- fenegbin(n_trips ~ lag_1week + ..wctrl
                        | country_pair + hotspot + month^year,
                        trip_counts_weekly %>% filter(hotspot == "Asia"),
                        cluster = ~country_pair ^ year, lean = TRUE)

cat("Done.\n\n")

rows <- tribble(
  ~term,              ~"(1)",                                    ~"(2)",                                  ~"(3)",                                    ~"(4)",
  "",                 "",                                        "",                                      "",                                        "",
  "Observations",     format(nobs(m_nb_global), big.mark = ","), format(nobs(m_nb_aden), big.mark = ","), format(nobs(m_nb_guinea), big.mark = ","), format(nobs(m_nb_asia), big.mark = ","),
  "",                 "",                                        "",                                      "",                                        "",
  "Country-Pair FE",  "X",                                       "X",                                     "X",                                       "X",
  "Hotspot FE",       "X",                                       "$\\bullet$",                            "$\\bullet$",                              "$\\bullet$",
  "Month-by-Year FE", "X",                                       "X",                                     "X",                                       "X"
)

notes_text <- paste(
  "The unit of observation is a country-pair by 7-day window.",
  "The outcome is the count of cargo vessel trips on that route in that week.",
  "The model is Negative Binomial; coefficients are log-multiplicative and",
  "approximately equal to percentage changes in trips for small values.",
  "The sample spans from 2012 to 2023.",
  "Global uses the full sample; G. of Aden, G. of Guinea, and S.E. Asia restrict the sample to routes",
  "passing through each hotspot, respectively.",
  "Avg. Encounters (7-day lag) is the average of each voyage's 7-day pirate encounter count",
  "in the prior 7-day window on the same route, using a 5-degree spatial footprint.",
  "This mirrors the 7-day encounter window used in the main voyage-level analysis.",
  "Controls include average wind speed, the wind-resistance index, and average wave height.",
  "Fixed effects include country-to-country combination, hotspot, and month-by-year.",
  "Standard errors are clustered by country-to-country route by year."
)

msummary(
  list("Global"       = m_nb_global,
       "G. of Aden"   = m_nb_aden,
       "G. of Guinea" = m_nb_guinea,
       "S.E. Asia"    = m_nb_asia),
  coef_omit      = "wind|wave|theta",
  coef_rename    = c("lag_1week" = "Avg. Encounters (7-day lag)"),
  gof_omit       = ".*",
  stars          = c('*' = .1, '**' = .05, '***' = .01),
  fmt            = "%.4f",
  add_rows       = rows,
  title          = "Effect of Pirate Encounters on Trip Frequency. \\label{tab:trip-count}",
  notes          = list(notes_text),
  threeparttable = TRUE,
  escape         = FALSE,
  output         = here(output_dir, "trip_count.tex")
)

add_adjust_box(here(output_dir, "trip_count.tex"))
adjust_notes_font_size(here(output_dir, "trip_count.tex"))
append_fe_legend(here(output_dir, "trip_count.tex"), has_bullet = TRUE)
strip_pkg_declarations(here(output_dir, "trip_count.tex"))

cat("Table written to: trip_count.tex\n\n")

# --- Part 2: Route choice (Suez Canal vs Cape of Good Hope) ---

cat("--- Part 2: Suez/Cape route choice ---\n")

find_latest_versioned_csv <- function(prefix) {
  files <- list.files(
    here("data", "processed"),
    pattern = paste0("^", prefix, "_[0-9]{8}\\.csv$"),
    full.names = TRUE
  )
  if (length(files) == 0) stop("No CSV found for prefix: ", prefix, call. = FALSE)
  files[which.max(stringr::str_extract(basename(files), "[0-9]{8}"))]
}

fmt_num   <- function(x, digits = 3) ifelse(is.na(x), "", formatC(x, digits = digits, format = "f"))
star_code <- function(p) ifelse(is.na(p), "", ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.1, "*", ""))))

format_est_se <- function(model, term, vc = ~month^year) {
  ct    <- coeftable(model, vcov = vc)
  if (!term %in% rownames(ct)) return(list(est = "", se = ""))
  est   <- ct[term, "Estimate"]
  se    <- ct[term, "Std. Error"]
  p_col <- grep("Pr\\(>", colnames(ct), value = TRUE)[1]
  p     <- ct[term, p_col]
  list(est = paste0(fmt_num(est), star_code(p)), se = paste0("(", fmt_num(se), ")"))
}

# Binarize encounter counts at the 75th percentile for the high-encounter indicator
make_high_indicator <- function(x, q = 0.75) {
  cutoff <- as.numeric(quantile(x, q, na.rm = TRUE))
  if (cutoff <= min(x, na.rm = TRUE)) {
    list(ind = as.integer(x > 0), label = ">0 (p75 = 0, using >0 fallback)")
  } else {
    list(ind = as.integer(x >= cutoff), label = paste0(">= ", cutoff, " (p75)"))
  }
}

trip_file <- find_latest_versioned_csv("suez_canal_or_cape_good_hope_trip_level")
cat("Using trip file:", basename(trip_file), "\n")

trips <- read_csv(trip_file, show_col_types = FALSE) %>%
  mutate(
    departure_date = as.Date(departure_date),
    cape           = as.integer(trip_location_flag == "cape_good_hope"),
    ship           = as.character(mmsi)
  )

voyage_controls <- wdb %>%
  select(trip_id, vessel_type, tonnage_decile, wind_speed, wind_vector, wave_height,
         country_pair, route_port_pair, top_route)

trips <- trips %>% left_join(voyage_controls, by = "trip_id")
rm(voyage_controls)

# Build daily encounter series from Gulf of Aden attack data and compute rolling windows
aden_attacks <- read_csv(
  here("data", "processed", "attack_timeseries_gulf_aden_daily.csv"),
  show_col_types = FALSE
) %>% mutate(attack_date = as.Date(attack_date))

attack_daily <- tibble(
  departure_date = seq(min(trips$departure_date), max(trips$departure_date), by = "day")
) %>%
  left_join(aden_attacks, by = c("departure_date" = "attack_date")) %>%
  mutate(number_attacks = if_else(is.na(number_attacks), 0L, as.integer(number_attacks))) %>%
  arrange(departure_date) %>%
  mutate(
    attacks_lag1 = dplyr::lag(number_attacks, 1),
    attacks_7d   = roll_sum(attacks_lag1, 7),
    attacks_15d  = roll_sum(attacks_lag1, 15),
    attacks_30d  = roll_sum(attacks_lag1, 30)
  )

high_7d_obj  <- make_high_indicator(attack_daily$attacks_7d,  q = 0.75)
high_15d_obj <- make_high_indicator(attack_daily$attacks_15d, q = 0.75)
high_30d_obj <- make_high_indicator(attack_daily$attacks_30d, q = 0.75)

attack_daily <- attack_daily %>%
  mutate(high_7d  = high_7d_obj$ind,
         high_15d = high_15d_obj$ind,
         high_30d = high_30d_obj$ind)

dat <- trips %>%
  left_join(
    attack_daily %>% select(departure_date, attacks_7d, attacks_15d, attacks_30d,
                             high_7d, high_15d, high_30d),
    by = "departure_date"
  ) %>%
  mutate(
    year  = factor(format(departure_date, "%Y")),
    month = factor(format(departure_date, "%m")),
    dow   = factor(weekdays(departure_date))
  ) %>%
  filter(!is.na(attacks_30d))

setFixest_fml(..wctrl = ~ wind_speed + wind_vector + wave_height)

cat("Estimating models (3 specs x 3 windows x 2 panels = 18 models)...\n")

# Panel A: continuous encounter intensity
m1_7  <- feglm(cape ~ attacks_7d  + ..wctrl | month^year,                                                                 data = dat, family = binomial)
m2_7  <- feglm(cape ~ attacks_7d  + ..wctrl | vessel_type + tonnage_decile + dow + month^year,                           data = dat, family = binomial)
m3_7  <- feglm(cape ~ attacks_7d  + ..wctrl | vessel_type + tonnage_decile + country_pair + top_route + dow + month^year, data = dat, family = binomial)
m1_15 <- feglm(cape ~ attacks_15d + ..wctrl | month^year,                                                                 data = dat, family = binomial)
m2_15 <- feglm(cape ~ attacks_15d + ..wctrl | vessel_type + tonnage_decile + dow + month^year,                           data = dat, family = binomial)
m3_15 <- feglm(cape ~ attacks_15d + ..wctrl | vessel_type + tonnage_decile + country_pair + top_route + dow + month^year, data = dat, family = binomial)
m1_30 <- feglm(cape ~ attacks_30d + ..wctrl | month^year,                                                                 data = dat, family = binomial)
m2_30 <- feglm(cape ~ attacks_30d + ..wctrl | vessel_type + tonnage_decile + dow + month^year,                           data = dat, family = binomial)
m3_30 <- feglm(cape ~ attacks_30d + ..wctrl | vessel_type + tonnage_decile + country_pair + top_route + dow + month^year, data = dat, family = binomial)

# Panel B: binary high-encounter indicator
h1_7  <- feglm(cape ~ high_7d  + ..wctrl | month^year,                                                                 data = dat, family = binomial)
h2_7  <- feglm(cape ~ high_7d  + ..wctrl | vessel_type + tonnage_decile + dow + month^year,                           data = dat, family = binomial)
h3_7  <- feglm(cape ~ high_7d  + ..wctrl | vessel_type + tonnage_decile + country_pair + top_route + dow + month^year, data = dat, family = binomial)
h1_15 <- feglm(cape ~ high_15d + ..wctrl | month^year,                                                                 data = dat, family = binomial)
h2_15 <- feglm(cape ~ high_15d + ..wctrl | vessel_type + tonnage_decile + dow + month^year,                           data = dat, family = binomial)
h3_15 <- feglm(cape ~ high_15d + ..wctrl | vessel_type + tonnage_decile + country_pair + top_route + dow + month^year, data = dat, family = binomial)
h1_30 <- feglm(cape ~ high_30d + ..wctrl | month^year,                                                                 data = dat, family = binomial)
h2_30 <- feglm(cape ~ high_30d + ..wctrl | vessel_type + tonnage_decile + dow + month^year,                           data = dat, family = binomial)
h3_30 <- feglm(cape ~ high_30d + ..wctrl | vessel_type + tonnage_decile + country_pair + top_route + dow + month^year, data = dat, family = binomial)

cat("Done.\n\n")

o1 <- format(nobs(m1_7),  big.mark = ","); o2 <- format(nobs(m2_7),  big.mark = ","); o3 <- format(nobs(m3_7),  big.mark = ",")
o4 <- format(nobs(m1_15), big.mark = ","); o5 <- format(nobs(m2_15), big.mark = ","); o6 <- format(nobs(m3_15), big.mark = ",")
o7 <- format(nobs(m1_30), big.mark = ","); o8 <- format(nobs(m2_30), big.mark = ","); o9 <- format(nobs(m3_30), big.mark = ",")

rows_suez <- tribble(
  ~term,              ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)", ~"(7)", ~"(8)", ~"(9)",
  "",                 "",     "",     "",     "",     "",     "",     "",     "",     "",
  "Observations",     o1,     o2,     o3,     o4,     o5,     o6,     o7,     o8,     o9,
  "",                 "",     "",     "",     "",     "",     "",     "",     "",     "",
  "DoW FE",              "",     "X",    "X",    "",     "X",    "X",    "",     "X",    "X",
  "Country Combo. FE",   "",     "",     "X",    "",     "",     "X",    "",     "",     "X",
  "Vessel Type FE",      "",     "X",    "X",    "",     "X",    "X",    "",     "X",    "X",
  "Vessel Size FE",      "",     "X",    "X",    "",     "X",    "X",    "",     "X",    "X",
  "Top Route FE",        "",     "",     "X",    "",     "",     "X",    "",     "",     "X",
  "Weather controls",    "X",    "X",    "X",    "X",    "X",    "X",    "X",    "X",    "X",
  "Month-by-Year FE",    "X",    "X",    "X",    "X",    "X",    "X",    "X",    "X",    "X"
)

notes_suez <- paste(
  "The unit of observation is an individual vessel trip.",
  "The outcome is a binary indicator equal to one if the trip used the Cape of Good Hope route (zero for Suez Canal).",
  "The sample spans from 2012 to 2023.",
  "Encounters in prior X days is the count of pirate encounters recorded in the Gulf of Aden in the preceding X days from the departure date.",
  "Panel B defines high-encounter periods as those above the 75th percentile of the encounter distribution within each window.",
  "Columns (2), (5), and (8) add day-of-week, vessel-type, and vessel-size fixed effects.",
  "Columns (3), (6), and (9) further add country-pair and top-route fixed effects.",
  "Controls include average wind speed, the wind-resistance index, and average wave height.",
  "Standard errors are clustered by month-by-year."
)

msummary(
  list(
    "Panel (A): Continuous encounter intensity" = list(m1_7, m2_7, m3_7, m1_15, m2_15, m3_15, m1_30, m2_30, m3_30),
    "Panel (B): High-encounter indicator"       = list(h1_7, h2_7, h3_7, h1_15, h2_15, h3_15, h1_30, h2_30, h3_30)
  ),
  shape       = "rbind",
  vcov        = ~month^year,
  coef_omit   = "wind|wave",
  coef_rename = c(
    "attacks_7d"  = "Encounters in prior 7 days",
    "attacks_15d" = "Encounters in prior 15 days",
    "attacks_30d" = "Encounters in prior 30 days",
    "high_7d"     = "High encounters (7d window)",
    "high_15d"    = "High encounters (15d window)",
    "high_30d"    = "High encounters (30d window)"
  ),
  gof_omit       = ".*",
  stars          = c('*' = .1, '**' = .05, '***' = .01),
  fmt            = "%.3f",
  add_rows       = rows_suez,
  title          = "Corridor Choice Between the Suez Canal and the Cape of Good Hope: Trip-Level Evidence. \\label{tab:suez-cape-trip-level}",
  notes          = list(notes_suez),
  threeparttable = TRUE,
  escape         = FALSE,
  output         = here(output_dir, "suez_cape_route_choice_trip_level.tex")
)

add_adjust_box(here(output_dir, "suez_cape_route_choice_trip_level.tex"))
adjust_notes_font_size(here(output_dir, "suez_cape_route_choice_trip_level.tex"))
unstyle_panel_headers(here(output_dir, "suez_cape_route_choice_trip_level.tex"))
append_fe_legend(here(output_dir, "suez_cape_route_choice_trip_level.tex"), has_bullet = FALSE)
strip_pkg_declarations(here(output_dir, "suez_cape_route_choice_trip_level.tex"))

cat("Table written to: suez_cape_route_choice_trip_level.tex\n")
