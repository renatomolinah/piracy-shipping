# trip_count_extensive_margin.R
#
# Purpose: Test the extensive margin of piracy avoidance (R1, Point 1).
#          Instead of trip distance (intensive margin), uses number of trips
#          per route-period as the outcome. If piracy causes route abandonment,
#          we expect attacks_7day_num to reduce n_trips.
#
# The spec mirrors the main voyage-level regression exactly, so the results
# are directly comparable to Table 1.
#
# DO NOT RUN until the main route_port_pair robustness results are confirmed.

library(tidyverse)
library(fixest)

# ── 0. Assumed objects in environment ─────────────────────────────────────────
# wdb  : voyage-level panel (as used in main regressions)
# ..wctrl is not available after aggregation, so weather vars entered manually

# ── 1. Collapse wdb to route × vessel-type × month × year ─────────────────────
# We aggregate at the same grouping level as the main FEs so that the panel
# structure is consistent. Weather controls are averaged within each cell.

route_panel <- wdb |>
  group_by(
    country_pair,
    route_port_pair,
    vessel_type,
    tonnage_decile,
    hotspot,
    top_route,
    month,
    year,
    # hotspot indicators for subsample filters
    aden,
    guinea,
    asia
  ) |>
  summarise(
    n_trips        = n(),
    attacks_7day   = mean(attacks_7day_num,                          na.rm = TRUE),
    future_attacks = mean(number_future_attacks_12_months_5_degrees, na.rm = TRUE),
    wind_speed     = mean(wind_speed,                                na.rm = TRUE),
    wind_vector    = mean(wind_vector,                               na.rm = TRUE),
    wave_height    = mean(wave_height,                               na.rm = TRUE),
    .groups = "drop"
  )

# ── 2. Main spec: country_pair FEs (mirrors N1 from main table) ───────────────

m_trips_n1 <- feols(
  log1p(n_trips) ~ attacks_7day
    + future_attacks
    + wind_speed + wind_vector + wave_height
  | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
  data = route_panel |> filter(year > 2020),
  vcov = ~country_pair^year,
  lean = TRUE
)

# ── 3. Robustness: add route_port_pair FEs (mirrors robustness spec) ──────────

m_trips_route <- feols(
  log1p(n_trips) ~ attacks_7day
    + future_attacks
    + wind_speed + wind_vector + wave_height
  | route_port_pair + country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
  data = route_panel |> filter(year > 2020),
  vcov = ~country_pair^year,
  lean = TRUE
)

# ── 4. Poisson (preferred for count outcomes) ─────────────────────────────────
# fepois handles zeros correctly and coefficients are % changes

m_trips_pois <- fepois(
  n_trips ~ attacks_7day
    + future_attacks
    + wind_speed + wind_vector + wave_height
  | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
  data = route_panel |> filter(year > 2020),
  vcov = ~country_pair^year
)

# ── 5. By hotspot ─────────────────────────────────────────────────────────────

m_trips_aden <- fepois(
  n_trips ~ attacks_7day
    + future_attacks
    + wind_speed + wind_vector + wave_height
  | country_pair + vessel_type + tonnage_decile + top_route + month^year,
  data = route_panel |> filter(year > 2020, aden),
  vcov = ~country_pair^year
)

m_trips_guinea <- fepois(
  n_trips ~ attacks_7day
    + future_attacks
    + wind_speed + wind_vector + wave_height
  | country_pair + vessel_type + tonnage_decile + top_route + month^year,
  data = route_panel |> filter(year > 2020, guinea),
  vcov = ~country_pair^year
)

m_trips_asia <- fepois(
  n_trips ~ attacks_7day
    + future_attacks
    + wind_speed + wind_vector + wave_height
  | country_pair + vessel_type + tonnage_decile + top_route + month^year,
  data = route_panel |> filter(year > 2020, asia),
  vcov = ~country_pair^year
)

# ── 6. Summary ────────────────────────────────────────────────────────────────

etable(
  m_trips_n1,
  m_trips_route,
  m_trips_pois,
  m_trips_aden,
  m_trips_guinea,
  m_trips_asia,
  headers = c("log(n) N1", "log(n) +route FE", "Poisson global",
              "Poisson Aden", "Poisson Guinea", "Poisson Asia"),
  keep = c("attacks_7day", "future_attacks")
)

# ── Notes ─────────────────────────────────────────────────────────────────────
# Negative coefficient on attacks_7day  → piracy reduces trip counts (route abandonment)
# Positive or zero                       → intensive margin only (rerouting, no abandonment)
# If both distance and n_trips respond,  → paper understates total costs (lower bound confirmed)
# If only distance responds              → avoidance is path-adjustment, not abandonment

