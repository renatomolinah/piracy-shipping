################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  fixest,
  kableExtra,
  modelsummary,
  panelsummary,
  tidyverse
)

# Load data --------------------------------------------------------------------
grid_level_panel <- readRDS(file = here("processed_data",
                                        "attacks_and_activity_by_grid.rds"))

## PROCESSING ##################################################################

# Modify the panel -------------------------------------------------------------
reg_data <- grid_level_panel %>% 
  # Rename the variable of interest for consistency with Renato's regressions
  rename(TNE1 = number_previous_attacks_grid_1_month,
         TNE3 = number_previous_attacks_grid_3_months,
         TNE6 = number_previous_attacks_grid_6_months,
         TNE12 = number_previous_attacks_grid_12_months,) %>% 
  # Replace missing values (no transit detected in AIS) with zeroes.            << -------------- NOTE THIS!
  replace_na(replace = list(distance_km = 0,
                            time_hours = 0,
                            n_trips = 0,
                            n_vessels = 0,
                            n_ais_messages = 0)) %>% 
  mutate(year = year(date),
         month = month(date),
         ym = paste(year, month, sep = "-"))
  
# Table of summary stats -------------------------------------------------------
by_cluster <- datasummary(attack_cluster * (Mean + SD + Median + Max) ~ distance_km + time_hours + n_trips + n_vessels,
                          data = reg_data %>% 
                            mutate(attack_cluster = ifelse(attack_cluster == "None", "Rest of the world", attack_cluster),
                                   attack_cluster = fct_relevel(attack_cluster, "GoA", "GoG", "SEA", "Rest of the world")),
                          output = "dataframe")

kbl(x = by_cluster,
    booktabs = T,
    label = "grid_summary",
    caption = "Summary statistics for daily ship transit by grid cell.",
    col.names = c("", "", "Distance (km)", "Occupancy (hr)", "Voyages (#)", "Unique vessels (#)"),
    linesep = "",
    format = "latex") %>% 
  cat(file = here("tables", "grid_summary_stats.tex"))

## ESTIMATE ####################################################################
# Full estimation --------------------------------------------------------------
mod <- feols(data = reg_data,
             fml =
               # Outcome varibles
               c(distance_km,
                 time_hours,
                 n_trips,
                 n_vessels) ~
               # Regressors
               sw(TNE3, TNE6, TNE12) | 
               # Fixed effects
               asam_subregion + year ^ month ^ asam_region + grid_id,
             # SE specifictions
             vcov = vcov_conley(lat = "lat_bin",
                                lon = "lon_bin",
                                cutoff = 100),
             panel.id = ~grid_id + date,
             fsplit = ~attack_cluster,
             split.drop = "None",
             lean = TRUE)

# Quick local model inspection -------------------------------------------------
etable(mod,
       vcov = "iid") # IID is faster to compute just for now

# Summary of the models --------------------------------------------------------
fixest::models(mod) %>% 
  filter(rhs == "TNE3") %>% 
  arrange(lhs)

## BUILD TABLES ################################################################
gm <- tribble(~raw, ~clean, ~fmt,
              # "nobs", "Observations", 0,
              # "vcov.type", "SE", 0,
              "FE: grid_id", "FE: Grid ID", 0,
              "FE: asam_subregion", "FE: ASAM subregion", 0,
              "FE: year^month^asam_region", "FE: ASAM region-year-month", 0
)


msummary(list("Panel (A): Total Distance (km)" = mod[c(1, 13, 25, 37)],
              "Panel (B): Occupancy (hr)" = mod[c(4, 16, 28, 40)],
              "Panel (C): Voyages (\\#)" = mod[c(7, 19, 31, 43)],
              "Panel (D): Vessels (\\#)" = mod[c(10, 22, 34, 46)]),
         coef_omit = "Intercept",
         coef_rename = c("TNE3" = "Encounters (3 mo)"),
         gof_omit = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
         stars =  c('*' = .1, '**' = .05, '***' = .01),
         fmt = "%.2f",
         title = "Linear regression estimates for the average effect of piracy on grid-level ship transit.\\label{grid_reg}",
         notes = c(
           paste("All specifications include Fixed-effects by Grid ID, ASAM Subregion, and ASAM region by year by month.",
                 "Standard errors in parentheses are Conley (100 km cutoff).",
                 "Each column shows results for different samples: (1) Global, (2) Gulf of Aden, (3) G. of Guinea, and (4) South East Asia.")),
         threeparttable = TRUE,
         shape = "rbind",
         escape = FALSE,
         output = here("tables", "gridcell-dist-time.tex"))

add_adjust_box(here("tables", "gridcell-dist-time.tex"))


# Turn model into data.frame of coefficients
coef_data <- map_df(mod, broom::tidy, .id = "model", conf.int = T) %>%
  mutate(sample = str_squish(str_remove_all(str_remove_all(model, "sample.var: attack_cluster; sample:"), ";.+")),
         term = paste(str_extract(term, "[:digit:]+"), "mo"),
         var = case_when(str_detect(model, "distance_km") ~ "Distance (km)",
                         str_detect(model, "time_hours") ~ "Occupancy (hr)",
                         str_detect(model, "n_trips") ~ "Voyages (#)",
                         str_detect(model, "n_vessels") ~ "Vessels (#)")) %>% 
  mutate(term = fct_relevel(term, c("3 mo", "6 mo", "12 mo")),
         var = fct_relevel(var, c("Distance (km)", "Occupancy (hr)", "Voyages (#)", "Vessels (#)")))

# Plot
plot <- ggplot(coef_data, aes(x = sample, y = estimate, color = term)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high, group = term),
                 position = position_dodge(width = 0.5),
                 color = "black",
                 linewidth = 0.2) +
  geom_pointrange(aes(ymin = estimate - std.error, ymax = estimate + std.error),
                  position = position_dodge(width = 0.5),
                  size = 0.5,
                  linewidth = 1) +
  facet_wrap(~var, scales = "free_y", ncol = 1) + 
  scale_color_brewer(palette = "Set2") +
  labs(x = "Sample",
       y = "Estimate ± (std.error & 95%CI)",
       color = "Time window:") +
  theme_minimal(base_size = 7) +
  theme(legend.position = "bottom",
        legend.background = element_rect(fil = "transparent",
                                         color = "black")) +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5))

plot

ggsave(plot = plot,
       filename = here("figures", "grid_level_TWFE_coefficient_plot.pdf"),
       width = 5.7,
       height = 12,
       units = "cm")



#### NOW cumulatively adding FEs
# Just distance and slowly adding FEs
sw_fe_mod <- feols(data = reg_data,
                   fml = c(distance_km,
                           time_hours,
                           n_trips,
                           n_vessels) ~ TNE3 | csw0(grid_id, asam_subregion, year ^ month ^ asam_region),
                   # SE specifictions
                   vcov = vcov_conley(lat = "lat_bin",
                                      lon = "lon_bin",
                                      cutoff = 100),
                   panel.id = ~grid_id + date)

fixest::models(sw_fe_mod) %>% 
  arrange(lhs)

fe_rows <- tribble(
  ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)",
  "", "", "", "", "",
  "Grid ID FE", "", "X", "X", "X",
  "ASAM Subregion FE",  "", "", "X", "X",
  "ASAM Region-year-month FE",  "", "", "", "X",
)

msummary(list("Panel (A): Total Distance (km)" = sw_fe_mod[c(1, 5, 9, 13)],
              "Panel (B): Occupancy (hr)" = sw_fe_mod[c(2, 6, 10, 14)],
              "Panel (C): Voyages (\\#)" = sw_fe_mod[c(3, 7, 11, 15)],
              "Panel (D): Vessels (\\#)" = sw_fe_mod[c(4, 8, 12, 16)]),
         coef_omit = "Intercept",
         coef_rename = c("TNE3" = "Encounters (3 mo)"),
         gof_omit = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
         stars =  c('*' = .1, '**' = .05, '***' = .01),
         fmt = "%.2f",
         add_rows = fe_rows,
         title = "Linear regression estimates for the average effect of piracy on grid-level ship transit for different FE specifications.\\label{grid_reg_fe}",
         notes = c(
           paste("Standard errors in parentheses are Conley (100 km cutoff).",
                 "Number of observations:", as.character(nobs(sw_fe_mod[[1]]) %>% format(big.mark = ",")),".")),
         threeparttable = TRUE,
         shape = "rbind",
         escape = FALSE,
         output = here("tables", "gridcell-dist-time_FEs.tex"))

add_adjust_box(here("tables", "gridcell-dist-time_FEs.tex"))
