annual_summary_stats <- voyage_data %>%
  mutate(year = lubridate::year(departure_date)) %>%
  group_by(year) %>%
  summarize(across(c(total_route_attacks_last_3_months,
                     total_route_attacks_last_6_months,
                     total_route_attacks_last_12_months,
                     number_previous_attacks_3_months_5_degrees,
                     number_previous_attacks_6_months_5_degrees,
                     number_previous_attacks_12_months_5_degrees,
                     number_previous_attacks_3_months_3_degrees,
                     number_previous_attacks_3_months_7_degrees),
                   ~mean(.x, na.rm = TRUE))) %>%
  ungroup()

global_attacks <- asam_with_hotspots %>%
  # Only include encounters that are not suspicious approaches
  dplyr::filter(encounter_type != 'Suspicious Approach') %>%
  dplyr::filter(year >= 2013) %>%
  dplyr::filter(year <= 2021) %>%
  group_by(year) %>%
  dplyr::summarize(total_number_global_attacks = n_distinct(asam_reference)) %>%
  dplyr::ungroup()

indicator_order <- annual_summary_stats %>%
  filter(year == 2021) %>%
  pivot_longer(-year) %>%
  arrange(-value) %>%
  .$name

indicator_fig <- annual_summary_stats %>%
  pivot_longer(-year,
               values_to = "mean_value") %>%
  mutate(name = fct_relevel(name,indicator_order)) %>%
  ggplot(aes(x = year, y = mean_value, color = name)) +
  geom_line() +
  scale_color_brewer("attack indicator",type = "qual") +
  ggplot2::scale_x_continuous(breaks = seq(2013,2021,2))

global_attack_fig <-   global_attacks %>%
  ggplot(aes(x = year, y = total_number_global_attacks)) +
  geom_line() +
  ggplot2::scale_x_continuous(breaks = seq(2013,2021,2))

cowplot::plot_grid(indicator_fig,
                   global_attack_fig,
                   ncol = 1,
                   rel_heights = c(2,1),
                   align = "v", axis = 'lr')

summary_table <- annual_summary_stats  %>%
  bind_cols(global_attacks %>% dplyr::select(total_number_global_attacks)) %>%
  tibble::rownames_to_column() %>%  
  pivot_longer(-rowname) %>% 
  pivot_wider(names_from=rowname, values_from=value) 
