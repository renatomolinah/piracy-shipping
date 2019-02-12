test_case <- 6
test_case <- unique(point_analysis_summary$attack_reference)[test_case]
test<-point_analysis_summary %>% 
  filter(attack_reference==test_case)

test_baseline<-test %>%
  filter(days_since_attack_bin<0) %>%
  group_by(distance_to_attack_km_bin) %>%
  summarize(shipping_hours_baseline = mean(shipping_hours)) %>%
  ungroup()
test <- test %>%
  left_join(test_baseline,by="distance_to_attack_km_bin") %>%
  mutate(shipping_hours_normalized = shipping_hours/shipping_hours_baseline) %>%
  rename(`distance km` = distance_to_attack_km_bin)

test %>%
  ggplot(aes(x=days_since_attack_bin,y=shipping_hours_normalized)) +
  geom_line() +
  facet_wrap(.~`distance km`,labeller = label_both) +
  geom_vline(xintercept = 0,linetype=2) +
  geom_hline(yintercept = 1,linetype=1) +
  theme_bw() +
  labs(title=paste0("Shipping hours before and after attack id `",test_case,"`\nfor different distances from attack [km]"),
       subtitle = "Shipping hours normalized by mean of pre-attack hours",
       x="Days since attack",
       y="Normalized shipping hours")
