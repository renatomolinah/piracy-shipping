library(tidyverse)
point_analysis_summary <- read_csv("point_analysis_summary.csv")
baseline<-point_analysis_summary %>%
  filter(days_since_attack_bin<0) %>%
  group_by(attack_reference,distance_to_attack_km_bin) %>%
  summarize(shipping_hours_baseline = mean(shipping_hours,na.rm=TRUE),
            shipping_distance_baseline = mean(shipping_distance_traveled_km,na.rm=TRUE),
            unique_number_voyages_baseline = mean(unique_number_voyages,na.rm=TRUE)) %>%
  ungroup()

grouped <- point_analysis_summary %>%
  left_join(baseline,by=c("attack_reference","distance_to_attack_km_bin")) %>%
  mutate(shipping_hours_normalized = ifelse(shipping_hours_baseline==0,NA,shipping_hours/shipping_hours_baseline),
         shipping_distance_normalized = ifelse(shipping_distance_baseline==0,NA,shipping_distance_traveled_km/shipping_distance_baseline),
         unique_number_voyages_normalized = ifelse(unique_number_voyages_baseline==0,NA,unique_number_voyages/unique_number_voyages_baseline)) %>%
  rename(`distance km` = distance_to_attack_km_bin) %>%
  mutate(timing = ifelse(days_since_attack_bin<0,"Before","After"))

plot <- grouped  %>%
  filter(`distance km` == 100) %>%
  #filter(attack_reference==test_case) %>%
  group_by(timing,days_since_attack_bin,`distance km`) %>%
  summarize(`Hours traveled` = mean(shipping_hours_normalized,na.rm=TRUE),
            `Distance traveled [km]` = mean(shipping_distance_normalized,na.rm=TRUE),
            `Unique number of voyages` = mean(unique_number_voyages_normalized,na.rm=TRUE)) %>%
  ungroup() %>%
  gather(indicator,value,`Hours traveled`:`Unique number of voyages`,-days_since_attack_bin,-timing) %>%
  ggplot(aes(x=days_since_attack_bin,y=value)) +
  geom_smooth(aes(group=timing)) +
  facet_wrap(.~indicator,scales="free",ncol=1) +
  geom_vline(xintercept = 0,linetype=2) +
  geom_hline(yintercept = 1,linetype=1) +
  theme_minimal(base_size = 18) +
  labs(x="Days since attack",
       y="Mean normalized metric value",
       title="Normalized metrics relative to pre-attack baseline,\n50-100km from attack, mean across all attacks") 
ggsave(filename = "figs/point_analysis.eps", plot = plot, device = cairo_ps, dpi=300, width = 8.5, height = 8.5)
