# Load packages
library(tidyverse)
library(cowplot)
library(fixest)


# Load dataset (takes a while)
# Previously filtered to only include trips that pass through hotspots,
# as well as trips that are consistent with vessel design (not too fast - not too long).

wdb_m <- readRDS("~/Box Sync/Proyectos/piracy-shipping/Data sets/wdb.rds") %>%
  mutate(
    hotspot = ifelse(guinea == 1, "Guinea",
                     ifelse(aden == 1, "Aden", 
                            ifelse(asia == 1, "Asia", "None"))),
    distance = total_distance_km,
    time = total_hours/100,
    
    attacks_past_3 = attacks_window_last_3_month,
    attacks_past_6 = attacks_window_last_6_month,
    attacks_past_9 = attacks_window_last_9_month,
    attacks_past_12 = attacks_window_last_12_month
  ) 

# Sample data set to get quick estimations 

wdb <- wdb_m %>% sample_frac(0.1) #%>% filter(aden == 1) # 

# Variation in the number of past attacks

p1 <- wdb %>% 
  ggplot(aes(x = attacks_past_3,
             group = hotspot)) +
  geom_density(aes(color = hotspot)) +
  theme_bw() +
  ylab("Density") + 
  xlab("Past attacks") +
  labs(color = "Hotspot") +
  ggtitle("Past 3 Months") 

p2 <- wdb %>% 
  ggplot(aes(x = attacks_past_6,
             group = hotspot)) +
  geom_density(aes(color = hotspot)) +
  theme_bw() +
  ylab("Density") + 
  xlab("Past attacks") +
  labs(color = "Hotspot") +
  ggtitle("Past 6 Months")

p3 <- wdb %>% 
  ggplot(aes(x = attacks_past_9,
             group = hotspot)) +
  geom_density(aes(color = hotspot)) +
  theme_bw() +
  ylab("Density") + 
  xlab("Past attacks") +
  labs(color = "Hotspot") +
  ggtitle("Past 9 Months")

p4 <- wdb %>% 
  ggplot(aes(x = attacks_past_12,
             group = hotspot)) +
  geom_density(aes(color = hotspot)) +
  theme_bw() +
  ylab("Density") + 
  xlab("Past attacks") +
  labs(color = "Hotspot") +
  ggtitle("Past 12 Months")

plot_grid(p1, p2, p3, p4, ncol = 2, labels = "AUTO")

# Scatter plots of the effect of number of attacks

# Time

p1 <- wdb %>% sample_frac(0.1) %>% 
  ggplot(aes(x = log(attacks_past_3),
             y = log(time),
             group = hotspot)) +
  geom_point(aes(color = hotspot), alpha = 0.2) +
  theme_bw() +
  ylab("log(Time)") + 
  xlab("log(Past attacks)") +
  labs(color = "Hotspot") +
  ggtitle("Past 3 Months")

p2 <- wdb %>% sample_frac(0.1) %>%
  ggplot(aes(x = log(attacks_past_6),
             y = log(time),
             group = hotspot)) +
  geom_point(aes(color = hotspot), alpha = 0.2) +
  theme_bw() +
  ylab("log(Time)") + 
  xlab("log(Past attacks)") +
  labs(color = "Hotspot") +
  ggtitle("Past 6 Months")

p3 <- wdb %>% sample_frac(0.1) %>%
  ggplot(aes(x = log(attacks_past_9),
             y = log(time),
             group = hotspot)) +
  geom_point(aes(color = hotspot), alpha = 0.2) +
  theme_bw() +
  ylab("log(Time)") + 
  xlab("log(Past attacks)") +
  labs(color = "Hotspot") +
  ggtitle("Past 9 Months")

p4 <- wdb %>% sample_frac(0.1) %>%
  ggplot(aes(x = log(attacks_past_12),
             y = log(time),
             group = hotspot)) +
  geom_point(aes(color = hotspot), alpha = 0.2) +
  theme_bw() +
  ylab("log(Time)") + 
  xlab("log(Past attacks)") +
  labs(color = "Hotspot") +
  ggtitle("Past 12 Months")

plot_grid(p1, p2, p3, p4, ncol = 2, labels = "AUTO")

# Distance 

p1 <- wdb %>% sample_frac(0.1) %>%
  ggplot(aes(x = log(attacks_past_3),
             y = log(distance),
             group = hotspot)) +
  geom_point(aes(color = hotspot), alpha = 0.2) +
  theme_bw() +
  ylab("log(Distance)") + 
  xlab("log(Past attacks)") +
  labs(color = "Hotspot") +
  ggtitle("Past 3 Months")

p2 <- wdb %>% sample_frac(0.1) %>%
  ggplot(aes(x = log(attacks_past_6),
             y = log(distance),
             group = hotspot)) +
  geom_point(aes(color = hotspot), alpha = 0.2) +
  theme_bw() +
  ylab("log(Distance)") + 
  xlab("log(Past attacks)") +
  labs(color = "Hotspot") +
  ggtitle("Past 6 Months")

p3 <- wdb %>% sample_frac(0.1) %>%
  ggplot(aes(x = log(attacks_past_9),
             y = log(distance),
             group = hotspot)) +
  geom_point(aes(color = hotspot), alpha = 0.2) +
  theme_bw() +
  ylab("log(Distance)") + 
  xlab("log(Past attacks)") +
  labs(color = "Hotspot") +
  ggtitle("Past 9 Months")

p4 <- wdb %>% sample_frac(0.1) %>%
  ggplot(aes(x = log(attacks_past_12),
             y = log(distance),
             group = hotspot)) +
  geom_point(aes(color = hotspot), alpha = 0.2) +
  theme_bw() +
  ylab("log(Distance)") + 
  xlab("log(Past attacks)") +
  labs(color = "Hotspot") +
  ggtitle("Past 12 Months")

plot_grid(p1, p2, p3, p4, ncol = 2, labels = "AUTO")

# Speed 

p1 <- wdb %>% sample_frac(0.1) %>%
  ggplot(aes(x = log(attacks_past_3),
             y = log(speed),
             group = hotspot)) +
  geom_point(aes(color = hotspot), alpha = 0.2) +
  theme_bw() +
  ylab("log(Speed)") + 
  xlab("log(Past attacks)") +
  labs(color = "Hotspot") +
  ggtitle("Past 3 Months")

p2 <- wdb %>% sample_frac(0.1) %>%
  ggplot(aes(x = log(attacks_past_6),
             y = log(speed),
             group = hotspot)) +
  geom_point(aes(color = hotspot), alpha = 0.2) +
  theme_bw() +
  ylab("log(Speed)") + 
  xlab("log(Past attacks)") +
  labs(color = "Hotspot") +
  ggtitle("Past 6 Months")

p3 <- wdb %>% sample_frac(0.1) %>%
  ggplot(aes(x = log(attacks_past_9),
             y = log(speed),
             group = hotspot)) +
  geom_point(aes(color = hotspot), alpha = 0.2) +
  theme_bw() +
  ylab("log(Speed)") + 
  xlab("log(Past attacks)") +
  labs(color = "Hotspot") +
  ggtitle("Past 9 Months")

p4 <- wdb %>% sample_frac(0.1) %>%
  ggplot(aes(x = log(attacks_past_12),
             y = log(speed),
             group = hotspot)) +
  geom_point(aes(color = hotspot), alpha = 0.2) +
  theme_bw() +
  ylab("log(Speed)") + 
  xlab("log(Past attacks)") +
  labs(color = "Hotspot") +
  ggtitle("Past 12 Months")

plot_grid(p1, p2, p3, p4, ncol = 2, labels = "AUTO")


# So, it appears traffic patterns differ across hotspost, which suggests results might be affected
# by these patterns as well. Let's run some regressions to get insights on this


# Regressions

# effect of past attacks on travel time

m1 = feols(time ~ attacks_past_3 + wind + wind_vector
           | vtype + size + country + month + year, wdb,
           cluster = ~country + year)

m2 = feols(time ~ attacks_past_6 + wind + wind_vector
           | vtype + size + country + month + year, wdb,
           cluster = ~country + year)

m3 = feols(time ~ attacks_past_9 + wind + wind_vector
           | vtype + size + country + month + year, wdb,
           cluster = ~country + year)

m4 = feols(time ~ attacks_past_12 + wind + wind_vector
           | vtype + size + country + month + year, wdb,
           cluster = ~country + year)

etable(m1, m2, m3, m4, drop = c("wind", "wind_vector"))

# effect of past attacks on distance

m1 = feols(distance ~ attacks_past_3 + wind + wind_vector
           | vtype + size + country + month + year, wdb,
           cluster = ~country)

m2 = feols(distance ~ attacks_past_6 + wind + wind_vector
           | vtype + size + country + month + year, wdb,
           cluster = ~country)

m3 = feols(distance ~ attacks_past_9 + wind + wind_vector
           | vtype + size + country + month + year, wdb,
           cluster = ~country)

m4 = feols(distance ~ attacks_past_12 + wind + wind_vector
           | vtype + size + country + month + year, wdb,
           cluster = ~country)

etable(m1, m2, m3, m4, drop = c("wind", "wind_vector"))

# effect of past attacks on speed

m1 = feols(speed ~ attacks_past_3 + wind + wind_vector
           | vtype + size + country + month + year, wdb,
           cluster = ~country + year)

m2 = feols(speed ~ attacks_past_6 + wind + wind_vector
           | vtype + size + country + month + year, wdb,
           cluster = ~country + year)

m3 = feols(speed ~ attacks_past_9 + wind + wind_vector
           | vtype + size + country + month + year, wdb,
           cluster = ~country + year)

m4 = feols(speed ~ attacks_past_12 + wind + wind_vector
           | vtype + size + country + month + year, wdb,
           cluster = ~country + year)

etable(m1, m2, m3, m4, drop = c("wind", "wind_vector"))








