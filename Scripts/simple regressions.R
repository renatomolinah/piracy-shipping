# Load packages
library(tidyverse)
library(ggpubr)
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
    time = total_hours,
    
    attacks_past_3 = attacks_window_last_3_month,
    attacks_past_6 = attacks_window_last_6_month,
    attacks_past_9 = attacks_window_last_9_month,
    attacks_past_12 = attacks_window_last_12_month,
    
    h_attacks_past_3 = hotspot_attacks_window_last3_month,
    h_attacks_past_6 = hotspot_attacks_window_last6_month,
    h_attacks_past_9 = hotspot_attacks_window_last9_month,
    h_attacks_past_12 = hotspot_attacks_window_last12_month,
    
    odds_past_3 = h_attacks_past_3/unique_hotspot_vessels_last_3_month,
    odds_past_6 = h_attacks_past_6/unique_hotspot_vessels_last_6_month,
    odds_past_9 = h_attacks_past_9/unique_hotspot_vessels_last_9_month,
    odds_past_12 = h_attacks_past_12/unique_hotspot_vessels_last_12_month
    
  ) 

# Sample data set to get quick estimations 

wdb <- wdb_m #%>% filter(top_route == 1) # %>% sample_frac(0.1) #  %>% filter(top_route == 1) # 

# Variation in the number of past attacks

p1 <- wdb %>% 
  ggplot(aes(x = log(attacks_past_3),
             group = hotspot,
             fill = hotspot)) +
  geom_density(aes(fill = hotspot), alpha = 0.5) +
  theme_bw() +
  ylab("Density") + 
  xlab("log-Past attacks") +
  labs(fill = "Hotspot") +
  ggtitle("Past 3 Months") 

p2 <- wdb %>% 
  ggplot(aes(x = log(attacks_past_6),
             group = hotspot,
             fill = hotspot)) +
  geom_density(aes(fill = hotspot), alpha = 0.5) +
  theme_bw() +
  ylab("Density") + 
  xlab("log-Past attacks") +
  labs(fill = "Hotspot") +
  ggtitle("Past 6 Months")

p3 <- wdb %>% 
  ggplot(aes(x = log(attacks_past_9),
             group = hotspot,
             fill = hotspot)) +
  geom_density(aes(fill = hotspot), alpha = 0.5) +
  theme_bw() +
  ylab("Density") + 
  xlab("log-Past attacks") +
  labs(fill = "Hotspot") +
  ggtitle("Past 9 Months")

p4 <- wdb %>% 
  ggplot(aes(x = log(attacks_past_12),
             group = hotspot,
             fill = hotspot)) +
  geom_density(aes(fill = hotspot), alpha = 0.5) +
  theme_bw() +
  ylab("Density") + 
  xlab("log-Past attacks") +
  labs(fill = "Hotspot") +
  ggtitle("Past 12 Months")

# Generate plot illustrating that there's variation in the number of attacks across hotspots. Also, that there are fat tails.

ggarrange(p1, p2, p3, p4, common.legend = TRUE, legend = "bottom") + ggsave("attack_var.jpg", width = 10, height = 8)

# Regressions

# Setting a dictionary 
setFixest_dict(c(time = "Total Time (hrs)", 
                 distance = "Total Distance (km)", 
                 speed = "Average Speed (km/hr)",
                 
                 attacks_past_3 = "Encounters (3 mo)",
                 attacks_past_6 = "Encounters (6 mo)",
                 attacks_past_9 = "Encounters (9 mo)",
                 attacks_past_12 = "Encounters (12 mo)",
                 
                 hotspot = "Hotspot",
                 vtype = "Vessel type",
                 size = "Vesel size",
                 country = "Country comb.",
                 year = "Year",
                 month = "Month",
                 'month^year' = "month-by-year"
                 ))


# Effect of past attacks on total travel distance

m1 = feols(distance ~ attacks_past_3 + odds_past_3 + wind + wind_vector
           | hotspot + vtype + size + country + month^year, wdb,
           cluster = ~country + year)

m2 = feols(distance ~ attacks_past_6 + odds_past_6 + wind + wind_vector
           | hotspot + vtype + size + country + month^year, wdb,
           cluster = ~country + year)

m3 = feols(distance ~ attacks_past_9 + odds_past_9 + wind + wind_vector
           | hotspot + vtype + size + country + month^year, wdb,
           cluster = ~country + year)

m4 = feols(distance ~ attacks_past_12 + odds_past_12 + wind + wind_vector
           | hotspot + vtype + size + country + month^year, wdb,
           cluster = ~country + year)

etable(m1, m2, m3, m4, drop = c("odds_past_3",
                                "odds_past_6",
                                "odds_past_9",
                                "odds_past_12",
                                "wind",
                                "wind_vector"),
       style.df = style.df(depvar.title = "",
                           fixef.title = " ",
                           fixef.suffix = " FE", 
                           yesNo = "X"),
       fitstat = ~ r2 + n)

# Effect of past attacks on total travel time

m1 = feols(time ~ attacks_past_3 + odds_past_3 + wind + wind_vector
           | hotspot + vtype + size + country + month^year, wdb,
           cluster = ~country + year)

m2 = feols(time ~ attacks_past_6 + odds_past_6 + wind + wind_vector
           | hotspot + vtype + size + country + month^year, wdb,
           cluster = ~country + year)

m3 = feols(time ~ attacks_past_9 + odds_past_9 + wind + wind_vector
           | hotspot + vtype + size + country + month^year, wdb,
           cluster = ~country + year)

m4 = feols(time ~ attacks_past_12 + odds_past_12 + wind + wind_vector
           | hotspot + vtype + size + country + month^year, wdb,
           cluster = ~country + year)

etable(m1, m2, m3, m4, drop = c("odds_past_3",
                                "odds_past_6",
                                "odds_past_9",
                                "odds_past_12",
                                "wind",
                                "wind_vector"),
       style.df = style.df(depvar.title = "",
                           fixef.title = " ",
                           fixef.suffix = " FE", 
                           yesNo = "X"),
       fitstat = ~ r2 + n)

# Effect of past attacks on average travel speed

m1 = feols(speed ~ attacks_past_3 + odds_past_3 + wind + wind_vector
           | hotspot + vtype + size + country + month^year, wdb,
           cluster = ~country + year)

m2 = feols(speed ~ attacks_past_6 + odds_past_6 + wind + wind_vector
           | hotspot + vtype + size + country + month^year, wdb,
           cluster = ~country + year)

m3 = feols(speed ~ attacks_past_9 + odds_past_9 + wind + wind_vector
           | hotspot + vtype + size + country + month^year, wdb,
           cluster = ~country + year)

m4 = feols(speed ~ attacks_past_12 + odds_past_12 + wind + wind_vector
           | hotspot + vtype + size + country + month^year, wdb,
           cluster = ~country + year)

etable(m1, m2, m3, m4, drop = c("odds_past_3",
                                "odds_past_6",
                                "odds_past_9",
                                "odds_past_12",
                                "wind",
                                "wind_vector"),
       style.df = style.df(depvar.title = "",
                           fixef.title = " ",
                           fixef.suffix = " FE", 
                           yesNo = "X"),
       fitstat = ~ r2 + n)


################################
# Linear estimation on distance

m1 = feols(distance ~ attacks_past_12
           + odds_past_12 + wind + wind_vector
           | hotspot + vtype + size + country + month^year,
           wdb,
           cluster = ~country + year)

m2 = feols(distance ~ attacks_past_12
           + odds_past_12 + wind + wind_vector
           | hotspot + vtype + size + country + month^year,
           wdb %>% filter(hotspot == "Aden"),
           cluster = ~country + year)

m3 = feols(distance ~ attacks_past_12
           + odds_past_12 + wind + wind_vector
           | hotspot + vtype + size + country + month^year,
           wdb %>% filter(hotspot == "Guinea"),
           cluster = ~country + year)

m4 = feols(distance ~ attacks_past_12
           + odds_past_12 + wind + wind_vector
           | hotspot + vtype + size + country + month^year,
           wdb %>% filter(hotspot == "Asia"),
           cluster = ~country + year)

etable(m1, m2, m3, m4, drop = c("odds_past_12", "wind", "wind_vector"),
       subtitles = c("Full Sample", "Aden", "Guinea", "SE Asia"),
       style.df = style.df(depvar.title = "",
                           fixef.title = " ",
                           fixef.suffix = " FE", 
                           yesNo = "X"),
       fitstat = ~ r2 + n)

etable(m1, m2, m3, m4, drop = c("odds_past_12","wind", "wind_vector"),
       subtitles = c("Full Sample", "Aden", "Guinea", "SE Asia"),
       style.tex = style.tex("aer",
                             depvar.title = "",
                             fixef.title = " ",
                             fixef.suffix = " FE", 
                             yesNo = "X"),
       fitstat = ~ r2 + n, signifCode = c("***"=0.001, "**"=0.01, "*"=0.05), tex = TRUE)

# Quadratic estimation on time

m1 = feols(time ~ attacks_past_12
           + odds_past_12 + wind + wind_vector
           | hotspot + vtype + size + country + month^year,
           wdb,
           cluster = ~country + year)

m2 = feols(time ~ attacks_past_12
           + odds_past_12 + wind + wind_vector
           | hotspot + vtype + size + country + month^year,
           wdb %>% filter(hotspot == "Aden"),
           cluster = ~country + year)

m3 = feols(time ~ attacks_past_12
           + odds_past_12 + wind + wind_vector
           | hotspot + vtype + size + country + month^year,
           wdb %>% filter(hotspot == "Guinea"),
           cluster = ~country + year)

m4 = feols(time ~ attacks_past_12
           + odds_past_12 + wind + wind_vector
           | hotspot + vtype + size + country + month^year,
           wdb %>% filter(hotspot == "Asia"),
           cluster = ~country + year)


etable(m1, m2, m3, m4, drop = c("odds_past_12", "wind", "wind_vector"),
       subtitles = c("Full Sample", "Aden", "Guinea", "SE Asia"),
       style.df = style.df(depvar.title = "",
                           fixef.title = " ",
                           fixef.suffix = " FE", 
                           yesNo = "X"),
       fitstat = ~ r2 + n)

# Linear estimation on speed

m1 = feols(speed ~ attacks_past_12
           + odds_past_12 + wind + wind_vector
           | hotspot + vtype + size + country + month^year,
           wdb,
           cluster = ~country + year)

m2 = feols(speed ~ attacks_past_12
           + odds_past_12 + wind + wind_vector
           | hotspot + vtype + size + country + month^year,
           wdb %>% filter(hotspot == "Aden"),
           cluster = ~country + year)

m3 = feols(speed ~ attacks_past_12 
           + odds_past_12 + wind + wind_vector
           | hotspot + vtype + size + country + month^year,
           wdb %>% filter(hotspot == "Guinea"),
           cluster = ~country + year)

m4 = feols(speed ~ attacks_past_12 
           + odds_past_12 + wind + wind_vector
           | hotspot + vtype + size + country + month^year,
           wdb %>% filter(hotspot == "Asia"),
           cluster = ~country + year)


etable(m1, m2, m3, m4, drop = c("odds_past_12", "wind", "wind_vector"),
       subtitles = c("Full Sample", "Aden", "Guinea", "SE Asia"),
       style.df = style.df(depvar.title = "",
                           fixef.title = " ",
                           fixef.suffix = " FE", 
                           yesNo = "X"),
       fitstat = ~ r2 + n)

# Linear estimation on total cost

# Fuel
m1 = feols(total_fuel_cost ~ attacks_past_12
           + odds_past_12 + wind + wind_vector
           | hotspot + vtype + size + country + month^year,
           wdb,
           cluster = ~country + year)

m2 = feols(total_fuel_cost ~ attacks_past_12
           + odds_past_12 + wind + wind_vector
           | hotspot + vtype + size + country + month^year,
           wdb %>% filter(hotspot == "Aden"),
           cluster = ~country + year)

m3 = feols(total_fuel_cost ~ attacks_past_12 
           + odds_past_12 + wind + wind_vector
           | hotspot + vtype + size + country + month^year,
           wdb %>% filter(hotspot == "Guinea"),
           cluster = ~country + year)

m4 = feols(total_fuel_cost ~ attacks_past_12 
           + odds_past_12 + wind + wind_vector
           | hotspot + vtype + size + country + month^year,
           wdb %>% filter(hotspot == "Asia"),
           cluster = ~country + year)

etable(m1, m2, m3, m4, drop = c("odds_past_12", "wind", "wind_vector"),
       subtitles = c("Full Sample", "Aden", "Guinea", "SE Asia"),
       style.df = style.df(depvar.title = "",
                           fixef.title = " ",
                           fixef.suffix = " FE", 
                           yesNo = "X"),
       fitstat = ~ r2 + n)

etable(m1, m2, m3, m4, drop = c("odds_past_12","wind", "wind_vector"),
       subtitles = c("Full Sample", "Aden", "Guinea", "SE Asia"),
       style.tex = style.tex("aer",
                             depvar.title = "",
                             fixef.title = " ",
                             fixef.suffix = " FE", 
                             yesNo = "X"),
       fitstat = ~ r2 + n, signifCode = c("***"=0.001, "**"=0.01, "*"=0.05), tex = TRUE)

# Labor
m1 = feols(labor_cost ~ attacks_past_12
           + odds_past_12 + wind + wind_vector
           | hotspot + vtype + size + country + month^year,
           wdb,
           cluster = ~country + year)

m2 = feols(labor_cost ~ attacks_past_12
           + odds_past_12 + wind + wind_vector
           | hotspot + vtype + size + country + month^year,
           wdb %>% filter(hotspot == "Aden"),
           cluster = ~country + year)

m3 = feols(labor_cost ~ attacks_past_12 
           + odds_past_12 + wind + wind_vector
           | hotspot + vtype + size + country + month^year,
           wdb %>% filter(hotspot == "Guinea"),
           cluster = ~country + year)

m4 = feols(labor_cost ~ attacks_past_12 
           + odds_past_12 + wind + wind_vector
           | hotspot + vtype + size + country + month^year,
           wdb %>% filter(hotspot == "Asia"),
           cluster = ~country + year)

etable(m1, m2, m3, m4, drop = c("odds_past_12", "wind", "wind_vector"),
       subtitles = c("Full Sample", "Aden", "Guinea", "SE Asia"),
       style.df = style.df(depvar.title = "",
                           fixef.title = " ",
                           fixef.suffix = " FE", 
                           yesNo = "X"),
       fitstat = ~ r2 + n)

etable(m1, m2, m3, m4, drop = c("odds_past_12","wind", "wind_vector"),
       subtitles = c("Full Sample", "Aden", "Guinea", "SE Asia"),
       style.tex = style.tex("aer",
                             depvar.title = "",
                             fixef.title = " ",
                             fixef.suffix = " FE", 
                             yesNo = "X"),
       fitstat = ~ r2 + n, signifCode = c("***"=0.001, "**"=0.01, "*"=0.05), tex = TRUE)


# total
m1 = feols(total_cost ~ attacks_past_12
           + odds_past_12 + wind + wind_vector
           | hotspot + vtype + size + country + month^year,
           wdb,
           cluster = ~country + year)

m2 = feols(total_cost ~ attacks_past_12
           + odds_past_12 + wind + wind_vector
           | hotspot + vtype + size + country + month^year,
           wdb %>% filter(hotspot == "Aden"),
           cluster = ~country + year)

m3 = feols(total_cost ~ attacks_past_12 
           + odds_past_12 + wind + wind_vector
           | hotspot + vtype + size + country + month^year,
           wdb %>% filter(hotspot == "Guinea"),
           cluster = ~country + year)

m4 = feols(total_cost ~ attacks_past_12 
           + odds_past_12 + wind + wind_vector
           | hotspot + vtype + size + country + month^year,
           wdb %>% filter(hotspot == "Asia"),
           cluster = ~country + year)

etable(m1, m2, m3, m4, drop = c("odds_past_12", "wind", "wind_vector"),
       subtitles = c("Full Sample", "Aden", "Guinea", "SE Asia"),
       style.df = style.df(depvar.title = "",
                           fixef.title = " ",
                           fixef.suffix = " FE", 
                           yesNo = "X"),
       fitstat = ~ r2 + n)

etable(m1, m2, m3, m4, drop = c("odds_past_12","wind", "wind_vector"),
       subtitles = c("Full Sample", "Aden", "Guinea", "SE Asia"),
       style.tex = style.tex("aer",
                             depvar.title = "",
                             fixef.title = " ",
                             fixef.suffix = " FE", 
                             yesNo = "X"),
       fitstat = ~ r2 + n, signifCode = c("***"=0.001, "**"=0.01, "*"=0.05), tex = TRUE)


# Estimation on emissions

# Carbon
m1 = feols(total_co2 ~ attacks_past_12
           + odds_past_12 + wind + wind_vector
           | hotspot + vtype + size + country + month^year,
           wdb,
           cluster = ~country + year)

m2 = feols(total_co2 ~ attacks_past_12
           + odds_past_12 + wind + wind_vector
           | hotspot + vtype + size + country + month^year,
           wdb %>% filter(hotspot == "Aden"),
           cluster = ~country + year)

m3 = feols(total_co2 ~ attacks_past_12 
           + odds_past_12 + wind + wind_vector
           | hotspot + vtype + size + country + month^year,
           wdb %>% filter(hotspot == "Guinea"),
           cluster = ~country + year)

m4 = feols(total_co2 ~ attacks_past_12 
           + odds_past_12 + wind + wind_vector
           | hotspot + vtype + size + country + month^year,
           wdb %>% filter(hotspot == "Asia"),
           cluster = ~country + year)

etable(m1, m2, m3, m4, drop = c("odds_past_12", "wind", "wind_vector"),
       subtitles = c("Full Sample", "Aden", "Guinea", "SE Asia"),
       style.df = style.df(depvar.title = "",
                           fixef.title = " ",
                           fixef.suffix = " FE", 
                           yesNo = "X"),
       fitstat = ~ r2 + n)

etable(m1, m2, m3, m4, drop = c("odds_past_12","wind", "wind_vector"),
       subtitles = c("Full Sample", "Aden", "Guinea", "SE Asia"),
       style.tex = style.tex("aer",
                             depvar.title = "",
                             fixef.title = " ",
                             fixef.suffix = " FE", 
                             yesNo = "X"),
       fitstat = ~ r2 + n, signifCode = c("***"=0.001, "**"=0.01, "*"=0.05), tex = TRUE)

# NOX
m1 = feols(total_nox ~ attacks_past_12
           + odds_past_12 + wind + wind_vector
           | hotspot + vtype + size + country + month^year,
           wdb,
           cluster = ~country + year)

m2 = feols(total_nox ~ attacks_past_12
           + odds_past_12 + wind + wind_vector
           | hotspot + vtype + size + country + month^year,
           wdb %>% filter(hotspot == "Aden"),
           cluster = ~country + year)

m3 = feols(total_nox ~ attacks_past_12 
           + odds_past_12 + wind + wind_vector
           | hotspot + vtype + size + country + month^year,
           wdb %>% filter(hotspot == "Guinea"),
           cluster = ~country + year)

m4 = feols(total_nox ~ attacks_past_12 
           + odds_past_12 + wind + wind_vector
           | hotspot + vtype + size + country + month^year,
           wdb %>% filter(hotspot == "Asia"),
           cluster = ~country + year)

etable(m1, m2, m3, m4, drop = c("odds_past_12", "wind", "wind_vector"),
       subtitles = c("Full Sample", "Aden", "Guinea", "SE Asia"),
       style.df = style.df(depvar.title = "",
                           fixef.title = " ",
                           fixef.suffix = " FE", 
                           yesNo = "X"),
       fitstat = ~ r2 + n)

etable(m1, m2, m3, m4, drop = c("odds_past_12","wind", "wind_vector"),
       subtitles = c("Full Sample", "Aden", "Guinea", "SE Asia"),
       style.tex = style.tex("aer",
                             depvar.title = "",
                             fixef.title = " ",
                             fixef.suffix = " FE", 
                             yesNo = "X"),
       fitstat = ~ r2 + n, signifCode = c("***"=0.001, "**"=0.01, "*"=0.05), tex = TRUE)

# SOX
m1 = feols(total_sox ~ attacks_past_12
           + odds_past_12 + wind + wind_vector
           | hotspot + vtype + size + country + month^year,
           wdb,
           cluster = ~country + year)

m2 = feols(total_sox ~ attacks_past_12
           + odds_past_12 + wind + wind_vector
           | hotspot + vtype + size + country + month^year,
           wdb %>% filter(hotspot == "Aden"),
           cluster = ~country + year)

m3 = feols(total_sox ~ attacks_past_12 
           + odds_past_12 + wind + wind_vector
           | hotspot + vtype + size + country + month^year,
           wdb %>% filter(hotspot == "Guinea"),
           cluster = ~country + year)

m4 = feols(total_sox ~ attacks_past_12 
           + odds_past_12 + wind + wind_vector
           | hotspot + vtype + size + country + month^year,
           wdb %>% filter(hotspot == "Asia"),
           cluster = ~country + year)

etable(m1, m2, m3, m4, drop = c("odds_past_12", "wind", "wind_vector"),
       subtitles = c("Full Sample", "Aden", "Guinea", "SE Asia"),
       style.df = style.df(depvar.title = "",
                           fixef.title = " ",
                           fixef.suffix = " FE", 
                           yesNo = "X"),
       fitstat = ~ r2 + n)

etable(m1, m2, m3, m4, drop = c("odds_past_12","wind", "wind_vector"),
       subtitles = c("Full Sample", "Aden", "Guinea", "SE Asia"),
       style.tex = style.tex("aer",
                             depvar.title = "",
                             fixef.title = " ",
                             fixef.suffix = " FE", 
                             yesNo = "X"),
       fitstat = ~ r2 + n, signifCode = c("***"=0.001, "**"=0.01, "*"=0.05), tex = TRUE)



# Back of the envelope calculations

# Annual cost (Run cost model first!)

cost <- wdb %>% 
  mutate(piracy_cost = m1$coefficients[1]*attacks_past_12
         ) %>%
  group_by(year
           ) %>%
  summarize(total_cost = sum(total_cost/1e6),
            piracy_cost = sum(piracy_cost/1e6)
            ) %>%
  mutate(share = piracy_cost/total_cost*100)

mean(cost$piracy_cost)
mean(cost$share)

# Emissions (Run the respective models first!)

carbon <- wdb %>% 
  mutate(piracy_co2 = m1$coefficients[1]*attacks_past_12
  ) %>%
  group_by(year
  ) %>%
  summarize(total_co2 = sum(total_co2/1e6),
            piracy_co2 = sum(piracy_co2/1e6)
  ) %>%
  mutate(share = piracy_co2/total_co2*100)

mean(carbon$piracy_co2)
mean(carbon$share) 


nox <- wdb %>% 
  mutate(piracy_nox = m1$coefficients[1]*attacks_past_12
  ) %>%
  group_by(year
  ) %>%
  summarize(total_nox = sum(total_nox/1e3),
            piracy_nox = sum(piracy_nox/1e3)
  ) %>%
  mutate(share = piracy_nox/total_nox*100)

mean(nox$piracy_nox)
mean(nox$share) 


sox <- wdb %>% 
  mutate(piracy_sox = m1$coefficients[1]*attacks_past_12
  ) %>%
  group_by(year
  ) %>%
  summarize(total_sox = sum(total_sox/1e3),
            piracy_sox = sum(piracy_sox/1e3)
  ) %>%
  mutate(share = piracy_sox/total_sox*100)

mean(sox$piracy_sox)
mean(nox$share) 


