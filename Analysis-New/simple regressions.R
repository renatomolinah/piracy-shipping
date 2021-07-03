# Libraries ---------------------------------------------------------------

library(fst)
library(data.table)
library(fixest)
library(ggplot2)
library(here)


# Data --------------------------------------------------------------------

# Load dataset (takes a while)
# Previously filtered to only include trips that pass through hotspots,
# as well as trips that are consistent with vessel design (not too fast - not too long).

wdb = read_fst(here("data/wdb.fst")); setDT(wdb)

## Remove a bunch of unnecessary columns (to free up memory)
rcols = grep(paste(setdiff(1:24, seq(3, 12, 3)), collapse="|"), 
             names(wdb), value = TRUE)
rcols = rcols[!grepl("12|total_co2", rcols)] ## Minor catches for 12mo periods and co2
wdb[, (rcols) := NULL]
rm(rcols); gc()

## Cheaper to have 1/0 vars as TRUE/FALSE
bvars = c("guinea", "aden", "asia")
wdb[ , (bvars) := lapply(.SD, as.logical), .SDcols = bvars]
rm(bvars)

wdb[, hotspot := fcase(guinea, "Guinea",
                       aden, "Aden",
                       asia, "Asia",
                       default = "None")]
setnames(wdb,
         old = c("total_distance_km", "total_hours",
                 paste0("attacks_window_last_", seq(3, 12, 3), "_month"),
                 paste0("hotspot_attacks_window_last", seq(3, 12, 3), "_month")),
         new = c("distance", "time",
                 paste0("attacks_past_", seq(3, 12, 3)),
                 paste0("h_attacks_past_", seq(3, 12, 3))))

wdb[, `:=` (odds_past_3 = h_attacks_past_3/unique_hotspot_vessels_last_3_month,
            odds_past_6 = h_attacks_past_6/unique_hotspot_vessels_last_6_month,
            odds_past_9 = h_attacks_past_9/unique_hotspot_vessels_last_9_month,
            odds_past_12 = h_attacks_past_12/unique_hotspot_vessels_last_12_month)]


# Variation in the number of past attacks

tmp = melt(wdb[, c("hotspot", paste0("attacks_past_", c(3, 6, 9, 12)))],
           id = "hotspot", measure = patterns(f="^attacks_", attacks = "^attacks"),
           variable = "months_prior")
tmp[, f := NULL]
tmp = tmp[attacks!=0]

ggplot(tmp, aes(attacks, group = hotspot, col = hotspot, fill = hotspot)) +
  geom_density(alpha = 0.5) +
  scale_x_log10() +
  labs(x = "Log past attacks", y = "Density") +
  scale_colour_discrete(name = "Hotspot", aesthetics = c("colour", "fill")) +
  theme_minimal() +
  facet_wrap(~ paste("Past", months_prior, "month(s)"), scales = "free_y") +
  ggsave(here("Analysis-New/attack.png"), width = 10, height = 6)

# Manual version
# tmp = tmp[, .(dens = list(density(attacks))), by = .(hotspot, months_prior)]
# tmp = tmp[, .(x = dens[[1]]$x, y = dens[[1]]$y), by = .(hotspot, months_prior)]
# ggplot(tmp, aes(x, y, group = hotspot, col = hotspot, fill = hotspot)) +
#   geom_area(alpha = 0.5) +
#   scale_x_log10() +
#   labs(x = "Log past attacks", y = "Density") +
#   scale_colour_discrete(name = "Hotspot", aesthetics = c("colour", "fill")) +
#   theme_minimal() +
#   facet_wrap(~ paste("Months prior:", months_prior))

rm(tmp); gc()


# Regressions -------------------------------------------------------------

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

# * Main regs -------------------------------------------------------------

## We'll use fixest's multimodel features to run all the regressions in one call
tic = Sys.time()
mods = feols(c(distance, time, speed) ~ 
               wind + wind_vector +
               sw(attacks_past_3 + odds_past_3, 
                  attacks_past_6 + odds_past_6, 
                  attacks_past_9 + odds_past_9, 
                  attacks_past_12 + odds_past_12)
             | hotspot + vtype + size + country + month^year, 
             data = wdb,
             cluster = ~country + year,
             lean = TRUE, mem.clean = TRUE)
Sys.time()- tic
# Time difference of 5.782035 mins
gc()

## Quickly print them in separate tables by dependent variable
lapply(
  list(distance = 1:4, time = 5:8, speed = 9:12),
  function(i) {
    etable(mods[i], 
           drop = c("odds_past", "wind"),
           style.df = style.df(depvar.title = "",
                               fixef.title = " ",
                               fixef.suffix = " FE", 
                               yesNo = "X"),
           fitstat = ~ r2 + n)
  }
  )


# * Cost and pollution models ----------------------------------------------

## For these, we'll just use the 12-month prior attacks measure. But we will
## split the estimations by hotspot (incl. one on the full sample)
tic = Sys.time()
costpoll_mods =
  feols(
    c(total_fuel_cost, labor_cost, total_cost, total_co2, total_nox, total_sox) ~ 
      wind + wind_vector +
      attacks_past_12 + odds_past_12
    | hotspot + vtype + size + country + month^year,
    data = wdb,
    cluster = ~country + year,
    fsplit = ~hotspot,
    lean = TRUE, mem.clean = TRUE
    )
Sys.time() - tic
# Time difference of 7.277224 mins
gc()

## The resulting fixest multi object is grouped by sample split. E.g. The first
## few objects are all on the full sample, but differ by dep. variable. (The
## estimation works like this for efficiency reasons)
names(costpoll_mods)

## To view in the right groups --- i.e. by dep. var. --- we'll loop over 
## sequences.
nms = c('fuel_cost', 'labour_cost', 'total_cost', 'co2', 'nox', 'sox')
l = lapply(nms, function(n) seq(which(nms==n), which(nms==n)+(length(nms)*length(unique(wdb$hotspot))+1), length(nms)))
names(l) = nms
lapply(
  l,
  function(i) {
    etable(
      costpoll_mods[i], drop = c("odds_past_12", "wind", "wind_vector"),
      subtitles = names(costpoll_mods),
      style.df = style.df(depvar.title = "",
                          fixef.title = " ",
                          fixef.suffix = " FE", 
                          yesNo = "X"),
      fitstat = ~ r2 + n#,
      # signifCode = c("***"=0.001, "**"=0.01, "*"=0.05), tex = TRUE
      )
  }
)
rm(nms, l)


# Welfare -----------------------------------------------------------------


# Back of the envelope calculations

# Annual cost (Run cost model first!)

## GM: Renato, which model you're referring to here? Assuming the total cost
## one, it'd be something like:
## costpoll_mods$`Full sample`$total_cost$coefficients[['attacks_past_12']]

cost_coef = costpoll_mods$`Full sample`$total_cost$coefficients[['attacks_past_12']]

co2_coef = costpoll_mods$`Full sample`$total_co2$coefficients[['attacks_past_12']]

nox_coef = costpoll_mods$`Full sample`$total_nox$coefficients[['attacks_past_12']]

sox_coef = costpoll_mods$`Full sample`$total_sox$coefficients[['attacks_past_12']]

pcols = paste0("piracy_", c('cost', 'co2', 'nox', 'sox'))
wdb[, 
    (pcols) := 
      lapply(c(cost_coef, co2_coef, nox_coef, sox_coef), `*`, attacks_past_12)]

tot_cols = paste0("total_", c('cost', 'co2', 'nox', 'sox'))


## Summarise
piracy = 
  wdb[,
      lapply(.SD, function(x) sum(x)/1e6),
      by = year,
      .SDcols = c(pcols, tot_cols)]
piracy = melt(piracy, id = "year")
piracy[, c("type", "var") := tstrsplit(variable, "_")][]
piracy = dcast(piracy, year + var~ type, value.var = 'value')
piracy[, share := piracy/total*100][]

piracy[, .(mean_share = mean(share)), by = var]
