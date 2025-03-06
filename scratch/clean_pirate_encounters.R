################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# jc_villasenor@miami.edu
# date
#
# Cleans up our attack data base by removing things that are not relevant to our case
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  tidyverse,
  sf
)

# Load data --------------------------------------------------------------------
all_encounters <- st_read("/Users/jcvd/Library/CloudStorage/Box-Box/piracy-shipping/Analysis-New/ASAM_N_shp") |>
  st_drop_geometry() |>
  filter(between(year(dateofocc), 2012, 2021))

# A quick function to table unique values
get_values <- function(data, var) {
  data |>
    pull(var = var) |>
    str_to_upper() |>
    str_squish() |>
    str_trim() |>
    table() |>
    data.frame() |>
    arrange(desc(Freq))
}

## PROCESSING ##################################################################

# Find unique values -----------------------------------------------------------
# get_values(all_encounters, "hostility_") # To find all types of hostiles
# get_values(all_encounters, "hostilit_D") # To find all types of hostilities
# Take a list of descriptions, pass it to GPT and asked for a list of nouns, verbs, and adverbs associated with violecne and piracyv

# Build vectors of unique values to keep ---------------------------------------
# List of hostiles
hostiles <- c("PIRATE",
              "PIRATES",
              "ROBBER",
              "ROBBERS",
              "THIEF",
              "THIEVES",
              "BOARDING",
              "BOARDER",
              "BOARDERS",
              "ROBBERY",
              "INTRUDERS",
              "ATTACK",
              "ATTACKER",
              "ATTACKERS",
              "HIJACKER",
              "HIJACKERS",
              "HIJACKING",
              "KIDNAPPERS",
              "ARMED",
              "GUNMEN",
              "BANDITS",
              "GUERRILLAS",
              "STOWAWAY",
              "STOWAWAYS",
              "REBEL",
              "REBELS",
              "TERRORIST",
              "TERRORISTS",
              # Transliterations
              "PRIATES",
              "ROBBER(S)"
              )

# Types of hostilities
hostilit_Ds <- c("PIRATE ASAULT",
                "ROBBERY",
                "KIDNAPPING",
                "ATTEMPTED BOARDING",
                "HIJACKING",
                "MOTHERSHIP ACTIVITY",
                "SUSPICIOUS APPROACH")

# Nouns from GTP
nouns <- c("Alarm",
           "Assault",
           "Attack",
           "Gunpoint",
           "Guns",
           "Hijacking",
           "Hostage",
           "Kidnapping",
           "Knife",
           "Knives",
           "Lock",
           "Mothership",
           "Pirate",
           "Pirates",
           "Robbers",
           "Robbery") |>
  str_to_upper()

# Verbs from HPT
verbs <- c("Attack",
           "Attacked",
           "Boarded",
           "Detain",
           "Detained",
           "Escape",
           "Escaped",
           "Fled",
           "Kill",
           "Killed",
           "Murder",
           "Murdered",
           "Punch",
           "Punched",
           "Robb",
           "Robbed",
           "Steal",
           "Stole",
           "Threaten",
           "Threatened",
           "Tie",
           "Tied") |>
  str_to_upper()

# Adverbs from GPT
adverbs <- c("Heavily",
             "Violently") |>
  str_to_upper()

keywords <- c(nouns, verbs, adverbs) |>
  unique()

# Now vectors of unique values to exclude --------------------------------------
exclude_hostility_ <- c(
  "MILITARY",
  "ASSAULT",
  "DRUG SMUGGLING",
  "MISSILE ATTACK",
  "EXPLOSION",
  "DETAINED"
)

exclude_keywords <- c(
  "SUSPICIOUS APPROACH",
  "IN THE PORT",
  "IN THE DOCK",
  "IN THE PIER",
  "ON THE PORT",
  "ON THE DOCK",
  "ON THE PIER",
  "AT THE PORT",
  "AT THE DOCK",
  "AT THE PIER",
  "YARD",
  "BERTHED",
  "DOCKED",
  "BARGE",
  "BARGES",
  "YACHT",
  "YACHTS",
  "SAILING YACHT",
  "SAILING",
  "SAILBOAT",
  "SAILING VESSEL",
  "SAILBOATS")

# Implement filters ------------------------------------------------------------
clean_encounters <- all_encounters |>
  mutate_all(str_to_upper) |>
  # KEEP THESE
  filter(str_detect(hostility_, paste(hostiles, collapse = "|")) |
           str_detect(hostilit_D, paste(hostilit_Ds, collapse = "|")) |
           str_detect(descriptio, paste(keywords, collapse = "|"))) |>
  # EXCLUDE THESE
  filter(!str_detect(hostility_, paste(exclude_hostility_, collapse = "|")),
         !str_detect(descriptio, paste(exclude_keywords, collapse = "|")),
         !hostilityt == 0) |>
  drop_na(victim_d)


# How many did we remove?
dim(all_encounters)[1] - dim(clean_encounters)[1]

get_values(clean_encounters, "hostility_")
get_values(clean_encounters, "hostilit_D")


## EXPORT ######################################################################

# Export the file here ---------------------------------------------------------
# Wasn't sure if needed as rds, csv, shapefile or geojson to put in Bigquery?



