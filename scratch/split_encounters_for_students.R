################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# jc_villasenor@miami.edu
# date
#
# Description
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  xlsx,
  tidyverse,
  sf
)

# Load data --------------------------------------------------------------------
# all_encounters <- st_read("/Users/jcvd/Library/CloudStorage/Box-Box/piracy-shipping/Analysis-New/ASAM_N_shp/") |>
#   st_drop_geometry() |>
#   filter(between(year(dateofocc), 2012, 2021)) |>
#   arrange(dateofocc)
#
# optB <- st_read("data/Asam_data_download_ASAM_shp_20250305") |>
#   st_drop_geometry() |>
#   filter(between(year(dateofocc), 2012, 2021)) |>
#   arrange(dateofocc)

asam_data <- st_read("data/Asam_data_download.gdb") |>
  st_drop_geometry()

# all_encounters |> filter(reference == "2015-174") |> pull(descriptio)
# optB |> filter(reference == "2015-174") |> pull(descriptio)
# optC |> filter(reference == "2015-174") |> pull(description)

interns <- c(
  "Sebastian",
  "Felipe",
  "Alejandro",
  "Ariella",
  "Jnesse"
)

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
set.seed(5)
subset <- asam_data |>
  mutate(dateofocc = with_tz(dateofocc, tzone = "UTC")) |> # Need to do this because the geodatabase is shifting the times to UTC and Jan 1 was appearing as Dec 31
  filter(between(year(dateofocc), 2012, 2023)) |>
  select(reference, description) |>
  distinct() |>
  mutate(id = 1:n(),
         intern_A = sample(interns, size = length(reference), replace = T)) |>
  rowwise() |>
  mutate(intern_B = sample(interns[!interns == intern_A], size = length(reference), replace = T)) |>
  select(id, reference, description, intern_A, intern_B)

subs <- function(subset, name) {
  subset |>
  filter(intern_A == name | intern_B == name) |>
  mutate(keep = NA,
         notes = NA) |>
    select(id, reference, description, keep, notes)
}

Sebastian <- subs(subset, "Sebastian")
Felipe <- subs(subset, "Felipe")
Alejandro <- subs(subset, "Alejandro")
Ariella <- subs(subset, "Ariella")
Jnesse <- subs(subset, "Jnesse")

## TESTS #######################################################################

# How many records are assigned twice to the same student?
length(subset$reference[subset$intern_A == subset$intern_B])

# The number of rows should be equal to the number of unique references for each person
check_repeated <- function(x) {
  dim(x)[1] == length(unique(x$reference))
}
check_repeated(Sebastian)
check_repeated(Felipe)
check_repeated(Alejandro)
check_repeated(Ariella)
check_repeated(Jnesse)

# The number of concatenated unique references should be the same as the number of rows in the original data
dim(subset)[1] == length(unique(c(Sebastian$reference, Felipe$reference, Alejandro$reference, Ariella$reference, Jnesse$reference)))

# Lets combine (duplicate) all the data. Each reference should appear only twice
combined <- bind_rows(Sebastian, Felipe, Alejandro, Ariella, Jnesse)
count(combined, reference) |>
  filter(!n == 2)
count(combined, id) |>
  filter(!n == 2)

# How many records assigned to each student?
A <- subset |>
  count(intern_A)
B <- subset |>
  count(intern_B)
left_join(A, B, by = join_by("intern_A" == "intern_B"), suffix = c("_A", "_B")) |>
  mutate(ntot = n_A + n_B)

## EXPORT ######################################################################
# X ----------------------------------------------------------------------------
write.xlsx(x = subset,
          file = here("data", "intern_data", "master.xlsx"))
write.xlsx(x = Sebastian,
          file = here("data", "intern_data", "Sebastian.xlsx"),
          showNA = F)
write.xlsx(x = Felipe,
          file = here("data", "intern_data", "Felipe.xlsx"),
          showNA = F)
write.xlsx(x = Alejandro,
          file = here("data", "intern_data", "Alejandro.xlsx"),
          showNA = F)
write.xlsx(x = Ariella,
          file = here("data", "intern_data", "Ariella.xlsx"),
          showNA = F)
write.xlsx(x = Jnesse,
          file = here("data", "intern_data", "Jnesse.xlsx"),
          showNA = F)
