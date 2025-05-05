################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# jc_villasenor@miami.edu
# date
#
# Combine pirate encouner data after it was scored by students
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  readxl,
  sf,
  tidyverse
)

# Load data --------------------------------------------------------------------
asam_data <- st_read("data/Asam_data_download.gdb")

master <- read_excel(here("data/intern_data/master.xlsx"))

alejandro <- read_excel(here("data/intern_data/Alejandro_filled_RM.xlsx")) |> 
  mutate(keep = case_when(is.na(keep) ~ "exclude",
                          keep == "x" ~ "keep"),
         src = "Alejandro")

ariella <- read_excel(here("data/intern_data/Ariella_filled_RM.xlsx")) |> 
  mutate(keep = str_to_lower(keep),
         keep = ifelse(keep == "exlcude", "exclude", keep)) |> 
  mutate(src = "Ariella")
  
felipe <- read_excel(here("data/intern_data/Felipe_filled_RM.xlsx")) |> 
  replace_na(replace = list(keep = "exclude")) |> 
  mutate(keep = str_to_lower(keep),
         src = "Felipe")

jnesse <- read_excel(here("data/intern_data/jnesse_filled.xlsx")) |> 
  mutate(keep = str_to_lower(keep),
         src = "Jnesse")

sebastian <- read_excel(here("data/intern_data/Sebastian_filled.xlsx")) |> 
  mutate(keep = ifelse(keep == "-", "exclude", keep),
         src = "Sebastian")

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
combined <- bind_rows(alejandro,
                      ariella,
                      felipe,
                      jnesse,
                      sebastian) |> 
  select(id, reference, src, keep)

summary <- combined |> 
  mutate(choice = 1) |> 
  pivot_wider(names_from = keep, values_from = choice) |> 
  arrange(id) |> 
  group_by(id, reference) |> 
  summarize(agree_keep = sum(keep, na.rm = T),
            .groups = "drop")


# The data are in three categories:
# unanimous keep (2,476 obs)
# unanimous exclude (403 obs)
# mixed feelings (442 obs)
count(summary, agree_keep)

# The ones to keep
keep <- master |> 
  filter(id %in% (summary |> filter(agree_keep == 2) |> pull(id)))
# dim(keep) # should match the table above for agree_keep == 2

exclude <- master |> 
  filter(id %in% (summary |> filter(agree_keep == 0) |> pull(id)))
# dim(exclude) # should match the table above for agree_keep == 0

mixed_feelings <- master |> 
  filter(id %in% (summary |> filter(agree_keep == 1) |> pull(id)))
# dim(mixed_feelings) # should match the table above for agree_keep == 1

mixed_feelings_export <- mixed_feelings |>
  pivot_longer(cols = c(intern_A, intern_B), names_to = "assig", values_to = "intern") |> 
  left_join(combined, by = c("id", "reference", "intern" = "src")) |> 
  select(-intern) |> 
  pivot_wider(names_from = assig, values_from = "keep") |> 
  mutate(verdict = "")

# Export
write_csv(x = mixed_feelings_export,
          file = here("data/intern_data/disagreements.csv"))


# A final classification was performed by Ariella on April 25, 2025
# The file is now read in below
final_verdict <- read_csv(here("data/intern_data/disagreements_verdict.csv")) |> 
  select(id, reference, verdict)

# This second round confirmed the exclusion of 307 / 442 observations,
# and the inclusion of 135 / 442
count(final_verdict, verdict)

mixed_feelings_keep <- mixed_feelings |> 
  filter(id %in% (final_verdict |> filter(verdict == "keep") |> pull(id)))
# dim(mixed_feelings_keep) # should match the table above for verdict == "keep"

mixed_feelings_exclude <- mixed_feelings |> 
  filter(id %in% (final_verdict |> filter(verdict == "exclude") |> pull(id)))
# dim(mixed_feelings_exclude) # should match the table above for verdict == "keep"


## BRING IT TOGETHER ###########################################################

# X ----------------------------------------------------------------------------
final_sample <- bind_rows(keep, mixed_feelings_keep)

final_sample_sf <- asam_data |>
  mutate(dateofocc = with_tz(dateofocc, tzone = "UTC")) |> # Need to do this because the geodatabase is shifting the times to UTC and Jan 1 was appearing as Dec 31
  filter(between(year(dateofocc), 2012, 2023)) |>
  filter(reference %in% unique(final_sample$reference))
  


## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
