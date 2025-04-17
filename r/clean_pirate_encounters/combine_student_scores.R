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
  tidyverse
)

# Load data --------------------------------------------------------------------
asam_data <- st_read("data/Asam_data_download.gdb") |>
  st_drop_geometry()

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

master |> 
  filter(id %in% (summary |> filter(agree_keep == 1) |> pull(id))) |>
  rowwise() |> 
  mutate(A = min(intern_A, intern_B),
         B = max(intern_A, intern_B)) |> 
  count(A, B) |> 
  arrange(desc(n))

mixed_feelings <- master |> 
  filter(id %in% (summary |> filter(agree_keep == 1) |> pull(id))) |>
  pivot_longer(cols = c(intern_A, intern_B), names_to = "assig", values_to = "intern") |> 
  left_join(combined, by = c("id", "reference", "intern" = "src")) |> 
  select(-intern) |> 
  pivot_wider(names_from = assig, values_from = "keep") |> 
  mutate(verdict = "")


write_csv(x = mixed_feelings,
          file = here("data/intern_data/disagreements.csv"))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------


## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
