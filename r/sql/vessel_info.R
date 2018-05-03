# First, get all relevant vessel info
sql <- "SELECT
mmsi,
year,
iso3 flag,
(CASE 
WHEN on_fishing_list THEN 'fishing'
WHEN known_geartype =  'cargo,cargo' THEN 'cargo'
WHEN known_geartype =  'tug,tug' THEN 'tug'
WHEN known_geartype =  'passenger,passenger' THEN 'passenger'
WHEN known_geartype =  'cargo,cargo' THEN 'cargo'
WHEN known_geartype =  'tanker,tanker' THEN 'tanker'
ELSE known_geartype
END) vessel_type,
known_length,
known_engine_power,
known_crew,
inferred_length_allyears,
inferred_engine_power_allyears,
inferred_crew_size_allyears,
inferred_tonnage_allyears
FROM
[world-fishing-827.gfw_research.vessel_info_20180418]
WHERE
known_geartype = 'cargo'
OR known_geartype = 'cargo,cargo'
OR known_geartype = 'tanker'
OR known_geartype = 'tanker,tanker'
OR known_geartype = 'supply_vessel'
OR known_geartype = 'tug'
OR known_geartype = 'tug,tug'
OR known_geartype = 'passenger'
OR known_geartype = 'passenger,passenger'
OR known_geartype = 'reefer'
OR known_geartype = 'specialized_reefer'
OR known_geartype = 'fish_factory'
OR known_geartype = 'bunker'
OR known_geartype = 'bunker_or_tanker'
OR on_fishing_list"

bq_table(project = project,table = "vessel_info",dataset = "piracy") %>% 
  bq_table_delete()
vessel_info <- bq_dataset_query(project,query = sql, destination_table = NULL, use_legacy_sql = TRUE, allowLargeResults = TRUE)

# To fill in gaps in vessel info, figure out mean length, power, crew, and tonnage by flag, year, and vessel type using inferred data
vessel_gaps <- vessel_info %>%
  replace_na(list(flag = "unknown")) %>% 
  group_by(flag,year,vessel_type) %>%
  summarize(mean_inferred_length_allyears = mean(inferred_length_allyears,na.rm = TRUE),
            mean_inferred_engine_power_allyears = mean(inferred_engine_power_allyears,na.rm = TRUE),
            mean_inferred_crew_size_allyears = mean(inferred_crew_size_allyears,na.rm = TRUE),
            mean_inferred_tonnage_allyears = mean(inferred_tonnage_allyear,na.rm = TRUE))

vessel_info <- vessel_info %>%
  replace_na(list(flag = "unknown")) %>% 
  # Add gaps. Use known or non-gap data if available, gap data if necessary
  left_join(vessel_gaps, by = c("flag","year","vessel_type")) %>%
  mutate(length = case_when(!is.na(known_length) ~ known_length,
                            !is.na(inferred_length_allyears) ~inferred_length_allyears,
                            TRUE ~ mean_inferred_length_allyears),
         engine_power = case_when(!is.na(known_engine_power) ~ known_engine_power,
                                  !is.na(inferred_engine_power_allyears) ~inferred_engine_power_allyears,
                                  TRUE ~ mean_inferred_engine_power_allyears),
         crew = case_when(!is.na(known_crew) ~ known_crew,
                          !is.na(inferred_crew_size_allyears) ~inferred_crew_size_allyears,
                          TRUE ~ mean_inferred_crew_size_allyears),
         tonnage = case_when(!is.na(inferred_tonnage_allyears) ~inferred_tonnage_allyears,
                             TRUE ~ mean_inferred_tonnage_allyears)) %>%
  # Add aux power
  mutate(aux_engine_power = 0.25*engine_power) %>%
  # Add design speed
  mutate(design_speed_old = 3.30*10^(-4)*engine_power+2.151*10^(-5)*tonnage-2.742*10^(-9)*engine_power*tonnage+12.93,
         design_speed_ihs = 10.4818 + 0.0012*engine_power -3.84710*10^(-8)*engine_power^2) %>%
  # Add SFC
  mutate(
    main_sfc = case_when(
      flag == "CHN" ~ 280,
      flag %in% c("AUT", "BEL", "DNK", "FIN", "FRA",
                  "DEU", "GRC", "IRL", "ITA", "LUX",
                  "NLD", "PRT", "ESP", "SWE", "GBR") ~ 270,
      flag == "ISL" ~ 250,
      flag == "NOR" ~ 250,
      flag == "KOR" ~ 260,
      flag == "RUS" ~ 250,
      TRUE ~ 203),
    main_sfc_low = 203,
    aux_sfc = 217
  )

bq_table(project = project,table = "vessel_info",dataset = "piracy") %>% 
  bq_table_delete()
bq_perform_query(project,query = sql, destination_table = "vessel_info", use_legacy_sql = FALSE, allowLargeResults = TRUE) %>%
  bq_job_wait()