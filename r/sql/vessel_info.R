library(mice)
# First, get all relevant vessel info
# Get all unique vessels that self-identify as cargo, tanker, reefer, passenger, tug, 
# bunker, specialized_reefer, fish_factory, supply_vessel
# For cargo, tanker, and reefer (main vessels of interest) also take combinations that have
# at least 50 vessels in the data set
sql <- "#standardSQL
SELECT
mmsi,
year,
mmsi_iso3 flag,
(CASE 
WHEN on_fishing_list_best THEN 'fishing'
WHEN known_geartype =  'cargo,cargo' THEN 'cargo'
WHEN known_geartype = 'cargo,non_fishing' THEN 'cargo'
WHEN known_geartype = 'non_fishing,cargo' THEN 'cargo'
WHEN known_geartype =  'cargo|other_not_fishing' THEN 'cargo'
WHEN known_geartype =  'cargo,other_not_fishing' THEN 'cargo'
WHEN known_geartype =  'non_fishing,tug' THEN 'tug'
WHEN known_geartype =  'tug,non_fishing' THEN 'tug'
WHEN known_geartype =  'tug,tug' THEN 'tug'
WHEN known_geartype =  'passenger,passenger' THEN 'passenger'
WHEN known_geartype =  'passenger,non_fishing' THEN 'passenger'
WHEN known_geartype =  'non_fishing,passenger' THEN 'passenger'
WHEN known_geartype =  'cargo,cargo' THEN 'cargo'
WHEN known_geartype =  'tanker,tanker' THEN 'tanker'
WHEN known_geartype =  'non_fishing,tanker' THEN 'tanker'
WHEN known_geartype =  'tanker,non_fishing' THEN 'tanker'
WHEN known_geartype = 'other_not_fishing|supply_vessel' THEN 'supply_vessel'
WHEN known_geartype = 'tanker,cargo' THEN 'cargo_or_tanker'
WHEN known_geartype = 'cargo,tanker' THEN 'cargo_or_tanker'
WHEN known_geartype = 'cargo_or_tanker,cargo' THEN 'cargo_or_tanker'
WHEN known_geartype = 'cargo,cargo_or_tanker' THEN 'cargo_or_tanker'
WHEN known_geartype = 'cargo|tanker' THEN 'cargo_or_tanker'
WHEN known_geartype = 'cargo|tanker,tanker' THEN 'cargo_or_tanker'
WHEN known_geartype = 'tanker,non_fishing,cargo' THEN 'cargo_or_tanker'
WHEN known_geartype = 'tanker,cargo|tanker' THEN 'cargo_or_tanker'
WHEN known_geartype = 'cargo,cargo|tanker' THEN 'cargo_or_tanker'
WHEN known_geartype = 'cargo|reefer' THEN 'cargo_or_reefer'
WHEN known_geartype = 'cargo,cargo|reefer' THEN 'cargo_or_reefer'
WHEN known_geartype = 'cargo|reefer,cargo' THEN 'cargo_or_reefer'
WHEN known_geartype = 'cargo|reefer,reefer' THEN 'cargo_or_reefer'
ELSE known_geartype
END) vessel_type,
known_length,
known_engine_power,
known_tonnage,
known_crew,
inferred_length_allyears,
inferred_engine_power_allyears,
inferred_crew_size_allyears,
inferred_tonnage_allyears
FROM
`world-fishing-827.gfw_research.vessel_info_20180518`
WHERE
known_geartype = 'cargo'
OR known_geartype = 'tanker'
OR known_geartype = 'supply_vessel'
OR known_geartype = 'tug'
OR known_geartype = 'passenger'
OR known_geartype = 'reefer'
OR known_geartype = 'specialized_reefer'
OR known_geartype = 'fish_factory'
OR known_geartype = 'bunker'
OR known_geartype =  'cargo,cargo'
OR known_geartype = 'cargo,non_fishing'
OR known_geartype = 'non_fishing,cargo'
OR known_geartype =  'cargo|other_not_fishing'
OR known_geartype =  'cargo,other_not_fishing'
OR known_geartype =  'non_fishing,tug'
OR known_geartype =  'tug,non_fishing'
OR known_geartype =  'tug,tug'
OR known_geartype =  'passenger,passenger'
OR known_geartype =  'passenger,non_fishing'
OR known_geartype =  'non_fishing,passenger'
OR known_geartype =  'cargo,cargo'
OR known_geartype =  'tanker,tanker'
OR known_geartype =  'non_fishing,tanker'
OR known_geartype =  'tanker,non_fishing'
OR known_geartype = 'other_not_fishing|supply_vessel'
OR known_geartype = 'tanker,cargo'
OR known_geartype = 'cargo,tanker'
OR known_geartype = 'cargo_or_tanker,cargo'
OR known_geartype = 'cargo,cargo_or_tanker'
OR known_geartype = 'cargo|tanker'
OR known_geartype = 'cargo|tanker,tanker'
OR known_geartype = 'tanker,non_fishing,cargo'
OR known_geartype = 'tanker,cargo|tanker'
OR known_geartype = 'cargo,cargo|tanker'
OR known_geartype = 'cargo|reefer'
OR known_geartype = 'cargo,cargo|reefer'
OR known_geartype = 'cargo|reefer,cargo'
OR known_geartype = 'cargo|reefer,reefer'
OR on_fishing_list_best"
# Delete old table
bq_table(project = project,table = "vessel_info",dataset = "piracy") %>% 
  bq_table_delete()
# Run new query. Just save results in temp spot on big query, then download locally
vessel_info <- bq_project_query(project, sql) %>%
  bq_table_download(max_results = Inf)

# Cache for later
#write_csv(vessel_info,"processed_data/vessel_info.csv")
#vessel_info <- read.csv("processed_data/vessel_info.csv",stringsAsFactors = F)

# Process vessel info. Partially based on Juan's high seas work
vessel_info_processed <- vessel_info %>%
  replace_na(list(flag = "unknown")) %>% 
  # Use known length where possible
  mutate(length = case_when(!is.na(known_length) ~ known_length,
                            TRUE ~ inferred_length_allyears),
         engine_power = case_when(!is.na(known_engine_power) ~ known_engine_power,
                                  TRUE ~ inferred_engine_power_allyears),
         crew = case_when(!is.na(known_crew) ~ known_crew,
                          TRUE ~inferred_crew_size_allyears),
         tonnage = case_when(!is.na(known_tonnage) ~ known_tonnage,
                             TRUE ~inferred_tonnage_allyears)) %>%
  dplyr::select(mmsi,year,flag,vessel_type,length,engine_power,crew,tonnage) %>%
  # For each flag and vessel group, assign mean values anywhere there's an NA
  group_by(flag,vessel_type) %>%
  mutate(length = replace_na(length,mean(length,na.rm=TRUE)),
         engine_power = replace_na(engine_power,mean(engine_power,na.rm=TRUE)),
         crew = replace_na(crew,mean(crew,na.rm=TRUE)),
         tonnage = replace_na(tonnage,mean(tonnage,na.rm=TRUE))) %>%
  ungroup() %>%
  # Remove any vessels that don't have all info. Still leaves you with 99.98% of vessels
  filter(!is.na(length) &
           !is.na(engine_power) &
           !is.na(crew) &
           !is.na(tonnage)) %>%
  # Now add engine info
  # Add aux power
  # From Bren GP, page 130
  # Based on linear regression of known vessels
  # https://www.bren.ucsb.edu/research/documents/whales_report.pdf
  mutate(aux_engine_power = 0.1913 * engine_power + 287.2) %>%
  # Add design speed
  # From Bren GP, page 131
  # Based on linear regression of known vessels
  # https://www.bren.ucsb.edu/research/documents/whales_report.pdf
  mutate(design_speed = 3.39*10^(-4)*engine_power+2.151*10^(-5)*tonnage-2.742*10^(-9)*engine_power*tonnage+12.93) %>%
  # Add SFC
  # From Bren GP, page 65
  # https://www.bren.ucsb.edu/research/documents/whales_report.pdf
  # Assume use of IFO 380
  mutate(
    main_sfc = 195,
    aux_sfc = 227
  )

# Upload to big query
bq_table(project = project,table = "vessel_info",dataset = "piracy") %>% 
  bq_table_delete()
bq_table(project = project,table = "vessel_info",dataset = "piracy") %>% 
  bq_table_upload(values = vessel_info_processed)
