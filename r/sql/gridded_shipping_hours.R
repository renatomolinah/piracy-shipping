# gridded_shipping_hours
# Data for shipping map
sql <-
  "
#standardSQL
SELECT
EXTRACT(year FROM departure_timestamp) year,
FLOOR(start_lat / 0.5) * 0.5 lat_bin,
FLOOR(start_lon / 0.5) * 0.5 lon_bin,
SUM(hours) hours
FROM
`piracy.voyage_ais_positions`
GROUP BY
year,
lat_bin,
lon_bin"

bq_table(project = project,table = "gridded_shipping_hours",dataset = "piracy") %>% 
  bq_table_delete()

bq_project_query(project,query = sql, destination_table = bq_table(project = project,table = "gridded_shipping_hours",dataset = "piracy"),
                 allowLargeResults = TRUE)

gridded_shipping_hours <- bq_project_query(project, "SELECT * FROM `piracy.gridded_shipping_hours`") %>%
  bq_table_download(max_results = Inf)

# Arrange and cache data for later
# Make smaller, to 1 degree cells
gridded_shipping_hours_processed <- gridded_shipping_hours %>%
  filter(lat_bin < 90 &
           lat_bin > -90 &
           lon_bin < 180 &
           lon_bin > - 180) %>%
  group_by(year,lat_bin,lon_bin) %>%
  summarize(hours = sum(hours,na.rm=TRUE)) %>%
  filter(!is.na(year))

write_csv(gridded_shipping_hours_processed,"processed_data/gridded_shipping_hours_processed.csv")
