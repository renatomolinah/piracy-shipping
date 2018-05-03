# gridded_shipping_hours
# Data for shipping map
sql <-
  "
SELECT
YEAR(start_timestamp) year,
vessel_type,
FLOOR(start_lat / 0.5) * 0.5 lat_bin,
FLOOR(start_lon / 0.5) * 0.5 lon_bin,
SUM(hours) hours
FROM
[ucsb-gfw:piracy.all_cargo_AIS_positions_with_ports]
GROUP BY
year,
vessel_type,
lat_bin,
lon_bin"

bq_table(project = project,table = "gridded_shipping_hours",dataset = "piracy") %>% 
  bq_table_delete()
bq_table(project = project,table = "gridded_shipping_hours",dataset = "piracy") %>% 
  bq_table_upload(values = gridded_shipping_hours)

# Arrange and cache data for later
# Make smaller, to 1 degree cells
gridded_shipping_hours_processed <- gridded_shipping_hours %>%
  #  mutate(lat_bin = floor(lat_bin),
  #         lon_bin = floor(lon_bin)) %>%
  filter(lat_bin < 90 &
           lat_bin > -90 &
           lon_bin < 180 &
           lon_bin > - 180) %>%
  group_by(year,lat_bin,lon_bin) %>%
  summarize(hours = sum(hours,na.rm=TRUE))

write_csv(gridded_shipping_hours_processed,"../processed_data/gridded_shipping_hours_processed.csv")