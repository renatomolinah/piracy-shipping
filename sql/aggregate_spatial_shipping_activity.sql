# Description:
# Pull aggregate spatial shipping stats for global map (Figure 3 of the paper)
SELECT
  lat_bin,
  lon_bin,
  SUM(hours) hours,
  SUM(distance_km) distance_km
FROM
  `emlab-gcp.piracy.gridded_data_0_5`
GROUP BY
  lat_bin,
  lon_bin