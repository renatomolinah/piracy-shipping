SELECT
  lat_bin,
  lon_bin,
  SUM(hours) hours,
  SUM(distance_km) distance_km,
  3.17 * SUM(main_fuel_consumption_mt_inst + aux_fuel_consumption_mt_inst) emissions_co2_mt
FROM
  `emlab-gcp.piracy.gridded_data_0_5`
GROUP BY
  lat_bin,
  lon_bin