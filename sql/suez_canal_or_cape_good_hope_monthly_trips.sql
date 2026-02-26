SELECT
  TIMESTAMP_TRUNC(departure_timestamp, MONTH) departure_month,
  trip_location_flag,
  COUNT(DISTINCT trip_id) number_trips
FROM `emlab-gcp.piracy.{suez_canal_or_cape_good_hope_pings_table}`
GROUP BY
  departure_month,
  trip_location_flag