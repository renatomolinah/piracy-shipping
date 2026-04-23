SELECT
  mmsi,
  trip_id,
  DATE(departure_timestamp) departure_date,
  trip_location_flag,
  from_country,
  to_country,
  SUM(hours) hours,
  SUM(distance_km) distance_km
FROM `emlab-gcp.piracy.{suez_canal_or_cape_good_hope_pings_table}`
GROUP BY
  mmsi,
  trip_id,
  departure_date,
  trip_location_flag,
  from_country,
  to_country