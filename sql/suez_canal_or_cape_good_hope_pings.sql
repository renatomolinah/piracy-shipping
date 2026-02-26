WITH
  -- Get all ping data. Only need a few columns for mapping
  all_ping_data AS (
    SELECT
      mmsi,
      trip_id,
      lon,
      lat,
      timestamp,
      hours,
      distance_km,
      -- Classify pings as being either through the suez canal, around the cape of good hope, or neither
      CASE
        WHEN
          (lon >= 32 AND lon <= 33 AND lat >= 29 AND lat <= 32)
          THEN 'suez_canal'
        WHEN
          (
            lon >= 17
            AND lon <= 21
            AND lat >= -60
            AND lat <= -33)
          THEN 'cape_good_hope'
          ELSE NULL
        END location
    FROM `emlab-gcp.piracy.{ungridded_data_table}`
  ),
  -- Now, subset to only trips that went through the suez canal or around the cape of good hope
  trips_through_suez_canal_or_cape_good_hope AS(
    SELECT
    trip_id,
    location trip_location_flag,
    FROM  all_ping_data
    WHERE NOT location IS NULL
    GROUP BY trip_id, location
  ),
  -- Now, subset our pings to just those trips, so that we have pings from those entire trips
  all_pings_on_trips_through_suez_canal_or_cape_good_hope AS(
    SELECT
      *
    FROM
      all_ping_data
    JOIN
      trips_through_suez_canal_or_cape_good_hope
      USING(trip_id)
  ),
  -- Get the from and to country for each trip
  voyage_info AS (
    SELECT
      trip_id,
      from_country,
      to_country,
      departure_timestamp
    FROM
      `emlab-gcp.piracy.{voyage_info_table}`
  )
-- Put it all together
SELECT
  *
FROM
  all_pings_on_trips_through_suez_canal_or_cape_good_hope
JOIN
  voyage_info
  USING (trip_id)
