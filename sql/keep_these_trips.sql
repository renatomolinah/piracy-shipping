SELECT
  trip_id
FROM
  `emlab-gcp.piracy.{trip_summary_stats_for_filtering_table}`
WHERE
  # Remove trips that have 0 distance
  distance_km > 0
  # Remove trips that have 0 hours
  AND hours > 0
  # Remove trips longer than 60 days
  AND (hours / 24 <= 60)
  # Remove trips with a distance greater than the earth's circumference
  # Circumfrence measured at equato (source: https://en.wikipedia.org/wiki/Earth%27s_circumference)
  AND (distance_km <= 40075.017)
  # Remove trips have a distance greater than 4x the mean distance for that port-to-port route
  AND distance_km <= average_route_distance_km * 4
  # Remove trips where the total measured distance from AIS pings is less than the
  # haversine distance +7km between the starting and ending anchorage. 
  # Anything less than this is indicative of low ping count which doesn't fully capture voyage trajectory
  # We add 7km to allow for how GFW defines port visits and trips.
  # According to their documentation https://globalfishingwatch.org/datasets-and-code-anchorages/
  # "A vessel enters port when it comes within 3 kilometers of an anchorage point and exits port when it is outside 4 kilometers of the anchorage point."
  # So that means a trip doesn't actually start until it is 4km away from the departure anchorage; and it ends when it reach 3km away from the arrival anchorage.
  AND distance_km + 7 >= total_haversine_distance_km
  # Remove trips that have extremely low ping levels. Since we're using 5x5 degree pixels for our
  # voyage-levle analysis, we want to ensure on average at least 1 ping per pixel for each trip,
  # in order to determine attacks within each pixel voyage passes throough.
  # 5 degrees 110.57 km at the equator, so 5 degree pixels is 110.57 * 5 = 552.85 km
  # So we want 1 ping per 552.85 km, which 1/552.85 = 0.001808 pings per km
  # Use haversine distance as a more accurate measure of distance than distance_km, 
  # since distance_km is calculated using the pings we have, and if we have very low ping levels, distance_km may be much lower than the true distance traveled
  AND number_of_pings / total_haversine_distance_km >= (1 / (5 * 110.57))