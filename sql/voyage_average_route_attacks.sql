WITH
  voyage_base AS(
  SELECT
    trip_id,
    departure_date,
    from_port,
    to_port
  FROM
    `emlab-gcp.ucsb_gfw_legacy_piracy.voyages_5`),
  attack_info AS(
  SELECT
    departure_date,
    from_port,
    to_port,
    attacks_window_last_12_month
  FROM
    `emlab-gcp.ucsb_gfw_legacy_piracy.voyages_5` ),
  full_data AS(
  SELECT
    *
  FROM
    voyage_base
  JOIN
    attack_info
  ON
    attack_info.departure_date < voyage_base.departure_date
    AND voyage_base.from_port = attack_info.from_port
    AND voyage_base.to_port = attack_info.to_port)
SELECT
  trip_id,
  IFNULL(AVG(attacks_window_last_12_month),0) route_average_attacks_per_trip_last_12_months
FROM
  full_data
GROUP BY
  trip_id