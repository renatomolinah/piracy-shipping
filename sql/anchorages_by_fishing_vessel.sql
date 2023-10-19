WITH
  ------------------------------------------------------------
  -- Port Events Source data
  -- adjust the time range here
  ------------------------------------------------------------
  port_events AS (
  SELECT
    visit_id,
    ssvid,
    e.anchorage_id AS anchorage_id,
    EXTRACT(YEAR
    FROM
      e.timestamp) year
  FROM
    `world-fishing-827.pipe_production_v20201001.proto_port_visits`
  LEFT JOIN
    UNNEST (events) e
  WHERE
    start_timestamp BETWEEN TIMESTAMP("2016-01-01")
    AND TIMESTAMP("2022-12-31")
    AND (event_type = 'PORT_ENTRY'
      OR event_type = 'PORT_EXIT')
    AND confidence>=4),
  anchorage_info AS(
  SELECT
    s2id anchorage_id,
    iso3,
    lon,
    lat,
  FROM
    `world-fishing-827.gfw_research.named_anchorages` ),
  ####################################################################
  # Get the list of active fishing vessels that pass the noise filters
  fishing_vessels AS (
  SELECT
    ssvid,
    year
  FROM
    `world-fishing-827.gfw_research.fishing_vessels_ssvid_v20220601`),
  anchorage_events_by_fishing_vessels AS (
  SELECT
    *
  FROM
    port_events
  JOIN
    anchorage_info
  USING
    (anchorage_id)
  JOIN
    fishing_vessels
  USING
    (ssvid,
      year))
SELECT
  anchorage_id,
  lon,
  lat,
  iso3,
  COUNT(*) number_fishing_vessel_visits
FROM
  anchorage_events_by_fishing_vessels
GROUP BY
  anchorage_id,
  lon,
  lat,
  iso3