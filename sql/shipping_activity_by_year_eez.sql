#standardSQL
WITH
  summary_by_eez AS (
  SELECT
    year,
    CAST(value AS int64) AS eez_id,
    SUM(e.hours) hours
  FROM
    `world-fishing-827.gfw_research.vi_ssvid_byyear_v20210913`
  LEFT JOIN
    UNNEST(activity.eez) AS e
  WHERE
    NOT activity.offsetting
    AND activity.overlap_hours_multinames = 0
    AND year < 2018
    AND best.best_vessel_class IN('cargo',
      'cargo_or_tanker',
      'tanker',
      'cargo_or_reefer',
      'specialized_reefer',
      'container_reefer',
      'reefer')
  GROUP BY
    eez_id,
    year),
  eez_info AS(
  SELECT
    eez_id,
    sovereign1_iso3 iso3
  FROM
    `world-fishing-827.gfw_research.eez_info` )
SELECT
  year,
  iso3,
  hours
FROM
  summary_by_eez
LEFT JOIN
  eez_info
USING
  (eez_id)