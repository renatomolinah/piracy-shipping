SELECT
    lon_bin,
    lat_bin,
    from_port,
    from_country,
    to_port,
    to_country
  FROM
    `emlab-gcp.piracy.route_pixel_dates_v_20241105`
  GROUP BY
    lon_bin,
    lat_bin,
    from_port,
    from_country,
    to_port,
    to_country