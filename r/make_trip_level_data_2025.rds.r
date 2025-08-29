new_trip_level <- tbl(src = piracy, "voyage_data_5_v_20250210") |> 
  collect()

saveRDS(new_trip_level, "tirp_level_data_2025.rds")
