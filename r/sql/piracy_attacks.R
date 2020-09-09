bq_table(project = project,table = glue::glue("piracy_attacks_",cell_size),dataset = "piracy") %>% 
  bq_table_upload(values = expanded_asam,
                  fields = as_bq_fields(expanded_asam),
                  write_disposition = "WRITE_TRUNCATE")


