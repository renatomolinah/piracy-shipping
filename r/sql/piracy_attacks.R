bq_table(project = project,table = glue::glue("piracy_attacks_",cell_size),dataset = "piracy") %>% 
  bq_table_delete()
bq_table(project = project,table = glue::glue("piracy_attacks_",cell_size),dataset = "piracy") %>% 
  bq_table_upload(values = expanded_asam)
