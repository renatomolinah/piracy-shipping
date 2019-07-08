bq_table(project = project,table = "piracy_attacks_bad",dataset = "piracy") %>% 
  bq_table_delete()
bq_table(project = project,table = "piracy_attacks_bad",dataset = "piracy") %>% 
  bq_table_upload(values = expanded_asam)
