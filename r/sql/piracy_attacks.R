bq_table(project = project,table = "piracy_attacks",dataset = "piracy") %>% 
  bq_table_delete()
bq_table(project = project,table = "piracy_attacks",dataset = "piracy") %>% 
  bq_table_upload(values = expanded_asam)
