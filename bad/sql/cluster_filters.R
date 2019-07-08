bq_table(project = project,table = "cluster_filters_bad",dataset = "piracy") %>% 
  bq_table_delete()
bq_table(project = project,table = "cluster_filters_bad",dataset = "piracy") %>% 
  bq_table_upload(values = cluster_filters)
