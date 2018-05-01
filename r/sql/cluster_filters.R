delete_table(project, "piracy", "cluster_filters")
job <- insert_upload_job(project, "piracy", "cluster_filters", cluster_filters)
wait_for(job)