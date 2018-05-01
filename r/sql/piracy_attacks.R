delete_table(project, "piracy", "piracy_attacks")
job <- insert_upload_job(project, "piracy", "piracy_attacks", expanded_asam)
wait_for(job)