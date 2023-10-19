# This function pulls the necessary GFW data and optionally saves it locally as a CSV
# This requires special permissions, and is also very expensive to run, so will not be done often
# You can add any additional arguments for glue to make substitutions to the query, as necessary
run_gfw_query <- function(query, bq_table_name, download_data = FALSE, ...){
  
  billing_project <- "emlab-gcp" # emLab's billing project
  bq_dataset <- "piracy" # The dataset name for this project
  query_path <- "sql" # Define directory where SQL queries live
  
  # Load query
  sql <- glue::glue("{query}") %>%
    readr::read_file() %>%
    # Then make any substitutions necessary
    glue::glue(...)


  # Run query and save on BQ. We don't pull this locally yet.
  bigrquery::bq_project_query(billing_project,
                   sql,
                   destination_table = bigrquery::bq_table(project = billing_project,
                                                table = bq_table_name,
                                                dataset = bq_dataset),
                   use_legacy_sql = FALSE,
                   allowLargeResults = TRUE,
                   write_disposition = "WRITE_TRUNCATE")

  # Now we download data locally, if desired
  if(download_data) return(bigrquery::bq_project_query(billing_project, 
                                                glue::glue("SELECT * FROM emlab-gcp.{bq_dataset}.{bq_table_name}")) %>%
    bigrquery::bq_table_download(n_max = Inf))
  
  # If data is not to be pulled locally, simply return current system time
  if(!download_data) return(Sys.time())
}
