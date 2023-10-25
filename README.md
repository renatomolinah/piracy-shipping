# piracy-shipping

A draft working copy of the manuscript can be found here: [The Economic and Environmental Impact of Modern Piracy on Global Shipping](https://renatomolinah.com/assets/docs/piracy.pdf).

# Reproducibility  

## Package management  

To manage package dependencies, we use the `renv` package. When you first clone this repo onto your machine, run `renv::restore()` to ensure you have all correct package versions installed in the project. Please see the [renv website](https://rstudio.github.io/renv/articles/renv.html) for more information.

## Data processing and analysis pipeline

To ensure reproducibility in the data processing and analysis pipeline, we use the `targets` package. Targets is a Make-like pipeline tool. Using targets means that anytime an upstream change is made to the data or models, all downstream components of the data processing and analysis will be re-run automatically when the `targets::tar_make()` command is run. It also means that once components of the analysis have already been run and are up-to-date, they will not need to be re-run. All objects are cached in a `_targets` directory. Please see the [targets website](https://github.com/ropensci/targets) for more information.

For targets to function correctly, the user only needs to make one change to update the cache directory. In the file `_targets.R`, change the `data_directory` object to match the location location for the `piracy/data` folder on the shared folder on the emLab Shared Google Drive. Then run `tar_config_set(store = glue::glue("{data_directory}/_targets"))` to set the cache directory to that location. Setting this data directory will also provide the necessary pointer to the raw data, which are also current stored on the Shared Drive.

Once this has been done, you can simply run `targets::tar_make()` to reproduce the analysis. 

In order to see what the targets pipeline looks like, you can run `targets::tar_manifest()` or `targets::tar_visnetwork()`, which also shows which targets are current or out-of-date. In overview, the pipeline:

1. Processes the raw piracy attack, wind, and fuel data in R. Note that these raw data are currently stored on the emLab Shared Drive. The wind data in particular are too large for GitHub. We may eventually need to figure out a different place to store these.    
2. Using those processed data, GFW queries are performed to generated ungridded, gridded, and voyage-level versions of the dataset with all necessary piracy attack, wind, and fuel columns. Note that this step requires special permissions to acces the GFW data on Google BigQuery. I've included this in the pipeline for now, but we may eventually need to somehow take this out of the pipeline so that the analysis can be fully reproducible by others.  
3. Performs analyses on those data.  
4. Generates final figures and tables.  

When you run `targets::tar_visnetwork`, it should look something like this:

![](target_network.png)

# Repository Structure 

The repository uses the following basic structure:  
```
piracy-shipping
  |__ figures
  |__ r
  |__ renv
  |__ sql
  |__ tables
```