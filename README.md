# piracy-shipping

A draft working copy of the manuscript can be found here: [The Economic and Environmental Impact of Modern Piracy on Global Shipping](https://renatomolinah.com/assets/docs/piracy.pdf).

# Reproducibility  

## Package management  

To manage package dependencies, we use the `renv` package. When you first clone this repo onto your machine, run `renv::restore()` to ensure you have all correct package versions installed in the project. Please see the [renv website](https://rstudio.github.io/renv/articles/renv.html) for more information.

## Data processing and analysis pipeline

To ensure reproducibility in the data processing and analysis pipeline, we use the `targets` package. Targets is a Make-like pipeline tool. Using targets means that anytime an upstream change is made to the data or models, all downstream components of the data processing and analysis will be re-run automatically when the `targets::tar_make()` command is run. It also means that once components of the analysis have already been run and are up-to-date, they will not need to be re-run. All objects are cached in a `_targets` directory. Please see the [targets website](https://github.com/ropensci/targets) for more information.

For targets to function correctly, the user only needs to make one change to update the cache directory. First run `targets::tar_edit()`, which will open the `_targets.R` file for editing. Next change the path stored to `data_directory` to a local directory path where you wish to store the interim `targets` files.  Once this has been done, you can simply run `targets::tar_make()` to reproduce the analysis. 

In order to see what the targets pipeline looks like, you can run `targets::tar_manifest()` or `targets::tar_visnetwork()`, which also shows which targets are current or out-of-date. In overview, the pipeline: 1) processes the raw data; 2) performs analyses on those data; 3) generates final figures and tables.

# Repository Structure 

The repository uses the following basic structure:  
```
piracy-shipping
  |__ data
      |__ processed
      |__ raw
  |__ figures
  |__ r
  |__ renv
  |__ tables
```