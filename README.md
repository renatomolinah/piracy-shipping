# piracy-shipping

Replication code for **The Economic and Environmental Impact of Modern Piracy on Global Shipping**.

A draft working copy of the manuscript is available here: [The Economic and Environmental Impact of Modern Piracy on Global Shipping](https://renatomolinah.com/assets/docs/piracy.pdf).

## Overview

The pipeline links piracy-attack records with vessel-tracking, wind, and fuel data to estimate how modern piracy affects shipping routes, voyage costs, and emissions. It then constructs counterfactual maps and tables. Analysis is implemented in R, with data-preparation queries in BigQuery SQL.

## Repository structure

```
piracy-shipping
  |__ code      # Numbered analysis scripts (0.x -> 4.x); run in order
  |__ r         # Helper functions and data-processing utilities
  |__ sql       # BigQuery SQL for voyage- and grid-cell-level datasets
  |__ figures   # Static figures used in the manuscript
  |__ results   # Generated figures and tables
  |__ renv      # renv package management
  |__ data      # Local data store (gitignored; not distributed in this repo)
```

The `code/` directory uses a numbered naming convention indicating execution order:

- `0.x` — data setup
- `1.x` — summary statistics
- `2.x` — grid-cell analysis (event study, `did_multiplegt_dyn`)
- `3.x` — voyage-level main estimation and robustness
- `4.x` — cost and emissions regressions, counterfactual maps and tables

## Reproducibility

### Software environment

Package dependencies are managed with [`renv`](https://rstudio.github.io/renv/articles/renv.html). After cloning the repository, run `renv::restore()` to install the recorded package versions into the project library.

### Analysis pipeline

The data-processing and analysis pipeline is orchestrated with the [`targets`](https://docs.ropensci.org/targets/) package, a Make-like tool that re-runs only the parts of the pipeline affected by upstream changes. Run `targets::tar_make()` to execute the pipeline, and `targets::tar_manifest()` or `targets::tar_visnetwork()` to inspect its structure and status.

Before the first run, point `targets` at a local cache directory with `targets::tar_config_set(store = "<path>/data/_targets")`. The same data directory provides the pointer to the input data.

In overview, the pipeline:

1. Processes the raw piracy-attack, wind, and fuel data in R.
2. Runs BigQuery SQL queries to build ungridded, gridded, and voyage-level datasets with the piracy-attack, wind, and fuel covariates. Interim datasets (e.g., the `0.5°` gridded dataset) are materialized in BigQuery; the voyage-level dataset is pulled locally.
3. Estimates the grid-cell and voyage-level models, including robustness checks.
4. Generates the final figures and tables.

### Data availability

Some of the input data are **proprietary** and cannot be redistributed in this repository. In particular, the vessel-tracking and fuel data are accessed under data-use agreements and require separate credentials; the wind data are too large to host on GitHub. Scripts that depend on restricted inputs note this at the top of the file.

A self-contained replication package that bundles the processed, redistributable data is in preparation. We are currently securing a hosting service for it; this README will be updated with access instructions once that is finalized. In the meantime, researchers who need access to the restricted inputs for replication should contact the corresponding author of the manuscript.

## License

The code in this repository is released under the MIT License. See [`LICENSE`](LICENSE). Note that the underlying data are subject to separate terms and are not covered by this license.
