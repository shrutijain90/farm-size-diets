# Overview

Code for analysing how the global distribution of crop production, consumption and trade across farm sizes could change under future dietary scenarios.

This project builds on the diet scenario outputs for production and trade from [future_food_scenarios](https://github.com/shrutijain90/future_food_scenarios).

## Repository structure

```
farm-size-diets/
├── farm_size_scen.py       # builds current (2020) production/area by farm-size class per country/crop,
│                            and projects these forward to 2050 under different farm-size trends
├── farm_size_supply.py     # combines farm-size shares with the diet/trade scenario outputs to estimate
│                            supply and trade by farm-size class, crop, country and year
├── results_figures.ipynb   # exploratory analysis and summary tables used to prepare figures
└── figures.R               # generates figures 
```

## Data

Input scripts read from external data directories (e.g. `../../data/farm_size/` and `../../../GoogleDrive-.../DPhil/`) and include:

- Farm-size distributions, processed in Google Earth Engine:
  - [area by farm size](https://code.earthengine.google.com/c46eb857d355e0f76da1d34ce39fbd84)
  - [production by farm size](https://code.earthengine.google.com/9ca0fabc46a050add5fffca6e9360af1)
- Country-level farm-size trend projections from [Wang et al. 2025](https://www.nature.com/articles/s41467-025-64319-9)
- FAOSTAT [Supply Utilization Accounts](https://www.fao.org/faostat/en/#data)
- Scenario outputs from [future_food_scenarios](https://github.com/shrutijain90/future_food_scenarios)

These are not tracked in version control and must be available locally to run.

## Dependencies

Python: pandas, numpy, geopandas, matplotlib, seaborn, networkx, shapely, basemap.

R: ggplot2, dplyr, readr, tidyr, scales, patchwork, circlize, legendry, conflicted, shadowtext, forcats, ggh4x.
