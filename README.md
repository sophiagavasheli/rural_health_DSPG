# rural_health_DSPG

## Directory structure

```
rural_health_DSPG/
├── data/
│   ├── source/
│   │     ├── CLH
│   │     └── FCC
│   │     └── ...
│   └── outcome/
│   │     ├── CLH
│   │     └── FCC
│   │     └── ...
├── program/
│   └── clean/
│   └── intermediate/
│   └── analysis/
├── shiny_dashboard/
├── reference/
└── README.md

```
- `source/` contains raw data according to source
- `outcome/` contains cleaned data according to source
- `clean/` contains scripts for cleaning
- `intermediate/` contains scripts in between cleaning and analysis, like calculating drive times to hospitals
- `analysis/` contains scripts for data analysis
- `reference/`contains technical documentation for the datasets
- `archive/` folders contain scripts and data that were not used in the final dashboard

## Script Names

Each name has three parts:
1. Two-letter abbreviation of the folder it's in e.g., `in` or `cl`
2. Two numbers representing the order the scripts should be run in, e.g. `00` comes before `01`. This order is not always absolute, so check to see if some scripts run others automatically
3. A string representing the data source or function of the script

## Reproducibility
- Run the scripts according to their order. `clean`, `intermediate`, and finally `analysis`.
- Some of the outcome data, especially the `osm.pbf` files, are too large to store on GitHub, so make sure to generate everything with the scripts.
- Archived scripts and data were not used in the final dashboard, but can be useful to see the evolution of the project. They also show how to clean data from sources like ACS and CDC, which the CLH database pulls from. See the Data Availability Dashboard and Data Sources pages on the Shiny app to learn more.
- A few of the shell scripts were run with `sbatch` on [VT Advanced Research Computing](https://www.docs.arc.vt.edu/index.html) resources due to the very large file sizes of OpenStreetMap data. Further details are in the scripts.
- To calculate drive times, I used Docker Desktop to run the local server to query the OSM roads. Details in the scripts.
