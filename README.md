# rural_health_DSPG

## Directory structure

```
rural_health_DSPG/
├── data/
│   ├── source/
│   │     ├── CLH/
│   │     └── FCC/
│   │     └── archive/
│   │     └── ...
│   └── outcome/
│   │     ├── CLH/
│   │     └── FCC/
│   │     └── archive/
│   │     └── ...
│   └── analysis/
├── programs/
│   └── clean/
│   └── archive/
│   └── analysis/
├── shiny_dashboard/
├── reference/
└── README.md

```
- `data/source/` contains raw data according to source
- `data/outcome/` contains cleaned data according to source
- `data/analysis/` contains cleaned/joined data for analysis
- `programs/clean/` contains scripts for cleaning
- `programs/analysis/` contains scripts for data analysis and calculating health site drive times
- `reference/`contains technical documentation for the datasets
- `archive/` folders contain scripts and data that were not used in the final dashboard

## Script Names

Each name has three parts:
1. Two-letter abbreviation of the folder it's in e.g., `an` or `cl`
2. Two numbers representing the order the scripts should be run in, e.g. `00` comes before `01`
3. A string representing the data source or function of the script

If a script is not named according to this convention, then it is a helper script called from another script

## Reproducibility
- Run the scripts according to their order: `clean` then `analysis`.
- Some of the outcome data, especially the `osm.pbf` files, are too large to store on GitHub, so make sure to generate everything with the scripts.
- Archived scripts and data were not used in the final dashboard, but can be useful to see the evolution of the project. They also show how to clean data from sources like ACS and CDC, which the CLH database pulls from. See the Data Availability Dashboard and Data Sources pages on the Shiny app to learn more.
- A few of the shell scripts were run with `sbatch` on [VT Advanced Research Computing](https://www.docs.arc.vt.edu/index.html) resources due to the very large file sizes of OpenStreetMap data. Further details are in the scripts.
- To calculate drive times, I initially used Docker Desktop to run the local server to query the OSM roads. This script is archived. The final pipeline was completed using apptainer on ARC.

## Data Collection Details
The following table describes how the data was collected for reproducibility.
| Data Source                                                                                                                                            | Variables                                                                | Processing                                                                                                                                                                                                                     | Years Available            | Geography      |
| ------------------------------------------------------------------------------------------------------------------------------------------------------ | ------------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | -------------------------- | -------------- |
| [FCC Form 477](https://www.fcc.gov/form-477-county-data-internet-access-services)                                                                      | Broadband adoption                                                       | Downloaded the **County-Level Tier Data** and **County-Level Connection Data** datasets.                                                                                                                                       | 2008–2025                  | County         |
| [CDC WONDER](https://wonder.cdc.gov/ucd-icd10-expanded.html)                                                                                           | Mortality                                                                | Selected the **Underlying Cause of Death** databases (1999–2012 and 2018–2024), grouped results by county and year, selected the desired years, enabled **Show Suppressed** and **Show Zero Values**, and downloaded the data. | 2018–2024 (also 1999–2012) | County         |
| [Community-Level Health Database (AHRQ)](https://www.ahrq.gov/data/innovations/clh-data.html)                                                          | Demographics, economic variables, health infrastructure, health outcomes | Downloaded the yearly datasets.                                                                                                                                                                                                        | 2009–2023                  | County         |
| [UNC Cecil G. Sheps Research Center – List of Hospitals](https://www.shepscenter.unc.edu/programs-projects/rural-health/list-of-hospitals-in-the-u-s/) | Hospital locations                                                       | Downloaded the annual hospital lists.                                                                                                                                                                                          | 2016–2025                  | Not applicable |
| [U.S. Census Bureau – Centers of Population](https://www.census.gov/geographies/reference-files/time-series/geo/centers-population.html)               | Population-weighted centroids                                            | Downloaded the **Centers of Population by Census Tract** files.                                                                                                                                                                | 2010, 2020                 | Census tract   |
| [Appalachian Regional Commission – Appalachian Counties Served by ARC](https://www.arc.gov/appalachian-counties-served-by-arc/)                        | List of Appalachian counties                                             | Downloaded the county list.                                                                                                                                                                                                    | As of 2021                 | County         |
| [OpenStreetMap Planet History](https://planet.osm.org/planet/full-history/)                                                                            | Roads, healthcare sites                                                  | Downloaded the **Latest Full History Planet PBF** file using `wget`.                                                                                                                                                           | 2006–present               | State, county  |

