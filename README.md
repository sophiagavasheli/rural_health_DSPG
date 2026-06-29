# rural_health_DSPG

## Directory structure

```
rural_health_DSPG/
├── data/
│   ├── source/
│   │     ├── ACS
│   │     └── HRSA
│   │     └── ...
│   └── outcome/
│   │     ├── ACS
│   │     └── HRSA
│   │     └── ...
├── program/
│   └── clean/
│   └── intermediate/
│   └── analysis/
├── shiny_dashboard/
├── reference/
└── README.md

```
- `source/` contains raw data
- `outcome/` contains cleaned data
- `clean/` contains scripts for cleaning
- `intermediate/` contains scripts for joining and calculating transportation data
- `analysis/` contains scripts for data analysis
- `reference/`contains technical documentation for the datasets
- `archive/` folders contain scripts and data that were not used in the final dashboard
