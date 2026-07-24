#!/bin/bash
#SBATCH --job-name=drive_times
#SBATCH --account=dspg_viz
#SBATCH --partition=normal_q
#SBATCH --array=0-50
#SBATCH --time=20:00:00
#SBATCH --mem=32G
#SBATCH --cpus-per-task=4
#SBATCH --mail-user=sophiag23@vt.edu # enter desired email address for updates
#SBATCH --mail-type=BEGIN, END, FAIL


set -euo pipefail

#working directory
cd "/home/sophiag23/drive_times"

# make sure to run the following in an interactive job before running this script
# interact --account=<your_account> --time=0:30:00 --cpus-per-task=2
  # module load apptainer
  # mkdir -p drive_times/data/containers
  # apptainer pull drive_times/data/containers/osrm-backend.sif docker://osrm/osrm-backend

  # module load R-bundle-CRAN/2025.11-foss-2025b
  # R
  # required_packages <- c("dplyr", "osrm", "sf", "stringr", "tidyr", "tibble")
  # missing_packages <- required_packages[!sapply(required_packages, requireNamespace)] 
# if the printed list is missing any of the packages do, install.packages()
  # q() to exit R and exit to end the job

# MAKE SURE ALL THE DATA IS CORRECLY COPIED
# osm.pbf files in "drive_times/data/OSM_states_2020" (roads only exist for 2020, reused for every year)
# counties in "drive_times/data/us_counties_2020.rds"
# health sites (all years, with a `year` column) in "drive_times/data/clean_UNC_hosps_all_2019_2023.rds"
# centroids "drive_times/data/clean_pop_centroids_2020.rds"

mkdir -p results # data output will be here

STATES=(
  alabama alaska arizona arkansas california colorado connecticut delaware
  district-of-columbia florida georgia hawaii idaho illinois indiana iowa
  kansas kentucky louisiana maine maryland massachusetts michigan minnesota
  mississippi missouri montana nebraska nevada new-hampshire new-jersey
  new-mexico new-york north-carolina north-dakota ohio oklahoma oregon
  pennsylvania rhode-island south-carolina south-dakota tennessee texas
  utah vermont virginia washington west-virginia wisconsin wyoming
)

YEARS=(2019 2020 2021 2022)
# 2023 was run separately while I was still getting the pipeline to work

STATE=${STATES[$SLURM_ARRAY_TASK_ID]}

# Unique port per array task in case multiple tasks land on the same node
export OSRM_PORT=$((5000 + SLURM_ARRAY_TASK_ID % 100))

module load apptainer
module load R-bundle-CRAN/2025.11-foss-2025b

# Each array task handles ONE state, looping over all years sequentially.
for YEAR in "${YEARS[@]}"; do
  echo "Array task $SLURM_ARRAY_TASK_ID -> state: $STATE, year: $YEAR (OSRM port $OSRM_PORT)"
  Rscript an_00b_health_site_drive_times2019_2022.R "$STATE" "$YEAR"
done
