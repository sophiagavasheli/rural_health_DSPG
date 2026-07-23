#!/bin/bash

# this script creates an osm.pbf file for each state for a given year from the entire us pbf file. run on VT ARC

#SBATCH -J states_osm # job name
#SBATCH -N1
#SBATCH --cpus-per-task=2
#SBATCH --time=10:00:00
#SBATCH --mem=32G
#SBATCH -p normal_q
#SBATCH -A dspg_viz # project
#SBATCH --mail-user=sophiag23@vt.edu # enter desired email address for updates
#SBATCH --mail-type=BEGIN,END,FAIL

set -euo pipefail

source ~/miniconda3/bin/activate
conda activate osm

BASE="/home/sophiag23/osm"

# this was first done for 2023 when I was still getting the pipeline to work
# then I decided to use 2020 roads for multiple years of drive time data since the roads don't change much
YEAR=2020
INPUT_PBF="${BASE}/usa_${YEAR}.osm.pbf"

# Output directory
OUTDIR="${BASE}/OSM_states_${YEAR}"
mkdir -p "$OUTDIR"

# Base URL for Geofabrik poly files
BASE_URL="https://download.geofabrik.de/north-america/us"

# List of states (Geofabrik naming)
STATES=(
  alabama alaska arizona arkansas california colorado connecticut delaware
  florida georgia hawaii idaho illinois indiana iowa kansas kentucky louisiana
  maine maryland massachusetts michigan minnesota mississippi missouri montana
  nebraska nevada new-hampshire new-jersey new-mexico new-york north-carolina
  north-dakota ohio oklahoma oregon pennsylvania rhode-island south-carolina
  south-dakota tennessee texas utah vermont virginia washington west-virginia
  wisconsin wyoming district-of-columbia
)

for state in "${STATES[@]}"; do
  echo "=============================="
  echo "Processing: $state"

  POLY_URL="${BASE_URL}/${state}.poly"
  POLY_FILE="${state}.poly"
  OUT_FILE="${OUTDIR}/${state}.osm.pbf"

  # Download .poly file if not already present
  if [[ ! -f "$POLY_FILE" ]]; then
    echo "Downloading $POLY_FILE ..."
    wget "$POLY_URL" -O "$POLY_FILE"
  fi

  # Run osmium extract
  echo "Extracting $state ..."
  osmium extract -p "$POLY_FILE" "$INPUT_PBF" -o "$OUT_FILE" 
    

  echo "Done: $OUT_FILE"
done

echo "All states complete."