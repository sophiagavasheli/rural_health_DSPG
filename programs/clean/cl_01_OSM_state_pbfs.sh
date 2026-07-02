#!/bin/bash

# this script creates an osm.pbf file for each state from the entire us pbf file
# run on VT ARC

#SBATCH -J states_osm # job name
#SBATCH -N1
#SBATCH --ntasks-per-node=1
#SBATCH --time=05:00:00
#SBATCH --mem=32G
#SBATCH -p normal_q
#SBATCH -A dspg_viz # project
#SBATCH --mail-user=sophiag23@vt.edu # enter desired email address for updates
#SBATCH --mail-type=BEGIN # get emailed when job begins
#SBATCH --mail-type=END   # get emailed when job ends
#SBATCH --mail-type=FAIL  # get emailed if job fails

set -euo pipefail

source ~/miniconda3/bin/activate
conda activate osm

# Input PBF (download this first if you don't have it)
BASE="/home/sophiag23/states"

INPUT_PBF="${BASE}/usa_2023.osm.pbf"

# Output directory
OUTDIR="${BASE}/states_osm"
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