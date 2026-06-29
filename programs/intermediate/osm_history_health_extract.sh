#!/bin/bash
# script to get historical osm data and extract health features. All commands run in VT ARC

#SBATCH -J osm # job name
#SBATCH -N1
#SBATCH --ntasks-per-node=128
#SBATCH --time=02:00:00
#SBATCH -p normal_q
#SBATCH -A dspg_viz # project
#SBATCH --mail-user=sophiag23@vt.edu # enter desired email address for updates
#SBATCH --mail-type=BEGIN # get emailed when job begins
#SBATCH --mail-type=END   # get emailed when job ends
#SBATCH --mail-type=FAIL  # get emailed if job fails


# data retreived with
# wget https://planet.osm.org/pbf/full-history/history-latest.osm.pbf -data file
# wget https://download.geofabrik.de/north-america/us.poly -file describing the region

# osmium documentation: https://osmcode.org/osmium-tool/manual.html#introduction 

# required conda environment:
#conda create -n osm -c conda-forge osmium-tool gdal geopandas pyogrio

set -euo pipefail # prevent script from continuing if a command fails

#conda
source ~/miniconda3/bin/activate
conda activate osm

BASE="/home/sophiag23/osm/"
PBF="${BASE}/history-latest.osm.pbf"
POLY="${BASE}/us.poly"
TIME="${BASE}/history-2023.osm.pbf"
FINAL="${BASE}/usa_2023.osm.pbf"

# filtering up until 2023
echo "filtering for 2023"

osmium time-filter "$PBF" 2023-01-01T00:00:00Z -o "$TIME"

# extract usa using poly file
echo "extracting usa"

osmium extract -p "$POLY" "$TIME" -o "$FINAL"


# output files
OUTPUT_PBF="${BASE}/us_health_2023.osm.pbf"
OUTPUT_GPKG="${BASE}/us_health_2023.gpkg"

echo "Extracting healthcare features..."

#extract health features
osmium tags-filter \
    "$FINAL" \
    nwr/amenity=hospital \
    nwr/amenity=clinic \
    nwr/amenity=pharmacy \
    nwr/amenity=doctors \
    nwr/amenity=dentist \
    nwr/amenity=nursing_home \
    nwr/healthcare=* \
    -o "$OUTPUT_PBF" \
    --overwrite

echo "Exporting to GeoPackage..."

ogr2ogr \
    -f GPKG \
    "$OUTPUT_GPKG" \
    "$OUTPUT_PBF"

echo "deduplicating"

python "${BASE}/deduplicate_health_sites.py" "$OUTPUT_GPKG" "${BASE}/us_health_2023_deduplicated.geojson"

echo "Done!"