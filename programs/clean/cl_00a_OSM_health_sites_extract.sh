#!/bin/bash
# script to get historical osm data and extract health features. All commands run in VT ARC

#SBATCH -J osm # job name
#SBATCH -N1
#SBATCH --cpus-per-task=2
#SBATCH --mem=32G
#SBATCH --time=10:00:00
#SBATCH -p normal_q
#SBATCH -A dspg_viz # project
#SBATCH --mail-user=sophiag23@vt.edu # enter desired email address for updates
#SBATCH --mail-type=BEGIN,END,FAIL


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

BASE="/home/sophiag23/osm"
PBF="${BASE}/history-latest.osm.pbf"
POLY="${BASE}/us.poly"
OUT="${BASE}/health_sites"
mkdir -p $OUT

for YEAR in {2019..2022}; do

    echo "========================================"
    echo "Processing ${YEAR}"
    echo "========================================"

    TIME="${BASE}/history-${YEAR}.osm.pbf"
    FINAL="${BASE}/usa_${YEAR}.osm.pbf"

    OUTPUT_PBF="${OUT}/us_health_${YEAR}.osm.pbf"
    OUTPUT_GPKG="${OUT}/us_health_${YEAR}.gpkg"
    OUTPUT_GEOJSON="${OUT}/us_health_${YEAR}_deduplicated.geojson"

    
    # Create snapshot for January 1 of the given year
    echo "Filtering history to ${YEAR}..."

    osmium time-filter \
        "$PBF" \
        "${YEAR}-01-01T00:00:00Z" \
        -o "$TIME" \
        --overwrite

    
    # Extract United States
    echo "Extracting USA..."

    osmium extract \
        -p "$POLY" \
        "$TIME" \
        -o "$FINAL" \
        --overwrite

    
    # Extract healthcare features
    echo "Extracting healthcare features..."

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

    
    # Convert to GeoPackage
    echo "Exporting to GeoPackage..."

    ogr2ogr \
        -f GPKG \
        "$OUTPUT_GPKG" \
        "$OUTPUT_PBF"

    
    # Deduplicate
    echo "Deduplicating..."

    python "${BASE}/cl_00b_OSM_health_sites_deduplicate.py" \
        "$OUTPUT_GPKG" \
        "$OUTPUT_GEOJSON"

    echo "Finished ${YEAR}"

done

echo "========================================"
echo "All years completed!"
