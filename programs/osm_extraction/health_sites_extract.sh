#!/bin/bash
# script to extract health sites from osm.pbf files

# required conda environment:
#conda create -n osm -c conda-forge osmium-tool gdal
#conda activate osm

# Directory containing state .osm.pbf files
INPUT_DIR="/c/Users/sopog/Documents/College/DSPG/rural_health_DSPG/data/source/OSM"

# Temporary directory for filtered files
TMP_DIR="./health_tmp"

# Output files
OUTPUT_PBF="$INPUT_DIR/us_health.osm.pbf"
OUTPUT_GPKG="$INPUT_DIR/us_health.gpkg"

mkdir -p "$TMP_DIR"

echo "Extracting healthcare features..."

for pbf in "$INPUT_DIR"/*.osm.pbf; do
    state=$(basename "$pbf" .osm.pbf)

    echo "  Processing $state..."

    osmium tags-filter \
        "$pbf" \
        nwr/amenity=hospital \
        nwr/amenity=clinic \
        nwr/amenity=pharmacy \
        nwr/amenity=doctors \
        nwr/amenity=dentist \
        nwr/amenity=nursing_home \
        --add-metadata=timestamp \
        -o "$TMP_DIR/${state}_health.osm.pbf" \
        --overwrite
done

echo "Merging extracted files..."

osmium merge \
    "$TMP_DIR"/*_health.osm.pbf \
    --add-metadata=timestamp \
    -o "$OUTPUT_PBF" \
    --overwrite

echo "Exporting to GeoPackage..."

ogr2ogr \
    -f GPKG \
    "$OUTPUT_GPKG" \
    "$OUTPUT_PBF"

rm -r $TMP_DIR #remove temp directory

echo "Done!"
echo "Merged PBF:  $OUTPUT_PBF"
echo "GeoPackage: $OUTPUT_GPKG"