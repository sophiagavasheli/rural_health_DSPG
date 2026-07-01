#!/bin/bash

# this is a script to run a OSM server with Docker for a specified state

set -e

# 1. CHECK ARGUMENTS
if [ -z "$1" ] || [ -z "$2" ]; then
    echo "Error: Missing arguments."
    echo "Usage: ./in_00_start_osrm.sh <state-name> <absolute-path-to-data-dir>"
    exit 1
fi

STATE=$(echo "$1" | tr '[:upper:]' '[:lower:]')
DATA_DIR=$2

PBF_FILE="${STATE}.osm.pbf" 

# Verify the file exists inside the target data directory
if [ ! -f "${DATA_DIR}/${PBF_FILE}" ]; then
    echo "Error: Local file '${PBF_FILE}' not found in ${DATA_DIR}"
    exit 1
fi

echo "=== Preprocessing local OSM data in: ${DATA_DIR} ==="

# 2. RUN OSRM PREPROCESSING PIPELINE (Mounting DATA_DIR instead of PWD)
docker run --rm -t -v "${DATA_DIR}:/data" osrm/osrm-backend osrm-extract -p /profile/car.lua "/data/${PBF_FILE}"
docker run --rm -t -v "${DATA_DIR}:/data" osrm/osrm-backend osrm-partition "/data/${STATE}.osrm"
docker run --rm -t -v "${DATA_DIR}:/data" osrm/osrm-backend osrm-customize "/data/${STATE}.osrm"

# 3. LAUNCH THE ROUTING ENGINE
echo "=== Launching OSRM Server for ${STATE} ==="
docker run -d \
  --name "osrm_${STATE}" \
  -p 5000:5000 \
  -v "${DATA_DIR}:/data" \
  osrm/osrm-backend \
  osrm-routed --algorithm mld --max-table-size 100000 "/data/${STATE}.osrm"

echo "OSRM server for ${STATE} is now up on port 5000."