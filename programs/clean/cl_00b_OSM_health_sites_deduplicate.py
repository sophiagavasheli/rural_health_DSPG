# the following script is to deduplicate any health sites into unique point features

# if not already installed:
# conda install -c conda-forge geopandas pyogrio

import geopandas as gpd
import pandas as pd
import sys

# paths used when running locally
#gpkg_path = r"C:\Users\sopog\Documents\College\DSPG\rural_health_DSPG\data/source/OSM/us_health.gpkg"
#output_path = r"C:\Users\sopog\Documents\College\DSPG\rural_health_DSPG\data/outcome/OSM/us_health_deduplicated.geojson"

# command line arguments
gpkg_path = sys.argv[1]
output_path = sys.argv[2]


# 1. Load layers using the fast pyogrio engine
print("Loading layers from GeoPackage...")
points = gpd.read_file(gpkg_path, layer="points", engine="pyogrio")
polygons = gpd.read_file(gpkg_path, layer="multipolygons", engine="pyogrio")

# 2. Find overlapping points (points inside polygons)
print("Finding overlapping points...")
points_inside_polygons = gpd.sjoin(points, polygons, how="inner", predicate="within")

# 3. Keep only points that are NOT duplicates
clean_points = points[~points.index.isin(points_inside_polygons.index)].copy()

# 4. Convert polygons to centroids
print("Converting polygons to centroids...")
polygon_centroids = polygons.copy()
polygon_centroids["geometry"] = polygon_centroids.geometry.centroid

# 5. Subset core columns and combine
columns_to_keep = ['osm_id', 'name', 'amenity', 'geometry']
columns_points = [col for col in columns_to_keep if col in clean_points.columns]
columns_poly = [col for col in columns_to_keep if col in polygon_centroids.columns]

master_health_sites = pd.concat([
    clean_points[columns_points], 
    polygon_centroids[columns_poly]
], ignore_index=True)

# 6. Save out the clean file
master_health_sites.to_file(output_path, driver="GeoJSON")

print(f"Success! Cleaned dataset saved. Total unique health sites: {len(master_health_sites)}")
