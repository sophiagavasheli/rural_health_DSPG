# calculating meshedness for VA counties

library(tigris)
library(sf)
library(sfnetworks)
library(igraph)
library(osmextract)
library(dplyr)

# Get road network for virginia
va_roads <- oe_get(
  place = "Virginia",
  layer = 'lines',
  query = "
    SELECT *
    FROM lines
    WHERE highway IN (
      'motorway',
      'trunk',
      'primary',
      'secondary',
      'tertiary',
      'residential',
      'unclassified'
    )
  "
)

# get va counties, transorm crs
va_roads <- st_transform(va_roads, 4326)

va_counties <- counties(state = "VA", cb = TRUE, class = "sf") %>%
  st_transform(st_crs(va_roads))

#make valid geometry
va_roads <- st_make_valid(va_roads)
va_counties <- st_make_valid(va_counties)

#split roads by counties
va_roads_county <- st_join(
  va_roads,
  va_counties[, c("GEOID", "NAME")],
  join = st_intersects,
  largest = TRUE
)

# Convert the roadsto a network

net <- as_sfnetwork(roads)

# Use igraph to compute stuff because this is what I am familiar with

g <- as.igraph(net)

# Find the number of vertices (rule out intersections of degree 1 because these are just dead ends)

deg <- degree(g)

verts <- sum(deg >= 2)

# Apply the meshedness formula

meshy <- (nrow(roads) - verts + 1) / (2*verts - 5)

