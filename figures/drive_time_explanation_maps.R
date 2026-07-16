# code to generate images for shiny to explain drive time calculations

library(sf)
library(dplyr)
library(ggplot2)
library(leaflet)

state <- "VA"

state_counties <- readRDS("data/outcome/census/us_counties_2020.rds") %>%
  filter(STUSPS == state) %>% 
  st_transform(4326)

# County centroids
state_centers <- readRDS("data/outcome/census/clean_pop_centroids_2020.rds") %>%
  filter(STUSPS == state) %>% 
  st_transform(4326)

# Hospitals
state_hosps <- readRDS("data/outcome/UNC_shep/clean_UNC_hosps_acute_2023.rds") %>%
  filter(STUSPS == state) %>% 
  st_transform(4326)

# hosps and centroids -----------------------------------------------------
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(lng = -80, lat = 37, zoom = 6) %>%
  
  addPolygons(
    data = state_counties,
    color = "darkgray",
    weight = 1,
    fillColor = "#d8e2dc",
    opacity = 0.8,
    group = "County Boundaries",
    popup = ~NAME
  ) %>% 
  
  
  # All hospitals
  addCircleMarkers(
    data = state_hosps,
    radius = 3,
    color = "darkgreen",
    stroke = FALSE,
    fillOpacity = 0.6,
    popup = ~name
  ) %>%
  
  
  # County centroid
  addCircleMarkers(
    data = state_centers,
    radius = 1,
    color = "steelblue",
    fillColor = "steelblue",
    stroke = FALSE,
    fillOpacity = 1,
  )


# 1 centroid and 10 nearest hospitals -------------------------------------

# Pick one county
center <- state_centers %>%
  filter(COUNTYFP == "121") %>%
  slice(1)

# Compute distances
dists <- st_distance(center, state_hosps)

nearest10 <- state_hosps[
  order(as.numeric(dists))[1:10],
]

# Distance to the furthest of the 10
radius <- max(dists[order(as.numeric(dists))[1:10]])

# Buffer (20% padding)
buffer <- st_buffer(center, radius)


leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(lng = -80, lat = 37, zoom = 6) %>%
  
  addPolygons(
    data = state_counties,
    color = "darkgray",
    weight = 1,
    fillColor = "#d8e2dc",
    opacity = 0.8,
    group = "County Boundaries",
    popup = ~NAME
  ) %>% 
  
  # Buffer
  addPolygons(
    data = buffer,
    color = "steelblue",
    weight = 2,
    fillColor = "lightblue",
    fillOpacity = 0.25
  ) %>%
  
  # All hospitals
  addCircleMarkers(
    data = state_hosps,
    radius = 3,
    color = "darkgreen",
    stroke = FALSE,
    fillOpacity = 0.6,
    popup = ~name
  ) %>%
  
  # Nearest 10
  addCircleMarkers(
    data = nearest10,
    radius = 3,
    color = "darkred",
    fillOpacity = 1,
    stroke = FALSE,
    popup = ~name
  ) %>%
  
  # County centroid
  addCircleMarkers(
    data = center,
    radius = 8,
    color = "steelblue",
    fillColor = "steelblue",
    stroke = FALSE,
    fillOpacity = 1,
    popup = paste0("County: ", center$COUNTYFP)
    
  ) 

# centroids linked to nearest ---------------------------------------------

county_centers <- state_centers %>%
  filter(COUNTYFP == "121")

# For each centroid, find nearest hospital
nearest_idx <- st_nearest_feature(county_centers, state_hosps)

nearest_hosps <- state_hosps[nearest_idx, ]

lines_sf <- st_sf(
  geometry = st_nearest_points(
    county_centers,
    state_hosps[nearest_idx, ],
    pairwise = TRUE
  )
)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  
  addPolygons(
    data = state_counties,
    color = "darkgray",
    weight = 1,
    fillColor = "#d8e2dc",
    fillOpacity = 0.5
  ) %>%
  
  addPolylines(
    data = lines_sf,
    color = "steelblue",
    weight = 2,
    opacity = 0.7
  ) %>%
  
  addCircleMarkers(
    data = county_centers,
    radius = 3,
    color = "steelblue",
    stroke = FALSE,
    fillOpacity = 1
  ) %>%
  
  addCircleMarkers(
    data = nearest_hosps,
    radius = 4,
    color = "darkred",
    stroke = FALSE,
    fillOpacity = 1,
  )