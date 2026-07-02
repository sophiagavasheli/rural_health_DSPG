library(sf)
library(dplyr)
library(ggplot2)
library(leaflet)

# Example state
state <- "VA"

spatial_data = readRDS("data/outcome/OSM/drive_times/va_acute_hosp_drive_times.rds") %>% st_transform(4326)

state_counties <- readRDS("data/outcome/census/us_counties_2023.rds") %>%
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

# Pick one county
center <- state_centers %>%
  filter(COUNTYFP == "003") %>%
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
  
  
  addPolygons(
    data = state_counties,
    color = "darkgray",
    weight = 1,
    fill = "beige",
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

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  
  
  addPolygons(
    data = state_counties,
    color = "darkgray",
    weight = 1,
    fill = "beige",
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
    popup = paste0("County: ", center$COUNTYFP)
  )



# 2. DEFINE PALETTE
pal <- colorNumeric(
  palette = "YlOrRd",
  domain = spatial_data$avg_drive_time_minutes,
  na.color = "#808080"
)

# 3. LABELS FOR COUNTIES
county_labels <- sprintf(
  "<strong>County:</strong> %s<br/>
   <strong>Avg Drive Time:</strong> %0.1f mins",
  spatial_data$NAME, spatial_data$avg_drive_time_minutes
) %>% lapply(htmltools::HTML)

# 4. BUILD THE MAP WITH OVERLAY
map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  
  # Base Layer: Counties
  addPolygons(
    data = spatial_data,
    fillColor = ~pal(avg_drive_time_minutes),
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.6,
    highlightOptions = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.8, bringToFront = FALSE),
    label = county_labels,
    group = "Counties"
  ) %>%
  
  # # Overlay Layer: Population Centroids
  # addCircleMarkers(
  #   data = state_centers,
  #   radius = 1,
  #   color = "black",
  #   stroke = FALSE,
  #   fillOpacity = 0.5,
  #   label = ~paste("Tract:", TRACTCE, "<br>Pop:", POPULATION) %>% lapply(htmltools::HTML),
  #   group = "Pop Centroids"
  # ) %>%
  # 
  # # Layer Control to toggle points on/off
  # addLayersControl(
  #   overlayGroups = c("Counties", "Pop Centroids"),
  #   options = layersControlOptions(collapsed = FALSE)
  # ) %>%
  
  addLegend(
    data = spatial_data,
    pal = pal,
    values = ~avg_drive_time_minutes,
    title = "Avg Drive Time (Mins)",
    position = "bottomright"
  )

map