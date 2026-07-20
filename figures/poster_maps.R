# maps for poster

library(dplyr)
library(ggplot2)
library(sf)

health = readRDS("shiny_dashboard/health_map_data.rds") 
county_geom = readRDS("shiny_dashboard/county_geometry.rds") %>% st_transform(4326)
drive = readRDS("shiny_dashboard/health_site_drive_times_2023.rds") %>% st_transform(4326)

outdir = "figures/poster_figs"

save_plot <- function(plot, filename, width = 10, height = 7){
  ggsave(
    filename = file.path(outdir, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 300
  )
}

county_geom = county_geom %>% 
  filter(TIGER_YEAR == 2022)

health_sf = health %>% 
  filter(YEAR == 2022) %>% 
  left_join(county_geom, by = c("COUNTYFIPS" = "GEOID")) %>% 
  filter(substr(COUNTYFIPS, 1, 2) == "51") %>% 
  st_as_sf()

drive = drive %>% 
  filter(health_site_type == "acute_care_hospital", STUSPS == "VA")

drv_map = ggplot(drive) +
  geom_sf(aes(fill = avg_drive_time_minutes), color = "white", linewidth = 0.1) +
  scale_fill_distiller(
    palette = "YlOrRd",
    direction = 1,
    name = "Average Drive Time (min)"
  ) +
  theme_void() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.text = element_text(color = "black", size = 20),
    legend.title = element_text(color = "black", size = 26)
    
  )

save_plot(drv_map, "drive_map.png")

inj_map = ggplot(health_sf) +
  geom_sf(aes(fill = CDCW_INJURY_DTH_RATE), color = "white", linewidth = 0.1) +
  scale_fill_distiller(
    palette = "YlOrRd",
    direction = 1,
    name = "Injury Death Rate"
  ) +
  theme_void() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.text = element_text(color = "black", size = 20),
    legend.title = element_text(color = "black", size = 26)
  )

save_plot(inj_map, "injury_death_map.png")
