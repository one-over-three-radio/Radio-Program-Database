library(tidyverse)
library(cowplot)   # for theme_minimal_grid()
library(sf)        # for manipulation of simple features objects
library(rworldmap) # for getMap()
library(rworldxtra)
library(tidygeocoder)

# Read the tab-separated file
df <- read_csv("next.csv")


# Parse the Comments column to extract city, region, country
locations <- df |>
  select(Name, Artist, Comments) |>
  mutate(
    city    = str_split_fixed(Comments, ",\\s*", 5)[, 1],
    region  = str_split_fixed(Comments, ",\\s*", 5)[, 2],
    country = str_split_fixed(Comments, ",\\s*", 5)[, 3],
    address = paste(city, region, country, sep = ", ")
  )
# Geocode
points_sf <- locations |>
  geocode(address, method = "arcgis", lat = lat, lon = lon) |>
  filter(!is.na(lat), !is.na(lon)) |>       # drop any failed geocodes
  st_as_sf(coords = c("lon", "lat"), crs = 4326) 


## making the map
world_sf <- st_as_sf(getMap(resolution = "li"))

crs_goode <- "+proj=igh"

# projection outline in long-lat coordinates
lats <- c(
  90:-90, # right side down
  -90:0, 0:-90, # third cut bottom
  -90:0, 0:-90, # second cut bottom
  -90:0, 0:-90, # first cut bottom
  -90:90, # left side up
  90:0, 0:90, # cut top
  90 # close
)
longs <- c(
  rep(180, 181), # right side down
  rep(c(80.01, 79.99), each = 91), # third cut bottom
  rep(c(-19.99, -20.01), each = 91), # second cut bottom
  rep(c(-99.99, -100.01), each = 91), # first cut bottom
  rep(-180, 181), # left side up
  rep(c(-40.01, -39.99), each = 91), # cut top
  180 # close
)

goode_outline <- 
  list(cbind(longs, lats)) %>%
  st_polygon() %>%
  st_sfc(
    crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  )

# now we need to work in transformed coordinates, not in long-lat coordinates
goode_outline <- st_transform(goode_outline, crs = crs_goode)

# get the bounding box in transformed coordinates and expand by 10%
xlim <- st_bbox(goode_outline)[c("xmin", "xmax")]*1.1
ylim <- st_bbox(goode_outline)[c("ymin", "ymax")]*1.1

# turn into enclosing rectangle
goode_encl_rect <- 
  list(
    cbind(
      c(xlim[1], xlim[2], xlim[2], xlim[1], xlim[1]), 
      c(ylim[1], ylim[1], ylim[2], ylim[2], ylim[1])
    )
  ) %>%
  st_polygon() %>%
  st_sfc(crs = crs_goode)

# calculate the area outside the earth outline as the difference
# between the enclosing rectangle and the earth outline
goode_without <- st_difference(goode_encl_rect, goode_outline)

# Add to your plot
ggplot(world_sf) + 
  geom_sf(fill = "#55463b", color = "#d8d4c7", size = 0.25/.pt) +
  geom_sf(data = goode_without, fill = "#d8d4c7", color = "#d8d4c7") +
  geom_sf(data = goode_outline, fill = NA, color = "#55463b", size = 0.5/.pt) +
  geom_sf(data = points_sf, color = "#e7651c", size = 1.5, shape = 16) +  # <-- points layer
  coord_sf(crs = crs_goode, xlim = 0.95*xlim, ylim = 0.95*ylim, expand = FALSE) +
  theme_minimal_grid() +
  theme(
    panel.background = element_rect(fill = "#d8d4c7", color = "#d8d4c7", linewidth = 1),
    panel.grid.major = element_line(color = "#55463b", linewidth = 0.5),
    plot.background = element_rect(fill = "#d8d4c7")
  )

ggsave("Ep.114.png", bg = "#d8d4c7")


 