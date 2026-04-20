library(tidyverse)
library(cowplot)
library(sf)
library(rworldmap)
library(rworldxtra)
library(tidygeocoder)

# Read the file
df <- read_csv("NewThisWeekCC.csv")

# Parse the Comments column
locations <- df |>
  select(Name, Artist, Comments) |>
  mutate(
    city    = str_split_fixed(Comments, ",\\s*", 5)[, 1],
    region  = str_split_fixed(Comments, ",\\s*", 5)[, 2],
    country = str_split_fixed(Comments, ",\\s*", 5)[, 3],
    address = paste(city, region, country, sep = ", ")
  )

# Geocode all first, then filter to Canada after to avoid size-0 error
points_sf <- locations |>
  geocode(address, method = "arcgis", lat = lat, lon = lon) |>
  filter(!is.na(lat), !is.na(lon)) |>
  filter(str_trim(country) == "Canada") |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)  # WGS84 geographic

# UTM Zone 15N + WGS84 datum (EPSG:32615)
crs_utm <- st_crs(32615)

# Canada outline
world_sf  <- st_as_sf(getMap(resolution = "high"))
canada_sf <- world_sf |> filter(ADMIN == "Canada")

# Provincial boundaries via st_read — no extra packages needed
provinces_sf <- st_read(
  "https://raw.githubusercontent.com/nvkelso/natural-earth-vector/master/geojson/ne_10m_admin_1_states_provinces.geojson",
  quiet = TRUE
) |>
  filter(admin == "Canada")

# Transform all layers to UTM / WGS84
canada_utm    <- st_transform(canada_sf,    crs_utm)
provinces_utm <- st_transform(provinces_sf, crs_utm)
points_utm    <- st_transform(points_sf,    crs_utm)

# Bounding box with 5% padding
bbox  <- st_bbox(canada_utm)
x_pad <- (bbox["xmax"] - bbox["xmin"]) * 0.05
y_pad <- (bbox["ymax"] - bbox["ymin"]) * 0.05

# Plot
ggplot(canada_utm) +
  geom_sf(fill = "#55463b", color = "#d8d4c7", linewidth = 0.1 / .pt) +
  geom_sf(data = provinces_utm, fill = NA, color = "#d8d4c7", linewidth = 0.25 / .pt) +
  geom_sf(data = points_utm, color = "#e7651c", size = 3, shape = 16) +
  coord_sf(
    crs    = crs_utm,
    xlim   = c(bbox["xmin"] - x_pad, bbox["xmax"] + x_pad),
    ylim   = c(bbox["ymin"] - y_pad, bbox["ymax"] + y_pad),
    expand = FALSE
  ) +
  theme_minimal_grid() +
  theme(
    panel.background = element_rect(fill = "#d8d4c7", color = "#d8d4c7", linewidth = 0.1),
    panel.grid.major = element_line(color = "#55463b", linewidth = 0.5),
    plot.background  = element_rect(fill = "#d8d4c7"),
    axis.text        = element_blank(),
    axis.title       = element_blank()
  )

ggsave("april19mapcc.png", bg = "#d8d4c7", width = 12, height = 8)
