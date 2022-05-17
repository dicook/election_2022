library(ggthemes)
library(sf)
library(sugarbag)
library(tidyverse)

# Spatial polygons
electorates <- sf::st_read("2021-Cwlth_electoral_boundaries_ESRI/2021_ELB_region.shp")
electorates_small <- electorates %>% rmapshaper::ms_simplify(keep = 0.01, keep_shapes = TRUE)

ggplot() + geom_sf(data=electorates_small) + theme_map()

# Read in data on current electoral map
current <- read_csv("current_electoral.csv") %>%
  select(Electorate:Party)
current_major <- current %>%
  mutate(Party_maj = fct_collapse(Party,
                                  LNP = c("LIB", "LNP", "NAT")))

electorates_small <- electorates_small %>%
  left_join(current_major, by=c("Elect_div"="Electorate"))
ggplot() +
  geom_sf(data=electorates_small,
                   aes(fill = Party_maj),
                       colour="white") +
  scale_fill_manual("", values=c("ALP"="#E13940",
                                   "LNP"="#1C4F9C",
                                   "GRN"="#009C3D",
                                   "KAP"="#FDDA0D",
                                   "CA"="#FFC000",
                                   "IND"="#66b2b2")) +
  theme_map()

# Find centroids of polygons
sf_use_s2(FALSE)
centroids <- electorates %>%
  create_centroids(., "Elect_div")
ggplot() +
  geom_sf(data=electorates_small, fill="grey90", colour="white") +
  geom_point(data=centroids, aes(x=longitude, y=latitude),
             colour="orange", size=1) +
  geom_point(data=capital_cities, aes(x=longitude, y=latitude),
             colour="red", shape=8, size=2) +
  theme_map()

## Create hexagon grid
hs <- 0.1
grid <- create_grid(centroids = centroids,
                    hex_size = hs,
                    buffer_dist = 5)

## Allocate polygon centroids to hexagon grid points
electorate_hexmap <- allocate(
  centroids = centroids,
  hex_grid = grid,
  sf_id = "Elect_div",
  ## same column used in create_centroids
  hex_size = hs,
  ## same size used in create_grid
  hex_filter = 10,
  focal_points = capital_cities,
  width = 35,
  verbose = FALSE
)
ggplot() +
  geom_sf(data=electorates_small, fill="grey90", colour="white") +
  geom_point(data=electorate_hexmap, aes(x=longitude, y=latitude),
             colour="orange", size=1) +
  theme_map()

# Use hexagons - this is producing size 0 hexagons
e_hex <- fortify_hexagon(data = electorate_hexmap,
                            sf_id = "Elect_div",
                            hex_size = hs)  #%>%
# Just plot centroids
electorate_hexmap_current <- electorate_hexmap %>%
  left_join(current_major, by=c("Elect_div"="Electorate"))
ggplot() +
  geom_sf(data=electorates_small, fill="grey90", colour="white") +
  geom_point(data=electorate_hexmap_current,
             aes(x=longitude, y=latitude,
                 group = hex_id,
                 colour=Party_maj),
             size=1) +
  scale_colour_manual("", values=c("ALP"="#E13940",
                            "LNP"="#1C4F9C",
                            "GRN"="#009C3D",
                            "KAP"="#FDDA0D",
                            "CA"="#FFC000",
                            "IND"="#66b2b2")) +
  theme_map()

