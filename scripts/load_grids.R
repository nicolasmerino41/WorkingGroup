library(sf)
library(dplyr)
library(ggplot2)

# Read the 10-km layer
st_layers("data/grids/osbng_grids.gpkg")

grid10 <- st_read(
  "data/grids/osbng_grids.gpkg",
  layer = "10km_grid"
)

# Plot
ggplot(grid10) +
  geom_sf(fill = "grey95",
          colour = "grey70",
          linewidth = 0.1) +
  coord_sf(datum = NA) +
  theme_void()