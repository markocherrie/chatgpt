library(SpatialKDE)
library(sp)
library(sf)
library(dplyr)
library(tmap)


## overpass API call
#node
#({{bbox}})
#[amenity=bicycle_parking];
#out;


pts <- st_read("data/bikeparking/bikeparking.geojson") %>%
  st_transform(27700)


cell_size <- 200
band_width <- 200

grid_pts <- pts %>%
  create_grid_hexagonal(cell_size = cell_size, side_offset = band_width)

kde <- pts %>%
  kde(band_width = band_width, kernel = "quartic", grid = grid_pts)

library(mapview)

kde<-kde[kde$kde_value>0.5,]

mapview(kde, alpha.regions=0.6, lwd =0, legend=F)