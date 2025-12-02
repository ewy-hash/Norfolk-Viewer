# install.packages("rasterVis")
library(tidyverse)
library(tidyterra)
library(sf)
library(terra)
library(rasterVis)
library(rgl)


#small raster of area
rast.small5 <- rast("Data/raster-small5.tif")
rast.small5 <- raster::raster(rast.small5)

rast.big <- rast("Data/VA_Southern_GCS_3m_NAVDm.tif")
rast.big5 <- terra::aggregate(rast.big, fact = 5)
rast.big5 <- raster::raster(rast.big5)
plot(rast.big5)

plot3D(rast.big5)

rast.big20 <- aggregate(rast.big, fact = 20)
rast.big20 <- raster::raster(rast.big20)

plot3D(rast.big20, maxpixels=1e6, zfac = 0.1)
