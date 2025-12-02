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

unclean.norf <- rast("Data/VA_Southern_GCS_3m_NAVDm.tif")

bbox <- ext(c(xmin = -76.392655, xmax = -76.388909, ymin = 36.832284, ymax = 36.835033))
small.sample.rast <- mask(unclean.norf, bbox)
small.sample.rast <- trim(small.sample.rast)
plot(small.sample.rast)
small.sample.rast <- raster::raster(small.sample.rast)
plot3D(small.sample.rast, maxpixels = 1e7)

rast.small <- rast("Data/unclean-norf-trimmed.tif")
rast.small <- raster::raster(rast.small)
plot3D(rast.small, zfac = 0.1, maxpixels = 1e6)




rast.big <- rast("Data/VA_Southern_GCS_3m_NAVDm.tif")
rast.big5 <- terra::aggregate(rast.big, fact = 5)
rast.big5 <- raster::raster(rast.big5)
plot(rast.big5)

plot3D(rast.big5)

rast.big20 <- aggregate(rast.big, fact = 20)
rast.big20 <- raster::raster(rast.big20)

plot3D(rast.big20, maxpixels=1e6, zfac = 0.1)
