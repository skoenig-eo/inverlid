library(sf)
library(raster)
library(tidyverse)

cover <- raster(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/understory/LIDAR2017_Cover_NPBW.tif")

shrub <- raster(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/understory/LIDAR2017_Shrub_NPBW.tif"
)

understory <- raster(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/understory/LIDAR2017_Understory_NPBW.tif"
)

setwd("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/polygons")
list.files(pattern = "\\.shp$")

go_100 <- cover %>% 
  extract(st_read("GO_100mB50.shp"))
  