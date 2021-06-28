library(sf)
library(raster)
library(tidyverse)

cover <- read_stars(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/understory/LIDAR2017_Cover_NPBW.tif")

shrub <- read_stars(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/understory/LIDAR2017_Shrub_NPBW.tif"
)

understory <- read_stars(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/understory/LIDAR2017_Understory_NPBW.tif"
)

setwd("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/polygons")
list.files(pattern = "\\.shp$")

go_100 <- cover %>% 
  st_extract(at =
               st_read("GO_100mB50.shp") %>% 
               st_transform(st_crs(cover)))
  