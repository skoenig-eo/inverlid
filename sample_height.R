library(sf)
library(tidyverse)

setwd("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/polygons")
list.files(pattern = "\\.shp$")

go_catch <- st_read("GO_catch.shp")
go_stream <- st_read("GO_StreamB50.shp")
go_100 <- st_read("GO_100mB50.shp")
go_500 <- st_read("GO_500mB50.shp")

vy_catch <- st_read("VY_catch.shp")
vy_stream <- st_read("VY_StreamB50.shp")
vy_100 <- st_read("VY_100mB50.shp")
vy_500 <- st_read("VY_500mB50.shp")

trees <- st_read(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/all_blocks_fixed_clipped.gpkg")
trees

# max_catch <- go_catch %>% 
#   bind_rows(vy_catch) %>% 
#   st_union() %>% 
#   st_buffer(dist = 1000)
# plot(max_catch)
# 
# tree_clip <- st_intersection(trees, max_catch)
# st_is_valid(trees)
