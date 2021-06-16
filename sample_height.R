library(sf)
library(tidyverse)

setwd("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/polygons")
list.files(pattern = "\\.shp$")

trees <- st_read(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/all_blocks_fixed_clipped.gpkg")
trees


go_catch <- st_read("GO_catch.shp") %>% 
  st_intersection(trees) %>% 
  st_drop_geometry() %>% 
  mutate(Type = "go_catch")
go_stream <- st_read("GO_StreamB50.shp") %>% 
  st_intersection(trees) %>% 
  st_drop_geometry()%>% 
  mutate(Type = "go_stream")
go_100 <- st_read("GO_100mB50.shp") %>% 
  st_intersection(trees) %>% 
  st_drop_geometry()%>% 
  mutate(Type = "go_100")
go_500 <- st_read("GO_500mB50.shp") %>% 
  st_intersection(trees) %>% 
  st_drop_geometry()%>% 
  mutate(Type = "go_500")

vy_catch <- st_read("VY_catch.shp") %>% 
  st_intersection(trees) %>% 
  st_drop_geometry()%>% 
  mutate(Type = "vy_catch")
vy_stream <- st_read("VY_StreamB50.shp") %>% 
  st_intersection(trees) %>% 
  st_drop_geometry()%>% 
  mutate(Type = "vy_stream")
vy_100 <- st_read("VY_100mB50.shp") %>% 
  st_intersection(trees) %>% 
  st_drop_geometry()%>% 
  mutate(Type = "vy_100")
vy_500 <- st_read("VY_500mB50.shp") %>% 
  st_intersection(trees) %>% 
  st_drop_geometry()%>% 
  mutate(Type = "vy_500")


# max_catch <- go_catch %>% 
#   bind_rows(vy_catch) %>% 
#   st_union() %>% 
#   st_buffer(dist = 1000)
# plot(max_catch)
# 
# tree_clip <- st_intersection(trees, max_catch)
# st_is_valid(trees)

# t <- go_100 %>% st_intersection(trees) %>% st_drop_geometry()
