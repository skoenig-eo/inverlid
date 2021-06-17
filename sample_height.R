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
  as_tibble() %>% 
  mutate(Type = "go_catch") %>% 
  write_csv(
    "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/extracted/go_catch.csv")

go_stream <- st_read("GO_StreamB50.shp") %>% 
  st_intersection(trees) %>% 
  st_drop_geometry()%>% 
  as_tibble() %>% 
  mutate(Type = "go_stream") %>% 
  write_csv(
    "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/extracted/go_stream.csv")

go_100 <- st_read("GO_100mB50.shp") %>% 
  st_intersection(trees) %>% 
  st_drop_geometry()%>% 
  as_tibble() %>% 
  mutate(Type = "go_100") %>% 
  write_csv(
    "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/extracted/go_100.csv")

go_500 <- st_read("GO_500mB50.shp") %>% 
  st_intersection(trees) %>% 
  st_drop_geometry()%>% 
  as_tibble() %>% 
  mutate(Type = "go_500") %>% 
  write_csv(
    "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/extracted/go_500.csv")

vy_catch <- st_read("VY_catch.shp") %>% 
  st_intersection(trees) %>% 
  st_drop_geometry()%>% 
  mutate(Type = "vy_catch") %>% 
  write_csv(
    "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/extracted/vy_catch.csv")
rm(vy_catch)
gc()

# use already clipped file because it takes too long otherwise
vy_stream <- st_read("vy_stream.gpkg") %>% 
  st_drop_geometry()%>% 
  as_tibble() %>% 
  mutate(Type = "vy_stream") %>% 
  write_csv(
    "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/extracted/vy_stream.csv")
rm(vy_stream)
gc()

vy_100 <- st_read("VY_100mB50.shp") %>% 
  st_intersection(trees) %>% 
  st_drop_geometry()%>%
  as_tibble() %>% 
  mutate(Type = "vy_100") %>% 
  write_csv(
    "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/extracted/vy_100.csv")

vy_500 <- st_read("VY_500mB50.shp") %>% 
  st_intersection(trees) %>% 
  st_drop_geometry()%>% 
  as_tibble() %>% 
  mutate(Type = "vy_500") %>% 
  write_csv(
    "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/extracted/vy_500.csv")


# max_catch <- go_catch %>% 
#   bind_rows(vy_catch) %>% 
#   st_union() %>% 
#   st_buffer(dist = 1000)
# plot(max_catch)
# 
# tree_clip <- st_intersection(trees, max_catch)
# st_is_valid(trees)

# t <- go_100 %>% st_intersection(trees) %>% st_drop_geometry()
