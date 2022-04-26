library(sf)
library(tidyverse)

all_blocks <- st_read(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/all_blocks_fixed_clipped.gpkg")



go_100 <- st_read(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/polygons/GO_100mB50.shp") %>% 
  st_intersection(all_blocks %>% filter(TREE_CLASS %in% c("conif", "decid"))) %>% 
  group_by(LocCode) %>% 
  summarise() %>% 
  mutate(Area = st_area(geometry))

