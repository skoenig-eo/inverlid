library(sf)
library(tidyverse)

all_blocks <- st_read(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/all_blocks_fixed_clipped.gpkg")

sizes_go <- read_csv("D:/OneDrive/repositories/inverlid/data/sizes_go.csv")
sizes_vy <- read_csv("D:/OneDrive/repositories/inverlid/data/sizes_vy.csv")

# conif and decid
go_100 <- st_read(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/polygons/GO_100mB50.shp") %>% 
  st_intersection(all_blocks %>% filter(TREE_CLASS %in% c("conif", "decid"))) %>% 
  group_by(LocCode) %>% 
  summarise() %>% 
  mutate(Area = as.numeric(st_area(geometry))) %>% 
  left_join(sizes_go, by = "LocCode") %>% 
  mutate(Share = Area/go_100) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  select(LocCode, Area, Share) %>% 
  mutate(River = "GO", Class = 100) %>% 
  rename(Area_full = Area, Share_full = Share)

