library(sf)
library(tidyverse)

habitats <- st_read(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/habitats/full_habitats.shp") %>% 
  filter(ID %in% c("mdw_c", "mdw_n", "mdw_w"))

setwd("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/polygons")


# GO
go_100 <- st_read("GO_100mB50.shp") %>% 
  st_transform(st_crs(habitats)) %>% 
  st_intersection(habitats) %>% 
  mutate(Area = st_area(geometry)) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  group_by(LocCode, ID) %>% 
  summarise(Area_sum = sum(Area)) %>% 
  spread(ID, Area_sum) %>% 
  mutate(River = "GO",
         Class = "100")

go_500 <- st_read("GO_500mB50.shp") %>% 
  st_transform(st_crs(habitats)) %>% 
  st_intersection(habitats) %>% 
  mutate(Area = st_area(geometry)) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  group_by(LocCode, ID) %>% 
  summarise(Area_sum = sum(Area)) %>% 
  spread(ID, Area_sum) %>% 
  mutate(River = "GO",
         Class = "500")

go_catch <- st_read("GO_catch.shp") %>% 
  st_transform(st_crs(habitats)) %>%
  st_intersection(habitats) %>% 
  mutate(Area = st_area(geometry)) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  group_by(LocCode, ID) %>% 
  summarise(Area_sum = sum(Area)) %>% 
  spread(ID, Area_sum) %>% 
  mutate(River = "GO",
         Class = "Catch")

go_stream <- st_read("GO_StreamB50.shp") %>% 
  st_transform(st_crs(habitats)) %>% 
  st_intersection(habitats) %>% 
  mutate(Area = st_area(geometry)) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  group_by(LocCode, ID) %>% 
  summarise(Area_sum = sum(Area)) %>% 
  spread(ID, Area_sum) %>% 
  mutate(River = "GO",
         Class = "Stream")

# VY
vy_100 <- st_read("VY_100mB50.shp") %>% 
  st_transform(st_crs(habitats)) %>% 
  st_intersection(habitats) %>% 
  mutate(Area = st_area(geometry)) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  group_by(LocCode, ID) %>% 
  summarise(Area_sum = sum(Area)) %>% 
  spread(ID, Area_sum) %>% 
  mutate(River = "VY",
         Class = "100")

vy_500 <- st_read("VY_500mB50.shp") %>% 
  st_transform(st_crs(habitats)) %>% 
  st_intersection(habitats) %>% 
  mutate(Area = st_area(geometry)) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  group_by(LocCode, ID) %>% 
  summarise(Area_sum = sum(Area)) %>% 
  spread(ID, Area_sum) %>% 
  mutate(River = "VY",
         Class = "500")

vy_catch <- st_read("VY_catch.shp") %>% 
  st_transform(st_crs(habitats)) %>% 
  st_intersection(habitats) %>% 
  mutate(Area = st_area(geometry)) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  group_by(LocCode, ID) %>% 
  summarise(Area_sum = sum(Area)) %>% 
  spread(ID, Area_sum) %>% 
  mutate(River = "VY",
         Class = "Catch")

vy_stream <- st_read("VY_StreamB50.shp") %>% 
  st_transform(st_crs(habitats)) %>%
  st_intersection(habitats) %>% 
  mutate(Area = st_area(geometry)) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  group_by(LocCode, ID) %>% 
  summarise(Area_sum = sum(Area)) %>% 
  spread(ID, Area_sum) %>% 
  mutate(River = "VY",
         Class = "Stream")

catch_comb <- bind_rows(
  go_100, go_500, go_catch, go_stream,
  vy_100, vy_500, vy_catch, vy_stream)

rm(go_100, go_500, go_catch, go_stream,
   vy_100, vy_500, vy_catch, vy_stream)

sizes_go <- read_csv("D:/OneDrive/repositories/inverlid/sizes_go.csv")
sizes_vy <- read_csv("D:/OneDrive/repositories/inverlid/sizes_vy.csv")

sizes <- sizes_go %>% 
  select(-LocCode) %>% 
  gather() %>% bind_rows(
    sizes_vy %>% 
      select(-LocCode) %>% 
      gather()
  ) %>% 
  select(value) %>% 
  rename(Area = value)

result <- read_csv("D:/OneDrive/repositories/inverlid/results_biomass.csv") %>% 
  select(LocCode, River, Class) %>% 
  left_join(catch_comb, by = c("LocCode", "River", "Class")) %>%
  bind_cols(sizes) %>% 
  mutate(mdw_c = replace_na(as.numeric(mdw_c), 0)/Area,
         mdw_n = replace_na(as.numeric(mdw_n), 0)/Area,
         mdw_w = replace_na(as.numeric(mdw_w), 0)/Area,
         LC_meadow = mdw_c + mdw_n,
         LC_mire = mdw_w,
         LC_grassland = mdw_c + mdw_n + mdw_w) %>% 
  select(LocCode, River, Class, LC_meadow, LC_mire, LC_grassland)
