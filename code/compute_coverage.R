# Header --------------------------------------------
#
# Original Author: Simon König, simon.koenig@npv-bw.bayern.de 
# Created on: 2022-04-24
# 
# Last modified by: Simon König, simon.koenig@npv-bw.bayern.de 
# On: 2022-05-02
#
# Script Description: estimate coverage
#
# Notes: 
#
#



# load libraries and basic data ------------------------------

library(sf)
library(tidyverse)

all_blocks <- st_read(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/all_blocks_fixed_clipped.gpkg")

sizes_go <- read_csv("D:/OneDrive/repositories/inverlid/data/sizes_go.csv")
sizes_vy <- read_csv("D:/OneDrive/repositories/inverlid/data/sizes_vy.csv")



# estimate for conif & decid combined -------------------------

# Große Ohe
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
  mutate(River = "GO", Class = "100") %>% 
  rename(Area_full = Area, Share_full = Share)

go_500 <- st_read(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/polygons/GO_500mB50.shp") %>% 
  st_intersection(all_blocks %>% filter(TREE_CLASS %in% c("conif", "decid"))) %>% 
  group_by(LocCode) %>% 
  summarise() %>% 
  mutate(Area = as.numeric(st_area(geometry))) %>% 
  left_join(sizes_go, by = "LocCode") %>% 
  mutate(Share = Area/go_500) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  select(LocCode, Area, Share) %>% 
  mutate(River = "GO", Class = "500") %>% 
  rename(Area_full = Area, Share_full = Share)

go_catch <- st_read(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/polygons/GO_catch.shp") %>% 
  st_intersection(all_blocks %>% filter(TREE_CLASS %in% c("conif", "decid"))) %>% 
  group_by(LocCode) %>% 
  summarise() %>% 
  mutate(Area = as.numeric(st_area(geometry))) %>% 
  left_join(sizes_go, by = "LocCode") %>% 
  mutate(Share = Area/go_catch) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  select(LocCode, Area, Share) %>% 
  mutate(River = "GO", Class = "Catch") %>% 
  rename(Area_full = Area, Share_full = Share)

go_stream <- st_read(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/polygons/GO_StreamB50.shp") %>% 
  st_intersection(all_blocks %>% filter(TREE_CLASS %in% c("conif", "decid"))) %>% 
  group_by(LocCode) %>% 
  summarise() %>% 
  mutate(Area = as.numeric(st_area(geometry))) %>% 
  left_join(sizes_go, by = "LocCode") %>% 
  mutate(Share = Area/go_stream) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  select(LocCode, Area, Share) %>% 
  mutate(River = "GO", Class = "Stream") %>% 
  rename(Area_full = Area, Share_full = Share)

go_results <- bind_rows(go_100, go_500, go_catch, go_stream)
write_csv(go_results,
          "data/go_results_coverage.csv")

# Vydra
vy_100 <- st_read(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/polygons/VY_100mB50.shp") %>% 
  st_intersection(all_blocks %>% filter(TREE_CLASS %in% c("conif", "decid"))) %>% 
  group_by(LocCode) %>% 
  summarise() %>% 
  mutate(Area = as.numeric(st_area(geometry))) %>% 
  left_join(sizes_vy, by = "LocCode") %>% 
  mutate(Share = Area/vy_100) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  select(LocCode, Area, Share) %>% 
  mutate(River = "VY", Class = "100") %>% 
  rename(Area_full = Area, Share_full = Share)

vy_500 <- st_read(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/polygons/VY_500mB50.shp") %>% 
  st_intersection(all_blocks %>% filter(TREE_CLASS %in% c("conif", "decid"))) %>% 
  group_by(LocCode) %>% 
  summarise() %>% 
  mutate(Area = as.numeric(st_area(geometry))) %>% 
  left_join(sizes_vy, by = "LocCode") %>% 
  mutate(Share = Area/vy_500) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  select(LocCode, Area, Share) %>% 
  mutate(River = "VY", Class = "500") %>% 
  rename(Area_full = Area, Share_full = Share)

vy_catch <- st_read(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/polygons/VY_catch.shp") %>% 
  st_intersection(all_blocks %>% filter(TREE_CLASS %in% c("conif", "decid"))) %>% 
  group_by(LocCode) %>% 
  summarise() %>% 
  mutate(Area = as.numeric(st_area(geometry))) %>% 
  left_join(sizes_vy, by = "LocCode") %>% 
  mutate(Share = Area/vy_catch) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  select(LocCode, Area, Share) %>% 
  mutate(River = "VY", Class = "Catch") %>% 
  rename(Area_full = Area, Share_full = Share)

vy_stream <- st_read(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/polygons/VY_StreamB50.shp") %>% 
  st_intersection(all_blocks %>% filter(TREE_CLASS %in% c("conif", "decid"))) %>% 
  group_by(LocCode) %>% 
  summarise() %>% 
  mutate(Area = as.numeric(st_area(geometry))) %>% 
  left_join(sizes_vy, by = "LocCode") %>% 
  mutate(Share = Area/vy_stream) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  select(LocCode, Area, Share) %>% 
  mutate(River = "VY", Class = "Stream") %>% 
  rename(Area_full = Area, Share_full = Share)

vy_results <- bind_rows(vy_100, vy_500, vy_catch, vy_stream) %>% 
  mutate(River = "VY")
write_csv(vy_results,
          "data/vy_results_coverage.csv")


# combine results
results <- bind_rows(go_results, vy_results)
write_csv(results,
          "data/results_coverage.csv")



# coniferous only --------------------------------------

# Große Ohe
go_100_c <- st_read(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/polygons/GO_100mB50.shp") %>% 
  st_intersection(all_blocks %>% filter(TREE_CLASS %in% c("conif"))) %>% 
  group_by(LocCode) %>% 
  summarise() %>% 
  mutate(Area = as.numeric(st_area(geometry))) %>% 
  left_join(sizes_go, by = "LocCode") %>% 
  mutate(Share = Area/go_100) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  select(LocCode, Area, Share) %>% 
  mutate(River = "GO", Class = "100") %>% 
  rename(Area_full = Area, Share_full = Share)

go_500_c <- st_read(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/polygons/GO_500mB50.shp") %>% 
  st_intersection(all_blocks %>% filter(TREE_CLASS %in% c("conif"))) %>% 
  group_by(LocCode) %>% 
  summarise() %>% 
  mutate(Area = as.numeric(st_area(geometry))) %>% 
  left_join(sizes_go, by = "LocCode") %>% 
  mutate(Share = Area/go_500) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  select(LocCode, Area, Share) %>% 
  mutate(River = "GO", Class = "500") %>% 
  rename(Area_full = Area, Share_full = Share)

go_catch_c <- st_read(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/polygons/GO_catch.shp") %>% 
  st_intersection(all_blocks %>% filter(TREE_CLASS %in% c("conif"))) %>% 
  group_by(LocCode) %>% 
  summarise() %>% 
  mutate(Area = as.numeric(st_area(geometry))) %>% 
  left_join(sizes_go, by = "LocCode") %>% 
  mutate(Share = Area/go_catch) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  select(LocCode, Area, Share) %>% 
  mutate(River = "GO", Class = "Catch") %>% 
  rename(Area_full = Area, Share_full = Share)

go_stream_c <- st_read(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/polygons/GO_StreamB50.shp") %>% 
  st_intersection(all_blocks %>% filter(TREE_CLASS %in% c("conif"))) %>% 
  group_by(LocCode) %>% 
  summarise() %>% 
  mutate(Area = as.numeric(st_area(geometry))) %>% 
  left_join(sizes_go, by = "LocCode") %>% 
  mutate(Share = Area/go_stream) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  select(LocCode, Area, Share) %>% 
  mutate(River = "GO", Class = "Stream") %>% 
  rename(Area_full = Area, Share_full = Share)

go_results_c <- bind_rows(go_100_c, go_500_c, go_catch_c, go_stream_c)
write_csv(go_results_c,
          "data/go_results_coverage_c.csv")

# Vydra
vy_100_c <- st_read(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/polygons/VY_100mB50.shp") %>% 
  st_intersection(all_blocks %>% filter(TREE_CLASS %in% c("conif"))) %>% 
  group_by(LocCode) %>% 
  summarise() %>% 
  mutate(Area = as.numeric(st_area(geometry))) %>% 
  left_join(sizes_vy, by = "LocCode") %>% 
  mutate(Share = Area/vy_100) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  select(LocCode, Area, Share) %>% 
  mutate(River = "VY", Class = "100") %>% 
  rename(Area_full = Area, Share_full = Share)

vy_500_c <- st_read(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/polygons/VY_500mB50.shp") %>% 
  st_intersection(all_blocks %>% filter(TREE_CLASS %in% c("conif"))) %>% 
  group_by(LocCode) %>% 
  summarise() %>% 
  mutate(Area = as.numeric(st_area(geometry))) %>% 
  left_join(sizes_vy, by = "LocCode") %>% 
  mutate(Share = Area/vy_500) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  select(LocCode, Area, Share) %>% 
  mutate(River = "VY", Class = "500") %>% 
  rename(Area_full = Area, Share_full = Share)

vy_catch_c <- st_read(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/polygons/VY_catch.shp") %>% 
  st_intersection(all_blocks %>% filter(TREE_CLASS %in% c("conif"))) %>% 
  group_by(LocCode) %>% 
  summarise() %>% 
  mutate(Area = as.numeric(st_area(geometry))) %>% 
  left_join(sizes_vy, by = "LocCode") %>% 
  mutate(Share = Area/vy_catch) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  select(LocCode, Area, Share) %>% 
  mutate(River = "VY", Class = "Catch") %>% 
  rename(Area_full = Area, Share_full = Share)

vy_stream_c <- st_read(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/polygons/VY_StreamB50.shp") %>% 
  st_intersection(all_blocks %>% filter(TREE_CLASS %in% c("conif"))) %>% 
  group_by(LocCode) %>% 
  summarise() %>% 
  mutate(Area = as.numeric(st_area(geometry))) %>% 
  left_join(sizes_vy, by = "LocCode") %>% 
  mutate(Share = Area/vy_stream) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  select(LocCode, Area, Share) %>% 
  mutate(River = "VY", Class = "Stream") %>% 
  rename(Area_full = Area, Share_full = Share)

vy_results_c <- bind_rows(vy_100_c, vy_500_c, vy_catch_c, vy_stream_c)# %>% 
  #mutate(River = "VY")
write_csv(vy_results_c,
          "data/vy_results_coverage_c.csv")


# combine results
results <- bind_rows(go_results_c, vy_results_c)
write_csv(results_c,
          "data/results_coverage_c.csv")