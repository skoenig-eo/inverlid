library(sf)
library(tidyverse)

habitats <- st_read(
  "D:/OneDrive/NPBW/Potenzielle Untersuchungsflächen 2021/reference_data.gpkg",
  layer = "habitats") %>% 
  rename(Class = KLS_ENG) %>% 
  mutate(Class_broad = case_when(Class == "Clear-cut area" ~ "Clearcut", 
                                 Class == "Coniferous stand - mature" ~ "Coniferous",
                                 Class == "Coniferous stand - medium" ~ "Coniferous",
                                 Class == "Coniferous stand - young" ~ "Coniferous",
                                 Class == "Dead wood - lying" ~ "Deadwood",
                                 Class == "Dead wood - lying - coniferous regeneration" ~ "Coniferous",
                                 Class == "Dead wood - lying - deciduous regeneration" ~ "Deciduous",
                                 Class == "Dead wood - standing" ~ "Deadwood",
                                 Class == "Deciduous stand - mature" ~ "Deciduous",
                                 Class == "Deciduous stand - medium" ~ "Deciduous",
                                 Class == "Deciduous stand - young" ~ "Deciduous",
                                 Class == "Ecotone" ~ "Ecotone",
                                 Class == "Meadow - cultivated" ~ "Meadow",
                                 Class == "Meadow - natural" ~ "Meadow",
                                 Class == "Meadow - wetland" ~ "Wetland",
                                 Class == "Mixed stand - mature" ~ "Mixed",
                                 Class == "Mixed stand - medium" ~ "Mixed",
                                 Class == "Mixed stand - young" ~ "Mixed",
                                 Class == "Railway" ~ "Built-up",
                                 Class == "Residential area" ~ "Built-up",
                                 Class == "Road" ~ "Built-up",
                                 Class == "Rock" ~ "Rock",
                                 Class == "Scrub pine" ~ "Shrub",
                                 Class == "Water body"  ~ "Water",
                                 TRUE ~ Class))

setwd("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/polygons")

sizes_go <- read_csv("D:/OneDrive/repositories/inverlid/sizes_go.csv")
sizes_vy <- read_csv("D:/OneDrive/repositories/inverlid/sizes_vy.csv")

go_100 <- st_read("GO_100mB50.shp") %>% 
  st_transform(crs = st_crs(habitats)) %>% 
  st_intersection(habitats) %>% 
  mutate(Area = st_area(geometry)) %>% 
  group_by(LocCode, Class_broad) %>% 
  summarise(Total_Area = sum(Area)) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  mutate(Total_Area = as.numeric(Total_Area)) %>% 
  pivot_wider(names_from = Class_broad, values_from = Total_Area) %>% 
  select(LocCode, Coniferous, Deciduous, Deadwood, Mixed, Meadow) %>% 
  left_join(sizes_go %>% select(LocCode, go_100)) %>% 
  rename(Area = go_100) %>%
  transmute(LocCode = LocCode,
            AS_Coniferous = Coniferous/Area,
            AS_Deciduous = Deciduous/Area,
            AS_Deadwood = Deadwood/Area,
            AS_Mixed = Mixed/Area,
            AS_Meadow = Meadow/Area,
            River = "GO",
            Class = "100")

go_500 <- st_read("GO_500mB50.shp") %>% 
  st_transform(crs = st_crs(habitats)) %>% 
  st_intersection(habitats) %>% 
  mutate(Area = st_area(geometry)) %>% 
  group_by(LocCode, Class_broad) %>% 
  summarise(Total_Area = sum(Area)) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  mutate(Total_Area = as.numeric(Total_Area)) %>% 
  pivot_wider(names_from = Class_broad, values_from = Total_Area) %>% 
  select(LocCode, Coniferous, Deciduous, Deadwood, Mixed, Clearcut, Meadow, Shrub) %>% 
  left_join(sizes_go %>% select(LocCode, go_500)) %>% 
  rename(Area = go_500) %>%
  transmute(LocCode = LocCode,
            AS_Coniferous = Coniferous/Area,
            AS_Deciduous = Deciduous/Area,
            AS_Deadwood = Deadwood/Area,
            AS_Mixed = Mixed/Area,
            AS_Clearcut = Clearcut/Area,
            AS_Meadow = Meadow/Area,
            AS_Shrub = Shrub/Area,
            River = "GO",
            Class = "500")

go_stream <- st_read("GO_stream.shp") %>% 
  st_transform(crs = st_crs(habitats)) %>% 
  st_intersection(habitats) %>% 
  mutate(Area = st_area(geometry)) %>% 
  group_by(LocCode, Class_broad) %>% 
  summarise(Total_Area = sum(Area)) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  mutate(Total_Area = as.numeric(Total_Area)) %>% 
  pivot_wider(names_from = Class_broad, values_from = Total_Area) %>% 
  select(LocCode, Coniferous, Deciduous, Deadwood, Mixed, Clearcut, Meadow, Shrub) %>% 
  left_join(sizes_go %>% select(LocCode, go_stream)) %>% 
  rename(Area = go_stream) %>%
  transmute(LocCode = LocCode,
            AS_Coniferous = Coniferous/Area,
            AS_Deciduous = Deciduous/Area,
            AS_Deadwood = Deadwood/Area,
            AS_Mixed = Mixed/Area,
            AS_Clearcut = Clearcut/Area,
            AS_Meadow = Meadow/Area,
            AS_Shrub = Shrub/Area,
            River = "GO",
            Class = "stream")

go_catch <- st_read("GO_catch.shp") %>% 
  st_transform(crs = st_crs(habitats)) %>% 
  st_intersection(habitats) %>% 
  mutate(Area = st_area(geometry)) %>% 
  group_by(LocCode, Class_broad) %>% 
  summarise(Total_Area = sum(Area)) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  mutate(Total_Area = as.numeric(Total_Area)) %>% 
  pivot_wider(names_from = Class_broad, values_from = Total_Area) %>% 
  select(LocCode, Coniferous, Deciduous, Deadwood, Mixed, Clearcut, Meadow, Shrub) %>% 
  left_join(sizes_go %>% select(LocCode, go_catch)) %>% 
  rename(Area = go_catch) %>%
  transmute(LocCode = LocCode,
            AS_Coniferous = Coniferous/Area,
            AS_Deciduous = Deciduous/Area,
            AS_Deadwood = Deadwood/Area,
            AS_Mixed = Mixed/Area,
            AS_Clearcut = Clearcut/Area,
            AS_Meadow = Meadow/Area,
            AS_Shrub = Shrub/Area,
            River = "GO",
            Class = "Catch")

