library(raster)
library(tidyverse)



setwd("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/polygons")

sizes_go <- read_csv("D:/OneDrive/repositories/inverlid/sizes_go.csv")
sizes_vy <- read_csv("D:/OneDrive/repositories/inverlid/sizes_vy.csv")

make_table <- function(i){
  j <- i %>% as_factor() %>% table() %>% as_tibble() %>% rename(Class = 1, Count = 2) %>% 
    pivot_wider(names_from = Class, values_from = Count)
  return(j)
}
  

go_100 <- raster("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/rasters/s2lc_reprojected.tif") %>% 
  extract(st_read("GO_100mB50.shp")) %>% 
  lapply(make_table) %>% 
  bind_rows(.id = "column_label") %>% 
  dplyr::select(-column_label) %>% 
  mutate(Pixels = rowSums(across(where(is.numeric)), na.rm = TRUE)) %>% 
  mutate_at(vars(-Pixels), funs(./Pixels)) %>% 
  bind_cols(sizes_go %>% dplyr::select(LocCode)) %>% 
  mutate(River = "GO",
         Class = "100")

go_500 <- raster("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/rasters/s2lc_reprojected.tif") %>% 
  extract(st_read("GO_500mB50.shp")) %>% 
  lapply(make_table) %>% 
  bind_rows(.id = "column_label")%>% 
  dplyr::select(-column_label) %>% 
  mutate(Pixels = rowSums(across(where(is.numeric)), na.rm = TRUE)) %>% 
  mutate_at(vars(-Pixels), funs(./Pixels)) %>% 
  bind_cols(sizes_go %>% dplyr::select(LocCode)) %>% 
  mutate(River = "GO",
         Class = "500")

go_stream <- raster("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/rasters/s2lc_reprojected.tif") %>% 
  extract(st_read("GO_StreamB50.shp")) %>% 
  lapply(make_table) %>% 
  bind_rows(.id = "column_label")%>% 
  dplyr::select(-column_label) %>% 
  mutate(Pixels = rowSums(across(where(is.numeric)), na.rm = TRUE)) %>% 
  mutate_at(vars(-Pixels), funs(./Pixels)) %>% 
  bind_cols(sizes_go %>% dplyr::select(LocCode)) %>% 
  mutate(River = "GO",
         Class = "Stream")

go_catch <- raster("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/rasters/s2lc_reprojected.tif") %>% 
  extract(st_read("GO_catch.shp")) %>% 
  lapply(make_table) %>% 
  bind_rows(.id = "column_label")%>% 
  dplyr::select(-column_label) %>% 
  mutate(Pixels = rowSums(across(where(is.numeric)), na.rm = TRUE)) %>% 
  mutate_at(vars(-Pixels), funs(./Pixels)) %>% 
  bind_cols(sizes_go %>% dplyr::select(LocCode)) %>% 
  mutate(River = "GO",
         Class = "Catch")

vy_100 <- raster("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/rasters/s2lc_reprojected.tif") %>% 
  extract(st_read("VY_100mB50.shp")) %>% 
  lapply(make_table) %>% 
  bind_rows(.id = "column_label")%>% 
  dplyr::select(-column_label) %>% 
  mutate(Pixels = rowSums(across(where(is.numeric)), na.rm = TRUE)) %>% 
  mutate_at(vars(-Pixels), funs(./Pixels)) %>% 
  bind_cols(sizes_vy %>% dplyr::select(LocCode)) %>% 
  mutate(River = "VY",
         Class = "100")

vy_500 <- raster("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/rasters/s2lc_reprojected.tif") %>% 
  extract(st_read("VY_500mB50.shp")) %>% 
  lapply(make_table) %>% 
  bind_rows(.id = "column_label")%>% 
  dplyr::select(-column_label) %>% 
  mutate(Pixels = rowSums(across(where(is.numeric)), na.rm = TRUE)) %>% 
  mutate_at(vars(-Pixels), funs(./Pixels)) %>% 
  bind_cols(sizes_vy %>% dplyr::select(LocCode)) %>% 
  mutate(River = "VY",
         Class = "500")

vy_stream <- raster("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/rasters/s2lc_reprojected.tif") %>% 
  extract(st_read("VY_StreamB50.shp")) %>% 
  lapply(make_table) %>% 
  bind_rows(.id = "column_label")%>% 
  dplyr::select(-column_label) %>% 
  mutate(Pixels = rowSums(across(where(is.numeric)), na.rm = TRUE)) %>% 
  mutate_at(vars(-Pixels), funs(./Pixels)) %>% 
  bind_cols(sizes_vy %>% dplyr::select(LocCode)) %>% 
  mutate(River = "VY",
         Class = "Stream")

vy_catch <- raster("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/rasters/s2lc_reprojected.tif") %>% 
  extract(st_read("VY_catch.shp")) %>% 
  lapply(make_table) %>% 
  bind_rows(.id = "column_label")%>% 
  dplyr::select(-column_label) %>% 
  mutate(Pixels = rowSums(across(where(is.numeric)), na.rm = TRUE)) %>% 
  mutate_at(vars(-Pixels), funs(./Pixels)) %>% 
  bind_cols(sizes_vy %>% dplyr::select(LocCode)) %>% 
  mutate(River = "VY",
         Class = "Catch")

landcover_result <- bind_rows(
  go_100, go_500, go_stream, go_catch,
  vy_100, vy_500, vy_stream, vy_catch) %>% 
  rename(Cultivated = 1, Deciduous = 2, Coniferous = 3, Marshes = 4,
         Peatbogs = 5, Herbaceous = 6, Water = 7, Built_up = 12) %>% 
  write_csv("D:/OneDrive/repositories/inverlid/landcover_results.csv")


# go_100 %>% 
#   dplyr::select(-column_label) %>% 
#   mutate(Pixels = rowSums(across(where(is.numeric)), na.rm = TRUE)) %>% 
#   mutate_at(vars(-Pixels), funs(./Pixels)) %>% 
#   bind_cols(sizes_go %>% pull(LocCode)) %>% 
#   mutate(River = "GO",
#          Class = "100")
