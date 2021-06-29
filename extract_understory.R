library(sf)
library(raster)
library(tidyverse)

cover <- raster(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/understory/LIDAR2017_Cover_NPBW.tif")

shrub <- raster(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/understory/LIDAR2017_Shrub_NPBW.tif"
)

understory <- raster(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/understory/LIDAR2017_Understory_NPBW.tif"
)

setwd("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/polygons")
list.files(pattern = "\\.shp$")

compute_stats <- function(vector){
  output <- vector %>% 
    as_tibble() %>% 
    rename(Values = 1) %>% 
    summarise(Count = n(),
              Max = max(Values, na.rm = TRUE),
              Min = min(Values, na.rm = TRUE),
              Avg = mean(Values, na.rm = TRUE),
              Med = median(Values, na.rm = TRUE),
              Std = sd(Values, na.rm = TRUE))
  return(output)
}

# GO 100
go_100 <- st_read("GO_100mB50.shp")

go_100_result <- cover %>% 
  extract(go_100) %>% 
  lapply(compute_stats) %>% 
  bind_rows() %>% 
  rename_with(.fn = ~ paste(., "cover", sep = "_")) %>% 
  bind_cols(shrub %>% 
              extract(go_100) %>% 
              lapply(compute_stats) %>% 
              bind_rows() %>% 
              rename_with(.fn = ~ paste(., "shrub", sep = "_"))) %>% 
  bind_cols(understory %>% 
              extract(go_100) %>% 
              lapply(compute_stats) %>% 
              bind_rows() %>% 
              rename_with(.fn = ~ paste(., "understory", sep = "_"))) %>% 
  bind_cols(go_100 %>% dplyr::select(LocCode) %>% st_drop_geometry()) %>% 
  mutate(River = "GO",
         Class = "100") %>% 
  dplyr::select(-c(Count_shrub, Count_understory)) %>% 
  rename(Count = Count_cover)

# GO 500
go_500 <- st_read("go_500mB50.shp")

go_500_result <- cover %>% 
  extract(go_500) %>% 
  lapply(compute_stats) %>% 
  bind_rows() %>% 
  rename_with(.fn = ~ paste(., "cover", sep = "_")) %>% 
  bind_cols(shrub %>% 
              extract(go_500) %>% 
              lapply(compute_stats) %>% 
              bind_rows() %>% 
              rename_with(.fn = ~ paste(., "shrub", sep = "_"))) %>% 
  bind_cols(understory %>% 
              extract(go_500) %>% 
              lapply(compute_stats) %>% 
              bind_rows() %>% 
              rename_with(.fn = ~ paste(., "understory", sep = "_"))) %>% 
  bind_cols(go_500 %>% dplyr::select(LocCode) %>% st_drop_geometry()) %>% 
  mutate(River = "GO",
         Class = "500") %>% 
  dplyr::select(-c(Count_shrub, Count_understory)) %>% 
  rename(Count = Count_cover)

# GO Stream
go_stream <- st_read("GO_StreamB50.shp")

go_stream_result <- cover %>% 
  extract(go_stream) %>% 
  lapply(compute_stats) %>% 
  bind_rows() %>% 
  rename_with(.fn = ~ paste(., "cover", sep = "_")) %>% 
  bind_cols(shrub %>% 
              extract(go_stream) %>% 
              lapply(compute_stats) %>% 
              bind_rows() %>% 
              rename_with(.fn = ~ paste(., "shrub", sep = "_"))) %>% 
  bind_cols(understory %>% 
              extract(go_stream) %>% 
              lapply(compute_stats) %>% 
              bind_rows() %>% 
              rename_with(.fn = ~ paste(., "understory", sep = "_"))) %>% 
  bind_cols(go_stream %>% dplyr::select(LocCode) %>% st_drop_geometry()) %>% 
  mutate(River = "GO",
         Class = "Stream") %>% 
  dplyr::select(-c(Count_shrub, Count_understory)) %>% 
  rename(Count = Count_cover)

# GO Catch
go_catch <- st_read("GO_catch.shp")

go_catch_result <- cover %>% 
  extract(go_catch) %>% 
  lapply(compute_stats) %>% 
  bind_rows() %>% 
  rename_with(.fn = ~ paste(., "cover", sep = "_")) %>% 
  bind_cols(shrub %>% 
              extract(go_catch) %>% 
              lapply(compute_stats) %>% 
              bind_rows() %>% 
              rename_with(.fn = ~ paste(., "shrub", sep = "_"))) %>% 
  bind_cols(understory %>% 
              extract(go_catch) %>% 
              lapply(compute_stats) %>% 
              bind_rows() %>% 
              rename_with(.fn = ~ paste(., "understory", sep = "_"))) %>% 
  bind_cols(go_catch %>% dplyr::select(LocCode) %>% st_drop_geometry()) %>% 
  mutate(River = "GO",
         Class = "Catch") %>% 
  dplyr::select(-c(Count_shrub, Count_understory)) %>% 
  rename(Count = Count_cover)

