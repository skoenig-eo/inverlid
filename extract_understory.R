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
