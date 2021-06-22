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
  lapply(make_table)

go_500 <- raster("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/rasters/s2lc_reprojected.tif") %>% 
  extract(st_read("GO_500mB50.shp")) %>% 
  lapply(make_table)

go_stream <- raster("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/rasters/s2lc_reprojected.tif") %>% 
  extract(st_read("GO_StreamB50.shp")) %>% 
  lapply(make_table)

go_catch <- raster("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/rasters/s2lc_reprojected.tif") %>% 
  extract(st_read("GO_catch.shp")) %>% 
  lapply(make_table)

go_100[[1]] %>% 
  as_factor() %>% 
  table() %>%
  as_tibble() %>% 
  rename(Class = 1, Count = 2) %>% 
  pivot_wider(names_from = Class, values_from = Count)
  

