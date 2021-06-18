library(sf)
library(tidyverse)

setwd("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/polygons")
poly_list <- list.files(pattern = "\\.shp$")

areas <- list()

for (i in 1:length(poly_list)) {
  areas[[i]] <- st_read(poly_list[[i]]) %>% st_area() %>% sum(na.rm = TRUE)
}

area_names <- c(
  "go_100",
  "go_500",
  "go_catch",
  "go_stream",
  "vy_100",
  "vy_500",
  "vy_catch",
  "vy_stream"
)

area_result <- tibble(
  area_names,
  unlist(areas)) %>% 
  rename(Area = 1, Size = 2) %>% 
  write_csv("D:/OneDrive/repositories/inverlid/area_sizes.csv")
