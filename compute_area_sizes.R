library(sf)
library(tidyverse)

setwd("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/polygons")
poly_list <- list.files(pattern = "\\.shp$")

areas <- list()

for (i in 1:length(poly_list)) {
  areas[[i]] <- st_read(poly_list[i]) %>% 
    mutate(area = st_area(geometry)) %>% 
    st_drop_geometry()
}

sizes_go <- areas[[1]] %>% 
  left_join(areas[[2]], "LocCode") %>% 
  left_join(areas[[3]], "LocCode") %>% 
  left_join(areas[[4]], "LocCode") %>% 
  rename(go_100 = 1, go_500 = 2, go_catch = 3, go_stream = 4) %>% 
  write_csv(
    "D:/OneDrive/repositories/inverlid/sizes_go.csv"
  )

sizes_vy <- areas[[5]] %>% 
  left_join(areas[[6]], "LocCode") %>% 
  left_join(areas[[7]], "LocCode") %>% 
  left_join(areas[[8]], "LocCode") %>% 
  rename(vy_100 = 1, vy_500 = 2, vy_catch = 3, vy_stream = 4) %>% 
  write_csv(
    "D:/OneDrive/repositories/inverlid/sizes_vy.csv"
  )

# area_names <- c(
#   "go_100",
#   "go_500",
#   "go_catch",
#   "go_stream",
#   "vy_100",
#   "vy_500",
#   "vy_catch",
#   "vy_stream"
# )

# area_result <- tibble(
#   area_names,
#   unlist(areas)) %>% 
#   rename(Area = 1, Size = 2) %>% 
#   write_csv("D:/OneDrive/repositories/inverlid/area_sizes.csv")
