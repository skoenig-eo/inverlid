library(raster)
library(sf)
library(tidyverse)

setwd("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/polygons")

ref_crs <- st_read("GO_100mB50.shp") %>% st_crs()

cover <- raster("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/Cover_full_filled.tif")
crs(cover) <- ref_crs

shrub <- raster("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/Shrub_full_filled.tif")
crs(shrub) <- ref_crs

under <- raster("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/Under_full_filled.tif")
crs(under) <- ref_crs



## GO 100
go_100 <- st_read("GO_100mB50.shp")

go_100_cover <- cover %>% 
  raster::extract(go_100) %>% 
  lapply(mean) %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename(Mean_cover = 1) %>% 
  bind_cols(
    cover %>% 
      raster::extract(go_100) %>% 
      lapply(median) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Median_cover = 2) %>% 
  bind_cols(
    cover %>% 
      raster::extract(go_100) %>% 
      lapply(max) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Max_cover = 3) %>% 
  bind_cols(
    cover %>% 
      raster::extract(go_100) %>% 
      lapply(min) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Min_cover = 4) %>% 
  bind_cols(
    cover %>% 
      raster::extract(go_100) %>% 
      lapply(sd) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Sd_cover = 5)

go_100_shrub <- shrub %>% 
  raster::extract(go_100) %>% 
  lapply(mean) %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename(Mean_shrub = 1) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(go_100) %>% 
      lapply(median) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Median_shrub = 2) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(go_100) %>% 
      lapply(max) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Max_shrub = 3) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(go_100) %>% 
      lapply(min) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Min_shrub = 4) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(go_100) %>% 
      lapply(sd) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Sd_shrub = 5)

go_100_under <- under %>% 
  raster::extract(go_100) %>% 
  lapply(mean) %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename(Mean_under = 1) %>% 
  bind_cols(
    under %>% 
      raster::extract(go_100) %>% 
      lapply(median) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Median_under = 2) %>% 
  bind_cols(
    under %>% 
      raster::extract(go_100) %>% 
      lapply(max) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Max_under = 3) %>% 
  bind_cols(
    under %>% 
      raster::extract(go_100) %>% 
      lapply(min) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Min_under = 4) %>% 
  bind_cols(
    under %>% 
      raster::extract(go_100) %>% 
      lapply(sd) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Sd_under = 5)

go_100_result <- bind_cols(go_100_cover, go_100_shrub, go_100_under) %>%
  mutate(River = "GO",
         Class = "100")



## GO 500
go_500 <- st_read("GO_500mB50.shp")

go_500_cover <- cover %>% 
  raster::extract(go_500) %>% 
  lapply(mean) %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename(Mean_cover = 1) %>% 
  bind_cols(
    cover %>% 
      raster::extract(go_500) %>% 
      lapply(median) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Median_cover = 2) %>% 
  bind_cols(
    cover %>% 
      raster::extract(go_500) %>% 
      lapply(max) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Max_cover = 3) %>% 
  bind_cols(
    cover %>% 
      raster::extract(go_500) %>% 
      lapply(min) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Min_cover = 4) %>% 
  bind_cols(
    cover %>% 
      raster::extract(go_500) %>% 
      lapply(sd) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Sd_cover = 5)

go_500_shrub <- shrub %>% 
  raster::extract(go_500) %>% 
  lapply(mean) %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename(Mean_shrub = 1) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(go_500) %>% 
      lapply(median) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Median_shrub = 2) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(go_500) %>% 
      lapply(max) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Max_shrub = 3) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(go_500) %>% 
      lapply(min) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Min_shrub = 4) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(go_500) %>% 
      lapply(sd) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Sd_shrub = 5)

go_500_under <- under %>% 
  raster::extract(go_500) %>% 
  lapply(mean) %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename(Mean_under = 1) %>% 
  bind_cols(
    under %>% 
      raster::extract(go_500) %>% 
      lapply(median) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Median_under = 2) %>% 
  bind_cols(
    under %>% 
      raster::extract(go_500) %>% 
      lapply(max) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Max_under = 3) %>% 
  bind_cols(
    under %>% 
      raster::extract(go_500) %>% 
      lapply(min) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Min_under = 4) %>% 
  bind_cols(
    under %>% 
      raster::extract(go_500) %>% 
      lapply(sd) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Sd_under = 5)

go_500_result <- bind_cols(go_500_cover, go_500_shrub, go_500_under) %>%
  mutate(River = "GO",
         Class = "500") %>% 
  bind_cols(go_100 %>% select(LocCode))



## GO Catch
go_catch <- st_read("go_catch.shp")

go_catch_cover <- cover %>% 
  raster::extract(go_catch) %>% 
  lapply(mean) %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename(Mean_cover = 1) %>% 
  bind_cols(
    cover %>% 
      raster::extract(go_catch) %>% 
      lapply(median) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Median_cover = 2) %>% 
  bind_cols(
    cover %>% 
      raster::extract(go_catch) %>% 
      lapply(max) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Max_cover = 3) %>% 
  bind_cols(
    cover %>% 
      raster::extract(go_catch) %>% 
      lapply(min) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Min_cover = 4) %>% 
  bind_cols(
    cover %>% 
      raster::extract(go_catch) %>% 
      lapply(sd) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Sd_cover = 5)

go_catch_shrub <- shrub %>% 
  raster::extract(go_catch) %>% 
  lapply(mean) %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename(Mean_shrub = 1) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(go_catch) %>% 
      lapply(median) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Median_shrub = 2) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(go_catch) %>% 
      lapply(max) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Max_shrub = 3) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(go_catch) %>% 
      lapply(min) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Min_shrub = 4) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(go_catch) %>% 
      lapply(sd) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Sd_shrub = 5)

go_catch_under <- under %>% 
  raster::extract(go_catch) %>% 
  lapply(mean) %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename(Mean_under = 1) %>% 
  bind_cols(
    under %>% 
      raster::extract(go_catch) %>% 
      lapply(median) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Median_under = 2) %>% 
  bind_cols(
    under %>% 
      raster::extract(go_catch) %>% 
      lapply(max) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Max_under = 3) %>% 
  bind_cols(
    under %>% 
      raster::extract(go_catch) %>% 
      lapply(min) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Min_under = 4) %>% 
  bind_cols(
    under %>% 
      raster::extract(go_catch) %>% 
      lapply(sd) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Sd_under = 5)

go_catch_result <- bind_cols(go_catch_cover, go_catch_shrub, go_catch_under) %>%
  mutate(River = "GO",
         Class = "Catchment") %>% 
  bind_cols(go_100 %>% select(LocCode))



## GO Catch
go_stream <- st_read("GO_StreamB50.shp")

go_stream_cover <- cover %>% 
  raster::extract(go_stream) %>% 
  lapply(mean) %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename(Mean_cover = 1) %>% 
  bind_cols(
    cover %>% 
      raster::extract(go_stream) %>% 
      lapply(median) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Median_cover = 2) %>% 
  bind_cols(
    cover %>% 
      raster::extract(go_stream) %>% 
      lapply(max) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Max_cover = 3) %>% 
  bind_cols(
    cover %>% 
      raster::extract(go_stream) %>% 
      lapply(min) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Min_cover = 4) %>% 
  bind_cols(
    cover %>% 
      raster::extract(go_stream) %>% 
      lapply(sd) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Sd_cover = 5)

go_stream_shrub <- shrub %>% 
  raster::extract(go_stream) %>% 
  lapply(mean) %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename(Mean_shrub = 1) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(go_stream) %>% 
      lapply(median) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Median_shrub = 2) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(go_stream) %>% 
      lapply(max) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Max_shrub = 3) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(go_stream) %>% 
      lapply(min) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Min_shrub = 4) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(go_stream) %>% 
      lapply(sd) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Sd_shrub = 5)

go_stream_under <- under %>% 
  raster::extract(go_stream) %>% 
  lapply(mean) %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename(Mean_under = 1) %>% 
  bind_cols(
    under %>% 
      raster::extract(go_stream) %>% 
      lapply(median) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Median_under = 2) %>% 
  bind_cols(
    under %>% 
      raster::extract(go_stream) %>% 
      lapply(max) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Max_under = 3) %>% 
  bind_cols(
    under %>% 
      raster::extract(go_stream) %>% 
      lapply(min) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Min_under = 4) %>% 
  bind_cols(
    under %>% 
      raster::extract(go_stream) %>% 
      lapply(sd) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Sd_under = 5)

go_stream_result <- bind_cols(go_stream_cover, go_stream_shrub, go_stream_under) %>%
  mutate(River = "GO",
         Class = "Stream") %>% 
  bind_cols(go_100 %>% select(LocCode))