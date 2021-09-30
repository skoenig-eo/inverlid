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





##### Große Ohe #####

## GO 100
go_100 <- st_read("GO_100mB50.shp")

go_100_cover <- cover %>% 
  raster::extract(go_100) %>% 
  lapply(mean, na.rm = TRUE) %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename(Mean_cover = 1) %>% 
  bind_cols(
    cover %>% 
      raster::extract(go_100) %>% 
      lapply(median, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Median_cover = 2) %>% 
  bind_cols(
    cover %>% 
      raster::extract(go_100) %>% 
      lapply(max, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Max_cover = 3) %>% 
  bind_cols(
    cover %>% 
      raster::extract(go_100) %>% 
      lapply(min, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Min_cover = 4) %>% 
  bind_cols(
    cover %>% 
      raster::extract(go_100) %>% 
      lapply(sd, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Sd_cover = 5)

go_100_shrub <- shrub %>% 
  raster::extract(go_100) %>% 
  lapply(mean, na.rm = TRUE) %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename(Mean_shrub = 1) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(go_100) %>% 
      lapply(median, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Median_shrub = 2) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(go_100) %>% 
      lapply(max, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Max_shrub = 3) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(go_100) %>% 
      lapply(min, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Min_shrub = 4) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(go_100) %>% 
      lapply(sd, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Sd_shrub = 5)

go_100_under <- under %>% 
  raster::extract(go_100) %>% 
  lapply(mean, na.rm = TRUE) %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename(Mean_under = 1) %>% 
  bind_cols(
    under %>% 
      raster::extract(go_100) %>% 
      lapply(median, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Median_under = 2) %>% 
  bind_cols(
    under %>% 
      raster::extract(go_100) %>% 
      lapply(max, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Max_under = 3) %>% 
  bind_cols(
    under %>% 
      raster::extract(go_100) %>% 
      lapply(min, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Min_under = 4) %>% 
  bind_cols(
    under %>% 
      raster::extract(go_100) %>% 
      lapply(sd, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Sd_under = 5)

go_100_result <- bind_cols(go_100_cover, go_100_shrub, go_100_under) %>%
  mutate(River = "GO",
         Class = "100") %>% 
  bind_cols(go_100 %>% as_tibble %>%  select(LocCode))



## GO 500
go_500 <- st_read("GO_500mB50.shp")

go_500_cover <- cover %>% 
  raster::extract(go_500) %>% 
  lapply(mean, na.rm = TRUE) %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename(Mean_cover = 1) %>% 
  bind_cols(
    cover %>% 
      raster::extract(go_500) %>% 
      lapply(median, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Median_cover = 2) %>% 
  bind_cols(
    cover %>% 
      raster::extract(go_500) %>% 
      lapply(max, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Max_cover = 3) %>% 
  bind_cols(
    cover %>% 
      raster::extract(go_500) %>% 
      lapply(min, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Min_cover = 4) %>% 
  bind_cols(
    cover %>% 
      raster::extract(go_500) %>% 
      lapply(sd, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Sd_cover = 5)

go_500_shrub <- shrub %>% 
  raster::extract(go_500) %>% 
  lapply(mean, na.rm = TRUE) %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename(Mean_shrub = 1) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(go_500) %>% 
      lapply(median, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Median_shrub = 2) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(go_500) %>% 
      lapply(max, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Max_shrub = 3) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(go_500) %>% 
      lapply(min, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Min_shrub = 4) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(go_500) %>% 
      lapply(sd, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Sd_shrub = 5)

go_500_under <- under %>% 
  raster::extract(go_500) %>% 
  lapply(mean, na.rm = TRUE) %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename(Mean_under = 1) %>% 
  bind_cols(
    under %>% 
      raster::extract(go_500) %>% 
      lapply(median, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Median_under = 2) %>% 
  bind_cols(
    under %>% 
      raster::extract(go_500) %>% 
      lapply(max, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Max_under = 3) %>% 
  bind_cols(
    under %>% 
      raster::extract(go_500) %>% 
      lapply(min, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Min_under = 4) %>% 
  bind_cols(
    under %>% 
      raster::extract(go_500) %>% 
      lapply(sd, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Sd_under = 5)

go_500_result <- bind_cols(go_500_cover, go_500_shrub, go_500_under) %>%
  mutate(River = "GO",
         Class = "500") %>% 
  bind_cols(go_100 %>% as_tibble %>%  select(LocCode))



## GO Catch
go_catch <- st_read("go_catch.shp")

go_catch_cover <- cover %>% 
  raster::extract(go_catch) %>% 
  lapply(mean, na.rm = TRUE) %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename(Mean_cover = 1) %>% 
  bind_cols(
    cover %>% 
      raster::extract(go_catch) %>% 
      lapply(median, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Median_cover = 2) %>% 
  bind_cols(
    cover %>% 
      raster::extract(go_catch) %>% 
      lapply(max, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Max_cover = 3) %>% 
  bind_cols(
    cover %>% 
      raster::extract(go_catch) %>% 
      lapply(min, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Min_cover = 4) %>% 
  bind_cols(
    cover %>% 
      raster::extract(go_catch) %>% 
      lapply(sd, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Sd_cover = 5)

go_catch_shrub <- shrub %>% 
  raster::extract(go_catch) %>% 
  lapply(mean, na.rm = TRUE) %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename(Mean_shrub = 1) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(go_catch) %>% 
      lapply(median, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Median_shrub = 2) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(go_catch) %>% 
      lapply(max, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Max_shrub = 3) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(go_catch) %>% 
      lapply(min, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Min_shrub = 4) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(go_catch) %>% 
      lapply(sd, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Sd_shrub = 5)

go_catch_under <- under %>% 
  raster::extract(go_catch) %>% 
  lapply(mean, na.rm = TRUE) %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename(Mean_under = 1) %>% 
  bind_cols(
    under %>% 
      raster::extract(go_catch) %>% 
      lapply(median, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Median_under = 2) %>% 
  bind_cols(
    under %>% 
      raster::extract(go_catch) %>% 
      lapply(max, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Max_under = 3) %>% 
  bind_cols(
    under %>% 
      raster::extract(go_catch) %>% 
      lapply(min, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Min_under = 4) %>% 
  bind_cols(
    under %>% 
      raster::extract(go_catch) %>% 
      lapply(sd, na.rm = TRUE) %>% 
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
  lapply(mean, na.rm = TRUE) %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename(Mean_cover = 1) %>% 
  bind_cols(
    cover %>% 
      raster::extract(go_stream) %>% 
      lapply(median, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Median_cover = 2) %>% 
  bind_cols(
    cover %>% 
      raster::extract(go_stream) %>% 
      lapply(max, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Max_cover = 3) %>% 
  bind_cols(
    cover %>% 
      raster::extract(go_stream) %>% 
      lapply(min, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Min_cover = 4) %>% 
  bind_cols(
    cover %>% 
      raster::extract(go_stream) %>% 
      lapply(sd, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Sd_cover = 5)

go_stream_shrub <- shrub %>% 
  raster::extract(go_stream) %>% 
  lapply(mean, na.rm = TRUE) %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename(Mean_shrub = 1) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(go_stream) %>% 
      lapply(median, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Median_shrub = 2) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(go_stream) %>% 
      lapply(max, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Max_shrub = 3) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(go_stream) %>% 
      lapply(min, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Min_shrub = 4) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(go_stream) %>% 
      lapply(sd, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Sd_shrub = 5)

go_stream_under <- under %>% 
  raster::extract(go_stream) %>% 
  lapply(mean, na.rm = TRUE) %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename(Mean_under = 1) %>% 
  bind_cols(
    under %>% 
      raster::extract(go_stream) %>% 
      lapply(median, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Median_under = 2) %>% 
  bind_cols(
    under %>% 
      raster::extract(go_stream) %>% 
      lapply(max, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Max_under = 3) %>% 
  bind_cols(
    under %>% 
      raster::extract(go_stream) %>% 
      lapply(min, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Min_under = 4) %>% 
  bind_cols(
    under %>% 
      raster::extract(go_stream) %>% 
      lapply(sd, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Sd_under = 5)

go_stream_result <- bind_cols(go_stream_cover, go_stream_shrub, go_stream_under) %>%
  mutate(River = "GO",
         Class = "Stream") %>% 
  bind_cols(go_100 %>% select(LocCode))





##### Vydra #####

## VY 100
vy_100 <- st_read("VY_100mB50.shp")

vy_100_cover <- cover %>% 
  raster::extract(vy_100) %>% 
  lapply(mean, na.rm = TRUE) %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename(Mean_cover = 1) %>% 
  bind_cols(
    cover %>% 
      raster::extract(vy_100) %>% 
      lapply(median, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Median_cover = 2) %>% 
  bind_cols(
    cover %>% 
      raster::extract(vy_100) %>% 
      lapply(max, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Max_cover = 3) %>% 
  bind_cols(
    cover %>% 
      raster::extract(vy_100) %>% 
      lapply(min, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Min_cover = 4) %>% 
  bind_cols(
    cover %>% 
      raster::extract(vy_100) %>% 
      lapply(sd, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Sd_cover = 5)

vy_100_shrub <- shrub %>% 
  raster::extract(vy_100) %>% 
  lapply(mean, na.rm = TRUE) %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename(Mean_shrub = 1) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(vy_100) %>% 
      lapply(median, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Median_shrub = 2) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(vy_100) %>% 
      lapply(max, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Max_shrub = 3) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(vy_100) %>% 
      lapply(min, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Min_shrub = 4) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(vy_100) %>% 
      lapply(sd, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Sd_shrub = 5)

vy_100_under <- under %>% 
  raster::extract(vy_100) %>% 
  lapply(mean, na.rm = TRUE) %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename(Mean_under = 1) %>% 
  bind_cols(
    under %>% 
      raster::extract(vy_100) %>% 
      lapply(median, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Median_under = 2) %>% 
  bind_cols(
    under %>% 
      raster::extract(vy_100) %>% 
      lapply(max, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Max_under = 3) %>% 
  bind_cols(
    under %>% 
      raster::extract(vy_100) %>% 
      lapply(min, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Min_under = 4) %>% 
  bind_cols(
    under %>% 
      raster::extract(vy_100) %>% 
      lapply(sd, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Sd_under = 5)

vy_100_result <- bind_cols(vy_100_cover, vy_100_shrub, vy_100_under) %>%
  mutate(River = "VY",
         Class = "100") %>% 
  bind_cols(vy_100 %>% as_tibble %>%  select(LocCode))



## VY 500
vy_500 <- st_read("VY_500mB50.shp")

vy_500_cover <- cover %>% 
  raster::extract(vy_500) %>% 
  lapply(mean, na.rm = TRUE) %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename(Mean_cover = 1) %>% 
  bind_cols(
    cover %>% 
      raster::extract(vy_500) %>% 
      lapply(median, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Median_cover = 2) %>% 
  bind_cols(
    cover %>% 
      raster::extract(vy_500) %>% 
      lapply(max, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Max_cover = 3) %>% 
  bind_cols(
    cover %>% 
      raster::extract(vy_500) %>% 
      lapply(min, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Min_cover = 4) %>% 
  bind_cols(
    cover %>% 
      raster::extract(vy_500) %>% 
      lapply(sd, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Sd_cover = 5)

vy_500_shrub <- shrub %>% 
  raster::extract(vy_500) %>% 
  lapply(mean, na.rm = TRUE) %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename(Mean_shrub = 1) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(vy_500) %>% 
      lapply(median, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Median_shrub = 2) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(vy_500) %>% 
      lapply(max, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Max_shrub = 3) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(vy_500) %>% 
      lapply(min, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Min_shrub = 4) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(vy_500) %>% 
      lapply(sd, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Sd_shrub = 5)

vy_500_under <- under %>% 
  raster::extract(vy_500) %>% 
  lapply(mean, na.rm = TRUE) %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename(Mean_under = 1) %>% 
  bind_cols(
    under %>% 
      raster::extract(vy_500) %>% 
      lapply(median, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Median_under = 2) %>% 
  bind_cols(
    under %>% 
      raster::extract(vy_500) %>% 
      lapply(max, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Max_under = 3) %>% 
  bind_cols(
    under %>% 
      raster::extract(vy_500) %>% 
      lapply(min, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Min_under = 4) %>% 
  bind_cols(
    under %>% 
      raster::extract(vy_500) %>% 
      lapply(sd, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Sd_under = 5)

vy_500_result <- bind_cols(vy_500_cover, vy_500_shrub, vy_500_under) %>%
  mutate(River = "VY",
         Class = "500") %>% 
  bind_cols(vy_100 %>% as_tibble %>%  select(LocCode))



## VY Catch
vy_catch <- st_read("VY_catch.shp")

vy_catch_cover <- cover %>% 
  raster::extract(vy_catch) %>% 
  lapply(mean, na.rm = TRUE) %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename(Mean_cover = 1) %>% 
  bind_cols(
    cover %>% 
      raster::extract(vy_catch) %>% 
      lapply(median, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Median_cover = 2) %>% 
  bind_cols(
    cover %>% 
      raster::extract(vy_catch) %>% 
      lapply(max, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Max_cover = 3) %>% 
  bind_cols(
    cover %>% 
      raster::extract(vy_catch) %>% 
      lapply(min, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Min_cover = 4) %>% 
  bind_cols(
    cover %>% 
      raster::extract(vy_catch) %>% 
      lapply(sd, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Sd_cover = 5)

vy_catch_shrub <- shrub %>% 
  raster::extract(vy_catch) %>% 
  lapply(mean, na.rm = TRUE) %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename(Mean_shrub = 1) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(vy_catch) %>% 
      lapply(median, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Median_shrub = 2) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(vy_catch) %>% 
      lapply(max, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Max_shrub = 3) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(vy_catch) %>% 
      lapply(min, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Min_shrub = 4) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(vy_catch) %>% 
      lapply(sd, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Sd_shrub = 5)

vy_catch_under <- under %>% 
  raster::extract(vy_catch) %>% 
  lapply(mean, na.rm = TRUE) %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename(Mean_under = 1) %>% 
  bind_cols(
    under %>% 
      raster::extract(vy_catch) %>% 
      lapply(median, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Median_under = 2) %>% 
  bind_cols(
    under %>% 
      raster::extract(vy_catch) %>% 
      lapply(max, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Max_under = 3) %>% 
  bind_cols(
    under %>% 
      raster::extract(vy_catch) %>% 
      lapply(min, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Min_under = 4) %>% 
  bind_cols(
    under %>% 
      raster::extract(vy_catch) %>% 
      lapply(sd, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Sd_under = 5)

vy_catch_result <- bind_cols(vy_catch_cover, vy_catch_shrub, vy_catch_under) %>%
  mutate(River = "VY",
         Class = "Catchment") %>% 
  bind_cols(vy_100 %>% select(LocCode))



## VY Stream
vy_stream <- st_read("VY_StreamB50.shp")

vy_stream_cover <- cover %>% 
  raster::extract(vy_stream) %>% 
  lapply(mean, na.rm = TRUE) %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename(Mean_cover = 1) %>% 
  bind_cols(
    cover %>% 
      raster::extract(vy_stream) %>% 
      lapply(median, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Median_cover = 2) %>% 
  bind_cols(
    cover %>% 
      raster::extract(vy_stream) %>% 
      lapply(max, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Max_cover = 3) %>% 
  bind_cols(
    cover %>% 
      raster::extract(vy_stream) %>% 
      lapply(min, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Min_cover = 4) %>% 
  bind_cols(
    cover %>% 
      raster::extract(vy_stream) %>% 
      lapply(sd, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Sd_cover = 5)

vy_stream_shrub <- shrub %>% 
  raster::extract(vy_stream) %>% 
  lapply(mean, na.rm = TRUE) %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename(Mean_shrub = 1) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(vy_stream) %>% 
      lapply(median, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Median_shrub = 2) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(vy_stream) %>% 
      lapply(max, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Max_shrub = 3) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(vy_stream) %>% 
      lapply(min, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Min_shrub = 4) %>% 
  bind_cols(
    shrub %>% 
      raster::extract(vy_stream) %>% 
      lapply(sd, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Sd_shrub = 5)

vy_stream_under <- under %>% 
  raster::extract(vy_stream) %>% 
  lapply(mean, na.rm = TRUE) %>% 
  unlist() %>% 
  as_tibble() %>% 
  rename(Mean_under = 1) %>% 
  bind_cols(
    under %>% 
      raster::extract(vy_stream) %>% 
      lapply(median, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Median_under = 2) %>% 
  bind_cols(
    under %>% 
      raster::extract(vy_stream) %>% 
      lapply(max, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Max_under = 3) %>% 
  bind_cols(
    under %>% 
      raster::extract(vy_stream) %>% 
      lapply(min, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Min_under = 4) %>% 
  bind_cols(
    under %>% 
      raster::extract(vy_stream) %>% 
      lapply(sd, na.rm = TRUE) %>% 
      unlist() %>% 
      as_tibble()
  ) %>% 
  rename(Sd_under = 5)

vy_stream_result <- bind_cols(vy_stream_cover, vy_stream_shrub, vy_stream_under) %>%
  mutate(River = "VY",
         Class = "Stream") %>% 
  bind_cols(vy_100 %>% select(LocCode))

result <- bind_rows(
  go_100_result,
  go_500_result,
  go_catch_result,
  go_stream_result,
  vy_100_result,
  vy_500_result,
  vy_catch_result,
  vy_stream_result
)