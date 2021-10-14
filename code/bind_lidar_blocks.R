## script combining multiple blocks of lidar data for the study area

library(sf)
# library(tidyverse)

setwd("E:/Daten NPBW/Lidar_SingleTrees")
list.files(pattern = "\\.shp")

# create vector of all lidar blocks (6 x 6 km) relevant for the study area
# (Große Ohe + Vydra)
read_list <- c("mb_03_04_all_trees_conifBroadl_deadwood_snags.shp",
               "mb_03_05_all_trees_conifBroadl_deadwood_snags_cz.shp",
               "mb_03_05_all_trees_conifBroadl_deadwood_snags_de.shp",
               "mb_03_06_all_trees_conifBroadl_deadwood_snags_cz.shp",
               "mb_03_06_all_trees_conifBroadl_deadwood_snags_de.shp",
               "mb_03_07_all_trees_conifBroadl_deadwood_snags_cz.shp",
               "mb_03_07_all_trees_conifBroadl_deadwood_snags_de.shp",
               "mb_04_04_all_trees_conifBroadl_deadwood_snags.shp",
               "mb_04_05_all_trees_conifBroadl_deadwood_snags_cz.shp",
               "mb_04_05_all_trees_conifBroadl_deadwood_snags_de.shp",
               "mb_04_06_all_trees_conifBroadl_deadwood_snags.shp",
               "mb_04_07_all_trees_conifBroadl_deadwood_snags.shp",
               "mb_05_05_all_trees_conifBroadl_deadwood_snags_cz.shp",
               "mb_05_05_all_trees_conifBroadl_deadwood_snags_de.shp",
               "mb_05_06_all_trees_conifBroadl_deadwood_snags.shp")

# bind together and write as one large geopackage
all_blocks <- do.call(rbind,lapply(read_list, st_read))
st_write(all_blocks, 
         "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/all_blocks.gpkg")
