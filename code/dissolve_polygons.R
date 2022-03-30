## last change: 2021-03-30

library(sf)
library(tidyverse)

all_blocks <- st_read("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/all_blocks.gpkg")

some_blocks <- all_blocks %>% slice(1:1000)

dissolved_blocks <- st_union(some_blocks)
plot(dissolved_blocks)
st_join(some_blocks)
st_write(dissolved_blocks,
         "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/diss_blocks_test.gpkg")

# this seems to work! do for all!

dissolved_blocks_all <- st_union(all_blocks)
