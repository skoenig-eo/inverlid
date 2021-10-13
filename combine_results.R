library(tidyverse)

header <- read_csv("results_biomass.csv") %>% 
  rowid_to_column() %>% 
  rename(Index = rowid) %>% 
  select(Index, LocCode, River, Class)

area <- read_csv(
  "D:/OneDrive/repositories/inverlid/results_01_re.csv") %>%
  select(Area)

tree <- read_csv()

# area <- read_csv(
#   "D:/OneDrive/repositories/inverlid/results_01_re.csv") %>% 
#   select(LocCode, Class, Area) %>% 
#   pull(Area)

# area2 <- c(sizes_go$go_100, sizes_go$go_500,
#            sizes_go$go_catch, sizes_go$go_stream,
#            sizes_vy$vy_100, sizes_vy$vy_500,
#            sizes_vy$vy_catch, sizes_vy$vy_stream)
# round(area) == round(area2)
# all(round(area) == round(area2))
# 
# area_comp <- area %>% 
#   left_join(sizes_go %>% select(LocCode, go_100) %>% rename(Area_2 = 2) %>% mutate(Class = "100"), by = c("LocCode", "Class")) %>% 
#   left_join(sizes_go %>% select(LocCode, go_500) %>% rename(Area_2 = 2) %>% mutate(Class = "500"), by = c("LocCode", "Class")) %>%
#   left_join(sizes_go %>% select(LocCode, go_catch) %>% rename(Area_2 = 2) %>% mutate(Class = "Catch"), by = c("LocCode", "Class"))
