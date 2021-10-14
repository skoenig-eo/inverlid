library(tidyverse)

header <- read_csv("results_biomass.csv") %>% 
  rowid_to_column() %>% 
  rename(Index = rowid) %>% 
  select(Index, LocCode, River, Class)

area <- read_csv(
  "D:/OneDrive/repositories/inverlid/results_02_re.csv") %>%
  select(Area)

tree <- read_csv("D:/OneDrive/repositories/inverlid/results_02_re.csv") %>% 
  select(Height_max_nodw, Height_ave_nodw, Height_med_nodw, Height_std_nodw,
         Crown_max_nodw, Crown_ave_nodw, Crown_med_nodw, Crown_std_nodw,
         Height_max_conif, Height_ave_conif, Height_med_conif, Height_std_conif,
         Crown_max_conif, Crown_ave_conif, Crown_med_conif, Crown_std_conif,
         Height_max_decid, Height_ave_decid, Height_med_decid, Height_std_decid,
         Crown_max_decid, Crown_ave_decid, Crown_med_decid, Crown_std_decid)

composition <- read_csv("D:/OneDrive/repositories/inverlid/results_02_re.csv") %>% 
  select(Count_full, Count_conif, Count_decid, Count_nodw, Count_deadw, Count_snags,
         Share_conif, Share_decid, Share_nodw, Share_deadw, Share_snags,
         Crown_sum_nodw, Crown_sum_conif, Crown_sum_decid,
         Cover_nodw, Cover_conif, Cover_decid)

biomass <- read_csv("results_biomass.csv") %>% 
  select(Mean_Biomass, Median_Biomass, Std_Biomass, Max_Biomass,
         Min_Biomass, Sum_Biomass, Biomass_Area)

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
