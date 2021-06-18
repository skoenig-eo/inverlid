library(tidyverse)

setwd("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/extracted")
dir()

sizes_go <- read_csv("D:/OneDrive/repositories/inverlid/sizes_go.csv")
sizes_vy <- read_csv("D:/OneDrive/repositories/inverlid/sizes_vy.csv")

go_100 <- read_csv("go_100.csv")
nrow(go_100)

go_100_meas_full <- go_100 %>% 
  group_by(LocCode) %>% 
  summarise(Count_full = n(),
            Height_max_full = max(HEIGHT, na.rm = TRUE),
            Height_ave_full = mean(HEIGHT, na.rm = TRUE),
            Height_med_full = median(HEIGHT, na.rm = TRUE),
            Height_std_full = sd(HEIGHT, na.rm = TRUE),
            Biomass_sum_full = sum(CROWN_VOL, na.rm = TRUE),
            Biomass_max_full = max(CROWN_VOL, na.rm = TRUE),
            Biomass_ave_full = mean(CROWN_VOL, na.rm = TRUE),
            Biomass_med_full = median(CROWN_VOL, na.rm = TRUE),
            Biomass_std_full = sd(CROWN_VOL, na.rm = TRUE),
            Crown_sum_full = sum(Crown_Area, na.rm = TRUE),
            Crown_max_full = max(Crown_Area, na.rm = TRUE),
            Crown_ave_full = mean(Crown_Area, na.rm = TRUE),
            Crown_med_full = median(Crown_Area, na.rm = TRUE),
            Crown_std_full = sd(Crown_Area, na.rm = TRUE))

go_100_meas_nodw <- go_100 %>% 
  filter(TREE_CLASS %in% c("conif", "decid")) %>% 
  group_by(LocCode) %>% 
  summarise(Height_max_nodw = max(HEIGHT, na.rm = TRUE),
            Height_ave_nodw = mean(HEIGHT, na.rm = TRUE),
            Height_med_nodw = median(HEIGHT, na.rm = TRUE),
            Height_std_nodw = sd(HEIGHT, na.rm = TRUE),
            Biomass_sum_nodw = sum(CROWN_VOL, na.rm = TRUE),
            Biomass_max_nodw = max(CROWN_VOL, na.rm = TRUE),
            Biomass_ave_nodw = mean(CROWN_VOL, na.rm = TRUE),
            Biomass_med_nodw = median(CROWN_VOL, na.rm = TRUE),
            Biomass_std_nodw = sd(CROWN_VOL, na.rm = TRUE),
            Crown_sum_nodw = sum(Crown_Area, na.rm = TRUE),
            Crown_max_nodw = max(Crown_Area, na.rm = TRUE),
            Crown_ave_nodw = mean(Crown_Area, na.rm = TRUE),
            Crown_med_nodw = median(Crown_Area, na.rm = TRUE),
            Crown_std_nodw = sd(Crown_Area, na.rm = TRUE))

go_100_meas_conif <- go_100 %>% 
  filter(TREE_CLASS %in% c("conif", "decid")) %>% 
  group_by(LocCode) %>% 
  summarise(Height_max_conif = max(HEIGHT, na.rm = TRUE),
            Height_ave_conif = mean(HEIGHT, na.rm = TRUE),
            Height_med_conif = median(HEIGHT, na.rm = TRUE),
            Height_std_conif = sd(HEIGHT, na.rm = TRUE),
            Biomass_sum_conif = sum(CROWN_VOL, na.rm = TRUE),
            Biomass_max_conif = max(CROWN_VOL, na.rm = TRUE),
            Biomass_ave_conif = mean(CROWN_VOL, na.rm = TRUE),
            Biomass_med_conif = median(CROWN_VOL, na.rm = TRUE),
            Biomass_std_conif = sd(CROWN_VOL, na.rm = TRUE),
            Crown_sum_conif = sum(Crown_Area, na.rm = TRUE),
            Crown_max_conif = max(Crown_Area, na.rm = TRUE),
            Crown_ave_conif = mean(Crown_Area, na.rm = TRUE),
            Crown_med_conif = median(Crown_Area, na.rm = TRUE),
            Crown_std_conif = sd(Crown_Area, na.rm = TRUE))

go_100_meas_decid <- go_100 %>% 
  filter(TREE_CLASS %in% c("decid", "decid")) %>% 
  group_by(LocCode) %>% 
  summarise(Height_max_decid = max(HEIGHT, na.rm = TRUE),
            Height_ave_decid = mean(HEIGHT, na.rm = TRUE),
            Height_med_decid = median(HEIGHT, na.rm = TRUE),
            Height_std_decid = sd(HEIGHT, na.rm = TRUE),
            Biomass_sum_decid = sum(CROWN_VOL, na.rm = TRUE),
            Biomass_max_decid = max(CROWN_VOL, na.rm = TRUE),
            Biomass_ave_decid = mean(CROWN_VOL, na.rm = TRUE),
            Biomass_med_decid = median(CROWN_VOL, na.rm = TRUE),
            Biomass_std_decid = sd(CROWN_VOL, na.rm = TRUE),
            Crown_sum_decid = sum(Crown_Area, na.rm = TRUE),
            Crown_max_decid = max(Crown_Area, na.rm = TRUE),
            Crown_ave_decid = mean(Crown_Area, na.rm = TRUE),
            Crown_med_decid = median(Crown_Area, na.rm = TRUE),
            Crown_std_decid = sd(Crown_Area, na.rm = TRUE))

go_100_share <- go_100 %>% 
  group_by(LocCode, TREE_CLASS) %>% 
  summarise(Count = n()) %>% 
  pivot_wider(names_from = TREE_CLASS, values_from = Count) %>% 
  rename(Count_conif = conif,
         Count_decid = decid,
         Count_deadw = deadw,
         Count_snags = snag) %>% 
  replace_na(list(Count_conif = 0,
                  Count_decid = 0,
                  Count_deadw = 0,
                  Count_snags = 0)) %>%
  relocate(LocCode, Count_conif, Count_decid, Count_deadw, Count_snags)


go_100_result <- sizes_go %>% 
  select(LocCode, go_100) %>% 
  rename(Area = 2) %>% 
  left_join(go_100_meas_full) %>% 
  left_join(go_100_meas_nodw) %>% 
  left_join(go_100_meas_conif) %>% 
  left_join(go_100_meas_decid) %>% 
  left_join(go_100_share) %>% 
  mutate(Share_conif = Count_conif/Count_full,
         Share_decid = Count_decid/Count_full,
         Share_deadw = Count_deadw/Count_full,
         Share_snags = Count_snags/Count_full,
         Cover_full = Crown_sum_full/Area,
         Cover_conif = Crown_sum_conif/Area,
         Cover_decid = Crown_sum_decid/Area)
