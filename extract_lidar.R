library(tidyverse)

setwd("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/extracted")
dir()

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
            Biomass_std_full = sd(CROWN_VOL, na.rm = TRUE))

go_100_meas_nodw <- go_100 %>% 
  filter(TREE_CLASS %in% c("conif", "decid")) %>% 
  group_by(LocCode) %>% 
  summarise(Count_nowdw = n(),
            Height_max_nodw = max(HEIGHT, na.rm = TRUE),
            Height_ave_nodw = mean(HEIGHT, na.rm = TRUE),
            Height_med_nodw = median(HEIGHT, na.rm = TRUE),
            Height_std_nodw = sd(HEIGHT, na.rm = TRUE),
            Biomass_sum_nodw = sum(CROWN_VOL, na.rm = TRUE),
            Biomass_max_nodw = max(CROWN_VOL, na.rm = TRUE),
            Biomass_ave_nodw = mean(CROWN_VOL, na.rm = TRUE),
            Biomass_med_nodw = median(CROWN_VOL, na.rm = TRUE),
            Biomass_std_nodw = sd(CROWN_VOL, na.rm = TRUE))

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
            Biomass_std_conif = sd(CROWN_VOL, na.rm = TRUE))

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
            Biomass_std_decid = sd(CROWN_VOL, na.rm = TRUE))

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


go_100_result <- go_100_meas_full %>% 
  left_join(go_100_share, by = "LocCode") %>% 
  mutate(Share_conif = Count_conif/Count_full,
         Share_decid = Count_decid/Count_full,
         Share_deadw = Count_deadw/Count_full,
         Share_snags = Count_snags/Count_full) %>% 
  left_join(go_100_meas_nodw, by = "LocCode") %>% 
  left_join(go_100_meas_conif, by = "LocCode") %>% 
  left_join(go_100_meas_decid, by = "LocCode")
