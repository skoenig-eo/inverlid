library(tidyverse)

set.seed(711)

v.GRI <- function(art, bhd, hoehe) {
  if(is.na(art) | is.na(bhd) | is.na(hoehe) | (bhd < 6.1) | (hoehe < 1.3) | !(art %in% c(1:9))){
    v <- NA
  } else {
    # Parameter der Funktion. Erste Spalte = Baumart: 1-Fichte, 2-Tanne, 3-Kiefer, 4-L?rche, 5-Buche, 6-Eiche, 7-Douglasie, 8-sonstige Laubh?lzer, 9-Weichlaubholz
    GRI.param <- c(1, -0.359624E+1, 0.180213E+1, -0.288243E+0, 0.106247E+1, -0.128993E+0, 0.353434E-1,  0.142264E+0, -0.582590E-1, 0.459854E-2,
                   2, -0.741365E+1, 0.333667E+1, -0.426419E+0, 0.400998E+1, -0.139533E+1, 0.165198E+0, -0.321612E+0, 0.144010E+0, -0.165461E-1,
                   3, -0.580915E+1, 0.338700E+1, -0.494392E+0, 0.367116E+1, -0.183211E+1, 0.273999E+0, -0.459282E+0, 0.299890E+0, -0.444931E-1,
                   4, -0.926182E+1, 0.475438E+1, -0.672495E+0, 0.517159E+1, -0.227654E+1, 0.311633E+0, -0.555379E+0, 0.302799E+0, -0.412510E-1,
                   5, -0.272840E+1, 0.837563E+0, -0.105843E+0, 0.162283E+1, -0.214812E+0, 0.289272E-1, -0.879719E-1, 0.325667E-1, -0.446295E-2,
                   6, -0.306118E+1, 0.145506E+1, -0.199920E+0, 0.193898E+1, -0.689727E+0, 0.112653E+0, -0.165102E+0, 0.120127E+0, -0.202543E-1,
                   7, -0.125017E+2, 0.662441E+1, -0.911185E+0, 0.727277E+1, -0.358346E+1, 0.489149E+0, -0.877150E+0, 0.515586E+0, -0.714395E-1,
                   8, -0.272840E+1, 0.837563E+0, -0.105843E+0, 0.162283E+1, -0.214812E+0, 0.289272E-1, -0.879719E-1, 0.325667E-1, -0.446295E-2,
                   9, -0.598031E+1, 0.265905E+1, -0.337400E+0, 0.378395E+1, -0.147318E+1, 0.188661E+0, -0.540955E+0, 0.296957E+0, -0.385165E-1)
    # In Matrix verwandeln
    GRI.param <- matrix(GRI.param, nrow = 9, byrow = TRUE)
    # Spaltennamen vergeben
    colnames(GRI.param) <- c("Art", "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9")  # Nur zu Kontrollzwecken
    # In data.frame verwandeln
    GRI.param <- as.data.frame(GRI.param)
    
    # Parameter der Art werden ausgew?hlt und in eine praktische Matrix gestellt.
    par.art <- matrix(as.numeric(GRI.param[GRI.param$Art == art, 2:10]), 3)
    # Vektor mit 1, log(bhd) und log(bhd)^2 wegen praktischer Multiplikation.
    bhd.vec <- c(1, log(bhd), log(bhd)^2)
    # Wir erhalten einen Zeilenvektor A, der die Koeffizienten f?r die
    # Endbeziehung enth?lt.
    A <- t(bhd.vec) %*% par.art
    # Jetzt einen praktischen H?henvektor.
    hoehe.vec <- c(1, log(hoehe), log(hoehe)^2)
    # Das f?hrt uns zur Franz'schen Formh?he...
    form.hoehe <- exp(A %*% hoehe.vec)
    # ...und damit geradewegs zum Volumen:
    v <- pi/40000 * bhd^2 * form.hoehe
    if (v < 0) { v <- 0 }
  } # else
  
  return(as.vector(v)) # Volumen v zur?ckgeben
}


go_100 <- read_csv(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/extracted/go_100.csv") %>% 
  mutate(
    BHD = case_when(
      TREE_CLASS == "conif" ~ exp(2.213 + 0.04064*HEIGHT + 0.00005367*CROWN_VOL + 0.006923*Crown_Area),
      TREE_CLASS == "decid" ~ exp(2.3029989 + 0.0407713*HEIGHT -0.0006137*CROWN_VOL + 0.0077772*Crown_Area),
      TRUE~NA_real_)) %>% 
  rowwise() %>% 
  mutate(
    Biomass = case_when(
      TREE_CLASS == "conif" ~ v.GRI(1, BHD, HEIGHT),
      TREE_CLASS == "decid" ~ v.GRI(5, BHD, HEIGHT))) %>% 
  ungroup() %>% 
  sample_n(10000) %>%
  select(BHD, Biomass)

go_500 <- read_csv(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/extracted/go_500.csv") %>% 
  mutate(
    BHD = case_when(
      TREE_CLASS == "conif" ~ exp(2.213 + 0.04064*HEIGHT + 0.00005367*CROWN_VOL + 0.006923*Crown_Area),
      TREE_CLASS == "decid" ~ exp(2.3029989 + 0.0407713*HEIGHT -0.0006137*CROWN_VOL + 0.0077772*Crown_Area),
      TRUE~NA_real_)) %>% 
  rowwise() %>% 
  mutate(
    Biomass = case_when(
      TREE_CLASS == "conif" ~ v.GRI(1, BHD, HEIGHT),
      TREE_CLASS == "decid" ~ v.GRI(5, BHD, HEIGHT))) %>% 
  ungroup() %>% 
  sample_n(10000) %>%
  select(BHD, Biomass)

vy_100 <- read_csv(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/extracted/vy_100.csv") %>% 
  mutate(
    BHD = case_when(
      TREE_CLASS == "conif" ~ exp(2.213 + 0.04064*HEIGHT + 0.00005367*CROWN_VOL + 0.006923*Crown_Area),
      TREE_CLASS == "decid" ~ exp(2.3029989 + 0.0407713*HEIGHT -0.0006137*CROWN_VOL + 0.0077772*Crown_Area),
      TRUE~NA_real_)) %>% 
  rowwise() %>% 
  mutate(
    Biomass = case_when(
      TREE_CLASS == "conif" ~ v.GRI(1, BHD, HEIGHT),
      TREE_CLASS == "decid" ~ v.GRI(5, BHD, HEIGHT))) %>% 
  ungroup() %>% 
  sample_n(10000, replace = TRUE) %>%
  select(BHD, Biomass)

vy_500 <- read_csv(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/extracted/vy_500.csv") %>% 
  mutate(
    BHD = case_when(
      TREE_CLASS == "conif" ~ exp(2.213 + 0.04064*HEIGHT + 0.00005367*CROWN_VOL + 0.006923*Crown_Area),
      TREE_CLASS == "decid" ~ exp(2.3029989 + 0.0407713*HEIGHT -0.0006137*CROWN_VOL + 0.0077772*Crown_Area),
      TRUE~NA_real_)) %>% 
  rowwise() %>% 
  mutate(
    Biomass = case_when(
      TREE_CLASS == "conif" ~ v.GRI(1, BHD, HEIGHT),
      TREE_CLASS == "decid" ~ v.GRI(5, BHD, HEIGHT))) %>% 
  ungroup() %>% 
  sample_n(10000) %>%
  select(BHD, Biomass)


all_catch <- go_100 %>% mutate(Catch = "go_100") %>%
  bind_rows(go_500 %>% mutate(Catch = "go_500")) %>%
  bind_rows(vy_100 %>% mutate(Catch = "vy_100")) %>%
  bind_rows(vy_500 %>% mutate(Catch = "vy_500"))

ggplot(all_catch) +
  geom_histogram(aes(x = BHD, fill = Catch), alpha = 0.75) +
  scale_x_continuous(limits = c(0, 100), name = "DBH [cm]") +
  scale_fill_manual(values = c("#264653", "#2a9d8f", "#e9c46a", "#e76f51")) +
  scale_y_continuous(name = "Count") +
  facet_wrap(~ Catch) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")
ggsave("./dbh_overview.png", width = 15, height = 10, units = "cm", dpi = 450)

ggplot(all_catch) +
  geom_histogram(aes(x = Biomass, fill = Catch), alpha = 0.75) +
  scale_x_continuous(limits = c(0, 10), name = "Biomass [m3]") +
  scale_fill_manual(values = c("#264653", "#2a9d8f", "#e9c46a", "#e76f51")) +
  scale_y_continuous(limits = c(0, 2000), name = "Count") +
  facet_wrap(~ Catch) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")
ggsave("./biomass_overview.png", width = 15, height = 10, units = "cm", dpi = 450)