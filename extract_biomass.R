library(tidyverse)

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
      TREE_CLASS == "conif" ~ (2.213 + 0.04064*HEIGHT + 0.00005367*CROWN_VOL + 0.006923*Crown_Area)*100,
      TREE_CLASS == "decid" ~ (2.3029989 + 0.0407713*HEIGHT -0.0006137*CROWN_VOL + 0.0077772*Crown_Area)*100,
      TRUE~NA_real_)) %>% 
  rowwise() %>% 
  mutate(
    Biomass = case_when(
      TREE_CLASS == "conif" ~ v.GRI(1, BHD, HEIGHT),
      TREE_CLASS == "decid" ~ v.GRI(5, BHD, HEIGHT))) %>% 
  ungroup() %>% 
  group_by(LocCode) %>% 
  summarise(Mean_Biomass = mean(Biomass, na.rm = TRUE),
            Median_Biomass = median(Biomass, na.rm = TRUE),
            Max_Biomass = max(Biomass, na.rm = TRUE),
            Min_Biomass = min(Biomass, na.rm = TRUE),
            Sum_Biomass = sum(Biomass, na.rm = TRUE))%>%
  mutate(River = "GO",
         Class = "100")


go_500 <- read_csv(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/extracted/go_500.csv") %>% 
  mutate(
    BHD = case_when(
      TREE_CLASS == "conif" ~ (2.213 + 0.04064*HEIGHT + 0.00005367*CROWN_VOL + 0.006923*Crown_Area)*100,
      TREE_CLASS == "decid" ~ (2.3029989 + 0.0407713*HEIGHT -0.0006137*CROWN_VOL + 0.0077772*Crown_Area)*100,
      TRUE~NA_real_)) %>% 
  rowwise() %>% 
  mutate(
    Biomass = case_when(
      TREE_CLASS == "conif" ~ v.GRI(1, BHD, HEIGHT),
      TREE_CLASS == "decid" ~ v.GRI(5, BHD, HEIGHT))) %>% 
  ungroup() %>% 
  group_by(LocCode) %>% 
  summarise(Mean_Biomass = mean(Biomass, na.rm = TRUE),
            Median_Biomass = median(Biomass, na.rm = TRUE),
            Max_Biomass = max(Biomass, na.rm = TRUE),
            Min_Biomass = min(Biomass, na.rm = TRUE),
            Sum_Biomass = sum(Biomass, na.rm = TRUE))%>%
  mutate(River = "GO",
         Class = "500")

go_catch <- read_csv(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/extracted/go_catch.csv") %>% 
  mutate(
    BHD = case_when(
      TREE_CLASS == "conif" ~ (2.213 + 0.04064*HEIGHT + 0.00005367*CROWN_VOL + 0.006923*Crown_Area)*100,
      TREE_CLASS == "decid" ~ (2.3029989 + 0.0407713*HEIGHT -0.0006137*CROWN_VOL + 0.0077772*Crown_Area)*100,
      TRUE~NA_real_)) %>% 
  rowwise() %>% 
  mutate(
    Biomass = case_when(
      TREE_CLASS == "conif" ~ v.GRI(1, BHD, HEIGHT),
      TREE_CLASS == "decid" ~ v.GRI(5, BHD, HEIGHT))) %>% 
  ungroup() %>% 
  group_by(LocCode) %>% 
  summarise(Mean_Biomass = mean(Biomass, na.rm = TRUE),
            Median_Biomass = median(Biomass, na.rm = TRUE),
            Max_Biomass = max(Biomass, na.rm = TRUE),
            Min_Biomass = min(Biomass, na.rm = TRUE),
            Sum_Biomass = sum(Biomass, na.rm = TRUE))%>%
  mutate(River = "GO",
         Class = "Catch")

go_stream <- read_csv(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/extracted/go_stream.csv") %>% 
  mutate(
    BHD = case_when(
      TREE_CLASS == "conif" ~ (2.213 + 0.04064*HEIGHT + 0.00005367*CROWN_VOL + 0.006923*Crown_Area)*100,
      TREE_CLASS == "decid" ~ (2.3029989 + 0.0407713*HEIGHT -0.0006137*CROWN_VOL + 0.0077772*Crown_Area)*100,
      TRUE~NA_real_)) %>% 
  rowwise() %>% 
  mutate(
    Biomass = case_when(
      TREE_CLASS == "conif" ~ v.GRI(1, BHD, HEIGHT),
      TREE_CLASS == "decid" ~ v.GRI(5, BHD, HEIGHT))) %>% 
  ungroup() %>% 
  group_by(LocCode) %>% 
  summarise(Mean_Biomass = mean(Biomass, na.rm = TRUE),
            Median_Biomass = median(Biomass, na.rm = TRUE),
            Max_Biomass = max(Biomass, na.rm = TRUE),
            Min_Biomass = min(Biomass, na.rm = TRUE),
            Sum_Biomass = sum(Biomass, na.rm = TRUE))%>%
  mutate(River = "GO",
         Class = "Stream")




vy_100 <- read_csv(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/extracted/vy_100.csv") %>% 
  mutate(
    BHD = case_when(
      TREE_CLASS == "conif" ~ (2.213 + 0.04064*HEIGHT + 0.00005367*CROWN_VOL + 0.006923*Crown_Area)*100,
      TREE_CLASS == "decid" ~ (2.3029989 + 0.0407713*HEIGHT -0.0006137*CROWN_VOL + 0.0077772*Crown_Area)*100,
      TRUE~NA_real_)) %>% 
  rowwise() %>% 
  mutate(
    Biomass = case_when(
      TREE_CLASS == "conif" ~ v.GRI(1, BHD, HEIGHT),
      TREE_CLASS == "decid" ~ v.GRI(5, BHD, HEIGHT))) %>% 
  ungroup() %>% 
  group_by(LocCode) %>% 
  summarise(Mean_Biomass = mean(Biomass, na.rm = TRUE),
            Median_Biomass = median(Biomass, na.rm = TRUE),
            Max_Biomass = max(Biomass, na.rm = TRUE),
            Min_Biomass = min(Biomass, na.rm = TRUE),
            Sum_Biomass = sum(Biomass, na.rm = TRUE))%>%
  mutate(River = "VY",
         Class = "100")


vy_500 <- read_csv(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/extracted/vy_500.csv") %>% 
  mutate(
    BHD = case_when(
      TREE_CLASS == "conif" ~ (2.213 + 0.04064*HEIGHT + 0.00005367*CROWN_VOL + 0.006923*Crown_Area)*100,
      TREE_CLASS == "decid" ~ (2.3029989 + 0.0407713*HEIGHT -0.0006137*CROWN_VOL + 0.0077772*Crown_Area)*100,
      TRUE~NA_real_)) %>% 
  rowwise() %>% 
  mutate(
    Biomass = case_when(
      TREE_CLASS == "conif" ~ v.GRI(1, BHD, HEIGHT),
      TREE_CLASS == "decid" ~ v.GRI(5, BHD, HEIGHT))) %>% 
  ungroup() %>% 
  group_by(LocCode) %>% 
  summarise(Mean_Biomass = mean(Biomass, na.rm = TRUE),
            Median_Biomass = median(Biomass, na.rm = TRUE),
            Max_Biomass = max(Biomass, na.rm = TRUE),
            Min_Biomass = min(Biomass, na.rm = TRUE),
            Sum_Biomass = sum(Biomass, na.rm = TRUE))%>%
  mutate(River = "VY",
         Class = "500")

vy_catch <- read_csv(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/extracted/vy_catch.csv") %>% 
  mutate(
    BHD = case_when(
      TREE_CLASS == "conif" ~ (2.213 + 0.04064*HEIGHT + 0.00005367*CROWN_VOL + 0.006923*Crown_Area)*100,
      TREE_CLASS == "decid" ~ (2.3029989 + 0.0407713*HEIGHT -0.0006137*CROWN_VOL + 0.0077772*Crown_Area)*100,
      TRUE~NA_real_)) %>% 
  rowwise() %>% 
  mutate(
    Biomass = case_when(
      TREE_CLASS == "conif" ~ v.GRI(1, BHD, HEIGHT),
      TREE_CLASS == "decid" ~ v.GRI(5, BHD, HEIGHT))) %>% 
  ungroup() %>% 
  group_by(LocCode) %>% 
  summarise(Mean_Biomass = mean(Biomass, na.rm = TRUE),
            Median_Biomass = median(Biomass, na.rm = TRUE),
            Max_Biomass = max(Biomass, na.rm = TRUE),
            Min_Biomass = min(Biomass, na.rm = TRUE),
            Sum_Biomass = sum(Biomass, na.rm = TRUE))%>%
  mutate(River = "VY",
         Class = "Catch")

vy_stream <- read_csv(
  "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/extracted/vy_stream.csv") %>% 
  mutate(
    BHD = case_when(
      TREE_CLASS == "conif" ~ (2.213 + 0.04064*HEIGHT + 0.00005367*CROWN_VOL + 0.006923*Crown_Area)*100,
      TREE_CLASS == "decid" ~ (2.3029989 + 0.0407713*HEIGHT -0.0006137*CROWN_VOL + 0.0077772*Crown_Area)*100,
      TRUE~NA_real_)) %>% 
  rowwise() %>% 
  mutate(
    Biomass = case_when(
      TREE_CLASS == "conif" ~ v.GRI(1, BHD, HEIGHT),
      TREE_CLASS == "decid" ~ v.GRI(5, BHD, HEIGHT))) %>% 
  ungroup() %>% 
  group_by(LocCode) %>% 
  summarise(Mean_Biomass = mean(Biomass, na.rm = TRUE),
            Median_Biomass = median(Biomass, na.rm = TRUE),
            Max_Biomass = max(Biomass, na.rm = TRUE),
            Min_Biomass = min(Biomass, na.rm = TRUE),
            Sum_Biomass = sum(Biomass, na.rm = TRUE))%>%
  mutate(River = "VY",
         Class = "Stream")

sizes_go <- read_csv("D:/OneDrive/repositories/inverlid/sizes_go.csv")
sizes_vy <- read_csv("D:/OneDrive/repositories/inverlid/sizes_vy.csv")

areas <- sizes_go %>% 
  select(2:5) %>% 
  gather() %>% 
  bind_rows(
    sizes_vy %>% 
      select(2:5) %>% 
      gather()) %>% 
  rename(Area = 2)

results <- bind_rows(
  go_100,
  go_500,
  go_catch,
  go_stream,
  vy_100,
  vy_500,
  vy_catch,
  vy_stream) %>% 
  mutate(Area = areas %>% select(Area)) %>% 
  rename(Area = 9) %>% 
  mutate(Biomass_Area = Sum_Biomass/Area)


