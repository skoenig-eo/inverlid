library(tidyverse)



# 0150
files_0150 <- list.files("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/under_ascii",
                         pattern = "h0.00-1.50.txt$", full.names = TRUE)

for(i in 1:length(files_0150)){
  storage[[i]] <- read_table(files_0150[i], col_types = cols(.default = "c"))
  cat(".")}

table_0150 <- bind_rows(storage) %>% 
  transmute_all(.funs = as.numeric) %>% 
  mutate(Type = "h0.00-1.50.txt") %>% 
  write_csv("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/under_0150.csv")



# 0max
files_0max <- list.files("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/under_ascii",
                         pattern = "h0.00-hmax.txt$", full.names = TRUE)

for(i in 1:length(files_0max)){
  storage[[i]] <- read_table(files_0max[i], col_types = cols(.default = "c"))
  cat(".")}

table_0max <- bind_rows(storage) %>% 
  transmute_all(.funs = as.numeric) %>% 
  mutate(Type = "h0.00-hmax.txt") %>% 
  write_csv("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/under_0max.csv")



# 0500
files_0500 <- list.files("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/under_ascii",
                         pattern = "h-0.50-0.00.txt$", full.names = TRUE)

for(i in 1:length(files_0500)){
  storage[[i]] <- read_table(files_0500[i], col_types = cols(.default = "c"))
  cat(".")}

table_0500 <- bind_rows(storage) %>% 
  transmute_all(.funs = as.numeric) %>% 
  mutate(Type = "h-0.50-0.00.txt") %>% 
  write_csv("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/under_0500.csv")



# 0520
files_0520 <- list.files("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/under_ascii",
                         pattern = "h0.50-2.00.txt$", full.names = TRUE)

for(i in 1:length(files_0520)){
  storage[[i]] <- read_table(files_0520[i], col_types = cols(.default = "c"))
  cat(".")}

table_0520 <- bind_rows(storage) %>% 
  transmute_all(.funs = as.numeric) %>% 
  mutate(Type = "h0.50-2.00.txt") %>% 
  write_csv("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/under_0520.csv")



# 0550
files_0550 <- list.files("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/under_ascii",
                         pattern = "h0.50-5.00.txt$", full.names = TRUE)

for(i in 1:length(files_0550)){
  storage[[i]] <- read_table(files_0550[i], col_types = cols(.default = "c"))
  cat(".")}

table_0550 <- bind_rows(storage) %>% 
  transmute_all(.funs = as.numeric) %>% 
  mutate(Type = "h0.50-5.00.txt") %>% 
  write_csv("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/under_0550.csv")



# 5max
files_5max <- list.files("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/under_ascii",
                         pattern = "h-0.50-hmax.txt$", full.names = TRUE)

for(i in 1:length(files_5max)){
  storage[[i]] <- read_table(files_5max[i], col_types = cols(.default = "c"))
  cat(".")}

table_5max <- bind_rows(storage) %>% 
  transmute_all(.funs = as.numeric) %>% 
  mutate(Type = "h-0.50-hmax.txt") %>% 
  write_csv("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/under_5max.csv")



# 1550
files_1550 <- list.files("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/under_ascii",
                         pattern = "h1.50-5.00.txt$", full.names = TRUE)

for(i in 1:length(files_1550)){
  storage[[i]] <- read_table(files_1550[i], col_types = cols(.default = "c"))
  cat(".")}

table_1550 <- bind_rows(storage) %>% 
  transmute_all(.funs = as.numeric) %>% 
  mutate(Type = "h1.50-5.00.txt") %>% 
  write_csv("D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/under_1550.csv")



























# t <- read_table(
# "D:/OneDrive/NPBW/Weitere Projekte/InverLid/Data/under_ascii/needed/mb_03_04__poly1_h0.00-1.50.txt")
