#####
# flagship report
# chapter 1 - reproducibility file
#####

### preamble ----

# load
rm(list = ls())
library(readxl)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(cowplot)
library(janitor)
theme_set(theme_cowplot())
setwd("C:/Users/wb582704/OneDrive - WBG/Public institutions flagship/Drafts Part 1")
egvpi_colors <- c("#002345", "#808080", "#D3D3D3", "#ff8000", "#FCDF2D", "#00ADE4")

### budget execution plot ----

# open harmonized boost data
BOOST <- read_csv("analysis/data/BOOST_COFOG_2024_10_31.csv", show_col_types = F)   #filter(!country_name %in% c("Burkina Faso", "Kenya", "Pakistan", "South Africa"))

# calculate budget execution 
BOOST <- BOOST %>% rename(approved = `sum(approved)`,
                          executed = `sum(executed)`)
BOOST <- BOOST %>% mutate(perc_exec = 100* (executed / approved))

# calculate across c
BOOST %>% group_by(country_name) %>% mutate(perc_exec_admin2 = mean(perc_exec, na.rm = T))

# calculate across country variance in perc_exec
BOOST %>% tabyl(country_name)
BOOSTacross <- BOOST %>% 
  filter(perc_exec != Inf & perc_exec != -Inf & !is.na(perc_exec)) %>% 
  group_by(country_name) %>% 
  summarise(perc_exec = mean(perc_exec, na.rm = T))




BOOST %>% filter(country_name == "Kenya") %>% tabyl(admin2)
  select(country_name, adm1_name, admin2, perc_exec, approved, executed)
