#####
# descriptives for background paper
#####

### preamble and open ----
setwd("C:/Users/wb582704/OneDrive - WBG/Public institutions flagship/Drafts Part 1/analysis/")
rm(list = ls())
library(haven)
library(tidyverse)
library(ggplot2)
library(janitor)
library(readxl)
library(stringr)
library(estimatr)
library(cowplot)
library(reshape2)
library(ggrepel)
theme_set(theme_cowplot())
egvpi_colors <- c("#002345", "#808080", "#D3D3D3", "#ff8000", "#008900", "#00ADE4", "#FCDF2D")

# open
GSPS <- read_csv("data/gsps.csv")