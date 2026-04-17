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
library(stringi)
library(estimatr)
library(cowplot)
library(reshape2)
library(ggrepel)
theme_set(theme_cowplot())
egvpi_colors <- c("#002345", "#808080", "#D3D3D3", "#ff8000", "#008900", "#00ADE4", "#FCDF2D")

### ARMENIA -----

# OPEN
ARM <- read_xlsx(path = "data/boost/boost_arm_2006_2022.xlsx", sheet = "expenditure") %>%
  select(-`econ/func`, -`...18`, -`...19`, -`...20`)

# remove accents
ARM <- ARM %>% mutate(across(.cols = everything(),
                             .fns = ~toupper(stri_trans_general(str = ., id = "Latin-ASCII"))))

# make numeric
ARM <- ARM %>% mutate(across(.cols = c("approved", "adjusted", "executed"),
                             .fns = ~as.numeric(.)))

# label ministries
ARM <- ARM %>% mutate(min_category = case_when(
  admin == "104021 MINISTRY OF FINANCE OF RA" ~ "Ministry of Finance",
  admin == "104010 MINISTRY OF EDUCATION AND SCIENCE OF RA" ~ "Ministry of Education",
  admin == "104018 MINISTRY OF TRANSPORT AND COMMUNICATION OF RA" ~ "Ministry of Infrastructure",
  TRUE ~ NA
))

# group ARM
ARMmin <- ARM %>% filter(!is.na(min_category)) %>%
  group_by(year, min_category) %>%
  summarise(approved = sum(approved, na.rm = T),
            executed = sum(executed, na.rm = T),
            perc_exec = executed / approved)
ARMmin$country <- "Armenia"

### PARAGUAY -----

# OPEN
PGY <- read_xlsx(path = "data/boost/boost_pgy_2006_2022.xlsx", sheet = "Central")

# remove accents
PGY <- PGY %>% mutate(across(.cols = everything(),
                             .fns = ~toupper(stri_trans_general(str = ., id = "Latin-ASCII"))))

# clean columns
PGY <- PGY[,-c(which(names(PGY) == "Foreign"):ncol(PGY))]

# make numeric
PGY <- PGY %>% mutate(across(.cols = c("APPROVED", "PAID"),
                             .fns = ~as.numeric(.)))

# label ministries
PGY <- PGY %>% mutate(min_category = case_when(
  ADMIN2 == "12.006 - MINISTERIO DE HACIENDA" ~ "Ministry of Finance",
  (ADMIN2 == "12.007 - MINISTERIO DE EDUCACION Y CIENCIAS" |  ADMIN2 == "12.007 - MINISTERIO DE EDUCACION Y CULTURA" )~ "Ministry of Education",
  ADMIN2 == "12.013 - MINISTERIO DE OBRAS PUBLICAS Y COMUNICACIONES" ~ "Ministry of Infrastructure",
  TRUE ~ NA
))

# change approved / paid names
PGY <- PGY %>% rename(approved = APPROVED,
                      paid = PAID,
                      year = YEAR)
PGY$year <- as.numeric(PGY$year)

# group PGY
PGYmin <- PGY %>% filter(!is.na(min_category)) %>%
  group_by(year, min_category) %>%
  summarise(approved = sum(approved, na.rm = T),
            executed = sum(paid, na.rm = T),
            perc_exec = executed / approved)
PGYmin$country <- "Paraguay"

# clean ministries
PGY <- PGY %>% mutate(min_clean = case_when(
  ADMIN2 == "12.006 - MINISTERIO DE HACIENDA" ~ "Finance",
  (ADMIN2 == "12.007 - MINISTERIO DE EDUCACION Y CIENCIAS" |  ADMIN2 == "12.007 - MINISTERIO DE EDUCACION Y CULTURA") ~ "Education",
  ADMIN2 == "12.008 - MINISTERIO DE SALUD PUBLICA Y BIENESTAR SOCIAL" ~ "Health",
  ADMIN2 == "12.010 - MINISTERIO DE AGRICULTURA Y GANADERIA" ~ "Agriculture",
  ADMIN2 == "12.013 - MINISTERIO DE OBRAS PUBLICAS Y COMUNICACIONES" ~ "Infrastructure",
  TRUE ~ NA
))
PGYmin_clean <- PGY %>% filter(!is.na(min_clean)) %>%
  group_by(year, min_clean) %>%
  summarise(approved = sum(approved, na.rm = T),
            executed = sum(paid, na.rm = T),
            perc_exec = executed / approved)
PGYmin_clean$country <- "Paraguay"

### UGANDA -----

# OPEN
UGA <- read_xlsx(path = "data/boost/boost_uga_2004_2022.xlsx", sheet = "BOOST")

# remove accents
UGA <- UGA %>% mutate(across(.cols = everything(),
                             .fns = ~toupper(stri_trans_general(str = ., id = "Latin-ASCII"))))

# make numeric
UGA <- UGA %>% mutate(across(.cols = c("Budget", "Expenditure"),
                             .fns = ~as.numeric(.)))

# label ministries
UGA <- UGA %>% mutate(min_category = case_when(
  (Vote == "008 MINISTRY OF FINANCE, PLANNING AND ECONOMIC DEVELOPMENT" | Vote == "008 MINISTRY OF FINANCE, PLANNING & ECONOMIC DEVELOPMENT") ~ "Ministry of Finance",
  (Vote == "013 MINISTRY OF EDUCATION AND SPORTS" | Vote == "013 MINISTRY OF EDUCATION & SPORTS") ~ "Ministry of Education",
  (Vote == "016 MINISTRY OF WORKS AND TRANSPORT" | Vote == "016 MINISTRY OF WORKS & TRANSPORT") ~ "Ministry of Infrastructure",
  TRUE ~ NA
))

# change approved / paid names
UGA <- UGA %>% rename(approved = Budget,
                      paid = Expenditure,
                      year = Year)
UGA$year <- as.numeric(str_extract(UGA$year, pattern = "^\\d{4}"))

# group UGA
UGAmin <- UGA %>% filter(!is.na(min_category)) %>%
  group_by(year, min_category) %>%
  summarise(approved = sum(approved, na.rm = T),
            executed = sum(paid, na.rm = T),
            perc_exec = executed / approved)
UGAmin$country <- "Uganda"

# clean ministries
UGA <- UGA %>% mutate(min_clean = case_when(
  (Vote == "008 MINISTRY OF FINANCE, PLANNING & ECONOMIC DEVELOPMENT" | Vote == "008 MINISTRY OF FINANCE, PLANNING AND ECONOMIC DEVELOPMENT") ~ "Finance",
  (Vote == "010 MINISTRY OF AGRICULTURE, ANIMAL INDUSTRY & FISHERIES" | Vote == "010 MINISTRY OF AGRICULTURE, ANIMAL INDUSTRY AND FISHERIES") ~ "Agriculture",
  (Vote == "013 MINISTRY OF EDUCATION AND SPORTS" | Vote == "013 MINISTRY OF EDUCATION & SPORTS") ~ "Education",
  (Vote == "014 MINISTRY OF HEALTH") ~ "Health",
  (Vote == "016 MINISTRY OF WORKS AND TRANSPORT" | Vote == "016 MINISTRY OF WORKS & TRANSPORT") ~ "Infrastructure",
  TRUE ~ NA
))
UGAmin_clean <- UGA %>% filter(!is.na(min_clean)) %>%
  group_by(year, min_clean) %>%
  summarise(approved = sum(approved, na.rm = T),
            executed = sum(paid, na.rm = T),
            perc_exec = executed / approved)
UGAmin_clean$country <- "Uganda"

### Togo -----

# OPEN
TGO <- read_xlsx(path = "data/boost/boost_tgo_2009_2021.xlsx", sheet = "BOOST")

# remove accents
TGO <- TGO %>% mutate(across(.cols = everything(),
                             .fns = ~toupper(stri_trans_general(str = ., id = "Latin-ASCII"))))

# make numeric
TGO <- TGO %>% mutate(across(.cols = c("Budget", "Expenditure"),
                             .fns = ~as.numeric(.)))

# change approved / paid names
TGO <- TGO %>% rename(approved = `Dotation Initiale`,
                      paid = `Mandat Prise en Charge`,
                      year = GESTION)
TGO$year <- as.numeric(str_extract(TGO$year, pattern = "^\\d{4}"))
TGO$approved <- as.numeric(TGO$approved)
TGO$paid <- as.numeric(TGO$paid)

# clean ministries
TGO <- TGO %>% mutate(min_clean = case_when(
  grepl(Section, pattern = "^210") ~ "Finance",
  grepl(Section, pattern = "^510") ~ "Education",
  grepl(Section, pattern = "^610") ~ "Health",
  grepl(Section, pattern = "^810") ~ "Agriculture",
  grepl(Section, pattern = "^83") ~ "Infrastructure",
  TRUE ~ NA
))
TGOmin_clean <- TGO %>% filter(!is.na(min_clean)) %>%
  group_by(year, min_clean) %>%
  summarise(approved = sum(approved, na.rm = T),
            executed = sum(paid, na.rm = T),
            perc_exec = executed / approved)
TGOmin_clean$country <- "Togo"

### combine armenia, paragruay, and uganda ----

# combine
ARMmin$year <- as.numeric(ARMmin$year)
PGYmin$year <- as.numeric(PGYmin$year)
UGAmin$year <- as.numeric(UGAmin$year)
BOOST <- bind_rows(ARMmin, PGYmin, UGAmin)
BOOST$min_category[BOOST$min_category == "Ministry of Infrastructure"] <- "Ministry of Infrastructure"

# plot
BOOST %>% filter(year >= 2011, year <= 2021) %>%
  ggplot(aes(x = year, y = perc_exec,
                     group = min_category, color = min_category)) +
  geom_line(linewidth = 1.1) +
  geom_hline(yintercept = 1, linetype = 2, linewidth = 1) +
  scale_y_continuous(limits = c(0, 2.1), breaks = seq(0, 2, 0.2), labels = scales::percent_format()) +
  scale_x_continuous(limits = c(2011, 2021), breaks = seq(2011, 2021, 2)) +
  scale_color_manual(values = egvpi_colors[c(1, 2, 6)]) +
  labs(color = "Ministry",
       y = "Budget Execution\n(Paid / Budgeted)",
       x = "Year",
       title = "Budget execution across countries and public institutions",
       caption = "Source: BOOST. These three countries are selected based on data availability \nas well as preference for countries with different income-levels form different regions.") +
  facet_wrap(~country) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 10))
ggsave(last_plot(), filename = "figs/finresources_institutions.png", width = 12, height = 8)





