#####
# descriptives for background paper
#####

### preamble and open ----
setwd("C:/Users/wb582704/OneDrive - WBG/EGVPI/Public institutions flagship/replication/chapter 1/")
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
options("print.matrix" = FALSE)
egvpi_colors <- c("#002345", "#808080", "#D3D3D3", "#ff8000", "#008900", "#00ADE4", "#FCDF2D")

# open ----

GTMI <- read_xlsx("C:/Users/wb582704/OneDrive - WBG/EGVPI/Public institutions flagship/Drafts Part 2/procurement/analysis/data/WBG_GovTech Dataset_Oct2022.xlsx",
                  sheet = "CG_GTMI_Data")
REG <- read_dta("C:/Users/wb582704/OneDrive - WBG/Documents/standardized_data/data/regional classification.dta")
WDI <- read_csv("C:/Users/wb582704/OneDrive - WBG/Documents/standardized_data/data/WDI_clean.csv", show_col_types = F) %>%
  select(-region, -incomegroup, -lendingcategory, -otheremuorhipc, -country)

# clean GTMI and merge -------

# rename key columns
GTMI <- GTMI %>% rename(year = Year, country_code = Code, country = Economy)
GTMI <- GTMI %>% filter(!is.na(year))

# merge
GTMI <- left_join(x = GTMI, y = REG, by = c("country_code"))
WDI_wide2022 <- WDI %>% filter(year == 2020 | year == 2022) %>%
  select(-var_name) %>%
  dcast(country_code + year ~ var_code)
GTMI <- left_join(x = GTMI, y = WDI_wide2022, by = c("country_code", "year"))

# from cliar...
# CLIAR made Index of PFM MIS based on GTMI data.
# Considers whether there's an operational FMIS (GTMI_I-5), a TSA (GTMI_I-6), a Tax MIS (GTMI_I-7), a DMS (GTMI_I-13), Customs MIS (GTMI_I-8), a PIMS (GTMI_I-14), and an e-Procurement system in place.

# fmis
# i-5 fmis
# i-5.5 fmis launch year
# i-5.7 scope of fmis
# i-5.13 does fmis exchange data with other systems
# i-5.14 governance of fmis operations
GTMI <- GTMI %>% rename(fmis = `I-5`,
                        fmis_year = `I-5.5`,
                        fmis_scope = `I-5.7`,
                        fmis_exchange = `I-5.13`,
                        fmis_gov = `I-5.14`)
GTMI <- GTMI %>% mutate(
  fmis = case_when(
    fmis == 0 | fmis == 1 ~ 0,
    fmis == 2 ~ 1
  ),
  fmis_year = ifelse(fmis_year == "-", NA, fmis_year),
  fmis_scope = case_when(
    fmis_scope == 0 ~ NA,
    fmis_scope == 1 ~ 0,
    fmis_scope == 2 ~ 1 # recode so == 1 if fmis is both central and local gov
  ),
  fmis_exchange = case_when(
    fmis_exchange == "New" ~ NA,
    (fmis_exchange == 0 | fmis_exchange == 1) ~ 0,
    fmis_exchange == 2 ~ 1 # recode so == 1 if fmis can exchange in any format
  ),
  fmis_gov = ifelse(fmis_gov == "New", NA, fmis_gov)
)

# tsa
# i-6 tsa
# i-6.2 tsa launch year
# i-6.5 scope of tsa
# i-6.7 tsa linked to fmis
# i-6.8 governance of tsa
GTMI <- GTMI %>% rename(tsa = `I-6`,
                        tsa_year = `I-6.2`,
                        tsa_scope = `I-6.5`,
                        tsa_link = `I-6.7`,
                        tsa_gov = `I-6.8`)
GTMI <- GTMI %>% mutate(
  tsa = case_when(
    tsa == 0 | tsa == 1 ~ 0,
    tsa == 2 ~ 1
  ),
  tsa_year = ifelse(tsa_year == "-", NA, tsa_year),
  tsa_scope = case_when(
    tsa_scope == "New" ~ NA,
    tsa_scope == 1 ~ 0,
    tsa_scope == 2 ~ 1
  ),
  tsa_link = case_when(
    tsa_link == "New" ~ NA,
    tsa_link == 1 ~ 0,
    tsa_link == 2 ~ 1
  ),
  tsa_gov = ifelse(tsa_gov == "New", NA, tsa_gov)
)

# customs
# i-8 customs
# i-8.2 customs launch year
# i-8.8 customs exchange
# i-8.9 customs governance
GTMI <- GTMI %>% rename(customs = `I-8`,
                        customs_year = `I-8.2`,
                        customs_exchange = `I-8.8`,
                        customs_gov = `I-8.9`)
GTMI <- GTMI %>% mutate(
  customs = case_when(
    customs == 0 | customs == 1 ~ 0,
    customs == 2 ~ 1
  ),
  customs_year = ifelse(customs_year == "-", NA, customs_year),
  customs_exchange = case_when(
    customs_exchange == "New" ~ NA,
    (customs_exchange == 0 | customs_exchange == 1) ~ 0,
    customs_exchange == 2 ~ 1 # recode so == 1 if customs can exchange in any format
  ),
  customs_gov = ifelse(customs_gov == "New", NA, customs_gov)
)

# hrmis
# i-9 hrmis
# i-9.3 hrmis launch year
# i-9.6 hrmis self serve
# i-9.7 hrmis exchange
# i-9.9 hrmis governance
GTMI <- GTMI %>% rename(hrmis = `I-9`,
                        hrmis_year = `I-9.3`,
                        hrmis_selfserve = `I-9.6`,
                        hrmis_exchange = `I-9.7`,
                        hrmis_gov = `I-9.9`)
GTMI <- GTMI %>% mutate(
  hrmis = case_when(
    hrmis == 0 | hrmis == 1 ~ 0,
    hrmis == 2 ~ 1
  ),
  hrmis_year = ifelse(hrmis_year == "-", NA, hrmis_year),
  hrmis_exchange = case_when(
    hrmis_exchange == "New" ~ NA,
    (hrmis_exchange == 0 | fmis_exchange == 1) ~ 0,
    hrmis_exchange == 2 ~ 1 # recode so == 1 if hrmis can exchange in any format
  ),
  hrmis_exchange = case_when(
    hrmis_exchange == "New" ~ NA,
    (hrmis_exchange == 0 | hrmis_exchange == 1) ~ 0,
    hrmis_exchange == 2 ~ 1 # recode so == 1 if hrmis can exchange in any format
  ),
  hrmis_gov = ifelse(hrmis_gov == "New", NA, hrmis_gov)
)

# payroll
# i-10 payroll
# i-10.3 payroll year
# i-10.6 payroll governance
GTMI <- GTMI %>% rename(payroll = `I-10`,
                        payroll_year = `I-10.3`,
                        payroll_gov = `I-10.6`)
GTMI <- GTMI %>% mutate(
  payroll = case_when(
    payroll == 0 | payroll == 1 ~ 0,
    payroll == 2 ~ 1
  ),
  payroll_year = ifelse(payroll_year == "-", NA, payroll_year),
  payroll_gov = ifelse(payroll_gov == "New", NA, payroll_gov)
)

# eGP
# i-12 e-procurement
# i-12.3 e-proc year
# i-12.4 e-proc capabilities
# i-12.5 e-proc OCDS
# i-12.6 e-proc exchange
# i-12.8 e-proc governance
GTMI <- GTMI %>% rename(proc = `I-12`,
                        proc_year = `I-12.3`,
                        proc_capab = `I-12.4`,
                        proc_ocds = `I-12.5`,
                        proc_exchange = `I-12.6`,
                        proc_gov = `I-12.8`)
GTMI <- GTMI %>% mutate(
  proc = case_when(
    proc == 0 | proc == 1 ~ 0,
    proc == 2 ~ 1
  ),
  proc_year = ifelse(proc_year == "-", NA, proc_year),
  proc_ocds = ifelse(proc_year == "New", NA, proc_ocds),
  proc_exchange = case_when(
    proc_exchange == "New" ~ NA,
    (proc_exchange == 0 | proc_exchange == 1) ~ 0,
    proc_exchange == 2 ~ 1 # recode so == 1 if proc can exchange in any format
  ),
  proc_gov = ifelse(proc_gov == "New", NA, proc_gov)
)

# debt
# i-13 debt management
# i-13.3 debt year
GTMI <- GTMI %>% rename(debt = `I-13`,
                        debt_year = `I-13.3`)
GTMI <- GTMI %>% mutate(
  debt = case_when(
    debt == 0 | debt == 1 ~ 0,
    debt == 2 ~ 1
  ),
  debt_year = ifelse(debt_year == "-", NA, debt_year)
)

# pims
# i-14 pims
# i-14.3 pims year
# i-14.5 pims functional capability
# i-14.6 pims exchange
# i-14.7 pims publishing
GTMI <- GTMI %>% rename(pims = `I-14`,
                        pims_year = `I-14.3`,
                        pims_capab = `I-14.5`,
                        pims_exchange = `I-14.6`,
                        pims_pub = `I-14.7`)
GTMI <- GTMI %>% mutate(
  pims = case_when(
    pims == 0 | pims == 1 ~ 0,
    pims == 2 ~ 1
  ),
  pims_year = ifelse(pims_year == "-", NA, pims_year),
  pims_exchange = case_when(
    pims_exchange == "New" ~ NA,
    (pims_exchange == 0 | pims_exchange == 1) ~ 0,
    pims_exchange == 2 ~ 1 # recode so == 1 if pims can exchange in any format
  ),
  pims_pub = case_when(
    pims_pub == "New" ~ NA,
    pims_pub == 1 | pims_pub == 0 ~ 0,
    pims_pub == 2 ~ 1
  )
)

# country/year descriptives
GTMI <- GTMI %>% group_by(country_code, year) %>% mutate(
  perc_coreMIS = (sum(fmis + tsa + customs + hrmis + payroll + proc + debt + pims) / 8)
)

# order incomegroup
GTMI <- GTMI %>% ungroup() %>% mutate(incomegroup_order = case_when(
  incomegroup == "High income" ~ 1,
  incomegroup == "Upper middle income" ~ 2,
  incomegroup == "Lower middle income" ~ 3,
  incomegroup == "Low income" ~ 4
))

# plot ----

# within country variation in transparency
GTMItrans2022 <- GTMI %>% filter(year == "2022") %>%
  mutate(fmis_trans = case_when(
    fmis == 0 ~ "No system",
    fmis == 1 & fmis_gov != 2 ~ "System present, but country does not publish",
    fmis == 1 & fmis_gov == 2 ~ "System present and country does publish"),
    tsa_trans = case_when(
      tsa == 0 ~ "No system",
      tsa == 1 & tsa_gov != 2 ~ "System present, but country does not publish",
      tsa == 1 & tsa_gov == 2 ~ "System present and country does publish"),
    customs_trans = case_when(
      customs == 0 ~ "No system",
      customs == 1 & customs_gov != 2 ~ "System present, but country does not publish",
      customs == 1 & customs_gov == 2 ~ "System present and country does publish"),
    hrmis_trans = case_when(
      hrmis == 0 ~ "No system",
      hrmis == 1 & hrmis_gov != 2 ~ "System present, but country does not publish",
      hrmis == 1 & hrmis_gov == 2 ~ "System present and country does publish"),
    payroll_trans = case_when(
      payroll == 0 ~ "No system",
      payroll == 1 & payroll_gov != 2 ~ "System present, but country does not publish",
      payroll == 1 & payroll_gov == 2 ~ "System present and country does publish"),
    proc_trans = case_when(
      proc == 0 ~ "No system",
      proc == 1 & proc_gov != 2 ~ "System present, but country does not publish",
      proc == 1 & proc_gov == 2 ~ "System present and country does publish")) %>%
  select(country, country_code, incomegroup, contains("trans")) %>%
  melt(id.vars = c("country", "country_code", "incomegroup")) %>%
  filter(!is.na(incomegroup) & incomegroup != "") %>%
  mutate(system = case_when(
    variable == "fmis_trans" ~ "FMIS",
    variable == "tsa_trans" ~ "TSA",
    variable == "customs_trans" ~ "Customs",
    variable == "hrmis_trans" ~ "HRMIS",
    variable == "payroll_trans" ~ "Payroll",
    variable == "proc_trans" ~ "Procurement"
  )) %>% rename(trans = value)
GTMItrans2022 <- GTMItrans2022 %>% mutate(flag = case_when(
  country == "Armenia" ~ "Armenia",
  country == "Malawi" ~ "Malawi",
  TRUE ~ "Not flagged"
), trans_order = case_when(
  trans == "No system" ~ 1,
  trans == "System present, but country does not publish" ~ 2,
  trans == "System present and country does publish" ~ 3
))

# plot
GTMItrans2022 %>% ggplot(aes(x = reorder(trans, trans_order), y = system,)) +
  geom_point(aes(color = flag, shape = flag, alpha = flag, size = flag), position = position_jitter(width = 0.15)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 18)) +
  scale_color_manual(name = "Countries", values = egvpi_colors[c(6,4,2)],
                     labels = c("Armenia", "Malawi", "All other countries")) +
  scale_alpha_manual(name = "Countries", values = c(1, 1, 1),
                     labels = c("Armenia", "Malawi", "All other countries")) +
  scale_shape_manual(name = "Countries", values = c(19, 19, 1),
                     labels = c("Armenia", "Malawi", "All other countries")) +
  scale_size_manual(name = "Countries", values = c(3.5, 3.5, 2),
                    labels = c("Armenia", "Malawi", "All other countries")) +
  labs(x = "Transparency in key system governance information\n(audits, compliance, etc.)",
       y = "Information system")
ggsave(filename = "figs/trans_permis.png", width = 12, height = 9)

# stacked bar graph versio
GTMItrans2022 %>%
  group_by(system, trans) %>%
  summarise(n_countries = n()) %>%
  ggplot(aes(x = system, y = n_countries, fill = trans)) +
  geom_bar(stat = "identity") +
  labs(x = "Information system", y = "Number of countries",
       fill = "Transparency\nin system governance") +
  scale_fill_manual(values = egvpi_colors[c(2,6,1)]) +
  guides(fill = guide_legend(ncol = 2)) +
  theme(legend.position = "bottom")
ggsave(filename = "figs/trans_permis_stacked.png", width = 12, height = 9)


