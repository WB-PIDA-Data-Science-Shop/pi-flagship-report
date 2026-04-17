# ==============================================================================
# public institutions report
# reproducibility file
# prepare data for chapter 2
# ==============================================================================
# ==============================================================================
# preamble
# ==============================================================================
# load
library(readxl)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(cowplot)
library(haven)
library(janitor)
library(stringi)
theme_set(theme_cowplot())
egvpi_colors <- c("#002345", "#808080", "#D3D3D3",
                  "#ff8000", "#FCDF2D", "#00ADE4")

# open data
MAPS <- read_excel("chapter-2/data/input/maps.xlsx")
REG <- read_dta("chapter-2/data/input/regions.dta")
CwG <- read_xlsx("chapter-2/data/input/cwg.xlsx",
                 sheet = "DB CwG score 191 economies") %>% 
  rename(country_code = EconomyCode) %>% 
  select(-Economy, -Region, -IncomeGroup)
WDI <- read_csv("chapter-2/data/input/wdi.csv", show_col_types = F)
GTMI <- read_xlsx("chapter-2/data/input/gtmi.xlsx",
                  sheet = "CG_GTMI_Data") %>% 
  filter(Year == 2022) 
BFA <- read_xlsx("chapter-2/data/input/bfa.xlsx", sheet = "2017-20")

# ==============================================================================
# clean and save MAPS 
# ==============================================================================

# keep only some columns
MAPS <- MAPS %>% select(-quant_flag)

# fix issue with indicator_number
MAPS$indicator_number[MAPS$indicator_number == "1.1000000000000001"] <- "1.1"

# replace N/As
MAPS <- MAPS %>% mutate(across(.cols = everything(), .fns = ~ifelse(. == "N/A", NA, .)))
MAPS <- MAPS %>% mutate(across(.cols = everything(), .fns = ~ifelse(. == "NA", NA, .)))
MAPS <- MAPS %>% mutate(across(.cols = everything(), .fns = ~ifelse(. == ".", NA, .)))

# reshape
MAPS <- MAPS %>% melt(id.vars = c("pillar_number", "pillar", "indicator_number", "indicator",
                                  "subindicator_number", "subindicator", "criteria_number",
                                  "criteria", "criteria_concept", "subindicator_concept", "legal"),
                      variable.name = "country")
MAPS$value <- as.numeric(MAPS$value)
MAPS$country <- as.character(MAPS$country)
MAPS$legal <- as.numeric(MAPS$legal)

# n and percentage of points by pillar
MAPSpillar <- MAPS %>% group_by(country, pillar_number) %>% 
  summarise(n_pillar = n(),
            sum_pillar = sum(value, na.rm = T),
            perc_pillar = (sum_pillar / (2 * n_pillar)))
MAPSpillar <- MAPSpillar %>% 
  pivot_wider(id_cols = country, names_from = pillar_number,
              values_from = c(n_pillar, sum_pillar, perc_pillar))

# n and percentage of points by pillar / legal
MAPSpillar_legal <- MAPS %>% group_by(country, pillar_number, legal) %>% 
  summarise(n_pillar = n(),
            sum_pillar = sum(value, na.rm = T),
            perc_pillar = (sum_pillar / (2 * n_pillar)))

# n and percentage of points by framework concept
MAPSconcept <- MAPS %>% filter(!is.na(criteria_concept)) %>% 
  group_by(country, criteria_concept) %>% 
  summarise(n_concept = n(),
            sum_concept = sum(value, na.rm = T),
            perc_concept = (sum_concept / (2* n_concept))) %>% 
  mutate(criteria_concept = case_when(
    criteria_concept == "accountability" ~ "account",
    criteria_concept == "independence" ~ "ind",
    criteria_concept == "information systems" ~ "infosyst",
    criteria_concept == "management systems" ~ "mansyst",
    criteria_concept == "money" ~ "money",
    criteria_concept == "people" ~ "personnel",
    criteria_concept == "transparency" ~ "trans",
    TRUE ~ NA
  ))
MAPSconcept <- MAPSconcept %>% 
  pivot_wider(id_cols = country, names_from = criteria_concept,
              values_from = c(n_concept, sum_concept, perc_concept))

# n and percentage of points by framework concept / legal
MAPSconcept_legal <- MAPS %>% filter(!is.na(criteria_concept)) %>% 
  group_by(country, criteria_concept, legal) %>% 
  summarise(n_concept = n(),
            sum_concept = sum(value, na.rm = T),
            perc_concept = (sum_concept / (2* n_concept))) %>% 
  mutate(criteria_concept = case_when(
    criteria_concept == "accountability" ~ "account",
    criteria_concept == "independence" ~ "ind",
    criteria_concept == "information systems" ~ "infosyst",
    criteria_concept == "management systems" ~ "mansyst",
    criteria_concept == "money" ~ "money",
    criteria_concept == "people" ~ "personnel",
    criteria_concept == "transparency" ~ "trans",
    TRUE ~ NA
  ))

# combine
MAPS <- left_join(x = MAPSpillar, y = MAPSconcept, by = c("country"))
MAPS <- left_join(x = MAPS, y = REG, by = c("country" = "economy")) %>% 
  select(country, country_code, region, incomegroup, lendingcategory, everything())

# remove countries whose data is not published
MAPS <- MAPS %>% filter(!country_code %in% c("ZMB", "IND", "DJI"))

# save 
write_csv(MAPS, file = "chapter-2/data/output/maps_clean.csv")

# ==============================================================================
# clean and save CwG
# ==============================================================================

# rename CwG 
CwG <- CwG %>% rename(
  score_procedure = `Score Procedure`,
  score_time = `Score Time`,
  score_legal = `Legal Index Score`,
  score_eproc = `E-Proc Index Score`,
  score_cwg = `Ease of contracting with the government score (0-100)`
)

# concert CwG to numeric
CwG <- CwG %>% mutate(across(.cols = contains("score_"), .fns = ~ifelse(. == "No practice", NA, .)))
CwG <- CwG %>% mutate(across(.cols = contains("score_"), .fns = ~as.numeric(.)))

# merge CwG and WDI
WDI22 <- WDI %>% filter(year == 2022) %>% select(country_code, var_code, value) %>% 
  dcast(country_code ~ var_code) 
CwG <- CwG %>% left_join(y = WDI22, by = c("country_code"))

# fix missing in lebanon
CwG$gdppercap_2015usd[CwG$country_code == "LBN"] <- WDI$value[WDI$country_code == "LBN" &
                                                                WDI$year == "2021" &
                                                                WDI$var_code == "gdppercap_2015usd"]

# save 
write_csv(CwG, file = "chapter-2/data/output/cwg_clean.csv")

# ==============================================================================
# clean and save GTMI
# ==============================================================================

# prep for GTMI panel
  
  # rename
  GTMIpanel <- GTMI %>%
  select(Year, Code, Economy, contains("I-12")) %>% 
    rename(
      year = Year,
    country = Economy,
    country_code = Code,
    eproc = `I-12`,
    eproc_name = `I-12.1`,
    eproc_url = `I-12.2`,
    eproc_year = `I-12.3`,
    eproc_capab = `I-12.4`,
    eproc_ocds = `I-12.5`,
    eproc_exch = `I-12.6`,
    eproc_innov = `I-12.7`,
    eproc_gov = `I-12.8`,
  )
  
  # separate details to merge back later
  details <- GTMIpanel %>% select(country_code, eproc_capab, eproc_ocds, eproc_exch, eproc_gov)
  details$eproc_onlinetender <- ifelse(details$eproc_capab >= 2, 1, 0)
  details$eproc_exch <- ifelse(details$eproc_exch == 2, 1, 0)
  details$eproc_gov <- ifelse(details$eproc_gov == 2, 1, 0)
  details <- details %>% select(-eproc_capab)
  
  # prep for GTMIpanel
    GTMIpanel <- GTMIpanel %>% select(country_code, eproc_year) 
    GTMIpanel$eproc_year[GTMIpanel$eproc_year == "-"] <- NA
    GTMIpanel$eproc_year <- as.numeric(GTMIpanel$eproc_year)
    
    GTMIpanel_eGPall <- GTMIpanel %>% filter(!is.na(eproc_year)) %>% 
      tidyr::expand(country_code, "eproc_year" = full_seq(eproc_year, 1)) 
    GTMIpanel$eproc <- 1
    
    # merge back details
    GTMIpanel <- GTMIpanel %>% left_join(y = details, by = "country_code")
    
    # create panel
    GTMIpanel_eGP <- GTMIpanel_eGPall %>% 
      left_join(GTMIpanel) %>% 
      arrange(country_code, eproc_year) %>% 
      group_by(country_code) %>% 
      fill(eproc, .direction = "down") %>% 
      fill(eproc_onlinetender, .direction = "down") %>% 
      fill(eproc_ocds, .direction = "down") %>% 
      fill(eproc_exch, .direction = "down") %>% 
      fill(eproc_gov, .direction = "down") 
    GTMIpanel <- GTMIpanel_eGP %>% mutate(across(.cols = contains("eproc"),
                                                 .fns = ~ifelse(is.na(.), 0, .)))
    GTMIpanel <- GTMIpanel %>% rename(year = eproc_year)
    
    # merge
    GTMIpanel <- left_join(x = GTMIpanel, y = REG, by = c("country_code"))
    
    # save GTMIpanel
    write_csv(GTMIpanel, file = "chapter-2/data/output/gtmipanel_clean.csv")

# prep for GTMIwide
    
  # rename key columns
  GTMIwide <- GTMI %>% rename(year = Year, country_code = Code, country = Economy)
  GTMIwide <- GTMIwide %>% filter(!is.na(year))
    
  # merge
  GTMIwide <- left_join(x = GTMIwide, y = REG, by = c("country_code"))
    
  # fmis
  # i-5 fmis
  # i-5.5 fmis launch year
  # i-5.7 scope of fmis
  # i-5.13 does fmis exchange data with other systems
  # i-5.14 governance of fmis operations
  GTMIwide <- GTMIwide %>% rename(fmis = `I-5`,
                          fmis_year = `I-5.5`,
                          fmis_scope = `I-5.7`,
                          fmis_exchange = `I-5.13`,
                          fmis_gov = `I-5.14`)
  GTMIwide <- GTMIwide %>% mutate(
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
  GTMIwide <- GTMIwide %>% rename(tsa = `I-6`,
                          tsa_year = `I-6.2`,
                          tsa_scope = `I-6.5`,
                          tsa_link = `I-6.7`,
                          tsa_gov = `I-6.8`)
  GTMIwide <- GTMIwide %>% mutate(
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
  GTMIwide <- GTMIwide %>% rename(customs = `I-8`,
                          customs_year = `I-8.2`,
                          customs_exchange = `I-8.8`,
                          customs_gov = `I-8.9`)
  GTMIwide <- GTMIwide %>% mutate(
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
  GTMIwide <- GTMIwide %>% rename(hrmis = `I-9`,
                          hrmis_year = `I-9.3`,
                          hrmis_selfserve = `I-9.6`,
                          hrmis_exchange = `I-9.7`,
                          hrmis_gov = `I-9.9`)
  GTMIwide <- GTMIwide %>% mutate(
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
  GTMIwide <- GTMIwide %>% rename(payroll = `I-10`,
                            payroll_year = `I-10.3`,
                            payroll_gov = `I-10.6`)
    GTMIwide <- GTMIwide %>% mutate(
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
  GTMIwide <- GTMIwide %>% rename(proc = `I-12`,
                            proc_year = `I-12.3`,
                            proc_capab = `I-12.4`,
                            proc_ocds = `I-12.5`,
                            proc_exchange = `I-12.6`,
                            proc_gov = `I-12.8`)
    GTMIwide <- GTMIwide %>% mutate(
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
  GTMIwide <- GTMIwide %>% rename(debt = `I-13`,
                            debt_year = `I-13.3`)
  GTMIwide <- GTMIwide %>% mutate(
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
  GTMIwide <- GTMIwide %>% rename(pims = `I-14`,
                            pims_year = `I-14.3`,
                            pims_capab = `I-14.5`,
                            pims_exchange = `I-14.6`,
                            pims_pub = `I-14.7`)
  GTMIwide <- GTMIwide %>% mutate(
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
    
  # set income group order
  GTMIwide <- GTMIwide %>% mutate(incomegroup_order = case_when(
    incomegroup == "High income" ~ 4,
    incomegroup == "Upper middle income" ~ 3,
    incomegroup == "Lower middle income" ~ 2,
    incomegroup == "Low income" ~ 1
  ))
  
  # keep and save
  GTMIwide <- GTMIwide %>% 
    select(year, country, country_code, economy, region, incomegroup, lendingcategory,
           fmis, tsa, customs, hrmis, payroll, proc, contains("_"))
  write_csv(GTMIwide, file = "chapter-2/data/output/gtmiwide_clean.csv")
  
# ==============================================================================
# clean and save burkina faso BOOST data
# ==============================================================================

# remove accents
BFA <- BFA %>% mutate(across(.cols = everything(),
                             .fns = ~stri_trans_general(str = ., id = "Latin-ASCII")))

# make numeric
BFA$Committed <- as.numeric(BFA$Committed)
BFA$Paid <- as.numeric(BFA$Paid)

# cleaning 
BFA$ADMIN2id <- str_extract(BFA$ADMIN2, pattern = "^\\d+")

# create an indicator for procurement units in ADMIN2
BFA$dmp <- ifelse(grepl(BFA$ADMIN2, pattern = "((M|m)arche)"), 1, 0)
BFA$dmp[grepl(BFA$ADMIN2, pattern = "Controle des Marches Publiques")] <- 0
BFA$daf <- ifelse(grepl(BFA$ADMIN2, pattern = "(((A|a)ffaires.*(F|f)ina(n)?c))|((A|a)dmin.*(F|f)inan)"), 1, 0)

# fill in proc as == 1 if there is a DMP; if there is no DMP, then fill in DAF == 1
BFA <- BFA %>% mutate(hasproc = ifelse(dmp == 1, 1, NA)) %>% 
  group_by(YEAR, ADMIN1) %>% fill(hasproc, .direction = "updown") 
# mutate(hasproc = ifelse(is.na(hasproc) & daf == 1, 1, hasproc)) %>% 
#  group_by(YEAR, ADMIN1) %>% fill(hasproc, .direction = "updown") 
BFA$proc <- ifelse((BFA$hasproc == 1 & BFA$dmp == 1) | 
                     (is.na(BFA$hasproc) & BFA$dmp == 0 & BFA$daf == 1), 1, 0)

# create an indicator for personnel spending in ECON1
BFA$pers <- ifelse(grepl(BFA$ECON1, pattern = "2 Depenses de personnel"), 1, 0)

# correlate share of spending on goods / services
admin1_perc_invest <- BFA %>% ungroup() %>%
  select(ADMIN1, ECON1, Paid) %>% 
  group_by(ADMIN1) %>% mutate(admin1_paid = sum(Paid)) %>% 
  group_by(ADMIN1, ECON1) %>% mutate(sum_econ1 = sum(Paid)) %>% 
  distinct(ADMIN1, ECON1, sum_econ1, admin1_paid) %>% 
  filter(sum_econ1 > 0) %>%
  mutate(perc_econ1 = sum_econ1 / admin1_paid) %>% 
  filter(ECON1 == "5 Investissements executes par l'Etat")
admin1_perc_proc <- BFA %>%
  ungroup() %>%
  select(ADMIN1, ADMIN2, Paid, proc) %>% 
  group_by(ADMIN1) %>% mutate(admin1_paid = sum(Paid)) %>% 
  group_by(ADMIN1, ADMIN2) %>% mutate(admin2_paid = sum(Paid)) %>% 
  filter(proc == 1) %>% 
  distinct(ADMIN1, ADMIN2, admin1_paid, admin2_paid) %>% 
  mutate(perc_proc_paid = admin2_paid / admin1_paid) %>% select(-admin1_paid)

# if there are both DMP and DAF then keep DMP
admin1_perc_proc <- admin1_perc_proc %>% 
  mutate(hasdmp = ifelse(grepl(ADMIN2, pattern = "Marche"), 1, NA)) %>% 
  group_by(ADMIN1) %>% fill(hasdmp, .direction = "updown") %>% 
  mutate(hasdmp = ifelse(is.na(hasdmp), 0, hasdmp))
admin1_perc_proc <- admin1_perc_proc %>% 
  mutate(daf = ifelse(grepl(ADMIN2, pattern = "Financ"), 1, 0)) 
admin1_perc_proc <- admin1_perc_proc %>% filter(!(hasdmp == 1 & daf == 1))
BFAcomb <- admin1_perc_invest %>% left_join(y = admin1_perc_proc, by = c("ADMIN1"))

# save 
write_csv(BFAcomb, file = "chapter-2/data/output/bfa_clean.csv")

