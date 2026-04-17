# ==============================================================================
# public institutions report
# reproducibility file
# prepare data for chapter 1
# ==============================================================================

# ==============================================================================
# preamble
# ==============================================================================
# load
library(readxl)
library(tidyverse)
library(haven)
library(janitor)
library(stringi)
library(here)

# ==============================================================================
# clean and prepare boost data
# ==============================================================================

### ARMENIA

# OPEN
ARM <- read_xlsx(
  path = here(
    "chapter-1",
    "data",
    "input",
    "boost",
    "boost_arm_2006_2022.xlsx"
  ),
  sheet = "expenditure"
) %>%
  select(-`econ/func`, -`...18`, -`...19`, -`...20`)

# remove accents
ARM <- ARM %>%
  mutate(across(
    .cols = everything(),
    .fns = ~ toupper(stri_trans_general(str = ., id = "Latin-ASCII"))
  ))

# make numeric
ARM <- ARM %>%
  mutate(across(
    .cols = c("approved", "adjusted", "executed"),
    .fns = ~ as.numeric(.)
  ))

# label ministries
ARM <- ARM %>%
  mutate(
    min_category = case_when(
      admin == "104021 MINISTRY OF FINANCE OF RA" ~ "Ministry of Finance",
      admin ==
        "104010 MINISTRY OF EDUCATION AND SCIENCE OF RA" ~ "Ministry of Education",
      admin ==
        "104018 MINISTRY OF TRANSPORT AND COMMUNICATION OF RA" ~ "Ministry of Infrastructure",
      TRUE ~ NA
    )
  )

# group ARM
ARMmin <- ARM %>%
  filter(!is.na(min_category)) %>%
  group_by(year, min_category) %>%
  summarise(
    approved = sum(approved, na.rm = T),
    executed = sum(executed, na.rm = T),
    perc_exec = executed / approved
  )
ARMmin$country <- "Armenia"

### PARAGUAY

# OPEN
PGY <- read_xlsx(
  path = here(
    "chapter-1",
    "data",
    "input",
    "boost",
    "boost_pgy_2006_2022.xlsx"
  ),
  sheet = "Central"
)

# remove accents
PGY <- PGY %>%
  mutate(across(
    .cols = everything(),
    .fns = ~ toupper(stri_trans_general(str = ., id = "Latin-ASCII"))
  ))

# clean columns
PGY <- PGY[, -c(which(names(PGY) == "Foreign"):ncol(PGY))]

# make numeric
PGY <- PGY %>%
  mutate(across(.cols = c("APPROVED", "PAID"), .fns = ~ as.numeric(.)))

# label ministries
PGY <- PGY %>%
  mutate(
    min_category = case_when(
      ADMIN2 == "12.006 - MINISTERIO DE HACIENDA" ~ "Ministry of Finance",
      (ADMIN2 == "12.007 - MINISTERIO DE EDUCACION Y CIENCIAS" |
        ADMIN2 ==
          "12.007 - MINISTERIO DE EDUCACION Y CULTURA") ~ "Ministry of Education",
      ADMIN2 ==
        "12.013 - MINISTERIO DE OBRAS PUBLICAS Y COMUNICACIONES" ~ "Ministry of Infrastructure",
      TRUE ~ NA
    )
  )

# change approved / paid names
PGY <- PGY %>% rename(approved = APPROVED, paid = PAID, year = YEAR)
PGY$year <- as.numeric(PGY$year)

# group PGY
PGYmin <- PGY %>%
  filter(!is.na(min_category)) %>%
  group_by(year, min_category) %>%
  summarise(
    approved = sum(approved, na.rm = T),
    executed = sum(paid, na.rm = T),
    perc_exec = executed / approved
  )
PGYmin$country <- "Paraguay"

### UGANDA

# OPEN
UGA <- read_xlsx(
  path = here(
    "chapter-1",
    "data",
    "input",
    "boost",
    "boost_uga_2004_2022.xlsx"
  ),
  sheet = "BOOST"
)

# remove accents
UGA <- UGA %>%
  mutate(across(
    .cols = everything(),
    .fns = ~ toupper(stri_trans_general(str = ., id = "Latin-ASCII"))
  ))

# make numeric
UGA <- UGA %>%
  mutate(across(.cols = c("Budget", "Expenditure"), .fns = ~ as.numeric(.)))

# label ministries
UGA <- UGA %>%
  mutate(
    min_category = case_when(
      (Vote == "008 MINISTRY OF FINANCE, PLANNING AND ECONOMIC DEVELOPMENT" |
        Vote ==
          "008 MINISTRY OF FINANCE, PLANNING & ECONOMIC DEVELOPMENT") ~ "Ministry of Finance",
      (Vote == "013 MINISTRY OF EDUCATION AND SPORTS" |
        Vote == "013 MINISTRY OF EDUCATION & SPORTS") ~ "Ministry of Education",
      (Vote == "016 MINISTRY OF WORKS AND TRANSPORT" |
        Vote ==
          "016 MINISTRY OF WORKS & TRANSPORT") ~ "Ministry of Infrastructure",
      TRUE ~ NA
    )
  )

# change approved / paid names
UGA <- UGA %>% rename(approved = Budget, paid = Expenditure, year = Year)
UGA$year <- as.numeric(str_extract(UGA$year, pattern = "^\\d{4}"))

# group UGA
UGAmin <- UGA %>%
  filter(!is.na(min_category)) %>%
  group_by(year, min_category) %>%
  summarise(
    approved = sum(approved, na.rm = T),
    executed = sum(paid, na.rm = T),
    perc_exec = executed / approved
  )
UGAmin$country <- "Uganda"

### combine armenia, paraguay, and uganda

# combine
ARMmin$year <- as.numeric(ARMmin$year)
PGYmin$year <- as.numeric(PGYmin$year)
UGAmin$year <- as.numeric(UGAmin$year)
BOOST <- bind_rows(ARMmin, PGYmin, UGAmin)
BOOST$min_category[
  BOOST$min_category == "Ministry of Infrastructure"
] <- "Ministry of Infrastructure"

# save
write_csv(BOOST, file = here("chapter-1", "data", "output", "boost_clean.csv"))

# ==============================================================================
# clean and prepare gtmi data
# ==============================================================================

# open gtmi and regional classifications
GTMI <- read_xlsx(
  here("chapter-1", "data", "input", "WBG_GovTech Dataset_Oct2022.xlsx"),
  sheet = "CG_GTMI_Data"
)
REG <- read_dta(here(
  "chapter-1",
  "data",
  "input",
  "regional classification.dta"
))

# rename key columns
GTMI <- GTMI %>% rename(year = Year, country_code = Code, country = Economy)
GTMI <- GTMI %>% filter(!is.na(year))

# merge
GTMI <- left_join(x = GTMI, y = REG, by = c("country_code"))
# WDI_wide2022 <- WDI %>% filter(year == 2020 | year == 2022) %>%
#  select(-var_name) %>%
#  dcast(country_code + year ~ var_code)
#GTMI <- left_join(x = GTMI, y = WDI_wide2022, by = c("country_code", "year"))

# fmis
# i-5 fmis
# i-5.5 fmis launch year
# i-5.7 scope of fmis
# i-5.13 does fmis exchange data with other systems
# i-5.14 governance of fmis operations
GTMI <- GTMI %>%
  rename(
    fmis = `I-5`,
    fmis_year = `I-5.5`,
    fmis_scope = `I-5.7`,
    fmis_exchange = `I-5.13`,
    fmis_gov = `I-5.14`
  )
GTMI <- GTMI %>%
  mutate(
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
GTMI <- GTMI %>%
  rename(
    tsa = `I-6`,
    tsa_year = `I-6.2`,
    tsa_scope = `I-6.5`,
    tsa_link = `I-6.7`,
    tsa_gov = `I-6.8`
  )
GTMI <- GTMI %>%
  mutate(
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
GTMI <- GTMI %>%
  rename(
    customs = `I-8`,
    customs_year = `I-8.2`,
    customs_exchange = `I-8.8`,
    customs_gov = `I-8.9`
  )
GTMI <- GTMI %>%
  mutate(
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
GTMI <- GTMI %>%
  rename(
    hrmis = `I-9`,
    hrmis_year = `I-9.3`,
    hrmis_selfserve = `I-9.6`,
    hrmis_exchange = `I-9.7`,
    hrmis_gov = `I-9.9`
  )
GTMI <- GTMI %>%
  mutate(
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
GTMI <- GTMI %>%
  rename(payroll = `I-10`, payroll_year = `I-10.3`, payroll_gov = `I-10.6`)
GTMI <- GTMI %>%
  mutate(
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
GTMI <- GTMI %>%
  rename(
    proc = `I-12`,
    proc_year = `I-12.3`,
    proc_capab = `I-12.4`,
    proc_ocds = `I-12.5`,
    proc_exchange = `I-12.6`,
    proc_gov = `I-12.8`
  )
GTMI <- GTMI %>%
  mutate(
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
GTMI <- GTMI %>% rename(debt = `I-13`, debt_year = `I-13.3`)
GTMI <- GTMI %>%
  mutate(
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
GTMI <- GTMI %>%
  rename(
    pims = `I-14`,
    pims_year = `I-14.3`,
    pims_capab = `I-14.5`,
    pims_exchange = `I-14.6`,
    pims_pub = `I-14.7`
  )
GTMI <- GTMI %>%
  mutate(
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
GTMI <- GTMI %>%
  group_by(country_code, year) %>%
  mutate(
    perc_coreMIS = (sum(
      fmis + tsa + customs + hrmis + payroll + proc + debt + pims
    ) /
      8)
  )

# order incomegroup
GTMI <- GTMI %>%
  ungroup() %>%
  mutate(
    incomegroup_order = case_when(
      incomegroup == "High income" ~ 1,
      incomegroup == "Upper middle income" ~ 2,
      incomegroup == "Lower middle income" ~ 3,
      incomegroup == "Low income" ~ 4
    )
  )

# save gtmi
write_csv(
  GTMI,
  file = here("chapter-1", "data", "output", "gtmiwide_clean.csv")
)
