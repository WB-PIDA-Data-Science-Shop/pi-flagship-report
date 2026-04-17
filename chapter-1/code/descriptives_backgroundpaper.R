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
options("print.matrix" = FALSE)
egvpi_colors <- c("#002345", "#808080", "#D3D3D3", "#ff8000", "#008900", "#00ADE4", "#FCDF2D")

# open
GTMI <- read_xlsx("C:/Users/wb582704/OneDrive - WBG/Public institutions flagship/Drafts Part 2/procurement/analysis/data/WBG_GovTech Dataset_Oct2022.xlsx",
                  sheet = "CG_GTMI_Data")
REG <- read_dta("C:/Users/wb582704/OneDrive - WBG/Documents/standardized_data/data/regional classification.dta")
WDI <- read_csv("C:/Users/wb582704/OneDrive - WBG/Documents/standardized_data/data/WDI_clean.csv", show_col_types = F) %>%
  select(-region, -incomegroup, -lendingcategory, -otheremuorhipc, -country)
BOOST <- read_csv("data/BOOST_COFOG_2024_04_18.csv", show_col_types = F)
PEFA <- read_csv("C:/Users/wb582704/OneDrive - WBG/Public institutions flagship/Drafts Part 2/procurement/analysis/data/pefa_2016framework.csv", show_col_types = F)
CLIAR <- read_csv("C:/Users/wb582704/OneDrive - WBG/Public institutions flagship/Drafts Part 1/analysis/data/CLIAR Original indicators data.csv", show_col_types = F)

### MONEY ----

### clean BOOST and merge

# rename
BOOST <- BOOST %>% rename(approved = `sum(approved)`,
                          executed = `sum(executed)`)

# merge
BOOST <- left_join(x = BOOST, y = REG, by = c("country_name" = "economy"))
WDI_wide <- WDI %>%
  select(-var_name) %>%
  dcast(country_code + year ~ var_code)
BOOST <- left_join(x = BOOST, y = WDI_wide, by = c("country_code", "year"))

# perc_exec
BOOST$perc_exec <- BOOST$executed / (1 + BOOST$approved)

# fix an error in func and econ
BOOST$func[BOOST$func == "Defence"] <- "Defense"
BOOST$econ[BOOST$econ == "Capital Expenditure"] <- "Capital expenditure"
BOOST$econ[BOOST$econ == "Foreign funded expenditure"] <- "Foreign funded expenditures"

# merge PEFA to BOOST
names(PEFA) <- tolower(names(PEFA))
PEFA$country[PEFA$country == "Democratic Republic of Congo"] <- "Congo, Dem. Rep."

  # latest data
  PEFAlate <- PEFA %>% group_by(country) %>% filter(year == max(year)) %>% rename(pefa_year = year)
  # BOOSTlate <- BOOST %>% group_by(country_name) %>% filter(year == max(year)) %>% rename(boost_year = year)
  BOOSTpefa <- left_join(x = BOOST, y = PEFAlate, by = c("country_name" = "country"))

  # data over time
  # BOOST <- left_join(x = BOOST, y = PEFA, by = c("country_name" = "country", "year"))
  # pi_range <- c(min(grep(names(BOOST), pattern = "pi\\-")):max(grep(names(BOOST), pattern = "pi\\-")))
  # BOOST <- BOOST %>% arrange(country_name, year) %>% group_by(country_name) %>%
  #   fill(grep(names(BOOST), pattern = "pi\\-", value = T), .direction = "down")

# merge PEFA and REG and WDI
PEFA$country[PEFA$country == "Cote d'Ivoire"] <- "Côte d’Ivoire"
PEFA$country[PEFA$country == "Sao Tome and Principe"] <- "São Tomé and Príncipe"
PEFA$country[PEFA$country == "Macedonia"] <- "North Macedonia"
PEFA$country[PEFA$country == "Bosnia and Herzegovina - BiH"] <- "Bosnia and Herzegovina"
PEFA <- left_join(x = PEFA, y = REG, by = c("country" = "economy")) %>% filter(!is.na(country_code))
PEFA <- left_join(x = PEFA, y = WDI_wide, by = c("country_code", "year"))

# make PEFA numeric
PEFA <- PEFA %>% mutate(across(.cols = contains("PI"), .fns = ~gsub(., pattern = "\\*", replacement = "")))
PEFA <- PEFA %>% mutate(across(.cols = contains("PI"), .fns = ~ifelse(. == "NR", NA, .)))
PEFA <- PEFA %>% mutate(across(.cols = contains("PI"), .fns = ~case_when(
  . == "D" ~ 1,
  . == "D+" ~ 1.5,
  . == "C" ~ 2,
  . == "C+" ~ 2.5,
  . == "B" ~ 3,
  . == "B+" ~ 3.5,
  . == "A" ~ 4,
  . == "A+" ~ 4,
  TRUE ~ NA
)))

# filter CLIAR
CLIAR <- CLIAR %>% select(country_code, country_name, income_group, region, Year,
                 bs_sgi_196, wjp_rol_3_2, bs_sgi_195, wjp_rol_3_1, ibp_obs_obi, wjp_rol_3_4, vdem_core_v2xlg_legcon)
names(CLIAR) <- tolower(names(CLIAR))
CLIAR <- left_join(x = CLIAR, y = WDI_wide, by = c("country_code", "year"))

### descriptives ----

# perc_exec by country
BOOST %>% filter(admin0 == "Central") %>% group_by(country_name, year) %>%
  summarise(approved = sum(approved),
            executed = sum(executed),
            perc_exec = executed / (approved)) %>%
  filter(!is.na(perc_exec) & country_name != "Burkina Faso" & country_name != "Nigeria") %>%
  ggplot(aes(x = year, y = perc_exec, color = country_name)) + geom_line(linewidth = 1) +
  scale_y_continuous(limits = c(0, 1.6), breaks = seq(0, 1.6, 0.2)) +
  scale_x_continuous(limits = c(2006, 2022), breaks = seq(2006, 2022, 2)) +
  scale_color_manual(values = cbPalette) +
  geom_hline(yintercept = 1, linetype = 2) +
  labs(y = "Budget execution", x = "Year") +
  guides(color = guide_legend(title = "Country")) +
  theme(legend.position = "bottom")
ggsave(filename = "figs/money_boostyears.png", width = 11, height = 9)

# perc_exec by function
BOOST %>% filter(admin0 == "Central" & country_name != "Burkina Faso" & func != "Other" & !is.na(func) & !is.na(perc_exec)) %>%
  filter(year >= 2012) %>%
  group_by(year, func) %>%
  summarise(approved = sum(approved),
            executed = sum(executed),
            perc_exec = executed / (approved)) %>%
  filter(func %in% c("General public services", "Economic affairs", "Education", "Social protection", "Health")) %>%
  ggplot(aes(x = year, y = perc_exec, color = func)) + geom_line(linewidth = 1) +
  scale_x_continuous(limits = c(2012, 2022), breaks = seq(2012, 2022, 2)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(x = "Year", y = "Budget execution", caption = "Note: Only showing the five largest functions by total spending.") +
  guides(color = guide_legend(title = "Gov. function")) +
  theme(axis.text.x = element_text(size = 10, angle = 10))
ggsave(filename = "figs/money_boostfuncyears.png", width = 11, height = 9)

# perc_exec by econ
BOOST %>% filter(admin0 == "Central" & country_name != "Burkina Faso" & country_name != "Nigeria" &
                   econ != "Foreign funded expenditures" &
                   econ != "Other expenses" & econ != "Other grants and transfers" & !is.na(econ) & !is.na(perc_exec)) %>%
  filter(year >= 2012) %>%
  group_by(year, econ) %>%
  summarise(approved = sum(approved),
            executed = sum(executed),
            perc_exec = executed / (approved)) %>%
  ggplot(aes(x = year, y = perc_exec)) + geom_line(color = egvpi_colors[1], linewidth = 1.1) +
  scale_x_continuous(limits = c(2012, 2022), breaks = seq(2012, 2022, 2)) +
  scale_y_continuous(limits = c(0, 1.5), breaks = seq(0, 1.5, 0.5)) +
  geom_hline(yintercept = 1, linetype = 2) +
  labs(x = "Year", y = "Budget execution") +
  facet_wrap(~econ, ) +
  #labs(caption = "Note: The plots show national budget data from Bhutan, Colombia, Democratic Republic of the Congo,\n Kenya, Mozambique, Pakistan, Paraguay, and Tunisia.") +
  theme(axis.text.x = element_text(size = 12, angle = 10),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.caption = element_text(size = 8),
        strip.text = element_text(size = 18))
ggsave(filename = "figs/money_boosteconyears.png")

# distribution in perc exec within one country
KENmeans <- BOOST %>% filter(country_code == "KEN") %>%
  group_by(admin0, admin2, year) %>%
  summarise(approved = sum(approved),
            executed = sum(executed),
            perc_exec = executed / (approved)) %>%
  filter(perc_exec < 3 & perc_exec >= 0) %>% # removes some outliers
  group_by(admin0) %>% summarise(perc_exec = mean(perc_exec, na.rm = T))
BOOST %>% filter(country_code == "KEN") %>%
  group_by(admin0, admin2, year) %>%
  summarise(approved = sum(approved),
            executed = sum(executed),
            perc_exec = executed / (approved)) %>%
  filter(perc_exec < 3 & perc_exec >= 0) %>% # removes some outliers
  ggplot(aes(x = perc_exec)) + geom_histogram() +
  geom_vline(data = KENmeans, aes(xintercept = perc_exec), linetype = 2, color = "red") +
  labs(x = "Budget execution", y = "Frequency of institution/years",
       caption = "Note: The unit of observation is the institution/year. \nThe vertical lines are the mean of budget execution for each group.") +
  facet_wrap(~admin0, nrow = 2)
ggsave(filename = "figs/money_kenya_centralregional.png", width = 11, height = 9)

# perc_exec for finance ministries vs other ministries
BOOST %>% filter(country_code == "KEN") %>%
  filter(admin2 %in% c("The National Treasury", "Ministry of Transport and Infrastructure", "Ministry of Education, Science and Technology")) %>%
  group_by(admin2, year) %>%
  summarise(approved = sum(approved),
            executed = sum(executed),
            perc_exec = executed / (approved)) %>%
  ggplot(aes(x = year, y = perc_exec, color = admin2)) + geom_line(linewidth = 1) +
  scale_x_continuous(limits = c(2015, 2022), breaks = seq(2015, 2022, 1)) +
  scale_y_continuous(limits = c(0, 2), breaks = seq(0, 2, 0.5)) +
  geom_hline(yintercept = 1, linetype = 2) +
  labs(x = "Year", y = "Budget execution", title = "Budget execution in Kenya's central government") +
  guides(color = guide_legend(title = "Public Institution", nrow = 2)) +
  theme(legend.position = "bottom")
ggsave(filename = "figs/money_kenya_centralregional.png", width = 11, height = 9)

# PEFA PI-16 v budget execution
BOOSTpefa %>% filter(admin0 == "Central") %>% group_by(country_name, year) %>%
  summarise(approved = sum(approved),
            executed = sum(executed),
            perc_exec = executed / (approved),
            pi16 = unique(`pi-16`)) %>%
  mutate(pi16 = case_when(
    pi16 == "D" ~ 1,
    pi16 == "D+" ~ 1.5,
    pi16 == "C" ~ 2,
    pi16 == "C+" ~ 2.5,
    pi16 == "B" ~ 3,
    pi16 == "B+" ~ 3.5,
    pi16 == "A" ~ 4,
    TRUE ~ NA
  )) %>%
  filter(country_name != "Burkina Faso" & !is.na(pi16)) %>%
  ggplot(aes(x = pi16, y = perc_exec, color = country_name)) +
  geom_point(position = position_jitter(width = 0.11), size = 1.7) +
  scale_x_continuous(limits = c(0.75, 4.25),
                     breaks = seq(1, 4, 0.5), labels = c("D", "D+", "C", "C+", "B", "B+", "A")) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(x = "Medium-term perspective in exp. budgeting\n (PEFA PI-16)",
       y = "Budget execution") +
  guides(color = guide_legend(title = "Country"))
ggsave(filename = "figs/money_pefaPI16_boost.png", width = 11, height = 9)

# PEFA PI-16 v budget execution over time
cntrycodes_pi16 <- BOOSTpefa %>% filter(admin0 == "Central") %>% group_by(country_name, country_code, year) %>%
  summarise(approved = sum(approved, na.rm = T),
            executed = sum(executed, na.rm = T),
            perc_exec = executed / (approved),
            pi16 = unique(`pi-16`)) %>%
  filter(country_name != "Burkina Faso" & country_name != "Pakistan" & !is.na(pi16)) %>%
  group_by(country_name) %>% filter(year == max(year))
BOOSTpefa %>% filter(admin0 == "Central") %>% group_by(country_name, country_code, year) %>%
  summarise(approved = sum(approved, na.rm = T),
            executed = sum(executed, na.rm = T),
            perc_exec = executed / (approved),
            pi16 = unique(`pi-16`)) %>%
  mutate(pi16_order = case_when(
    pi16 == "D" ~ 4,
    pi16 == "D+" ~ 3.5,
    pi16 == "C" ~ 3,
    pi16 == "C+" ~ 3.5,
    pi16 == "B" ~ 2,
    pi16 == "B+" ~ 2.5,
    pi16 == "A" ~ 1,
    TRUE ~ NA
  )) %>%
  filter(country_name != "Burkina Faso" & country_name != "Pakistan" & !is.na(pi16)) %>%
  ggplot(aes(x = year, y = perc_exec, group = country_code)) +
  geom_line(aes(color = reorder(pi16, pi16_order)), linewidth = 1.25) +
  geom_label_repel(data = cntrycodes_pi16, aes(label = country_code)) +
  geom_hline(yintercept = 1, linetype = 2) +
  scale_y_continuous(limits = c(0, 1.6), breaks = seq(0, 1.6, .2)) +
  scale_x_continuous(limits = c(2006, 2024), breaks = seq(2006, 2024, 2)) +
  labs(y = "Budget execution", x = "Year",
       title = "Medium term perspective in expenditure budgeting (PEFA PI-16)") +
  guides(color = guide_legend(title = "PEFA PI-16"))
ggsave(filename = "figs/money_pefaPI16_boost_years.png", width = 11, height = 9)

# PEFA PI-11 v budget execution for investment
cntrycodes_pi11 <- BOOSTpefa %>% filter(admin0 == "Central" & econ == "Capital expenditure") %>%
  group_by(country_name, country_code, year) %>%
  summarise(approved = sum(approved, na.rm = T),
            executed = sum(executed, na.rm = T),
            perc_exec = executed / (approved),
            pi11 = unique(`pi-11`)) %>%
  filter(country_name != "Burkina Faso" & country_name != "Pakistan" & !is.na(pi11)) %>%
  group_by(country_name) %>% filter(year == max(year))
BOOSTpefa %>% filter(admin0 == "Central" & econ == "Capital expenditure") %>%
  group_by(country_name, country_code, year) %>%
  summarise(approved = sum(approved, na.rm = T),
            executed = sum(executed, na.rm = T),
            perc_exec = executed / (approved),
            pi11 = unique(`pi-11`)) %>%
  mutate(pi11_order = case_when(
    pi11 == "D" ~ 4,
    pi11 == "D+" ~ 3.5,
    pi11 == "C" ~ 3,
    pi11 == "C+" ~ 3.5,
    pi11 == "B" ~ 2,
    pi11 == "B+" ~ 2.5,
    pi11 == "A" ~ 1,
    TRUE ~ NA
  )) %>%
  filter(country_name != "Burkina Faso" & country_name != "Pakistan" & !is.na(pi11)) %>%
  ggplot(aes(x = year, y = perc_exec, group = country_code)) +
  geom_line(aes(color = reorder(pi11, pi11_order)), linewidth = 1.25) +
  geom_label_repel(data = cntrycodes_pi11, aes(label = country_code)) +
  geom_hline(yintercept = 1, linetype = 2) +
  scale_y_continuous(limits = c(0, 1.4), breaks = seq(0, 1.4, .2)) +
  scale_x_continuous(limits = c(2006, 2024), breaks = seq(2006, 2024, 2)) +
  labs(y = "Budget execution for capital expenditures", x = "Year",
       title = "Public investment management (PEFA PI-11)") +
  guides(color = guide_legend(title = "PEFA PI-11"))
ggsave(filename = "figs/money_pefaPI11_boost_years.png", width = 11, height = 9)

# PEFA PI-23 v budget execution for wage bill
cntrycodes_pi23 <- BOOSTpefa %>% filter(admin0 == "Central" & econ == "Wage bill") %>%
  group_by(country_name, country_code, year) %>%
  summarise(approved = sum(approved, na.rm = T),
            executed = sum(executed, na.rm = T),
            perc_exec = executed / (approved),
            pi23 = unique(`pi-23`)) %>%
  filter(country_name != "Burkina Faso" & country_name != "Pakistan" & !is.na(pi23)) %>%
  group_by(country_name) %>% filter(year == max(year))
BOOSTpefa %>% filter(admin0 == "Central" & econ == "Wage bill") %>%
  group_by(country_name, country_code, year) %>%
  summarise(approved = sum(approved, na.rm = T),
            executed = sum(executed, na.rm = T),
            perc_exec = executed / (approved),
            pi23 = unique(`pi-23`)) %>%
  mutate(pi23_order = case_when(
    pi23 == "D" ~ 4,
    pi23 == "D+" ~ 3.5,
    pi23 == "C" ~ 3,
    pi23 == "C+" ~ 3.5,
    pi23 == "B" ~ 2,
    pi23 == "B+" ~ 2.5,
    pi23 == "A" ~ 1,
    TRUE ~ NA
  )) %>%
  filter(country_name != "Burkina Faso" & country_name != "Pakistan" & !is.na(pi23)) %>%
  ggplot(aes(x = year, y = perc_exec, group = country_code)) +
  geom_line(aes(color = reorder(pi23, pi23_order)), linewidth = 1.25) +
  geom_label_repel(data = cntrycodes_pi23, aes(label = country_code)) +
  geom_hline(yintercept = 1, linetype = 2) +
  scale_y_continuous(limits = c(0, 2), breaks = seq(0, 2, .2)) +
  scale_x_continuous(limits = c(2006, 2024), breaks = seq(2006, 2024, 2)) +
  labs(y = "Budget execution for wage bill", x = "Year",
       title = "Payroll controls (PEFA PI-23)") +
  guides(color = guide_legend(title = "PEFA PI-23"))
ggsave(filename = "figs/money_pefaPI23_boost_years.png", width = 11, height = 9)

# PEFA PI-24 v budget execution for investment
cntrycodes_pi24 <- BOOSTpefa %>% filter(admin0 == "Central" & econ == "Goods and services") %>%
  group_by(country_name, country_code, year) %>%
  summarise(approved = sum(approved, na.rm = T),
            executed = sum(executed, na.rm = T),
            perc_exec = executed / (approved),
            pi24 = unique(`pi-24`)) %>%
  filter(country_name != "Burkina Faso" & country_name != "Pakistan" & !is.na(pi24)) %>%
  group_by(country_name) %>% filter(year == max(year))
BOOSTpefa %>% filter(admin0 == "Central" & econ == "Goods and services") %>%
  group_by(country_name, country_code, year) %>%
  summarise(approved = sum(approved, na.rm = T),
            executed = sum(executed, na.rm = T),
            perc_exec = executed / (approved),
            pi24 = unique(`pi-24`)) %>%
  mutate(pi24_order = case_when(
    pi24 == "D" ~ 4,
    pi24 == "D+" ~ 3.5,
    pi24 == "C" ~ 3,
    pi24 == "C+" ~ 3.5,
    pi24 == "B" ~ 2,
    pi24 == "B+" ~ 2.5,
    pi24 == "A" ~ 1,
    TRUE ~ NA
  )) %>%
  filter(country_name != "Burkina Faso" & country_name != "Pakistan" & !is.na(pi24)) %>%
  ggplot(aes(x = year, y = perc_exec, group = country_code)) +
  geom_line(aes(color = reorder(pi24, pi24_order)), linewidth = 1.25) +
  geom_label_repel(data = cntrycodes_pi24, aes(label = country_code)) +
  geom_hline(yintercept = 1, linetype = 2) +
  scale_y_continuous(limits = c(0, 1.8), breaks = seq(0, 1.8, .2)) +
  scale_x_continuous(limits = c(2006, 2024), breaks = seq(2006, 2024, 2)) +
  labs(y = "Budget execution for goods and services", x = "Year",
       title = "Procurement (PEFA PI-24)") +
  guides(color = guide_legend(title = "PEFA PI-24"))
ggsave(filename = "figs/money_pefaPI24_boost_years.png", width = 11, height = 9)

# PEFA PI-21 v budget execution
BOOSTpefa %>% filter(admin0 == "Central") %>% group_by(country_name, year) %>%
  summarise(approved = sum(approved),
            executed = sum(executed),
            perc_exec = executed / (approved),
            pi21 = unique(`pi-16`)) %>%
  mutate(pi21 = case_when(
    pi21 == "D" ~ 1,
    pi21 == "D+" ~ 1.5,
    pi21 == "C" ~ 2,
    pi21 == "C+" ~ 2.5,
    pi21 == "B" ~ 3,
    pi21 == "B+" ~ 3.5,
    pi21 == "A" ~ 4,
    TRUE ~ NA
  )) %>%
  filter(country_name != "Burkina Faso" & !is.na(pi21)) %>%
  ggplot(aes(x = pi21, y = perc_exec, color = country_name)) +
  geom_point(position = position_jitter(width = 0.1)) +
  scale_x_continuous(limits = c(0.75, 4.25),
                     breaks = seq(1, 4, 0.5), labels = c("D", "D+", "C", "C+", "B", "B+", "A")) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(x = "Predictability of in-year resource allocation\n (PEFA PI-21)",
       y = "Budget execution")

### INFORMATION SYSTEMS ----

### clean GTMI and merge ----

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

### descriptives ----

# number of systems by region
GTMI %>% filter(year == "2022" & !is.na(region)) %>%
  group_by(region) %>% summarise(perc_coreMIS = mean(perc_coreMIS)) %>%
  ggplot(aes(x = region, y = perc_coreMIS)) + geom_bar(stat = "identity")

# number of systems by gdp
infosyst_coresyst <- GTMI %>% filter(year == "2022") %>%
  filter(incomegroup != "" & !is.na(incomegroup)) %>%
  ggplot(aes(x = log(gdppercap_2015usd), y = perc_coreMIS)) +
  geom_point(aes(color = region), position = position_jitter(h = 0.03), size = 3.3) +
  geom_smooth(method = "lm", color = "red", linetype = 2, se = F) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_color_manual(values = egvpi_colors[c(1:5,7,6)]) +
  guides(color = guide_legend(title = "Region")) +
  labs(x = "GDP per capita, 2015 USD (log)", y = "Percent of core information systems") +
  theme(legend.position = "bottom", legend.text = element_text(size = 12))
ggsave(filename = "figs/infosyst_coresyst.png")

# % of countries that have each system
infosyst_indivsyst <- GTMI %>% filter(year == "2022") %>%
  filter(incomegroup != "" & !is.na(incomegroup)) %>%
  group_by(incomegroup, incomegroup_order) %>% summarise(FMIS = sum(fmis),
                                 Customs = sum(customs),
                                 HRMIS = sum(hrmis),
                                 Procurement = sum(proc),
                                 PIMS = sum(pims)) %>%
  melt(id.vars = c("incomegroup", "incomegroup_order")) %>%
  mutate(variable_order = case_when(
    variable == "FMIS" ~ 1,
    variable == "Procurement" ~ 2,
    variable == "HRMIS" ~ 3,
    variable == "PIMS" ~ 4,
    variable == "Customs" ~ 5
  ),
  incomegroup = case_when(
    incomegroup == "High income" ~ "High income (63 countries)",
    incomegroup == "Upper middle income" ~ "Upper middle income (52 countries)",
    incomegroup == "Lower middle income" ~ "Lower middle income (54 countries)",
    incomegroup == "Low income" ~ "Low income (26 countries)"
  )) %>%
  ggplot(aes(y = reorder(variable, -variable_order), x = value, fill = reorder(incomegroup, incomegroup_order))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(y = "Information system", x = "Number of countries in income group",
       title = "How common are information systems?") +
  scale_fill_manual(values = egvpi_colors[c(3,2,1,6)]) +
  scale_y_discrete(labels = ~ str_wrap(as.character(.x), 14)) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  guides(fill = guide_legend(title = "Income group\n(Number of countries overall)", nrow = 2)) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        title = element_text(size = 12),
        axis.text = element_text(size = 10))
ggsave(filename = "figs/infosyst_indivsyst.png")

# combine
plotcombined <- gridExtra::grid.arrange(infosyst_coresyst, infosyst_indivsyst, nrow = 1)
ggsave(plot = plotcombined, filename = "figs/infosyst_combined.png")

# scope variable
GTMI %>% filter(year == "2022") %>%
  tabyl(fmis_gov)

# gov variable
infosyst_gov <- GTMI %>% filter(year == "2022" & incomegroup != "" & !is.na(incomegroup)) %>%
mutate(
  fmis_pub = ifelse(fmis_gov == 2, 1, 0),
  tsa_pub = ifelse(tsa_gov == 2, 1, 0),
  customs_pub = ifelse(customs_gov == 2, 1, 0),
  hrmis_pub = ifelse(hrmis_gov == 2, 1, 0),
  payroll_pub = ifelse(payroll_gov == 2, 1, 0),
  proc_pub = ifelse(proc_gov == 2, 1, 0)
) %>% group_by(incomegroup, incomegroup_order) %>%
summarise(
  `FMIS` = sum(fmis_pub) / sum(fmis == 1),
  `Treasury single account` = sum(tsa_pub) / sum(tsa == 1),
  `Customs` = sum(customs_pub) / sum(customs == 1),
  `HRMIS` = sum(hrmis_pub) / sum(hrmis == 1),
  `Payroll` = sum(payroll_pub) / sum(payroll == 1),
  `Procurement` = sum(proc_pub) / sum(proc == 1)
) %>% melt(id.vars = c("incomegroup", "incomegroup_order")) %>%
ggplot(aes(x = value, y = variable, fill = reorder(incomegroup, incomegroup_order))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_fill_manual(values = egvpi_colors[c(3,2,1,6)]) +
  scale_y_discrete(labels = ~ str_wrap(as.character(.x), 14)) +
  labs(x = "Percent of countries in income group with the system\nand whose system publishes governance information",
       y = "Management information system",
       title = "Does each information system publish key governance information like audits?") +
  guides(fill = guide_legend(title = "Income group")) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        title = element_text(size = 12))
ggsave(filename = "figs/infosyst_gov.png", width = 12, height = 8)

# exchange variable
infosyst_exchange <- GTMI %>% filter(year == "2022" & incomegroup != "" & !is.na(incomegroup)) %>%
  group_by(incomegroup, incomegroup_order) %>%
  summarise(
    `FMIS` = sum(fmis_exchange) / sum(fmis),
    `Customs` = sum(customs_exchange) / sum(customs),
    `HRMIS` = sum(hrmis_exchange, na.rm = T) / sum(hrmis),
    `Procurement` = sum(proc_exchange) / sum(proc),
    `PIMS` = sum(pims_exchange) / sum(pims)
  ) %>%
  melt(id.vars = c("incomegroup", "incomegroup_order")) %>%
  mutate(variable_order = case_when(
    variable == "FMIS" ~ 1,
    variable == "Procurement" ~ 2,
    variable == "HRMIS" ~ 3,
    variable == "PIMS" ~ 4,
    variable == "Customs" ~ 5
  )) %>%
  ggplot(aes(x = value, y = reorder(variable, -variable_order), fill = reorder(incomegroup, incomegroup_order))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_fill_manual(values = egvpi_colors[c(3,2,1,6)]) +
  labs(x = "Percent of countries in income group with the system\n and whose system can exchange information",
       y = "Information system",
       title = "Can information systems easily exchange\ninformation with each other?") +
  guides(fill = guide_legend(title = "Income group")) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.title.y = element_blank())
ggsave(filename = "figs/infosyst_exchange.png", width = 12, height = 8)

# combine
infosyst <- ggpubr::ggarrange(infosyst_indivsyst, infosyst_exchange, nrow = 1, common.legend = T, legend = "bottom")
ggsave(plot = infosyst, filename = "figs/infosyst.png",
       width = 16, height = 8)

# conditional probability matrix
GTMIconting <- GTMI %>% select(fmis, tsa, customs, hrmis, payroll, proc, debt, pims) %>% as.matrix()
GTMIcondprob <- crossprod(GTMIconting, GTMIconting) / colSums(GTMIconting)
GTMIcondprob <- GTMIcondprob %>% melt() %>% rename(rows = Var1, cols = Var2)
GTMIcondprob <- GTMIcondprob %>% mutate(rows_id = case_when(
  rows == "fmis" ~ 1,
  rows == "hrmis" ~ 2,
  rows == "payroll" ~ 3,
  rows == "proc" ~ 4,
  rows == "debt" ~ 5,
  rows == "customs" ~ 6,
  rows == "tsa" ~ 7,
  rows == "pims" ~ 8
), cols_id = case_when(
  rows == "fmis" ~ 1,
  rows == "hrmis" ~ 2,
  rows == "payroll" ~ 3,
  rows == "proc" ~ 4,
  rows == "debt" ~ 5,
  rows == "customs" ~ 6,
  rows == "tsa" ~ 7,
  rows == "pims" ~ 8
))
GTMIcondprob %>%
  ggplot(aes(x = reorder(cols, -cols_id), y = reorder(rows, -rows_id), fill = value)) +
  geom_tile() + geom_text(aes(label = round(value, digits = 2))) +
  scale_fill_gradient(low = egvpi_colors[1], high = egvpi_colors[3]) +
  scale_x_discrete(labels = c("fmis" = "FMIS"))


### ACCOUNTABILITY ----

# CLIAR BS_SGI_196 audit office
CLIAR %>% filter(!is.na(bs_sgi_196)) %>%
  group_by(country_code) %>% filter(year == max(year)) %>%
  ggplot(aes(x = log(gdppercap_2015usd), y = bs_sgi_196)) +
  geom_point(aes(color = region), position = position_jitter(height = 0.05)) +
  scale_y_continuous(limits = c(1 , 10), breaks = seq(1, 10, 1)) +
  scale_x_continuous(limits = c(9, 11), breaks = seq(9, 11, 0.5)) +
  guides(color = guide_legend(title = "Region", nrow = 2)) +
  labs(x = "GDP per capita, 2015 USD (log)", y = "Audit office",
       caption = "Note: The audit office variable is from the Sustainable Governance Indicators.") +
  theme(legend.position = "bottom", legend.text = element_text(size = 10))

# CLIAR vdem_core_v2xlg_legcon legislative constraints on the executive index
CLIAR %>% filter(!is.na(vdem_core_v2xlg_legcon)) %>%
  group_by(country_code) %>% filter(year == max(year)) %>%
  ggplot(aes(x = log(gdppercap_2015usd), y = vdem_core_v2xlg_legcon)) +
  geom_point(aes(color = region), position = position_jitter(height = 0.05)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(limits = c(6, 11), breaks = seq(6, 11, 0.5)) +
  guides(color = guide_legend(title = "Region", nrow = 2)) +
  labs(x = "GDP per capita, 2015 USD (log)", y = "Legislative constraints on the executive",
       caption = "Note: The legislative constraints variable is from the Varieties of Democracy project.") +
  theme(legend.position = "bottom", legend.text = element_text(size = 10))

# PEFA PI-18 (legislative scrutiny of budgets)
pefa_pi18 <- PEFA %>% group_by(country_code) %>% filter(year == max(year)) %>%
  ggplot(aes(x = log(gdppercap_2015usd), y = `pi-18`)) +
  geom_point(aes(color = region), position = position_jitter(height = 0.03), size = 3.3) +
  scale_y_continuous(limits = c(.75, 4.25), breaks = seq(1, 4, 0.5),
                     labels = c("D", "D+", "C", "C+", "B", "B+", "A")) +
  scale_x_continuous(limits = c(6, 11), breaks = seq(6, 11, 1)) +
  scale_color_manual(values = egvpi_colors) +
  guides(color = guide_legend(title = "Region")) +
  labs(x = "GDP per capita, 2015 USD (log)", y = "Legislative scrutiny of budgets\n(PEFA PI-18)",
       title = "Legislative scrutiny of budgets (PEFA)") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        title = element_text(size = 12))
ggsave(filename = "figs/account_pefaPI18.png")

# PEFA PI-26 (internal audit)
pefa_pi26 <- PEFA %>% group_by(country_code) %>% filter(year == max(year)) %>%
  ggplot(aes(x = log(gdppercap_2015usd), y = `pi-26`)) +
  geom_point(aes(color = region), position = position_jitter(height = 0.03), size = 3.3) +
  scale_y_continuous(limits = c(.75, 4.25), breaks = seq(1, 4, 0.5),
                     labels = c("D", "D+", "C", "C+", "B", "B+", "A")) +
  scale_x_continuous(limits = c(6, 11), breaks = seq(6, 11, 1)) +
  scale_color_manual(values = egvpi_colors) +
  guides(color = guide_legend(title = "Region")) +
  labs(x = "GDP per capita, 2015 USD (log)", y = "Internal audit\n(PEFA PI-26)",
       title = "Internal audit (PEFA)") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        title = element_text(size = 12))
ggsave(filename = "figs/account_pefaPI26.png")

# combine
ggpubr::ggarrange(pefa_pi18, pefa_pi26, common.legend = T, legend = "bottom")
ggsave(filename = "figs/account_combined.png")

# plot pefa_pi18 v pefa_pi26

PEFA %>% group_by(country_code) %>% filter(year == max(year)) %>%
  ggplot(aes(x = `pi-18`, y = `pi-26`)) +
  geom_point(aes(color = region), position = position_jitter(height = 0.085, width = 0.085), size = 3.3) +
  scale_y_continuous(limits = c(.75, 4.25), breaks = seq(1, 4, 0.5),
                     labels = c("D", "D+", "C", "C+", "B", "B+", "A")) +
  scale_x_continuous(limits = c(.75, 4.25), breaks = seq(1, 4, 0.5),
                     labels = c("D", "D+", "C", "C+", "B", "B+", "A")) +
  scale_color_manual(values = egvpi_colors) +
  geom_smooth(method = "lm", se = F, color = "red", linetype = 2) +
  guides(color = guide_legend(title = "Region")) +
  annotate("text", x  = 1.7, y = 3.5, label = "Uganda", color = "red") +
  annotate("text", x  = 4, y = 1.25, label = "Kyrgyz\nRepublic", color = "red") +
  labs(x = "Legislative scrutiny of budgets\n(PEFA PI-18)", y = "Internal audit\n(PEFA PI-26)") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        title = element_text(size = 12))
ggsave(filename = "figs/account_pefa_audits.png")



# PEFA PI-30 (external audit)
PEFA %>% group_by(country_code) %>% filter(year == max(year)) %>%
  ggplot(aes(x = log(gdppercap_2015usd), y = `pi-30`)) +
  geom_point(aes(color = region), position = position_jitter(height = 0.05)) +
  scale_y_continuous(limits = c(.75, 4.25), breaks = seq(1, 4, 0.5),
                     labels = c("D", "D+", "C", "C+", "B", "B+", "A")) +
  scale_x_continuous(limits = c(6, 11), breaks = seq(6, 11, 1)) +
  guides(color = guide_legend(title = "Region")) +
  labs(x = "GDP per capita, 2015 USD (log)", y = "External audit\n(PEFA PI-30)") +
  theme(legend.position = "bottom")

# PEFA PI-31 (legislative scrutiny of audit reports)

### TRANSPARENCY ----

# PEFA PI-9 (public access to fiscal information)
pefa_pi9 <- PEFA %>% group_by(country_code) %>% filter(year == max(year)) %>%
  ggplot(aes(x = log(gdppercap_2015usd), y = `pi-09`)) +
  geom_point(aes(color = region), position = position_jitter(height = 0.08), size = 3.3) +
  scale_y_continuous(limits = c(.75, 4.25), breaks = seq(1, 4, 0.5),
                     labels = c("D", "D+", "C", "C+", "B", "B+", "A")) +
  scale_x_continuous(limits = c(6, 11), breaks = seq(6, 11, 1)) +
  scale_color_manual(values = egvpi_colors) +
  guides(color = guide_legend(title = "Region")) +
  labs(x = "GDP per capita, 2015 USD (log)", y = "Public access to fiscal information\n(PEFA PI-09)",
       title = "Public access to fiscal information (PEFA)") +
  theme(legend.position = "bottom")
ggsave(filename = "figs/trans_pefaPI9.png", width = 11, height = 9)

# CLIAR WJP_ROL_3_2 right to information
roi <- CLIAR %>% filter(!is.na(wjp_rol_3_2)) %>%
  group_by(country_code) %>% filter(year == max(year)) %>%
  ggplot(aes(x = log(gdppercap_2015usd), y = wjp_rol_3_2)) +
  geom_point(aes(color = region), position = position_jitter(height = 0.08), size = 3) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(limits = c(6, 11), breaks = seq(6, 11, 1)) +
  scale_color_manual(values = egvpi_colors[c(1:5,7,6)]) +
  guides(color = guide_legend(title = "Region", nrow = 2)) +
  labs(x = "GDP per capita, 2015 USD (log)", y = "Right to information",
       title = "Right to information (World Justice Project)") +
  theme(legend.position = "bottom", legend.text = element_text(size = 10))
ggsave(filename = "figs/trans_acces2info.png", width = 11, height = 9)

# combine
ggpubr::ggarrange(pefa_pi9, roi, legend = "bottom", common.legend = T)
ggsave(filename = "figs/trans_combined.png")

# CLIAR WJP_ROL_3_1 right to information
CLIAR %>% filter(!is.na(wjp_rol_3_1)) %>%
  group_by(country_code) %>% filter(year == max(year)) %>%
  ggplot(aes(x = log(gdppercap_2015usd), y = wjp_rol_3_1)) +
  geom_point(aes(color = region), position = position_jitter(height = 0.05)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(limits = c(6, 11), breaks = seq(6, 11, 0.5)) +
  guides(color = guide_legend(title = "Region", nrow = 2)) +
  labs(x = "GDP per capita, 2015 USD (log)", y = "Publicized laws and government data",
       caption = "Note: The publicized laws and government data variable is from the World Justice Project.") +
  theme(legend.position = "bottom", legend.text = element_text(size = 10))

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






# CLIAR WJP_ROL_3_1 right to information
CLIAR %>% filter(!is.na(ibp_obs_obi)) %>%
  group_by(country_code) %>% filter(year == max(year)) %>%
  ggplot(aes(x = log(gdppercap_2015usd), y = ibp_obs_obi)) +
  geom_point(aes(color = region), position = position_jitter(height = 0.05)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  scale_x_continuous(limits = c(6, 11), breaks = seq(6, 11, 0.5)) +
  guides(color = guide_legend(title = "Region", nrow = 2)) +
  labs(x = "GDP per capita, 2015 USD (log)", y = "Open budget Index",
       caption = "Note: The Open Budget Index is from the International Budget Partnership.") +
  theme(legend.position = "bottom", legend.text = element_text(size = 10))













