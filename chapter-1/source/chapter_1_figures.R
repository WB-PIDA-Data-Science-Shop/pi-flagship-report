# ==============================================================================
# public institutions report
# reproducibility file
# produce figures for chapter_1
# ==============================================================================
# set-up -----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(readr)
library(janitor)
library(here)
library(reshape2)
library(cowplot)
library(ggpubr)

# read-in data -----------------------------------------------------------
gsps <- read_csv(
  here("chapter_1", "data", "gsps.csv")
) |>
  clean_names()

gsps_management <- gsps |>
  filter(
    str_detect(section_org, "(?i)Management")
  )

# open data
BOOST <- read_csv("chapter_1/data/output/boost_clean.csv", show_col_types = F)
GTMI <- read_csv("chapter_1/data/output/gtmiwide_clean.csv", show_col_types = F) 

# figure 1.2 -------------------------------------------------------------
theme_set(
  theme_cowplot(
    font_size = 20
  ) +
    theme(
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 16)
    )
)

options(
  ggplot2.discrete.colour= c(
    "#00ADE4", "#002345",
    "#ff8000", "#fcdf2d"
  )
)

# 3. analysis ----------------------------------------------------------------
# 1. career
# meritocratic recruitment by institution and country
gsps |>
  filter(
    group == "Institution"
  ) |>
  # only retain latest year
  group_by(country) |>
  filter(year == max(year)) |>
  ungroup() |>
  filter(
    topic == "Recruitment" &
      indicator == "Recruitment (interview)"
  ) |>
  # generate median of agreement to order boxplots
  group_by(country) |>
  mutate(median_share = median(mean)) |>
  ungroup() |>
  ggplot(
    aes(mean, reorder(country, median_share))
  ) +
  geom_boxplot(
    width = 0.25,
    fill = "#00ADE4",
    outlier.shape = NA
  ) +
  geom_jitter(
    size = 1,
    alpha = 0.5,
    height = 0.25
  ) +
  scale_x_continuous(
    labels = scales::percent_format()
  ) +
  labs(
    x = "Share of Respondents",
    y = "Country"
  ) +
  theme(legend.position = "none")

ggsave(
  here("chapter_1", "figs", "fig_1_2.png"),
  height = 8,
  width = 12,
  bg = "white"
)

# ==============================================================================
# figure 1.3
# ==============================================================================
theme_set(theme_cowplot())
egvpi_colors <- c("#002345", "#808080", "#D3D3D3",
                  "#ff8000", "#FCDF2D", "#00ADE4")

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

ggsave(plot = last_plot(), filename = "chapter_1/figs/fig_1_3.png", width = 12, height = 9)

# ==============================================================================
# figure 1.4
# ==============================================================================

# figure 1.4 left panel
leftpanel <- GTMI %>% filter(year == "2022") %>%
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

# figure 1.4 right panel
rightpanel <- GTMI %>% filter(year == "2022" & incomegroup != "" & !is.na(incomegroup)) %>%
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
  ggplot(aes(x = value, y = reorder(variable, -variable_order), 
             fill = reorder(incomegroup, incomegroup_order))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_fill_manual(values = egvpi_colors[c(3,2,1,6)]) +
  labs(x = "Percent of countries in income group with the system\n and whose system can exchange information",
       y = "Information system",
       title = "Can information systems easily exchange\ninformation with each other?") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.title.y = element_blank())

# combine the two panels
ggarrange(leftpanel, rightpanel, common.legend = TRUE, legend="bottom")
ggsave(plot = last_plot(), filename = "chapter_1/figs/fig_1_4.png")


# figure 1.5 -------------------------------------------------------------
gsps_management |>
  filter(
    indicator == "Clear targets" &
      group == "Institution"
  ) |>
  ggplot(
    aes(mean, y = country, fill = country)
  ) +
  geom_boxplot(
    width = 0.25,
    outlier.shape = NA
  ) +
  geom_jitter(
    size = 3,
    alpha = 0.5,
    height = 0.25
  ) +
  scale_x_continuous(
    labels = scales::percent_format()
  ) +
  scale_fill_manual(
    name = "Country",
    values = c("#00ADE4", "#ff8000")
  ) +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = "Share of Respondents",
    y = "Country"
  )

ggsave(
  here("chapter 1", "figs", "fig_1_5.png"),
  height = 8,
  width = 12
)

# figure 1.6 -------------------------------------------------------------
gsps |>
  filter(
    topic == "Recruitment" &
      str_detect(indicator, "(?i)politic") &
      !str_detect(indicator, "non-political") &
      group == "Institution"
  ) |> 
  # generate median of agreement to order boxplots
  group_by(country) |>
  mutate(median_share = median(mean)) |>
  ungroup() |>
  ggplot(
    aes(
      mean, reorder(country, median_share)
    )
  ) +
  geom_boxplot(
    fill = "#00ADE4",
    width = 0.5,
    outlier.shape = NA
  ) +
  geom_jitter(
    size = 1,
    alpha = 0.5
  ) +
  scale_x_continuous(
    labels = scales::percent_format()
  ) +
  coord_cartesian(
    xlim = c(0, 1)
  ) +
  labs(
    x = "Share of Respondents",
    y = ""
  )

ggsave(
  here("chapter_1", "figs", "fig_1_6.png"),
  height = 8,
  width = 12
)

# figure 1.7 -------------------------------------------------------------

# prep for with-in country variation in transparency
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

# plot figure 1.7
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
       
ggsave(plot = last_plot(), filename = "chapter_1/figs/fig_1_7.png", width = 12, height = 9)


