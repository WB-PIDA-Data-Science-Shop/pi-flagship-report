# ==============================================================================
# public institutions report
# reproducibility file
# create figures for chapter-2
# ==============================================================================

# ==============================================================================
# preamble
# ==============================================================================
library(readxl)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(cowplot)
library(haven)
library(janitor)
library(stringi)
library(ggrepel)

theme_set(theme_cowplot())

egvpi_colors <- c(
  "#002345",
  "#808080",
  "#D3D3D3",
  "#ff8000",
  "#FCDF2D",
  "#00ADE4"
)

# open data
BFA <- read_csv("chapter-2/data/output/bfa_clean.csv", show_col_types = F)
CwG <- read_csv("chapter-2/data/output/cwg_clean.csv", show_col_types = F)
GTMIpanel <- read_csv(
  "chapter-2/data/output/gtmipanel_clean.csv",
  show_col_types = F
)
GTMIwide <- read_csv(
  "chapter-2/data/output/gtmiwide_clean.csv",
  show_col_types = F
)
MAPS <- read_csv("chapter-2/data/output/maps_clean.csv", show_col_types = F)

# ==============================================================================
### figure 2.2
# ==============================================================================

# merge MAPS and CwG
MAPS$hasmaps <- 1
CwG <- left_join(x = CwG, y = MAPS, by = c("country_code"))
CwG$hasmaps[is.na(CwG$hasmaps)] <- 0

# prep variables
CwG$log_gdp_2015usd <- log(CwG$gdp_2015usd)
CwG$log_pop_total <- log(CwG$pop_total)
CwG$log_gdppercap_2015usd <- log(CwG$gdppercap_2015usd)

# collect residuals
cwg_residuals <- residuals(lm(
  score_cwg ~ log_gdppercap_2015usd,
  data = CwG[CwG$hasmaps == 1, ]
))
personnel_residuals <- residuals(lm(
  perc_concept_personnel ~ log_gdppercap_2015usd,
  data = CwG[CwG$hasmaps == 1, ]
))
cwg_personnel_resid <- data.frame(
  "country_code" = CwG$country_code[CwG$hasmaps == 1 & !is.na(CwG$hasmaps)],
  "incomegroup" = CwG$incomegroup[CwG$hasmaps == 1 & !is.na(CwG$hasmaps)],
  "gdppercap_2015usd" = CwG$gdppercap_2015usd[
    CwG$hasmaps == 1 & !is.na(CwG$hasmaps)
  ],
  "cwg_residuals" = cwg_residuals,
  "personnel_residuals" = personnel_residuals
)
model_cwg_personnel_residuals <- lm(
  cwg_residuals ~ personnel_residuals,
  data = cwg_personnel_resid
)
personnel_cwg_cor_resid <- cor(
  cwg_personnel_resid$cwg_residuals,
  cwg_personnel_resid$personnel_residuals
)

# figure 2.2 - MAPS personnel score v CwG (raw)
CwG %>%
  ggplot(aes(x = perc_concept_personnel, y = score_cwg)) +
  geom_point(
    aes(fill = incomegroup),
    position = position_jitter(width = 0.02),
    pch = 21,
    size = 6
  ) +
  geom_smooth(method = "lm", se = F, color = "red", linetype = 2) +
  geom_text_repel(aes(label = country_code), size = 6) +
  # annotate("text", label = "Correlation:\n.40", x = 0.36, y = 11, color = "red") +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, 20)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_fill_manual(
    values = c(
      "High income" = egvpi_colors[6],
      "Upper middle income" = egvpi_colors[3],
      "Lower middle income" = egvpi_colors[2],
      "Low income" = egvpi_colors[1]
    ),
    name = "Income group"
  ) +
  guides(fill = guide_legend(nrow = 2)) +
  labs(
    x = "'Personnel' dimension",
    y = "'Contracting with the\nGovernment' score"
  ) +
  #  caption = "Note: The variables on either axis are the residuals of each variable regressed on GDP per capita.
  #   After controlling for GDP per capita, the relationship is statistically significant at the 90% confidence level.") +
  theme(legend.position = "bottom") +
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 20)
  )

ggsave(
  plot = last_plot(),
  filename = "chapter-2/figs/fig2_2.png",
  width = 12,
  height = 9,
  bg = "white"
)

# ==============================================================================
### figure 2.3
# ==============================================================================

# figure 2.3 - financial resources in BFA BOOST data
BFA %>%
  mutate(
    ratio = (admin1_paid / admin2_paid),
    code = case_when(
      grepl(ADMIN1, pattern = "^27") ~ "highlight",
      grepl(ADMIN1, pattern = "^30") ~ "highlight",
      TRUE ~ NA
    )
  ) %>%
  ggplot(aes(x = reorder(ADMIN1, ratio), y = ratio, fill = code)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Ministry, department, or agency",
    y = "Ratio of investment spending to\nspending on entity's procurement office"
  ) +
  scale_y_continuous(limits = c(0, 50000), breaks = seq(0, 50000, 10000)) +
  annotate(
    "text",
    x = 35,
    y = 47000,
    label = "Agriculture\nministry",
    color = egvpi_colors[6],
    size = 5
  ) +
  annotate(
    "text",
    x = 20,
    y = 10000,
    label = "Infrastructure\nministry",
    color = egvpi_colors[6],
    size = 5
  ) +
  scale_fill_manual(values = egvpi_colors[6]) +
  theme(axis.text.x = element_blank(), legend.position = "none")

ggsave(
  plot = last_plot(),
  filename = "chapter-2/figs/fig2_3.png",
  width = 12,
  height = 9,
  bg = "white"
)

# ==============================================================================
### figure 2.4
# ==============================================================================

# figure 2.4 - e-GP systems over years
GTMIpanel %>%
  filter(!is.na(year)) %>%
  group_by(year) %>%
  summarise(
    n_eproc = sum(eproc),
    n_eproc_onlinetender = sum(eproc_onlinetender),
    n_eproc_exch = sum(eproc_exch)
  ) %>%
  melt(id.vars = "year") %>%
  ggplot(aes(x = as.numeric(year), y = value, color = variable)) +
  geom_point(size = 3) +
  scale_color_manual(values = egvpi_colors[c(1, 6, 4)]) +
  scale_y_continuous(limits = c(0, 200), breaks = seq(0, 200, 20)) +
  scale_x_continuous(limits = c(1986, 2024.5), breaks = seq(1988, 2024, 4)) +
  annotate("text", x = 2018, y = 160, label = "Any e-GP system", size = 3.5) +
  annotate(
    "text",
    x = 2021,
    y = 72,
    label = "e-GP systems\nwith online tendering",
    size = 3.5
  ) +
  annotate(
    "text",
    x = 2022,
    y = 14,
    label = "e-GP systems\nintegrated with other MIS",
    size = 3.5
  ) +
  labs(y = "Number of countries", x = "Year") +
  theme(
    axis.text.x = element_text(angle = 30, size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none"
  )

ggsave(
  plot = last_plot(),
  filename = "chapter-2/figs/fig2_4.png",
  height = 9,
  width = 12,
  bg = "white"
)

# ==============================================================================
### figure 2.5
# ==============================================================================

# gov variable
GTMIwide %>%
  filter(year == "2022" & incomegroup != "" & !is.na(incomegroup)) %>%
  mutate(
    fmis_pub = ifelse(fmis_gov == 2, 1, 0),
    tsa_pub = ifelse(tsa_gov == 2, 1, 0),
    customs_pub = ifelse(customs_gov == 2, 1, 0),
    hrmis_pub = ifelse(hrmis_gov == 2, 1, 0),
    payroll_pub = ifelse(payroll_gov == 2, 1, 0),
    proc_pub = ifelse(proc_gov == 2, 1, 0)
  ) %>%
  group_by(incomegroup, incomegroup_order) %>%
  summarise(
    `FMIS` = sum(fmis_pub) / sum(fmis == 1),
    `Treasury single account` = sum(tsa_pub) / sum(tsa == 1),
    `Customs` = sum(customs_pub) / sum(customs == 1),
    `HRMIS` = sum(hrmis_pub) / sum(hrmis == 1),
    `Payroll` = sum(payroll_pub) / sum(payroll == 1),
    `Procurement` = sum(proc_pub) / sum(proc == 1)
  ) %>%
  melt(id.vars = c("incomegroup", "incomegroup_order")) %>%
  ggplot(aes(
    x = value,
    y = variable,
    fill = reorder(incomegroup, -incomegroup_order)
  )) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_fill_manual(values = egvpi_colors[c(3, 2, 1, 6)]) +
  scale_y_discrete(labels = ~ str_wrap(as.character(.x), 14)) +
  labs(
    x = "Percent of countries in income group with the system\nand whose system publishes governance information",
    y = "Management information system",
    title = "Does each information system publish key governance information like audits?"
  ) +
  guides(fill = guide_legend(title = "Income group")) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    title = element_text(size = 12)
  )

ggsave(
  filename = "chapter-2/figs/fig2_5.png",
  width = 12,
  height = 9,
  bg = "white"
)
