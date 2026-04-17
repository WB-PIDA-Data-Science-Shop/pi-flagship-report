# 1. set-up ---------------------------------------------------------------
library(dplyr)
library(readr)
library(ggplot2)
library(hrbrthemes)
library(stringr)
library(tidyr)
library(janitor)
library(countrycode)
library(here)
library(cowplot)

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

# 2. read-in data ------------------------------------------------------------
rais_mun <- read_csv(
  here("chapter 1", "data", "rais_mun.csv")
)

wwbi <- read_csv(
  here("chapter 1", "data", "wb_wwbi.csv")
) |>
  clean_names()

gsps <- read_csv(
  here("chapter 1", "data", "gsps.csv")
) |>
  clean_names()

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
  here("figs", "fig_1_2_a.png"),
  height = 8,
  width = 12,
  bg = "white"
)

# skills
wwbi |>
  filter(indicator_id == "WB.WWBI.BI.PWK.PUBS.TT.ZS") |>
  pivot_longer(
    c(x2000:x2020),
    names_to = "year",
    names_prefix = "x",
    values_to = "share_tertiary_edu"
  ) |>
  select(
    economy_iso3,
    economy_name,
    year,
    share_tertiary_edu
  ) |>
  filter(
    !(year %in% c("2019", "2020"))
  ) |>
  ggplot() +
  geom_boxplot(
    aes(year, share_tertiary_edu),
    fill = "#00ADE4",
    width = 0.25
  ) +
  scale_x_discrete(
    breaks = scales::pretty_breaks()
  ) +
  scale_y_continuous(
    labels = scales::percent_format()
  ) +
  labs(
    x = "Year",
    y = "Share of Public Employees"
  )

ggsave(
  here("figs", "fig_1_2_b.png"),
  height = 8,
  width = 12,
  bg = "white"
)
