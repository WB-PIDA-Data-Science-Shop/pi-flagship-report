# 1. set-up ---------------------------------------------------------------
library(dplyr)
library(readr)
library(ggplot2)
library(hrbrthemes)
library(stringr)
library(tidyr)
library(janitor)
library(ggridges)
library(cowplot)
library(here)

theme_set(
  theme_cowplot(
    font_size = 20
  )
)

# 2. read-in data ------------------------------------------------------------
gsps <- read_csv(
  here("chapter 1", "data", "gsps.csv")
) |>
  clean_names()

gsps_management <- gsps |>
  filter(
    str_detect(section_org, "(?i)Management")
  )

# 3. analyze --------------------------------------------------------------
# political connection in recruitment
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
  here("chapter 1", "figs", "fig_1_6_a.png"),
  height = 8,
  width = 12
)

# undue influence
gsps |>
  filter(
    str_detect(question_text, "(?i)undue influence") &
      group == "Institution"
  ) |>
  mutate(
    indicator = str_extract_all(
      indicator, "\\(([^()]+)\\)"
    ) |>
      unlist() |>
      str_replace_all("\\(|\\)", "") |>
      str_to_title()
  ) |>
  ggplot(
    aes(mean, reorder(indicator, mean))
  ) +
  geom_boxplot(
    fill = "#00ADE4",
    width = 0.25,
    outlier.shape = NA
  ) +
  geom_jitter(
    size = 1,
    height = 0.25,
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
  here("chapter 1", "figs", "fig_1_6_b.png"),
  height = 8,
  width = 12
)

# robustness --------------------------------------------------------------
# two-way anova
gsps_recruitment <- gsps |>
  filter(
    topic == "Recruitment" &
      str_detect(indicator, "(?i)politic") &
      !str_detect(indicator, "non-political") &
      group == "Institution"
  ) |>
  mutate(
    institution =  paste0(country, institutions_number)
  )

aov(
  formula = mean ~ country,
  data = gsps_recruitment
) |> summary()

