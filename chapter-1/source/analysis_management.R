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

options(
  ggplot2.discrete.colour= c(
    "#00ADE4", "#002345",
    "#ff8000", "#fcdf2d"
  )
)

set.seed(2025)

# 2. read-in data ------------------------------------------------------------
gsps <- read_csv(
  here("chapter 1", "data", "gsps.csv")
) |>
  clean_names()

gsps_identified <- read_csv(
  here("chapter 1", "data", "gsps_identified.csv")
) |>
  clean_names()

gsps_management <- gsps |>
  filter(
    str_detect(section_org, "(?i)Management")
  )

# 3. analysis -------------------------------------------------------------
# target-setting
gsps |>
  mutate(
    topic = str_to_title(topic)
  ) |>
  filter(
    indicator %in% c("Performance evaluation (performance evaluated)", "Performance evaluated") &
      group == "All" &
      !str_detect(question_text, "boss")
  ) |>
  ggplot() +
  geom_col(
    aes(mean, reorder(country, mean)),
    fill = "#00ADE4"
  ) +
  scale_x_continuous(
    labels = scales::percent_format()
  ) +
  labs(
    x = "Percentage of Agreement",
    y = "",
    caption = "Source: Global Survey of Public Servants."
  ) +
  coord_cartesian(
    xlim = c(0, 1)
  )

ggsave(
  here("figs", "mgmt_performance.png"),
  height = 8,
  width = 12
)

# gsps |>
#   filter(
#     str_detect(topic, "(?i)Target") &
#       str_detect(indicator, "target") &
#       group == "All"
#   ) |>
#   mutate(
#     question_text = str_replace(question_text, "\\(.*\\)", "")
#   ) |>
#   ggplot() +
#   geom_col(
#     aes(mean, country),
#     fill = "#00ADE4"
#   ) +
#   facet_wrap(
#     vars(indicator),
#     nrow = 3
#   ) +
#   scale_x_continuous(
#     labels = scales::percent_format()
#   ) +
#   labs(
#     x = "Share of Respondents",
#     y = "",
#     caption = "Source: Global Survey of Public Servants."
#   ) +
#   coord_cartesian(
#     xlim = c(0, 1)
#   )
#
# ggsave(
#   here("figs", "mgmt_target.png"),
#   height = 8,
#   width = 12
# )

# target setting
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
  here("chapter 1", "figs", "mgmt_target.png"),
  height = 8,
  width = 12
)

# correlation with performance
gsps_subset <- gsps_identified |>
  filter(
    str_detect(section_org, "(?i)Management")
  ) |>
  filter(
    indicator == "Clear targets" &
      group == "Institution"
  ) |>
  select(country, year, inst)

gsps_performance <- gsps_identified |>
  filter(
    group == "Institution"
  ) |>
  inner_join(gsps_subset) |>
  filter(
    section_org == "Performance Evaluation: Performance of the unit"
  ) |>
  select(
    country,
    year,
    inst,
    performance_score = mean
  )

gsps_identified |>
  filter(
    indicator == "Clear targets" &
      group == "Institution"
  ) |>
  select(
    country,
    year,
    inst,
    target_score = mean
  ) |>
  inner_join(
    gsps_performance
  ) |>
  ggplot(
    aes(target_score, performance_score)
  ) +
  geom_point() +
  geom_smooth(
    method = "lm"
  ) +
  labs(
    x = "Target-Setting",
    y = "Performance"
  )

ggsave(
  here("figs", "mgmt_target_performance.png"),
  height = 8,
  width = 12
)
