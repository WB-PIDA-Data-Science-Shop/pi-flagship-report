# 1. set-up ---------------------------------------------------------------
library(dplyr)
library(readr)
library(ggplot2)
library(hrbrthemes)
library(stringr)
library(vdemdata)
library(countrycode)
library(here)

# read-in data ------------------------------------------------------------
# vdem <- vdem

load(
  here("data", "world_value_survey.rdata")
)

wvs <- data1

# 3. visualization --------------------------------------------------------
# 3.1. evolution of polity scores
vdem |>
  select(country_id, year, e_p_polity) |>
  mutate(
    region = countrycode(
      country_id,
      origin = "vdem",
      destination = "region"
    )
  ) |>
  filter(!is.na(region)) |> # there are defective country_ids (128, 209)
  filter(year >= 1990) |>
  group_by(year, region) |>
  summarise(
    mean_polity = mean(e_p_polity, na.rm = TRUE)
  ) |>
  ggplot(
    aes(year, mean_polity, color = region)
  ) +
  geom_point(
    size = 3
  ) +
  geom_line(
    size = 1.5
    ) +
  theme_ipsum() +
  scale_color_ipsum() +
  facet_wrap(
    vars(region),
    nrow = 3
  ) +
  theme(legend.position = "none") +
  labs(
    x = "Year",
    y = "Polity IV Score",
    title = "Historical Evolution of Political Institutions:",
    subtitle = "Polity IV Score (Higher Scores = More Democratic)",
    caption = "Source: V-DEM Data"
  )

ggsave(
  here("figs", "democratic_institutions.png"),
  width = 10,
  height = 10,
  bg = "white"
)

# 3.2. impartial public administration
vdem |>
  select(country_id, year, v2clrspct) |>
  mutate(
    region = countrycode(
      country_id,
      origin = "vdem",
      destination = "region"
    )
  ) |>
  filter(!is.na(region)) |>
  filter(year >= 1990) |>
  group_by(region, year) |>
  summarise(
    mean_public_admin = mean(v2clrspct, na.rm = TRUE)
  ) |>
  ggplot(
    aes(year, mean_public_admin, color = region)
  ) +
  geom_point() +
  geom_line() +
  theme_ipsum() +
  facet_wrap(
    vars(region),
    nrow = 3
  ) +
  scale_color_ipsum() +
  theme(legend.position = "none") +
  labs(
    x = "Year",
    y = "Impartial Public Administration",
    title = "Historical Evolution of Public HRM Institutions:",
    subtitle = "Impartial Public Administration (Higher Scores = More Rigorous and Impartial)",
    caption = "Source: V-DEM Data"
  )

ggsave(
  here("figs", "public_admin_institutions.png"),
  width = 10,
  height = 10,
  bg = "white"
)

# 3.3. evolution of trust in others
wvs_trust <- wvs |>
  select(
    country = COUNTRY_ALPHA,
    year = S020,
    weight = S017,
    trust_courts = Y014C
  ) |>
  labelled::unlabelled() |>
  group_by(
    country, year
  ) |>
  summarise(
    trust_in_others = weighted.mean(as.numeric(trust_in_others), weight)
  )

wvs_trust |>
  mutate(
    year = as.numeric(
      as.character(year)
    )
  ) |>
  group_by(year) |>
  summarise(
    trust_in_others = mean(trust_in_others)
  ) |>
  ggplot(
    aes(year, trust_in_others)
  ) +
  geom_point() +
  geom_line() +
  theme_ipsum() +
  labs(
    x = "Year",
    y = "Trust in Others",
    title = "Historical Evolution of Social Institutions:",
    subtitle = "Trust in Others (Higher Scores = More Trust)",
    caption = "Source: World Values Survey"
  )

ggsave(
  here("figs", "social_institutions.png")
)
