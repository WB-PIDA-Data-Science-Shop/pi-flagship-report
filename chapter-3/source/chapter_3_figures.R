# set-up ------------------------------------------------------------------
library(readr)
library(dplyr)
library(ggplot2)
library(forcats)
library(here)
library(countrycode)
library(tidyr)
library(ggstats)
library(stringr)
library(scales)
library(RColorBrewer)
library(sf)
library(ggrepel)
library(cowplot)
library(janitor)
library(ggthemes)
library(tidytext)

theme_set(
    theme_cowplot(
        font_size = 24
    )
)

options(
    ggplot2.discrete.colour= c(
        "#00ADE4", "#002345",
        "#ff8000", "#fcdf2d"
    )
)

set.seed(2025)

# aux.funs ----------------------------------------------------------------
filter_last_value <- function(data, var){
    data_last_value <- data |> 
        group_by(country_iso) |> 
        filter(year == max(year[!is.na({{var}})])) |> 
        ungroup()

    return(data_last_value)
}

source(
  here("chapter-3", "source", "funs.R")
)

# read-in data ------------------------------------------------------------

countryclass <- read_csv(
  here("chapter-3", "data", "output", "countryclass.csv")
)

itu <- read_csv(
    here("chapter-3", "data", "output", "itu.csv")
)

wb_map <- st_read(
    here("chapter-3", "data", "input", "world-bank", "wb_shapefiles", "WB_countries_Admin0_10m")
) |> 
    rename(
        country_iso = ISO_A3,
        country = NAME_EN
    )

afdb <- read_csv(
    here("chapter-3", "data", "input", "afdb", "afdb_regulatory_gov.csv")
) |> 
    clean_names()

gdp_pc <- read_csv(
    here("chapter-3", "data", "input", "world-bank", "gdp_pc_ppp.csv")
) |> 
    pivot_longer(
        cols = c(`1960`:`2022`),
        values_to = "gdp_pc_ppp",
        names_to = "year"
    ) |> 
    transmute(
        country_iso = `Country Code`,
        country = `Country Name`,
        year = as.numeric(year),
        gdp_pc_ppp
    ) |> 
    na.omit()

gdp_pc_last <- gdp_pc |> 
  arrange(country_iso, year) |> 
  group_by(country_iso) |> 
  slice_tail(n = 1) |> 
  ungroup()

bready <- read_csv(
    here("chapter-3", "data", "output", "bready.csv")
)

bready_implementation_gap_topic <- bready |> 
    mutate(
        regulatory_gap = pillar_ii_overall - pillar_i_overall
    )

gsr <- read_csv(
  here("chapter-3", "data", "output", "oecd_gsr.csv")
)

gsr_original <- read_csv(
  here("chapter-3", "data", "output", "oecd_gsr_original.csv")
)

bank_regulator <- read_csv(
  here("chapter-3", "data", "output", "wb_bank_regulator.csv")
) |> 
  left_join(
    countryclass,
    by = c("countryname" = "economy")
  ) |> 
  filter(
    !is.na(income_group)
  )


# figure 3.2 -------------------------------------------------------------
# implementation (pillar 2) by topic
bready_implementation_gap_topic |> 
  filter(
    topic %in% c(
      "business_entry",
      "financial_services",
      "market_competition",
      "utility_services"
    )
  ) |>
  # only retain economies that have scores for all four topics
  add_count(economy) |> 
  filter(n == 4) |> 
  mutate(
    global_average = mean(pillar_ii_overall, na.rm = TRUE)
  ) |> 
  plot_scores(
    pillar_ii_overall,
    economy,
    color_col = topic
  ) +
  theme(
    # axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.justification = "center"
  ) +
  geom_vline(
    aes(xintercept = global_average),
    linetype = "dashed",
    color = "coral2"
  ) +
  labs(
    y = "",
    x = "Public Services (by Topic)"
  ) +
  scale_color_brewer(
    palette = "Paired",
    name = "Topic",
    labels = c(
      business_entry = "Business Entry",
      financial_services = "Financial Services",
      utility_services = "Utility Services",
      market_competition = "Market Competition"
    )
  ) +
  annotate(
    "text",
    x = mean(bready_implementation_gap_topic$pillar_ii_overall, na.rm = TRUE) - 8, # same as global_average
    y = length(unique(bready_implementation_gap_topic$economy_code)) - 8,   # just beyond last category
    label = "Global Average",
    color = "coral2",
    size = 6,
    hjust = 0
  )

ggsave(
  here("chapter-3", "figs", "fig_3_2.png"),
  width = 14,
  height = 16,
  bg = "white"
)

# figure 3.4 -------------------------------------------------------------
itu |> 
    filter(
        ppl_authority_appointment != "None of the above,"
    ) |> 
    mutate(
        ppl_authority_appointment = if_else(
            ppl_authority_appointment == "Head of Government",
            "President/Head of State",
            ppl_authority_appointment
        )
    ) |> 
    filter_last_value(ppl_authority_appointment) |> 
    group_by(ppl_authority_appointment) |> 
    summarise(
        total = n()
    ) |> 
    ungroup() |> 
    mutate(
        share_total = total/sum(total)
    ) |> 
    ggplot() +
    geom_col(
        aes(
            share_total,
            reorder(ppl_authority_appointment, share_total)
        ),
        fill = "#00ADE4"
    ) +
    scale_x_continuous(
        labels = scales::percent
    ) +
    labs(
        x = "Share of Countries",
        y = "",
        caption = "Source: ITU. Excludes non-responses and Other category."
    )

ggsave(
    here("chapter-3", "figs", "fig_3_4.png"),
    width = 14,
    height = 10,
    bg = "white"
)

# figure 3.5 -------------------------------------------------------------
bank_regulator |>
  filter(
    personnel_share_specialized <= 1
  ) |>
  ggplot(
    aes(personnel_share_specialized, fct_reorder(income_group, personnel_share_specialized, .na_rm = TRUE))
  ) +
  geom_boxplot(
    aes(fill = income_group),
    width = 0.35,
    outlier.shape = NA
  ) +
  geom_jitter(
    aes(fill  = income_group, size = personnel_workforce),
    shape = 21,
    alpha = 0.6
  ) +
  labs(
    x = "Share of Specialized Regulators",
    y = ""
  ) +
  scale_x_continuous(
    labels = scales::percent_format()
  ) +
  scale_size(range = c(1, 10)) +
  scale_color_brewer(
    palette = "Paired"
  ) +
  scale_fill_brewer(
    palette = "Paired"
  ) +
  theme(legend.position = "none")

ggsave(
  here("chapter-3", "figs", "fig_3_5.png"),
  height = 8,
  width = 14,
  bg = "white"
)

# figure 3.7 -------------------------------------------------------------
bready |> 
  filter(
    topic %in% c("business_entry", "business_location", "financial_services")
  ) |> 
  mutate(
    economy = iconv(economy, from = "latin1", to = "UTF-8")
  ) |>
  rename(
    information_systems = category_2_2_overall
  ) |> 
  group_by(topic) |> 
  mutate(
    information_systems = scale(information_systems)
  ) |> 
  ungroup() |> 
  group_by(economy) |> 
  mutate(
    score_min = min(information_systems),
    score_max = max(information_systems),
    score_mean = mean(information_systems)
  ) |> 
  ungroup() |> 
  mutate(
    economy = fct_reorder(economy, information_systems, .fun = mean)
  ) |> 
  ggplot() +
  # segment between min and max
  geom_segment(
    aes(
      x = score_min,
      xend = score_max,
      y = economy,
      yend = economy
    ),
    color = "grey70"
  ) +
  geom_point(
    aes(
      x = score_mean,
      y = economy
    ),
    shape = 1,
    size = 6,
    color = "grey50"
  ) +
  geom_point(
    aes(
      information_systems,
      economy,
      color = topic
    ),
    size = 4
  ) +
  scale_color_brewer(
    name = "Topic",
    palette = "Paired",
    labels = c(
      business_entry = "Business Entry",
      business_location = "Business Location",
      financial_services = "Financial Services"
    )
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 1.5,
    color = "grey50"
  ) +
  guides(
    color = guide_legend(nrow = 1)
  ) +
  theme(
    legend.position = "bottom"
  ) +
  labs(x = "Information Systems", y = "")


ggplot2::ggsave(
  here("chapter-3", "figs", "fig_3_7.png"),
  width = 14,
  height = 16,
  bg = "white"
)

# figure 3.8 -------------------------------------------------------------
gsr_original |> 
  filter(
    year == 2023 & sector != "Water"
  ) |>
  mutate(
    country_code = if_else(
      country_code == "BRA2",
      "BRA",
      country_code
    )
  ) |> 
  left_join(
    gdp_pc_last |> select(-year),
    by = c("country_code" = "country_iso")
  ) |> 
  filter(
    !is.na(response_score) &
      # filter only questions regarding strategic objectives
      str_detect(question_code, "b.b.9$")
  ) |> 
  add_count(country_code) |> 
  filter(n == 4) |> 
  summarize_scores(
    response_score,
    country_code
  ) |> 
  arrange(gdp_pc_ppp, country_code) |>
  mutate(
    country_code = fct_inorder(country_code)
  ) |> 
  ggplot() +
  geom_jitter(
    aes(response_score, country_code, color = sector),
    size = 4,
    width = 0.05, 
    height = 0
  ) +
  scale_color_brewer(
    name = "Sector",
    palette = "Paired"
  ) +
  scale_x_continuous(
    breaks = c(0, 1, 2, 3, 4),  # numeric positions
    labels = str_wrap(c(
      "0" = "No strategic objectives defined",
      "1" = "Strategic objectives defined but not measured/reported on",
      "2" = "Yes, internally/for internal use",
      "3" = "Yes, information published on website",
      "4" = "Yes, information reported to government ministry/parliament (accountable body)"
    ), width = 20)
  ) +
  labs(
    x = "",
    y = ""
  ) +
  theme(
    panel.grid.major.y = element_line(color = "grey80"),
    axis.text.x = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.justification = "center"
  )

ggsave(
  here("chapter-3", "figs", "fig_3_8.png"),
  height = 12,
  width = 18,
  bg = "white"
)

# figure 3.9 -------------------------------------------------------------

gsr_original |> 
  filter(
    year == 2023 
  ) |> 
  filter(
    question_text_2023 %in% c(
      "Is there a legislative requirement for the regulator to answer requests from or attend hearings organized by parliamentary/congressional committees?",
      "Are the following legislative requirements in place to enhance the transparency of the regulator's activities (with confidential and commercially sensitive information appropriately removed if needed)? - Public consultation on relevant activities"
    )
  ) |>
  mutate(
    type_accountability = if_else(
      question_text_2023 == "Is there a legislative requirement for the regulator to answer requests from or attend hearings organized by parliamentary/congressional committees?",
      "Parliamentary/Congressional hearings", "Public consultation"
    )
  ) |>
  group_by(sector, type_accountability) |> 
  summarise(
    share = mean(str_detect(response, "yes")),
    n_response = n(),
    .groups = "drop"
  ) |>
  group_by(sector) |> 
  mutate(
    average_share = mean(share)
  ) |> 
  ggplot(
    aes(
      share, fct_reorder(sector, average_share), fill = type_accountability
    )
  ) +
  geom_col(
    position = "dodge2"
  ) +
  scale_fill_brewer(
    name = "Type of Accountability",
    palette = "Paired"
  ) +
  scale_x_continuous(
    labels = scales::percent_format()
  ) +
  labs(
    x = "Share of Sector Regulators",
    y = ""
  ) +
  theme(
    legend.position = "bottom"
  )

ggsave(
  here("chapter-3", "figs", "fig_3_9.png"),
  bg = "white", 
  height = 12,
  width = 16
)

# figure 3.10 ------------------------------------------------------------
wb_map |> 
    filter(
        WB_REGION %in% c("AFR") |
        country_iso %in% c("EGY", "DZA", "LBY", "MAR", "TUN", "DJI")
    ) |> 
    left_join(
        afdb |> select(country, transparency),
        by = "country"
    ) |> 
    ggplot() +
    geom_sf(
        aes(
            fill = transparency
        )
    ) +
    scale_fill_distiller(
        palette = "Blues",
        direction = 1,
        na.value = "grey80",
        name = "Transparency"
    ) +
    theme_void(
        base_size = 18
    )

ggsave(
  here("chapter-3", "figs", "fig_3_10.png"),
  bg = "white",
  height = 10,
  width = 14
)

# figure 3.11 ------------------------------------------------------------
# panel a
# pooled
gsr |> 
  pivot_longer(
    cols = c(energy_average:water_scope),
    names_pattern = "(.*)_(.*)",
    names_to = c("sector", "score_type"),
    values_to = "score"
  ) |> 
  filter(
    score_type == "indep" &
      year == 2023
  ) |> 
  group_by(countryname) |> 
  mutate(
    score_min = min(score, na.rm = TRUE),
    score_max = max(score, na.rm = TRUE),
    score_mean = mean(score, na.rm = TRUE)
  ) |> 
  ungroup() |> 
  mutate(
    countryname = fct_reorder(countryname, score, .fun = mean)
  ) |> 
  ggplot() +
  # segment between min and max
  geom_segment(
    aes(
      x = score_min,
      xend = score_max,
      y = countryname,
      yend = countryname
    ),
    color = "grey70"
  ) +
  geom_point(
    aes(
      x = score_mean,
      y = countryname
    ),
    shape = 1,
    size = 6,
    color = "grey50"
  ) +
  geom_point(
    aes(
      score,
      countryname,
      color = sector
    ),
    size = 4
  ) +
  scale_color_brewer(
    palette = "Paired",
    labels = c(
      air = "Air Transport",
      comms = "E-communications",
      energy = "Energy",
      rail = "Rail transport",
      water = "Water"
    )
  ) +
  guides(
    color = guide_legend(nrow = 2)
  ) +
  theme(
    legend.position = "bottom"
  ) +
  labs(x = "Independence", y = "")

ggsave(
  here("chapter-3", "figs", "fig_3_11a.png"),
  bg = "white", 
  height = 12,
  width = 14
)

# panel b

# independence: financial resources
gsr_original |> 
  filter(
    year == 2023 &
      # only maintain relevant financial questions
      str_detect(question_code, "b\\.a\\.2[1-7]" ) &
      !str_detect(question_code, "b\\.a\\.22" )
  ) |> 
  mutate(
    country_code = if_else(
      country_code == "BRA2",
      "BRA",
      country_code
    )
  ) |> 
  left_join(
    countryclass,
    by = c("country_code")
  ) |> 
  # generate an average score by sector.
  # note that all scores are equally weighted in the OECD's schemata
  group_by(country_code, economy, sector) |> 
  summarise(
    response_score = mean(response_score, na.rm = TRUE)
  ) |>  
  group_by(economy) |> 
  mutate(
    score_min = min(response_score, na.rm = TRUE),
    score_max = max(response_score, na.rm = TRUE),
    score_mean = mean(response_score, na.rm = TRUE),
    n_sector = n_distinct(sector)
  ) |> 
  ungroup() |> 
  # reorder by GDP per capita
  left_join(
    gdp_pc_last,
    by = c("country_code" = "country_iso")
  ) |> 
  mutate(
    economy = fct_reorder(economy, response_score, .fun = mean)
  ) |> 
  ggplot() +
  # segment between min and max
  geom_segment(
    aes(
      x = score_min,
      xend = score_max,
      y = economy,
      yend = economy
    ),
    color = "grey70"
  ) +
  geom_point(
    aes(
      x = score_mean,
      y = economy
    ),
    shape = 1,
    size = 6,
    color = "grey50"
  ) +
  geom_point(
    aes(
      response_score,
      economy,
      color = sector
    ),
    size = 4
  ) +
  scale_color_brewer(
    palette = "Paired",
    labels = c(
      air = "Air Transport",
      comms = "E-communications",
      energy = "Energy",
      rail = "Rail transport",
      water = "Water"
    )
  ) +
  guides(
    color = guide_legend(nrow = 2)
  ) +
  theme(
    legend.position = "bottom"
  ) +
  labs(x = "Independence: Financial Resources", y = "")

ggsave(
  here("chapter-3", "figs", "fig_3_11b.png"),
  bg = "white", 
  height = 12,
  width = 14
)