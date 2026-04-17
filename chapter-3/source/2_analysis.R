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
  here("chapter 4", "source", "funs.R")
)

# read-in data ------------------------------------------------------------

countryclass <- read_csv(
  here("chapter 4", "data", "output", "countryclass.csv")
)

itu <- read_csv(
    here("chapter 4", "data", "output", "itu.csv")
)

boost <- read_csv(
    here("chapter 4", "data", "output", "boost.csv.gz")
)

wb_map <- st_read(
    here("chapter 4", "data", "input", "world-bank", "wb_shapefiles", "WB_countries_Admin0_10m")
) |> 
    rename(
        country_iso = ISO_A3,
        country = NAME_EN
    )

girg <- read_csv(
    here("chapter 4", "data", "input", "world-bank", "girg", "regulatory_governance_questions.csv")
) 

girg_questions <- girg |> 
    colnames() |> 
    str_subset("\\d\\. ")

girg_clean <- girg|> 
    select(
        country = `WDI Code`,
        economy = Economy,
        region = Region,

    )

afdb <- read_csv(
    here("chapter 4", "data", "input", "afdb", "afdb_regulatory_gov.csv")
) |> 
    clean_names()

gdp_pc <- read_csv(
    here("chapter 4", "data", "input", "world-bank", "gdp_pc_ppp.csv")
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
    here("chapter 4", "data", "output", "bready.csv")
)

enterprise_surveys <- read_csv(
    here("chapter 4", "data", "output", "enterprise_surveys.csv")
)

wdi <- read_csv(
    here("chapter 4", "data", "output", "wdi.csv")
)

bready_implementation_gap <- bready |> 
    group_by(economy_code) |>
    summarise(
        pillar_i_overall = mean(pillar_i_overall, na.rm = TRUE),
        pillar_ii_overall = mean(pillar_ii_overall, na.rm = TRUE)
    ) |> 
    mutate(
        regulatory_gap = pillar_ii_overall - pillar_i_overall
    )

bready_implementation_gap_topic <- bready |> 
    mutate(
        regulatory_gap = pillar_ii_overall - pillar_i_overall
    )

bready_competition <- read_csv(
  here("chapter 4", "data", "output", "bready_competition.csv")
) |> 
  inner_join(
    countryclass, by = c("economy_code" = "country_code")
  ) |>
  mutate(income_group = fct_relevel(
    income_group,
    c(
      "Low income",
      "Lower middle income",
      "Upper middle income",
      "High income"
    )
  )) 

gsr <- read_csv(
  here("chapter 4", "data", "output", "oecd_gsr.csv")
)

gsr_original <- read_csv(
  here("chapter 4", "data", "output", "oecd_gsr_original.csv")
)

bank_regulator <- read_csv(
  here("chapter 4", "data", "output", "wb_bank_regulator.csv")
) |> 
  left_join(
    countryclass,
    by = c("countryname" = "economy")
  ) |> 
  filter(
    !is.na(income_group)
  )

pmr <- read_rds(
  here("chapter 4", "data", "output", "pmr.rds")
)

# analysis: framework ----------------------------------------------------
## organizational: personnel ----------------------------------------------
# itu_latest_workforce <- itu |> 
#   mutate(
#     workforce_per_revenue = ppl_workforce/telecoms_revenue
#   ) |> 
#   filter_last_value(workforce_per_revenue)
# 
# wb_map |> 
#     left_join(
#         by = "country_iso"
#     ) |> 
#     ggplot() +
#     geom_sf(
#         aes(
#             fill = cut(
#             workforce_per_revenue, 
#             breaks = quantile(workforce_per_revenue, probs = seq(0, 1, 0.1), na.rm = TRUE), 
#             include.lowest = TRUE)
#         ),
#         size = 0
#     ) +
#     theme_void(
#         base_size = 18
#     ) +
#     ggtitle(
#         "People: Size of Workforce in Telecoms Regulator",
#         subtitle = "Normalized by Total Revenue in Telecoms Sector"
#     ) +
#     labs(
#         caption = "Source: International Telecoms Union. Latest Available Data by Country."
#     ) +
#     theme(
#         legend.position = "bottom"
#     )
# 
# ggsave(
#     here("chapter 4", "figs", "fig_4_3_a.png"),
#     bg = "white",
#     height = 10,
#     width = 14
# )

# people: banking regulator
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
  here("chapter 4", "figs", "bank_regulator_specialized.png"),
  height = 8,
  width = 14,
  bg = "white"
)

## organizational: financial resources ------------------------------------
boost_func_summary <- boost |> 
    group_by(year, func2) |> 
    summarise(
        expenditure = sum(paid, na.rm = TRUE),
        revenue = sum(as.numeric(approved), na.rm = TRUE),
        .groups = "drop"
    )

boost_annual_expenditure <- boost |> 
    group_by(year) |> 
    summarise(
        total_approved = sum(as.numeric(approved), na.rm = TRUE),
        total_expenditure = sum(paid, na.rm = TRUE)
    )

boost_regulation <- boost |> 
    filter(
        str_detect(admin2, "agência nacional") &
        !str_detect(admin2, "recursos|petróleo")
    ) |> 
    # remove codigo
    mutate(
        admin2 = str_replace(admin2, "\\d+ - ", "")
    )
    
# produce graph for each main regulatory sector
boost_regulation |> 
    mutate(
        admin2 = case_when(
            str_detect(admin2, "\\bana\\b") ~ "Water",
            str_detect(admin2, "\\baneel\\b") ~ "Electricity",
            str_detect(admin2, "\\banatel\\b") ~ "Telecoms",
            str_detect(admin2, "\\btransportes\\b") ~ "Transportation",
            T ~ NA_character_
        )
    ) |> 
    filter(!is.na(admin2)) |> 
    group_by(year, admin2) |> 
    summarise(
        total_approved = sum(
            as.numeric(approved), na.rm = TRUE
        ),
        total_paid = sum(paid, na.rm = TRUE),
        budget_execution = total_paid/total_approved
    ) |> 
    ggplot() +
    geom_line(
        aes(year, budget_execution, color = admin2)
    ) +
    geom_hline(
        yintercept = 1,
        linetype = "dashed"
    ) +
    scale_color_manual(
        values = palette()
    ) +
    scale_y_continuous(
        labels = scales::percent_format()
    ) +
    facet_wrap(
        ~ admin2,
        ncol = 2,
        labeller = label_wrap_gen(20)
    ) +
    theme(
        legend.position = "none",
        strip.text = element_text(size = 20)
    ) +
    labs(
        x = "Year",
        y = "Budget Execution"
    )

ggsave(
    here("chapter 4", "figs", "fig_4_4.png"),
    bg = "white",
    height = 16,
    width = 20
)

boost |> 
    group_by(year) |> 
    summarise(
        regulatory_expenditure = sum(paid[func2 == "125 - normatização e fiscalização"],
        na.rm = TRUE),
        total_expenditure = sum(paid, na.rm = TRUE)
    ) |> 
    mutate(
        share_regulatory = regulatory_expenditure/total_expenditure
    ) |> 
    # plot share of regulatory expenditure over time
    ggplot() + 
    geom_line(
        aes(year, share_regulatory),
        color = "#00ADE4",
        size = 1.5
    )

## organizational: information systems ------------------------------------
bank_regulator |> 
    select(
        country,
        infosys_risk_basic = Q12_19b_2016,
        infosys_risk_advanced = Q12_19c_2016
    ) |> 
    filter(country != "Euro Area") |> 
    mutate(
        region = countrycode(
            country,
            origin = "country.name",
            destination = "region"
        )
    ) |> 
    group_by(region) |> 
    summarise(
        share_risk_advanced = sum(infosys_risk_advanced == "X", na.rm = TRUE)/n()
    ) |> 
    ggplot() + 
    geom_col(
        aes(share_risk_advanced, reorder(region, share_risk_advanced)),
        fill = "#00ADE4"
    ) +
    scale_x_continuous(
        labels = scales::percent_format()
    ) +
    labs(
        x = "Share of banking regulatory institutions",
        y = ""
    ) +
    theme(
        legend.position = "none"
    )

ggsave(
    here("chapter 4", "figs", "fig_4_5.png"),
    bg = "white",
    width = 14
)


# information systems
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
  here("chapter 4", "figs", "bready_information_systems.png"),
  width = 14,
  height = 16,
  bg = "white"
)

## organizational: management practices -----------------------------------
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
    gdp_pc_last,
    by = c("country_code" = "country_iso")
  ) |> 
  filter(
    !is.na(response_score)
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

gsr_original |> 
  filter(year == 2023 & sector != "Water" & !is.na(response_score)) |> 
  count(response) |> 
  mutate(
    total = sum(n),
    share = n/total
  )

ggsave(
  here("chapter 4", "figs", "gsr_management_practices.png"),
  height = 12,
  width = 18,
  bg = "white"
)

gsr_original |> 
  filter(response != ".") |> 
  tabyl(response, sector) |>
  adorn_percentages("col") |> 
  adorn_pct_formatting() |> 
  write_csv(
    here("chapter 4", "tables", "gsr_management_practices.csv")
  )


## governance: accountability ---------------------------------------------
wb_map |> 
    left_join(
        itu |> filter_last_value(mgmt_dispute_resolution),
        by = "country_iso"
    ) |> 
    mutate(
        mgmt_dispute_resolution = str_replace(mgmt_dispute_resolution, "\\(.*\\)", "")
    ) |> 
    ggplot() +
    geom_sf(
        aes(
            fill = mgmt_dispute_resolution
        ),
        size = 0
    ) +
    scale_fill_brewer(
        palette = "Set2",
        na.value = "grey80",
        name = "Type of Dispute Resolution Mechanism"
    ) +
    theme_void(
        base_size = 18
    ) +
    ggtitle(
        "Accountability: Type of Dispute Resolution Mechanism",
        subtitle = "Main DRM Reported"
    ) +
    labs(
        caption = "Source: International Telecoms Union. Latest Available Data by Country."
    ) +
    theme(
        legend.position = "bottom"
    ) +
    guides(fill = guide_legend(nrow = 2))

wrggsave(
    here("chapter 4", "figs", "itu_accountability_map.png"),
    bg = "white",
    height = 10,
    width = 14
)

gsr |> 
  pivot_longer(
    cols = c(energy_average:water_scope),
    names_pattern = "(.*)_(.*)",
    names_to = c("sector", "score_type"),
    values_to = "score"
  ) |> 
  filter(
    score_type == "account" &
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
  # actual sector points
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
  labs(x = "Accountability", y = "")

ggsave(
  here("chapter 4", "figs", "gsr_accountability.png"),
  bg = "white", 
  height = 12,
  width = 14
)

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
  here("chapter 4", "figs", "gsr_accountability_type.png"),
  bg = "white", 
  height = 12,
  width = 16
)

## governance: transparency ----------------------------------------------

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
        ),
        size = 0
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
  here("chapter 4", "figs", "afdb_transparency_map.png"),
  bg = "white",
  height = 10,
  width = 14
)

afdb |> 
  mutate(
    country_iso = countrycode(country, "country.name.en", "wb"),
    country_iso = if_else(
      country == "CAR",
      "CAF",
      country_iso
    )
  ) |> 
  left_join(
    gdp_pc_last,
    by = c("country_iso")
  ) |> 
  ggplot(
    aes(gdp_pc_ppp, transparency)
  ) +
  geom_point() +
  scale_x_continuous(
    trans = "log10",
    labels = scales::comma
  )
  


## personnel: independence ------------------------------------------------
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
    ggtitle(
        "Distribution of Appointment Authority for Telecoms Regulatory Institutions",
        subtitle = "Latest Available Data per Country"
    ) +
    labs(
        x = "Share of Countries",
        y = "",
        caption = "Source: ITU. Excludes non-responses and Other category."
    )

ggsave(
    here("chapter 4", "figs", "fig_4_9.png"),
    width = 14,
    bg = "white"
)

## governance: independence -----------------------------------------------
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
  here("chapter 4", "figs", "gsr_independence.png"),
  bg = "white", 
  height = 12,
  width = 14
)

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
  here("chapter 4", "figs", "gsr_independence_financial.png"),
  bg = "white", 
  height = 12,
  width = 14
)

# analysis: outcomes ------------------------------------------------------
## regulatory gap ---------------------------------------------------------
set.seed(1789)
sample_countries <- bready |> 
    distinct(economy) |> 
    slice_sample(n = 10)

bready |> 
    # subset to random sample of 10 countries
    inner_join(sample_countries) |> 
    ggplot(
        aes(
            x = pillar_ii_overall,
            y = reorder(
                economy, pillar_ii_overall, FUN = mean
            )
        )
    ) +
    geom_point(
        aes(color = topic),
        size = 5
    ) +
    labs(
        x = "Public Services Overall Score",
        y = ""
    ) +
    theme(
        legend.position = "bottom"
    ) +
    scale_color_brewer(
        palette = "Paired",
        labels = c(
            business_entry = "Business Entry",
            business_insolvency = "Business Insolvency",
            business_location = "Business Location",
            dispute_resolution = "Dispute Resolution",
            financial_services = "Financial Services",
            utility_services = "Utility Services",
            labor = "Labor",
            international_trade = "International Trade",
            market_competition = "Market Competition",
            taxation = "Taxation"
        )
    ) 
   
ggplot2::ggsave(
    here("chapter 4", "figs", "bready_public_services.png"),
    width = 20,
    height = 16,
    bg = "white"
)

# gap between public services and regulatory frameworks
# by topic
bready_implementation_gap_topic |> 
  filter(
    topic %in% c(
      "business_entry",
      "financial_services",
      "market_competition",
      "utility_services"
    )
  ) |>
  mutate(
    global_average = mean(regulatory_gap, na.rm = TRUE)
  ) |> 
  plot_scores(
    regulatory_gap,
    economy_code,
    color_col = topic
  ) +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.justification = "center"
  ) +
  geom_vline(
    aes(xintercept = global_average),
    linetype = "dashed",
    color = "coral2"
  ) +
  coord_flip() +
  labs(
    y = "",
    x = "Regulatory Implementation Gap\n (by Topic)"
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
    x = mean(bready_implementation_gap_topic$regulatory_gap, na.rm = TRUE) - 8, # same as global_average
    y = length(unique(bready_implementation_gap_topic$economy_code)) - 8,   # just beyond last category
    label = "Global Average",
    color = "coral2",
    size = 6,
    hjust = 0
  )

ggsave(
  here("chapter 4", "figs", "bready_implementation_gap_topic.png"),
  width = 14,
  height = 10,
  bg = "white"
)

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
  here("chapter 4", "figs", "bready_pillar_ii_topic.png"),
  width = 14,
  height = 16,
  bg = "white"
)

# correlation between market competition independence and implementation gap
bready_competition |> 
  ggplot(
    aes(competition_authority_independence, -regulatory_gap)
  ) +
  geom_point(
    size = 6
  ) +
  geom_smooth(
    method = "lm",
    color = "orangered2",
    se = FALSE
  ) +
  labs(
    x = "Independence of Competition Authority", 
    y = "Regulatory Implementation Gap"
  )

ggplot2::ggsave(
  here("chapter 4", "figs", "bready_competition_independence_regulatory_gap.png"),
  width = 10,
  height = 8,
  bg = "white"
)

# correlation between market competition transparency and implementation gap
bready_competition |> 
  ggplot(
    aes(competition_authority_transparency, -regulatory_gap)
  ) +
  geom_point(
    size = 6
  ) +
  geom_smooth(
    method = "lm",
    color = "orangered2",
    se = FALSE
  ) +
  labs(
    x = "Transparency of Competition Authority", 
    y = "Regulatory Implementation Gap"
  )

ggplot2::ggsave(
  here("chapter 4", "figs", "bready_competition_transparency_regulatory_gap.png"),
  width = 10,
  height = 8,
  bg = "white"
)

# correlation between transparency and independence
bready_competition |> 
  ggplot(
    aes(competition_authority_transparency, competition_authority_independence)
  ) +
  geom_point(
    aes(color = income_group),
    size = 7,
    alpha = 0.6
  ) +
  geom_smooth(
    method = "lm",
    color = "orangered2",
    se = FALSE
  ) +
  scale_color_brewer(
    palette = "Paired"
  ) +
  guides(color = guide_legend(nrow = 2)) +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    x = "Transparency of Competition Authority", 
    y = "Independence of Competition Authority"
  )

ggplot2::ggsave(
  here("chapter 4", "figs", "bready_competition_transparency_independence.png"),
  width = 14,
  height = 12,
  bg = "white"
)


# international telecoms union -------------------------------------------

itu |> 
    filter(
        ppl_term_appointment != "Remark"
    ) |> 
    group_by(ppl_term_appointment) |> 
    summarise(
        average_investment = mean(annual_investment_telecoms/telecoms_revenue, na.rm = TRUE)/1e6,
        total = n()
    ) |> 
    ggplot() +
    geom_col(
        aes(ppl_term_appointment, average_investment),
        fill = "#00ADE4"
    ) +
    ggtitle(
        "Average Annual Investment in Telecoms by Type of Term",
        "Unit of Analysis: Country-Year"
    ) +
    labs(
        x = "Duration of Term Appointment",
        y = "Average Annual Investment (Millions USD)"
    )

ggsave(
    here("chapter 4", "figs", "itu_investment_by_term.png"),
    height = 6,
    width = 8,
    bg = "white"
)

# correlation between workforce (people) and calls
itu |>
    mutate(
        workforce_bin = ntile(ppl_workforce, n=25)
    ) |> 
    group_by(workforce_bin) |> 
    summarise(
        dropped_call_rate = mean(dropped_call_rate, na.rm = TRUE),
        n = n()
    ) |> 
    ungroup() |> 
    ggplot() +
    geom_point(
        aes(workforce_bin, dropped_call_rate, size = n)
    ) +
    coord_cartesian(
        ylim = c(0, 5)
    )

# correlation between term appointment (governance of people) and calls
itu |>
    # remove belize due to missingness
    filter(country_iso != "BLZ" & ppl_term_appointment != "Other") |> 
    group_by(ppl_term_appointment) |> 
    summarise(
        dropped_call_rate = mean(dropped_call_rate, na.rm = TRUE),
        n = n()
    ) |> 
    ggplot() +
    geom_point(
        aes(ppl_term_appointment, dropped_call_rate, size = n),
        outlier.shape = NA
    ) +
    coord_cartesian(
        ylim = c(0, 5)
    )

# correlation between governance dimensions and investment
itu_independence <- itu |> 
  rowwise() |> 
  mutate(
    gov_independence = sum(
      ppl_authority_appointment %in% c("Board of the Regulatory Authority", "Parliament"),
      ppl_authority_removal %in% c("Board of the regulatory Authority", "Parliament"),
      ppl_law_appointment_selection == "Yes",
      ppl_law_ground_for_removal == "Yes",
      independence_overrule == "No",
      independence_reorg == "Yes",
      independence_reg_autonomy == "Yes"
    )/7
  ) |> 
  ungroup() |> 
  group_by(country_iso) |> 
  summarise(
    average_gov_independence = mean(gov_independence, na.rm = TRUE),
    average_investment = mean(annual_investment_telecoms/telecoms_revenue, na.rm = TRUE),
    average_coverage_3g = mean(share_pop_coverage_3g, na.rm = TRUE),
    .groups = "drop"
  )

itu_independence |> 
  inner_join(
    countryclass,
    by = c("country_iso" = "country_code")
  ) |> 
  mutate(
    income_group = fct_relevel(
      income_group,
      c("Low income", "Lower middle income", "Upper middle income", "High income")
    )
  ) |> 
  filter(
    !is.na(income_group)
  ) |> 
  ggplot(
    aes(
      average_gov_independence,
      average_investment
    )
  ) +
  geom_point(
    aes(color = income_group),
    size = 6,
    alpha = 0.5
  ) +
  scale_color_brewer(
    palette = "Paired"
  ) +
  geom_smooth(
    color = "red3",
    se = FALSE,
    method = "lm"
  ) +
  coord_cartesian(
    ylim = c(0, 0.75)
  ) +
  scale_y_continuous(
    labels = percent_format()
  ) +
  theme(
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(nrow = 2)) +
  labs(
    x = "Independence of ICT Regulator",
    y = "Investment in Telecoms\n (Percentage of Revenue)"
  )

ggsave(
  here("chapter 4", "figs", "itu_independence_investment.png"),
  height = 12,
  width = 14,
  bg = "white"
)

# different measures of independence
# break down by region (?)
itu |> 
  mutate(
    ppl_authority_appointment_binary = if_else(
      ppl_authority_appointment %in% c("Board of the Regulatory Authority", "Parliament"), 1L, 0L
    ),
    ppl_authority_removal_binary = if_else(
      ppl_authority_removal %in% c("Board of the regulatory Authority", "Parliament"), 1L, 0L
    ),
    ppl_law_appointment_selection_binary = if_else(
      ppl_law_appointment_selection == "Yes", 1L, 0L
    ),
    ppl_law_ground_for_removal_binary = if_else(
      ppl_law_ground_for_removal == "Yes", 1L, 0L
    ),
    independence_overrule_binary = if_else(
      independence_overrule == "No", 1L, 0L
    ),
    independence_reorg_binary = if_else(
      independence_reorg == "Yes", 1L, 0L
    ),
    independence_reg_autonomy_binary = if_else(
      independence_reg_autonomy == "Yes", 1L, 0L
    )
  ) |> 
  group_by(country_iso) |> 
  slice_max(
    n = 1,
    order_by = year
  ) |> 
  ungroup() |> 
  group_by(region) |> 
  summarise(
    across(
      ends_with("binary"),
      \(var) mean(var, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |>
  select(region, starts_with("ppl")) |> 
  pivot_longer(
    -c(region)
  ) |> 
  ggplot() +
  geom_col(
    aes(
      value,
      region,
      fill = name
    ),
    position = position_dodge2(preserve = "single")
  ) +
  scale_fill_brewer(
    palette = "Paired"
  ) +
  theme(
    legend.position = "bottom"
  )

ggsave(
  here("chapter 4", "figs", "itu_independence.png"),
  height = 6,
  width = 8,
  bg = "white"
)

# enterprise surveys
enterprise_surveys |> 
    left_join(
        wdi,
        c("year", "economy_code" = "iso3c")
    ) |> 
    filter(
        !is.na(senior_management_time_reg)
    ) |> 
    group_by(economy_code) |> 
    slice_max(order_by = year) |> 
    ggplot(
        aes(
            senior_management_time_reg, 
            log(gdp_pc_ppp_2005)
        )
    ) +
    geom_point() +
    geom_smooth(
        method = "lm"
    )
