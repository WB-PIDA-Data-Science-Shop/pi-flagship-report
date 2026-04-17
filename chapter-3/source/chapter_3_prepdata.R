# ==============================================================================
# 1. set-up
# ==============================================================================
library(readr)
library(dplyr)
library(here)
library(countrycode)
library(tidyr)
library(purrr)
library(stringr)
library(janitor)
library(readxl)
library(openxlsx)

# ==============================================================================
# 2. read-in data
# ==============================================================================
# people
itu_appointment_raw <- read_csv(
    here("chapter 4", "data", "input", "itu", "itu_appointment.csv")
)

itu_workfoce_raw <- read_csv(
    here("chapter 4", "data", "input", "itu", "itu_workforce.csv")
)

# money: not available

# management system
itu_coordination_raw <- read_csv(
    here("chapter 4", "data", "input", "itu", "itu_coordination.csv")
)

itu_dispute_raw <- read_csv(
    here("chapter 4", "data", "input", "itu", "itu_dispute_resolution.csv")
)

itu_institutional_structure_raw <- read_csv(
    here("chapter 4", "data", "input", "itu", "itu_institutional_structure.csv")
)

itu_dropped_call_raw <- read_csv(
    here("chapter 4", "data", "input", "itu", "itu_mobile_dropped_call_ratio.csv")
)

itu_investment_raw <- read_csv(
    here("chapter 4", "data", "input", "itu", "itu_investment.csv")
)

# covariates
itu_revenue_raw <- read_csv(
    here("chapter 4", "data", "input", "itu", "itu_revenue.csv")
)

itu_broadband_raw <- read_csv(
    here("chapter 4", "data", "input", "itu", "itu_broadband.csv")
)

itu_mobile_subscription_raw <- read_csv(
    here("chapter 4", "data", "input", "itu", "itu_mobile_subscription.csv")
)

itu_coverage_raw <- read_csv(
    here("chapter 4", "data", "input", "itu", "itu_coverage.csv")
)

itu_decision_making_raw <- read_csv(
  here("chapter 4", "data", "input", "itu", "itu_decision_making.csv")
)

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
        year = as.numeric(year),
        gdp_pc_ppp
    ) |> 
    na.omit()

# bready
bready_files <- fs::dir_ls(
    here("chapter 4", "data", "input", "world-bank", "bready"),
    glob = "*.csv$"
)

topics_bready <- bready_files |> 
    str_replace(
        ".*bready_(.*)\\.csv$",
        "\\1"
    )

bready_raw <- bready_files |> 
    set_names(topics_bready) |> 
    map_dfr(
        \(x) read_csv(
            x, 
            col_select = any_of(
                c("Economy", "EconomyName", "EconomyCode", "Pillar I Overall", "Pillar II Overall", "Pillar III Overall", "Category 2.2 Overall")
            ),
            locale = locale(encoding = "latin1")
        ),
        .id = "topic"
        
    ) |> 
    mutate(
        Economy = coalesce(Economy, EconomyName)
    ) |> 
    select(-EconomyName) |>
    clean_names()

gsr_raw <- read_xlsx(
  here("chapter 4", "data", "input", "oecd", "oecd_gsr.xlsx"),
  sheet = "Clean"
)

gsr_original_raw <- read_xlsx(
  here("chapter 4", "data", "input", "oecd", "oecd_gsr_original.xlsx"),
  skip = 2,
  sheet = "Database"
)

# financial regulator
# url: https://datacatalog.worldbank.org/int/search/dataset/0038632
# 2021 edition
bank_regulator_raw <- read_csv(
  here("chapter 4", "data", "input", "world-bank", "brss", "banking_regulator.csv")
)

countryclass_raw <- read.xlsx(
  "https://ddh-openapi.worldbank.org/resources/DR0095333/download/"
)

# ==============================================================================
# 3. process data
# ==============================================================================
itu_appointment <- itu_appointment_raw |> 
    mutate(
        variable = case_match(
            seriesCode,
            "trAreGroundsRemovalHeadSet" ~ "ppl_law_ground_for_removal",
            "trDoesLawClearSelectionHead" ~ "ppl_law_appointment_selection",
            "trNormalPeriodOfAppointment" ~ "ppl_term_appointment",
            "trNormalPeriodOfAppointmentRenewable" ~ "ppl_term_renewable",
            "trPowerToRemoveHead" ~ "ppl_authority_removal",
            "trRA_WhoAppointsMembers" ~ "ppl_authority_appointment",
            "trReasonsForRemoval" ~ "ppl_reason_removal"
        )
    ) |> 
    pivot_wider(
        id_cols = c(entityIso, entityName, dataYear),
        names_from = variable,
        values_from = dataValue
    ) |> 
    select(
        country_iso = entityIso,
        country_name = entityName,
        year = dataYear,
        starts_with("ppl")
    )

itu_workforce <- itu_workfoce_raw |> 
    select(
        country_iso = entityIso,
        country_name = entityName,
        year = dataYear,
        ppl_workforce = dataValue
    )

itu_dispute <- itu_dispute_raw |> 
    filter(
        seriesCode %in% c("trDispMech", "trDisputeDecisionsOnWebsite")
    ) |> 
    mutate(
        variable = case_match(
            seriesCode,
            "trDispMech" ~ "mgmt_dispute_resolution",
            "trDisputeDecisionsOnWebsite" ~ "mgmt_dispute_resolution_publish"
        ),
        # extract main dispute resolution mechanism
        dataValue = if_else(
            variable == "mgmt_dispute_resolution",
            coalesce(`dataValue  1`, dataValue),
            dataValue
        )
    ) |> 
    pivot_wider(
        id_cols = c(entityIso, entityName, dataYear),
        names_from = variable,
        values_from = dataValue
    ) |> 
    select(
        country_iso = entityIso,
        country_name = entityName,
        year = dataYear,
        starts_with("mgmt")
    )


itu_institutional_structure <- itu_institutional_structure_raw |> 
     filter(
        seriesCode %in% c("trRA_CollegialBody", "trTotalNumberMembers")
    ) |> 
    mutate(
        variable = case_match(
            seriesCode,
            "trRA_CollegialBody" ~ "mgmt_collegial_body",
            "trTotalNumberMembers" ~ "mgmt_collegial_body_total"
        )
    ) |> 
    pivot_wider(
        id_cols = c(entityIso, entityName, dataYear),
        names_from = variable,
        values_from = dataValue
    ) |> 
    select(
        country_iso = entityIso,
        country_name = entityName,
        year = dataYear,
        starts_with("mgmt")
    ) |> 
    # fix collegial body totals when there is no collegial body
    mutate(
        mgmt_collegial_body_total = if_else(
            mgmt_collegial_body == "No",
            NA_character_, mgmt_collegial_body_total
        )
    )

itu_dropped_call <- itu_dropped_call_raw |> 
    select(
        country_iso = entityIso,
        country_name = entityName,
        year = dataYear,
        dropped_call_rate = dataValue
    )

itu_investment <- itu_investment_raw |>
    select(
        country_iso = entityIso,
        country_name = entityName,
        year = dataYear,
        annual_investment_telecoms = dataValue
    )

itu_broadband <- itu_broadband_raw |> 
    select(
        country_iso = entityIso,
        country_name = entityName,
        year = dataYear,
        broadband_traffic = dataValue
    )

itu_mobile_subscription <- itu_mobile_subscription_raw |> 
    select(
        country_iso = entityIso,
        country_name = entityName,
        year = dataYear,
        mobile_subscription = dataValue
    )

itu_revenue <- itu_revenue_raw |> 
    select(
        country_iso = entityIso,
        country_name = entityName,
        year = dataYear,
        telecoms_revenue = dataValue
    )

itu_coverage <- itu_coverage_raw |> 
  filter(
    seriesName == "At least 3G"
  ) |> 
  select(
    country_iso = entityIso,
    country_name = entityName,
    year = dataYear,
    share_pop_coverage_3g = dataValue
  )

itu_decision_making <- itu_decision_making_raw |> 
  filter(
    seriesCode %in% c("trRAAuton", "trSectorMinistryOverruleRA", "trRModifyStructureWithoutApproval")
  ) |> 
  mutate(
    variable = case_match(
      seriesCode,
      "trRAAuton" ~ "independence_reg_autonomy",
      "trSectorMinistryOverruleRA" ~ "independence_overrule",
      "trRModifyStructureWithoutApproval" ~ "independence_reorg"
    )
  ) |> 
  pivot_wider(
    id_cols = c(entityIso, entityName, dataYear),
    names_from = variable,
    values_from = dataValue
  ) |> 
  select(
    country_iso = entityIso,
    country_name = entityName,
    year = dataYear,
    starts_with("independence")
  )

itu_complete <- list(
        itu_appointment,
        itu_workforce,
        itu_institutional_structure,
        itu_dispute,
        itu_dropped_call,
        itu_investment,
        itu_broadband,
        itu_revenue,
        itu_mobile_subscription,
        itu_coverage,
        itu_decision_making
    ) |> 
    reduce(left_join) |> 
    left_join(
        gdp_pc,
        by = c("country_iso", "year")
    ) |> 
    mutate(
        region = countrycode(country_iso, origin = "iso3c", destination = "region")
    )

bready <- bready_raw |> 
    select(
        economy,
        economy_code,
        starts_with("pillar"),
        category_2_2_overall,
        topic
    )

gsr <- gsr_raw |> 
  mutate(
    countryname = str_remove_all(countryname, "[0-9*%]+"),
    across(
      c(energy_average:water_scope),
      as.numeric
    )
  ) |> 
  rename(
    rail_account = rain_account
  )

gsr_original <- gsr_original_raw |> 
  rename_with(
    \(col) str_replace(col, "\\*", "")
  ) |> 
  clean_names() |> 
  filter(
    question_text_2023 %in% c(
      "Does the regulator assess and report on the achievement of its strategic objectives?",
      "Does the regulator provide feedback on comments received by stakeholders?",
      "Is there a legislative requirement for the regulator to answer requests from or attend hearings organized by parliamentary/congressional committees?",
      "Are the following legislative requirements in place to enhance the transparency of the regulator's activities (with confidential and commercially sensitive information appropriately removed if needed)? - Public consultation on relevant activities"
    ) |
      # only maintain questions asking about financial independence
      str_detect(question_code, "b\\.a\\.2[1-7]")
  ) |> 
  pivot_longer(
    cols = starts_with("reply"),
    names_to = c("country_code", "year"),
    names_pattern = "reply_(.*)_(\\d+)",
    values_to = "response"
  ) |> 
  mutate(
    country_code = str_to_upper(country_code),
    year = as.numeric(year),
    response = str_trim(response, side = "both"),
    response_score = case_when(
      # Question 1: "Is the source of the financial budget of the regulator stated in the establishing legislation?"
      question_text_2023 == "Is the source of the financial budget stated in the establishing legislation?" &
        response == "yes" ~ 6,
      question_text_2023 == "Is the source of the financial budget stated in the establishing legislation?" &
        response == "no" ~ 0,
      
      # Question 2 is not scored
      
      # Question 3: "What is the length of budget appropriations?"
      question_text_2023 == "What is the length of budget appropriations?" &
        response == "at least three years" ~ 6,
      question_text_2023 == "What is the length of budget appropriations?" &
        response == "two years" ~ 3,
      question_text_2023 == "What is the length of budget appropriations?" &
        response == "annual" ~ 0,
      
      # Question 4: "If the regulator is financed in total or in part through fees paid by the regulated sector, who sets the level of the fees?"
      question_text_2023 == "If the regulator is financed in total or in part through fees paid by the regulated sector, who sets the level of the fees?" &
        response == "regulator within criteria set in legislation" ~ 6,
      question_text_2023 == "If the regulator is financed in total or in part through fees paid by the regulated sector, who sets the level of the fees?" &
        response == "parliament/congress/committee upon proposal of the regulator" ~ 3,
      question_text_2023 == "If the regulator is financed in total or in part through fees paid by the regulated sector, who sets the level of the fees?" &
        response == "governmental/ministerial body upon proposal of the regulator" ~ 2,
      question_text_2023 == "If the regulator is financed in total or in part through fees paid by the regulated sector, who sets the level of the fees?" &
        response == "governmental/ministerial body" ~ 0,
      question_text_2023 == "If the regulator is financed in total or in part through fees paid by the regulated sector, who sets the level of the fees?" &
        response == "n/a" ~ 6,
      
      # Question 5: "Who is responsible for proposing and discussing the regulator's budget?"
      question_text_2023 == "Who is responsible for proposing and discussing the regulator’s budget?" &
        response == "the regulator with no or limited interventions from other governmental/ministerial bodies" ~ 6,
      question_text_2023 == "Who is responsible for proposing and discussing the regulator’s budget?" &
        response == "the regulator and another governmental/ministerial body" ~ 3,
      question_text_2023 == "Who is responsible for proposing and discussing the regulator’s budget?" &
        response == "a governmental body other than the regulator" ~ 0,
      question_text_2023 == "Who is responsible for proposing and discussing the regulator’s budget?" &
        response == "n/a" ~ 6,
      
      # Question 6: "Does the regulator provide information to the legislature or the relevant budget authority on the costs and resources needed to fulfill its mandate prior to the next budget cycle?"
      str_detect(question_text_2023, "Does the regulator provide information to the legislature or the relevant budget authority on the costs and resources needed to fulfil? its mandate prior to the next budget cycle?") &
        response == "yes" ~ 6,
      str_detect(question_text_2023, "Does the regulator provide information to the legislature or the relevant budget authority on the costs and resources needed to fulfil? its mandate prior to the next budget cycle?") &        
        response == "no" ~ 0,
      
      # Question 7: "Which body is responsible for deciding the regulator's allocation of expenditures?"
      question_text_2023 == "Which body is responsible for deciding the regulator’s allocation of expenditures?" &
        response == "regulator within general financial management rules" ~ 6,
      question_text_2023 == "Which body is responsible for deciding the regulator’s allocation of expenditures?" &
        response == "governmental/ministerial body" ~ 0,
      
      # management practices
      question_text_2023 == "Does the regulator assess and report on the achievement of its strategic objectives?" & response == "yes, information reported to government ministry/parliament (accountable body)" ~ 4,
      question_text_2023 == "Does the regulator assess and report on the achievement of its strategic objectives?" & response == "yes, information published on website" ~ 3,
      question_text_2023 == "Does the regulator assess and report on the achievement of its strategic objectives?" & response == "yes, internally/for internal use" ~ 2,
      question_text_2023 == "Does the regulator assess and report on the achievement of its strategic objectives?" & response == "strategic objectives defined but not measured/reported on" ~ 1,
      question_text_2023 == "Does the regulator assess and report on the achievement of its strategic objectives?" & response == "no strategic objectives defined" ~ 0,
      TRUE ~ NA_real_
    )
  ) |> 
  select(
    country_code,
    sector,
    year,
    question_code,
    question_text_2023,
    response,
    response_score
  )

bank_regulator <- bank_regulator_raw |> 
  transmute(
    countryname = country,
    personnel_certified = Q12_42_2016,
    personnel_over_10_years = Q12_48_2016,
    personnel_tenure = Q12_49_2016,
    personnel_workforce = Q12_39_2016,
    personnel_specialized = as.numeric(Q12_40_2016),
    personnel_share_specialized = personnel_specialized/personnel_workforce
  ) |>
  mutate(
    year = 2019
  ) |> 
  mutate(
    countryname = case_when(
      countryname == "Vietnam" ~ "Viet Nam",
      countryname == "Czech Republic" ~ "Czechia",
      countryname == "Macedonia, FYR" ~ "North Macedonia",
      countryname == "São Tomé and Principle" ~ "São Tomé and Príncipe",
      countryname == "Turkey" ~ "Türkiye",
      countryname == "Côte d'Ivoire" ~ "Côte d’Ivoire",
      T ~ countryname
    )
  )

countryclass <- countryclass_raw |>
  select(
    economy = Economy,
    country_code = Code,
    region = Region,
    income_group = `Income.group`
  ) |>
  # fix country name
  mutate(
    economy = case_when(
      economy == "Vietnam" ~ "Viet Nam",
      TRUE ~ economy
    )
  )

# ==============================================================================
# 4. write-out
# ==============================================================================
itu_complete |> 
    write_csv(
        here("chapter 4", "data", "output", "itu.csv")
    )

bready |> 
    write_csv(
        here("chapter 4", "data", "output", "bready.csv")
    )

gsr |> 
  write_csv(
    here("chapter 4", "data", "output", "oecd_gsr.csv")
  )

gsr_original |> 
  write_csv(
    here("chapter 4", "data", "output", "oecd_gsr_original.csv")
  )

bank_regulator |> 
  write_csv(
    here("chapter 4", "data", "output", "wb_bank_regulator.csv")
  )

countryclass |> 
  write_csv(
    here("chapter 4", "data", "output", "countryclass.csv")
  )
