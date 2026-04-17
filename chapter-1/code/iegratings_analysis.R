#####
# public institutions and  IEG ratings
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
library(reshape2)

# open
RATINGS <- read_excel(path = "data/IEG_ICRR_PPAR_Ratings_2024-01-16.xlsx")
LESSONS <- read_excel(path = "data/IEG_ICRR-PPAR_Lessons_2024-01-16.xlsx")

### how often does capacity / capability appear? -----

# create least restrictive flag
LESSONS <- LESSONS %>% mutate(cap = ifelse(grepl(Lessons, pattern = "capability|capacity"), 1, 0))

# create most restrictive flag
LESSONS <- LESSONS %>% mutate(imp = ifelse(grepl(Lessons, pattern = "implementation"), 1, 0))

### merge LESSONS and RATINGS -----

# if there are dups in RATINGS, then keep the one that has "latest"
RATINGS <- RATINGS %>% group_by(`Project ID`) %>% mutate(n = n())
RATINGS <- RATINGS %>% filter(n == 1 | (n == 2 & grepl(`Data Source`, pattern = "Latest")))

# merge LESSONS to RATINGS (dropping those obs in LESSONS that are not in RATINGS)
LESSONS <- LESSONS %>% select(-`Project Name`, -`Country`, -`Country Lending Group`,
                              -`Practice Group`, -`Global Practice`, -`Closing FY`,
                              -`Evaluation Type`, -`Project Volume`, -`IEG Outcome Ratings`,
                              -`IEG Quality at Entry Ratings`, -`IEG Quality of Supervision Ratings`,
                              -`IEG Bank Performance Ratings`, -`IEG Monitoring and Evaluation Quality Ratings`)
RATINGS <- left_join(x = RATINGS, y = LESSONS, by = c("Project ID")) %>% select(-n)

# check this - if cap is NA (i.e. the project wasn't in LESSONS), then make cap = 0
RATINGS$cap[is.na(RATINGS$cap)] <- 0
RATINGS$imp[is.na(RATINGS$imp)] <- 0

### cap descriptive plots ----

# cap over time
RATINGS %>% filter(`Practice Group` != "Other") %>% group_by(`Practice Group`, `Closing FY`) %>%
    summarise(n_proj = n(), n_cap = sum(cap), `Percent of Projects` = n_cap / n_proj) %>%
    mutate(`Closing FY` = as.character(`Closing FY`),
           `Practice Group` = case_when(
               `Practice Group` == "EFI" ~ "EFI",
               `Practice Group` == "HD" ~ "Human Development",
               `Practice Group` == "INFRA" ~ "Infrastructure",
               `Practice Group` == "SD" ~ "Sustainable Development"
           )) %>%
    ggplot(aes(x = as.character(`Closing FY`), y = `Percent of Projects`,
               group = `Practice Group`, color = `Practice Group`)) + geom_line(size = 1.2) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    labs(x = "Closing FY", y = "Percent of projects \nin closing FY",
         title = "IEG consistently cites `capacity` as a constraint or problem",
         subtitle = "Percent of projects with `capacity`, `capability`, or `effectiveness` in IEG Lessons") +
    cowplot::theme_cowplot() +
    guides(color = guide_legend(nrow = 2, byrow = T)) +
    theme(legend.position = "bottom", legend.text = element_text(size = 14))
ggsave("figs/ieg_ratings.png", height = 7, width = 9)

# IEG outcome ratings by cap
RATINGS %>% filter(!is.na(`IEG Outcome Ratings`)) %>%
    mutate(cap = ifelse(cap == 1, "Capacity in lessons", "Not in lessons")) %>%
    group_by(`Practice Group`, cap) %>%
    summarise(n_proj = n(),
              perc_highsat = sum(ifelse(`IEG Outcome Ratings` == "Highly Satisfactory", 1, 0)) / n_proj,
              perc_highunsat = sum(ifelse(`IEG Outcome Ratings` == "Highly Unsatisfactory", 1, 0)) / n_proj,
              perc_modsat = sum(ifelse(`IEG Outcome Ratings` == "Moderately Satisfactory", 1, 0)) / n_proj,
              perc_modunsat = sum(ifelse(`IEG Outcome Ratings` == "Moderately Unsatisfactory", 1, 0)) / n_proj,
              perc_sat = sum(ifelse(`IEG Outcome Ratings` == "Satisfactory", 1, 0)) / n_proj,
              perc_unsat = sum(ifelse(`IEG Outcome Ratings` == "Unsatisfactory", 1, 0)) / n_proj) %>%
              melt(id.vars = c("Practice Group", "cap", "n_proj")) %>%
              mutate(variable_order = case_when(
                  variable == "perc_highunsat" ~ 1,
                  variable == "perc_unsat" ~ 2,
                  variable == "perc_modunsat" ~ 3,
                  variable == "perc_modsat" ~ 4,
                  variable == "perc_sat" ~ 5,
                  variable == "perc_highsat" ~ 6
              )) %>%
          #    filter(`Practice Group` == "HD") %>%
              ggplot(aes(x = reorder(variable, variable_order), y = value)) +
              geom_bar(stat = "identity", position = "dodge") + facet_wrap(~cap, nrow = 2)


### imp descriptive plots ----

# imp over time
RATINGS %>% filter(`Practice Group` != "Other") %>% group_by(`Practice Group`, `Closing FY`) %>%
    summarise(n_proj = n(), n_imp = sum(imp), `Percent of Projects` = n_imp / n_proj) %>%
    mutate(`Closing FY` = as.character(`Closing FY`)) %>%
    ggplot(aes(x = as.character(`Closing FY`), y = `Percent of Projects`,
               group = `Practice Group`, color = `Practice Group`)) + geom_line() +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    labs(x = "Closing FY", y = "Percent of projects \nin closing FY", title = "`Capacity` in IEG Lessons",
         subtitle = "Percent of projects with `capacity`, `cability`, or `effectiveness` in IEG Lessons") +
    cowplot::theme_cowplot()

# IEG Bank Performance Ratings by imp
RATINGS %>% filter(!is.na(`IEG Quality at Entry Ratings`)) %>%
    mutate(imp = ifelse(imp == 1, "Imp in lessons", "Not in lessons")) %>%
    group_by(`Global Practice`, imp) %>%
    summarise(n_proj = n(),
              perc_highsat = sum(ifelse(`IEG Quality at Entry Ratings` == "Highly Satisfactory", 1, 0)) / n_proj,
              perc_highunsat = sum(ifelse(`IEG Quality at Entry Ratings` == "Highly Unsatisfactory", 1, 0)) / n_proj,
              perc_modsat = sum(ifelse(`IEG Quality at Entry Ratings` == "Moderately Satisfactory", 1, 0)) / n_proj,
              perc_modunsat = sum(ifelse(`IEG Quality at Entry Ratings` == "Moderatel Unsatisfactory", 1, 0)) / n_proj,
              perc_sat = sum(ifelse(`IEG Quality at Entry Ratings` == "Satisfactory", 1, 0)) / n_proj,
              perc_unsat = sum(ifelse(`IEG Quality at Entry Ratings` == "Unsatisfactory", 1, 0)) / n_proj) %>%
    melt(id.vars = c("Global Practice", "imp", "n_proj")) %>%
    ggplot(aes(x = `Global Practice`, y = value, fill = imp)) +
    geom_bar(stat = "identity", position = "dodge") + coord_flip()

