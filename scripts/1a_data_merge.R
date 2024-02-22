# Final Project ----
# Initial merge; also create avg_kill_tort

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(patchwork)
library(doMC)
library(vdemdata)
library(naniar)
library(DT)
library(plotly)
library(ggthemes)

# handle common conflicts
tidymodels_prefer()

# load data ----
vdem <- vdem |> 
  filter(year > 1945)

hr_scores <- read_csv("data/raw/HumanRightsProtectionScores_v4.01.csv")
cow_codes <- read_csv("data/raw/COW-country-codes.csv")

# merge data ----
## prep hr_scores for merge ----
hr_scores <- hr_scores |> 
  select(
    YEAR, COW, theta_mean
  ) |> 
  rename(
    year = YEAR,
    COWcode = COW,
    hr_score = theta_mean
  ) |> 
  filter(year > 1945)

## complete merge ----
merge_data <- full_join(vdem, hr_scores) |> 
  mutate(
    cow_year = paste(
      COWcode, year, sep = "-"
      )
    ) |> 
  relocate(
    COWcode, year, cow_year, hr_score
    )

## create avg_kill_tort ----
merge_data <- merge_data |> 
  mutate(
  avg_kill_tort = (v2clkill + v2cltort)/2
  )

## save merge ----
merge_data |> 
  save(
    file = here("data/preprocessed/merge_data.rda")
    )

## BDK: skip this for now
cow_codes <- cow_codes |> 
  select(-StateAbb) |> 
  rename(
    COWcode = CCode,
    country_name = StateNme
  )

