# Final Project ----
# Initial data checks & data splitting

# Random process in script, seed set right before it

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(patchwork)
library(vdemdata)
library(naniar)
library(DT)
library(plotly)
library(ggthemes)

# handle common conflicts
tidymodels_prefer()

# load data ----
vdem <- vdem

hr_scores <- read_csv("data/raw/HumanRightsProtectionScores_v4.01.csv")
cow_codes <- read_csv("data/raw/COW-country-codes.csv")

hr_scores <- hr_scores |> 
  select(
    YEAR, COW, theta_mean
  ) |> 
  rename(
    year = YEAR,
    COWcode = COW,
    hr_score = theta_mean
  )

# merge data ----

full_data_original <- full_join(vdem, hr_scores)

full_data <- full_join(vdem, hr_scores) |> 
  mutate(
    cow_year = paste(
      COWcode, year, sep = "-"
    )
  ) |> 
  relocate(
    COWcode, year, cow_year, hr_score
  ) |> 
  filter(year > 1945)

cow_codes <- cow_codes |> 
  select(-StateAbb) |> 
  rename(
    COWcode = CCode,
    country_name = StateNme
  )