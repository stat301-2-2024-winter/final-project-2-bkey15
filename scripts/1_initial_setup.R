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
  )

## complete merge ----
merge_data <- full_join(vdem, hr_scores) |> 
  mutate(
    cow_year = paste(
      COWcode, year, sep = "-"
    )
  ) |> 
  relocate(
    COWcode, year, cow_year, hr_score
  ) |> 
  filter(year > 1945)

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

# viz. hr_scores
merge_data |> 
  ggplot(
    aes(
      x = hr_score
    )
  ) +
  geom_density() +
  labs(
    x = "HR Score",
    y = "Density",
    title = "Density Plot: Human Rights Scores",
    subtitle = "The distribution is relatively symmetrical"
  ) +
  theme_bw()

merge_data |> 
  ggplot(
    aes(
      x = hr_score
    )
  ) +
  geom_boxplot() +
  theme_bw()

# split data ----
## set seed ----
set.seed(2612)

## complete split ----
merge_split <- merge_data |> 
  initial_split(
    prop = 0.75,
    strata = hr_score
  )

merge_train <- training(merge_split)
merge_test <- testing(merge_split)

## fold training data ----
merge_train_folds <- merge_train |> 
  vfold_cv(
    v = 10,
    repeats = 5,
    strata = hr_score
  )

## save splits & folds ----
merge_train |> 
  save(
    file = here("data/data_splits/merge_train.rda")
    )

merge_test |> 
  save(
    file = here("data/data_splits/merge_test.rda")
    )

merge_train_folds |> 
  save(
    file = here("data/data_splits/merge_train_folds.rda")
    )
