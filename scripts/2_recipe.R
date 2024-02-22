# Final Project ----
# Creation of recipe

# Random process in script, seed set right before it

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

# load training set
load(here("data/data_splits/merge_train.rda"))

# write baseline recipes ----
## null ----

## basic ----

## avg_kill_tort only ----

avg_kill_tort_rec <- merge_train |> 
  recipe(hr_score ~ avg_kill_tort)

# write recipe ----
main_recipe <- merge_train |> 
  recipe(hr_score ~ .) |> 
  step_mutate(
    COWcode = factor(COWcode),
    year = factor(year)
    ) |> 
  step_dummy(COWcode, year) |> 
  step_rm(1:21) |> 
  step_zv(all_predictors()) |> 
  step_normalize(all_numeric_predictors()) |>
  step_impute_bag(all_predictors())
  
# check recipe ----
rec_check <- main_recipe |> 
  prep() |> 
  bake(new_data = NULL)


merge_data |> 
  specify(hr_score ~ v2x_polyarchy)
