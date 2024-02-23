# Final Project ----
# Recipe creation

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
load(here("data/splits/train.rda"))

# create baseline recipes ----
## null ----

## basic ----

## avg_kill_tort only ----
akt_rec <- train |> 
  recipe(hr_score ~ avg_kill_tort + cow_year) |> 
  update_role(cow_year, new_role = "id variable") |> 
  step_zv(all_predictors()) |> 
  step_normalize(all_predictors())

### rec check ----
akt_rec |> 
  prep() |> 
  bake(new_data = NULL)

### write rec ----
akt_rec |> 
  save(
    file = here("data/recipes/akt_rec.rda")
    )

# create recipe ----
main_recipe <- train |> 
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
