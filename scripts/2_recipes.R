# Final Project ----
# Recipe creation

# Seed set and used for recipe check (imputation w/ bagged trees)

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

# create recipes ----
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

## basic rec ----
basic_rec <- train |> 
  recipe(hr_score ~ .) |> 
  step_rm(country_name, year, PTS_A, PTS_H, PTS_S) |> 
  update_role(cow_year, new_role = "id variable") |> 
  step_mutate(
    cowcode = factor(cowcode),
    log10_e_pop = log10(e_pop),
    log10_e_gdp = log10(e_gdp),
    log10_e_gdppc = log10(e_gdppc)
    ) |> 
  step_rm(e_pop, e_gdp, e_gdppc) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_zv(all_predictors()) |> 
  step_normalize(all_numeric_predictors()) |> 
  step_impute_bag(all_predictors())

### rec check ----
#### set seed ----
set.seed(2612)

#### complete check ----
rec_check <- basic_rec |> 
  prep() |> 
  bake(new_data = NULL)

### write rec ----
basic_rec |> 
  save(
    file = here("data/recipes/basic_rec.rda")
  )


