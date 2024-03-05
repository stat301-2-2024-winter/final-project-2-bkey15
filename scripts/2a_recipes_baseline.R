# Final Project ----
# Baseline recipes creation

# Seed set and used for recipe check (imputation w/ knn)

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

## all else ----
### knn neighbors = 5 (default) ----
base_rec_imp_5 <- train |> 
  recipe(hr_score ~ .) |> 
  step_rm(PTS_A, PTS_H, PTS_S) |> 
  update_role(country_name, new_role = "id variable") |> 
  update_role(cow_year, new_role = "id variable") |> 
  step_mutate(
    cowcode = factor(cowcode),
    year = factor(year),
    log10_e_pop = log10(e_pop),
    log10_e_gdp = log10(e_gdp),
    log10_e_gdppc = log10(e_gdppc)
  ) |> 
  step_rm(e_pop, e_gdp, e_gdppc) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_impute_knn(
    all_predictors()
  ) |> 
  step_rm(
    starts_with("year")
  ) |> 
  step_zv(all_predictors()) |> 
  step_normalize(all_numeric_predictors())

### knn neighbors = 10 ----
base_rec_imp_10 <- train |> 
  recipe(hr_score ~ .) |> 
  step_rm(PTS_A, PTS_H, PTS_S) |> 
  update_role(country_name, new_role = "id variable") |> 
  update_role(cow_year, new_role = "id variable") |> 
  step_mutate(
    cowcode = factor(cowcode),
    year = factor(year),
    log10_e_pop = log10(e_pop),
    log10_e_gdp = log10(e_gdp),
    log10_e_gdppc = log10(e_gdppc)
  ) |> 
  step_rm(e_pop, e_gdp, e_gdppc) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_impute_knn(
    all_predictors(),
    neighbors = 10
  ) |> 
  step_rm(
    starts_with("year")
  ) |> 
  step_zv(all_predictors()) |> 
  step_normalize(all_numeric_predictors())

### knn neighbors = 20
base_rec_imp_20 <- train |> 
  recipe(hr_score ~ .) |> 
  step_rm(PTS_A, PTS_H, PTS_S) |> 
  update_role(country_name, new_role = "id variable") |> 
  update_role(cow_year, new_role = "id variable") |> 
  step_mutate(
    cowcode = factor(cowcode),
    year = factor(year),
    log10_e_pop = log10(e_pop),
    log10_e_gdp = log10(e_gdp),
    log10_e_gdppc = log10(e_gdppc)
  ) |> 
  step_rm(e_pop, e_gdp, e_gdppc) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_impute_knn(
    all_predictors(),
    neighbors = 20
  ) |> 
  step_rm(
    starts_with("year")
  ) |> 
  step_zv(all_predictors()) |> 
  step_normalize(all_numeric_predictors())

# rec check ----
## set seed ----
set.seed(2612)

## complete check ----
### 5 neighbors ----
rec_check <- base_rec_imp_5 |> 
  prep() |> 
  bake(new_data = NULL)

### 10 neighbors ----
rec_check <- base_rec_imp_10 |> 
  prep() |> 
  bake(new_data = NULL)

### 20 neighbors ----
rec_check <- base_rec_imp_20 |> 
  prep() |> 
  bake(new_data = NULL)

### write recs ----
base_rec_imp_5 |> 
  save(
    file = here("data/recipes/base_rec_imp_5.rda")
  )

base_rec_imp_10 |> 
  save(
    file = here("data/recipes/base_rec_imp_10.rda")
  )

base_rec_imp_20 |> 
  save(
    file = here("data/recipes/base_rec_imp_20.rda")
  )
