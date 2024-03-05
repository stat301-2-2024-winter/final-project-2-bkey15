# Final Project ----
# Creation of main recipes w/ additional feature engineering (interaction terms for basic, one-hot encoding for tree)

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
## main rec ----
rec_imp_5 <- train |> 
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
  step_interact(terms = ~ log10_e_gdppc:v2x_egal) |> 
  step_interact(terms = ~ log10_e_gdppc:starts_with("v2xeg")) |> 
  step_zv(all_predictors()) |> 
  step_normalize(all_numeric_predictors())

## tree (one_hot = TRUE) ----
rec_tree_imp_5 <- train |> 
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
  step_dummy(
    all_nominal_predictors(),
    one_hot = TRUE
    ) |> 
  step_impute_knn(
    all_predictors()
  ) |> 
  step_rm(
    starts_with("year")
  ) |> 
  step_zv(all_predictors()) |> 
  step_normalize(all_numeric_predictors())

# rec check ----
## set seed ----
set.seed(2612)

## main ----
rec_check <- rec_imp_5 |> 
  prep() |> 
  bake(new_data = NULL)

## tree ----
rec_check <- rec_tree_imp_5 |> 
  prep() |> 
  bake(new_data = NULL)

# write recs ----
rec_imp_5 |> 
  save(
    file = here("data/recipes/rec_imp_5.rda")
  )

rec_tree_imp_5 |> 
  save(
    file = here("data/recipes/rec_tree_imp_5.rda")
  )
