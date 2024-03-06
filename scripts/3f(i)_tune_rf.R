# Final Project ----
# rf tuning kc
# seed set below

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

# load recipes ----
load(here("data/recipes/rec_kc_tree.rda"))

# load folds ----
load(here("data/splits/train_folds.rda"))

# model specifications ----
rf_spec <- rand_forest(
    trees = 1000, 
    min_n = tune(),
    mtry = tune()
  ) |> 
  set_engine("ranger") |> 
  set_mode("regression")

# define workflows ----
rf_wfl <- workflow() |> 
  add_model(rf_spec) |> 
  add_recipe(rec_kc_tree)

# hyperparameter tuning values ----
## find mtry val as per rule of thumb ----
## BDK: when we bake the tree recipe, we get 224 predictors (+ 1 outcome & 2 ids). See 2b_recipes_kc.R
sqrt(224)

## update ----
rf_params <- parameters(rf_spec) |>  
  update(
    mtry = mtry(
      c(1, 15)
      )
    )

## create grid ----
rf_grid <- rf_params |> 
  grid_regular(levels = 5)

# fit workflows/models ----
## register cores ----
registerDoMC(cores = 8)

## set seed ----
set.seed(1226)

## fit models ----
rf_tuned_1 <- rf_wfl |> 
  tune_grid(
    train_folds, 
    grid = rf_grid, 
    control = control_grid(
      save_workflow = TRUE,
      save_pred = TRUE
    )
  )

## save fits ----
rf_tuned_1 |> 
  save(
    file = here("data/results/fits_cv/tuned/rf_tuned_1.rda")
  )
