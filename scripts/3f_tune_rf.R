# Final Project ----
# seed set for tuning below

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
load(here("data/recipes/tree_rec_5.rda"))

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
  add_recipe(tree_rec_5)

# hyperparameter tuning values ----
rf_params <- parameters(rf_spec) |>  
  update(
    mtry = mtry(
      c(1, 14)
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
rf_tuned <- rf_wfl |> 
  tune_grid(
    train_folds, 
    grid = rf_grid, 
    control = control_grid(
      save_workflow = TRUE,
      save_pred = TRUE
    )
  )

## save fits ----
rf_tuned |> 
  save(
    file = here("data/results/fits_cv/tuned/rf_tuned.rda")
  )
