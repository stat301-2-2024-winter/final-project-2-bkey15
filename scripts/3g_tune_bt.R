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
load(here("data/recipes/basic_rec_5.rda"))

# load folds ----
load(here("data/splits/train_folds.rda"))

# model specifications ----
bt_spec <- boost_tree(
    min_n = tune(),
    mtry = tune(),
    learn_rate = tune()
  ) |> 
  set_engine("xgboost") |> 
  set_mode("regression")

# define workflows ----
bt_wfl <- workflow() |> 
  add_model(bt_spec) |> 
  add_recipe(bt_rec_5)

# hyperparameter tuning values ----
bt_params <- parameters(bt_spec) |>  
  update(
    mtry = mtry(
      c(1, 14)
    ),
    learn_rate = learn_rate(
      range = c(-5, -0.2)
    )
  )

## create grid ----
bt_grid <- bt_params |> 
  grid_regular(levels = 5)

# fit workflows/models ----
## register cores ----
registerDoMC(cores = 8)

## set seed ----
set.seed(1226)

## fit models ----
bt_tuned <- bt_wfl |> 
  tune_grid(
    train_folds, 
    grid = bt_grid, 
    control = control_grid(
      save_workflow = TRUE,
      save_pred = TRUE
    )
  )

## save fits ----
bt_tuned |> 
  save(
    file = here("data/results/fits_cv/tuned/bt_tuned.rda")
  )
