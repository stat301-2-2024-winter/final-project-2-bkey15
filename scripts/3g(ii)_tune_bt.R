# Final Project ----
# bt tuning fe
# seed set for tuning below

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
load(here("data/recipes/rec_fe_tree.rda"))

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
  add_recipe(rec_fe_tree)

# hyperparameter tuning values ----
## find mtry val as per rule of thumb ----
sqrt(226)

## update ----
bt_params <- parameters(bt_spec) |>  
  update(
    mtry = mtry(
      c(1, 15)
      ),
    learn_rate = learn_rate(
      range = c(-3, -0.2)
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
bt_tuned_2 <- bt_wfl |> 
  tune_grid(
    train_folds, 
    grid = bt_grid, 
    control = control_grid(
      save_workflow = TRUE,
      save_pred = TRUE
    )
  )

## save fits ----
bt_tuned_2 |> 
  save(
    file = here("data/results/fits_cv/tuned/bt_tuned_2.rda")
  )
