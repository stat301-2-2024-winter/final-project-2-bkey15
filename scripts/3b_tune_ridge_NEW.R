# Final Project ----
# ridge tuning
# seed set for fit, below

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
ridge_spec <- 
  linear_reg(
    penalty = tune(),
    mixture = 0
  ) |> 
  set_engine("glmnet") |> 
  set_mode("regression")

# define workflows ----
ridge_wfl <- workflow() |> 
  add_model(ridge_spec) |> 
  add_recipe(basic_rec_5)

# hyperparameter tuning values ----
ridge_params <- parameters(ridge_spec)

## create grid ----
ridge_grid <- ridge_params |> 
  grid_regular(levels = 5)

# fit workflows/models ----
## register cores ----
registerDoMC(cores = 8)

## set seed ----
set.seed(1226)

## fit models ----
ridge_tuned <- ridge_wfl |> 
  tune_grid(
    train_folds, 
    grid = ridge_grid, 
    control = control_grid(
      save_workflow = TRUE,
      save_pred = TRUE
      )
    )

## save fits ----
ridge_tuned |> 
  save(
    file = here("data/results/fits_cv/tuned/ridge_tuned.rda")
    )
