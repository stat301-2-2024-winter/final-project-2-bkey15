# Final Project ----
# elastic net tuning

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
en_spec <- 
  linear_reg(
    penalty = tune(),
    mixture = tune()
  ) |> 
  set_engine("glmnet") |> 
  set_mode("regression")

# define workflows ----
en_wfl <- workflow() |> 
  add_model(en_spec) |> 
  add_recipe(basic_rec_5)

# hyperparameter tuning values ----
en_params <- parameters(en_spec)

## create grid ----
en_grid <- en_params |> 
  grid_regular(levels = 5)

# fit workflows/models ----
## register cores ----
registerDoMC(cores = 8)

## set seed ----
set.seed(1226)

## fit models ----
en_tuned <- en_wfl |> 
  tune_grid(
    train_folds, 
    grid = en_grid, 
    control = control_grid(
      save_workflow = TRUE,
      save_pred = TRUE
      )
    )

## save fits ----
lasso_tuned |> 
  save(
    file = here("data/results/fits_cv/tuned/lasso_tuned.rda")
  )
