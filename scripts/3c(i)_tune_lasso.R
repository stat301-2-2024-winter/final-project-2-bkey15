# Final Project ----
# lasso tuning fe1

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
load(here("data/recipes/rec_fe_main1.rda"))

# load folds ----
load(here("data/splits/train_folds.rda"))

# model specifications ----
lasso_spec <- 
  linear_reg(
    penalty = tune(),
    mixture = 1
  ) |> 
  set_engine("glmnet") |> 
  set_mode("regression")

# define workflows ----
lasso_wfl <- workflow() |> 
  add_model(lasso_spec) |> 
  add_recipe(rec_fe_main1)

# hyperparameter tuning values ----
lasso_params <- parameters(lasso_spec)

## create grid ----
lasso_grid <- lasso_params |> 
  grid_regular(levels = 10)

# fit workflows/models ----
## register cores ----
registerDoMC(cores = 8)

## set seed ----
set.seed(1226)

## fit models ----
lasso_tuned_1 <- lasso_wfl |> 
  tune_grid(
    train_folds, 
    grid = lasso_grid, 
    control = control_grid(
      save_workflow = TRUE,
      save_pred = TRUE
      )
    )

## save fits ----
lasso_tuned_1 |> 
  save(
    file = here("data/results/fits_cv/tuned/lasso_tuned_1.rda")
  )
