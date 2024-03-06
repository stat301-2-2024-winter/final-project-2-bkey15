# Final Project ----
# knn tuning tree fe
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
knn_spec <- nearest_neighbor(
  neighbors = tune()
  ) |> 
  set_engine("kknn") |> 
  set_mode("regression")

# define workflows ----
knn_wfl <- workflow() |> 
  add_model(knn_spec) |> 
  add_recipe(rec_fe_tree)

# hyperparameter tuning values ----
knn_params <- parameters(knn_spec)

## create grid ----
knn_grid <- knn_params |> 
  grid_regular(levels = 10)

# fit workflows/models ----
## register cores ----
registerDoMC(cores = 8)

## set seed ----
set.seed(1226)

## fit models ----
knn_tuned_2 <- knn_wfl |> 
  tune_grid(
    train_folds, 
    grid = knn_grid, 
    control = control_grid(
      save_workflow = TRUE,
      save_pred = TRUE
    )
  )

## save fits ----
knn_tuned_2 |> 
  save(
    file = here("data/results/fits_cv/tuned/knn_tuned_2.rda")
    )
