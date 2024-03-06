# Final Project ----
# elastic net tuning fe2

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
load(here("data/recipes/rec_fe_main2.rda"))

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
  add_recipe(rec_fe_main2)

# hyperparameter tuning values ----
## BDK: need to update mixture b/c there's an error on the programming side where the range floor isn't actually 0
en_params <- parameters(en_spec) |> 
  update(
    mixture = mixture(range = c(0, 1))
    )

## create grid ----
en_grid <- en_params |> 
  grid_regular(levels = 10)

# fit workflows/models ----
## register cores ----
registerDoMC(cores = 8)

## set seed ----
set.seed(1226)

## fit models ----
en_tuned_2 <- en_wfl |> 
  tune_grid(
    train_folds, 
    grid = en_grid, 
    control = control_grid(
      save_workflow = TRUE,
      save_pred = TRUE
      )
    )

## save fits ----
en_tuned_2 |> 
  save(
    file = here("data/results/fits_cv/tuned/en_tuned_2.rda")
  )
