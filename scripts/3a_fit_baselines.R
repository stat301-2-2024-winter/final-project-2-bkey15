# Final Project ----
# Baseline fits
# seed used for wfl_set_fits, below

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
load(here("data/recipes/akt_rec.rda"))
load(here("data/recipes/rec_imp_5.rda"))
load(here("data/recipes/rec_imp_10.rda"))
load(here("data/recipes/rec_imp_20.rda"))

# load folds ----
load(here("data/splits/train_folds.rda"))

# define model specs ----
## null ----
null_spec <- null_model() |> 
  set_engine("parsnip") |>  
  set_mode("regression")

## basic ----
basic_spec <- linear_reg() |> 
  set_engine("lm") |> 
  set_mode("regression")

# define workflows ----
## wfl_set ----
wfl_set <- workflow_set(
  preproc = list(
    neighbors_5 = rec_imp_5,
    neighbors_10 = rec_imp_10,
    neighbors_20 = rec_imp_20
    ),
  models = list(
    null = null_spec,
    lm = basic_spec
  ),
  cross = TRUE
)

## avg_kill_tort ----
akt_wflw <- workflow() |>  
  add_model(basic_spec) |>  
  add_recipe(akt_rec)

# fit models ----
## register cores ----
registerDoMC(cores = 8)

## complete fit ----
#### set seed ----
set.seed(1226)

### avg_kill_tort ----
akt_fit <- akt_wflw |> 
  fit_resamples(
    resamples = train_folds, 
    control = control_resamples(save_workflow = TRUE)
  )

#### remaining fits ----
wfl_set_fits <- wfl_set |> 
  workflow_map(
    "fit_resamples",
    seed = 1226,
    resamples = train_folds,
    control = control_resamples(
      save_pred = TRUE,
      save_workflow = TRUE
    )
  )

## combine fits ----
base_fits <- as_workflow_set(akt_lm = akt_fit) |> 
  bind_rows(wfl_set_fits)

## save fits ----
base_fits |> 
  save(
    file = here("data/results/fits_cv/baselines/base_fits.rda")
    )
