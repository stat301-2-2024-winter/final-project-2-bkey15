# Final Project ----
# Baseline fits

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

# load folds ----
load(here("data/splits/train_folds.rda"))

# define model specs ----
base_spec <- linear_reg() |> 
  set_mode("regression") |> 
  set_engine("lm")

# define workflow ----
akt_wflw <- workflow() |>  
  add_model(base_spec) |>  
  add_recipe(akt_rec)

# fit model ----
## register cores ----
registerDoMC(cores = 8)

## set seed ----
set.seed(2612)

## complete fit ----
akt_fit <- akt_wflw |> 
  fit_resamples(
    resamples = train_folds, 
    control = control_resamples(save_workflow = TRUE)
  )

## save fit ----
akt_fit |> 
  save(
    file = here("data/results/fits_cv/akt_fit.rda")
    )
