# Final Project ----
# Model selection/comparison & analysis

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

# load fits ----
load(here("data/results/fits_cv/akt_fit.rda"))

akt_best_rmse <- show_best(
  akt_fit,
  metric = "rmse"
) |> 
  select(mean, std_err) |> 
  mutate(model = "avg_kill_tort") |> 
  relocate(model, mean, std_err)
