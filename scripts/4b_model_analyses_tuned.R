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
library(knitr)

# handle common conflicts
tidymodels_prefer()

# load fits ----
load(here("data/results/fits_cv/tuned/ridge_tuned.rda"))
load(here("data/results/fits_cv/tuned/lasso_tuned.rda"))
load(here("data/results/fits_cv/tuned/en_tuned.rda"))

# best tuning params ----
## ridge ----
ridge_tuned |> 
  select_best(metric = "rmse")

## lasso ----
lasso_tuned |> 
  select_best(metric = "rmse")

## en ----
en_tuned |> 
  select_best(metric = "rmse")

collect_metrics(en_tuned) |> 
  print(n = 50)

## knn ----
