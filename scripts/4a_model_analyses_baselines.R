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
load(here("data/results/fits_cv/baselines/base_fits.rda"))
load(here("data/results/fits_cv/tuned/ridge_tuned.rda"))
load(here("data/results/fits_cv/tuned/lasso_tuned.rda"))

# baseline metrics ----
## compile ----
metrics_base_fits <- collect_metrics(base_fits)

## save ----
metrics_base_fits |> 
  save(
    file = here("data/results/fits_cv/baselines/metrics_base_fits.rda")
    )

## create RMSE kable ----
rmse_kbl_base_fits <- metrics_base_fits |> 
  select(wflow_id, .metric, mean, std_err) |> 
  filter(.metric == "rmse") |> 
  select(-.metric) |> 
  arrange(mean) |> 
  kable(
    col.names = c("Workflow ID", "Mean RMSE", "Std. Error")
    )

## save kable ----
rmse_kbl_base_fits |> 
  save(
    file = here("data/results/fits_cv/baselines/rmse_kbl_base_fits.rda")
      )

# best tuning params ----
## ridge ----
ridge_tuned |> 
  select_best(metric = "rmse")

## lasso ----
lasso_tuned |> 
  select_best(metric = "rmse")
collect_metrics(ridge_tuned)
## knn ----
