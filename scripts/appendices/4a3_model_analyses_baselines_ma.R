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
load(here("data/results/fits_cv/baselines/appendices/base_fits_ma.rda"))

# baseline metrics ----
## compile ----
metrics_base_fits_ma <- collect_metrics(base_fits_ma)

## create kable ----
rmse_kbl_base_fits_ma <- metrics_base_fits_ma |> 
  select(wflow_id, .metric, mean, std_err) |> 
  filter(.metric == "rmse") |> 
  select(-.metric) |> 
  arrange(mean) |> 
  kable(
    col.names = c("Workflow ID", "Mean RMSE", "Std. Error")
    )

## save kable ----
rmse_kbl_base_fits_ma |> 
  save(
    file = here("data/results/fits_cv/baselines/appendices/rmse_kbl_base_fits_ma.rda")
      )
