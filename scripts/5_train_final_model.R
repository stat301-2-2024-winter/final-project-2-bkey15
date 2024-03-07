# Final Project ----
# Train final model
# seed set below

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

# load tuning fits ----
load(here("data/results/fits_cv/tuned/knn_tuned_2.rda"))

# load training set ----
load(here("data/splits/train.rda"))

# finalize workflow ----
final_wflw <- knn_tuned_2 |> 
  extract_workflow() |>  
  finalize_workflow(
    select_best(
      knn_tuned_2,
      metric = "rmse"
      )
    )

# train final model ----
## set seed ----
set.seed(1226)

## complete fit
final_fit <- final_wflw |> 
  fit(train)

## save final fit
final_fit |> 
  save(
    file = here("data/results/final/final_fit.rda")
  )
