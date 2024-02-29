# Final Project ----
# Tune ARIMA & ETS

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
library(fable)
library(tsibble)

# handle common conflicts
tidymodels_prefer()

# load data ----
load(here("data/preprocessed/post-assess/ts_preproc.rda"))

# cross validate ----
## gdp ----
gdp_cv <- ts_preproc |> 
  stretch_tsibble(.init = 8) |>
  model(
    ETS(e_gdp),
    ARIMA(e_gdp)
  )

gdp_cv_preds <- gdp_cv
  forecast(h = 1)
  
gdp_cv_pred_mets <- gdp_cv_preds |> 
  accuracy(ts_preproc)

### save ----
gdp_cv |> 
  save(
    file = here("data/results/post-assess/cv/gdp_cv.rda")
    )

gdp_cv_preds |> 
  save(
    file = here("data/results/post-assess/cv/gdp_cv_preds.rda")
    )

gdp_cv_pred_mets |> 
  save(
    file = here("data/results/post-assess/cv/gdp_cv_pred_mets.rda")
    )

### evaluate ----
load(here("data/results/post-assess/cv/gdp_cv_pred_mets.rda"))

gdp_cv_pred_rmses <- gdp_cv_pred_mets |> 
  select(.model, cowcode, RMSE) |> 
  pivot_wider(
    names_from = .model,
    values_from = RMSE
    ) |> 
  rename(
    arima_rmse = "ARIMA(e_gdp)",
    ets_rmse = "ETS(e_gdp)"
    ) |> 
  mutate(diff = arima_rmse - ets_rmse)

