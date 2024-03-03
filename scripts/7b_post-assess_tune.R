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

# cross-validation & cv predictions ----
## hr_score ----
registerDoMC(cores = 8)

### cross-validate
hrsc_cv <- ts_preproc |> 
  stretch_tsibble(.init = 1) |>
  model(
    ETS(hr_score),
    ARIMA(hr_score)
  )

### make cv predictions ----
hrsc_cv_preds <- hrsc_cv |> 
  forecast(h = 1)

#### save results ----
hrsc_cv |> 
  save(
    file = here("data/results/post-assess/cv/hrsc_cv.rda")
  )

hrsc_cv_preds |> 
  save(
    file = here("data/results/post-assess/cv/hrsc_cv_preds.rda")
  )

## log10_e_pop ----
registerDoMC(cores = 8)

### cross-validate ----
log_pop_cv <- ts_preproc |> 
  stretch_tsibble(.init = 1) |>
  model(
    ETS(log10_e_pop),
    ARIMA(log10_e_pop)
    )

### make cv predictions ----
log_pop_cv_preds <- log_pop_cv |> 
  forecast(h = 1)

### save results ----
log_pop_cv |> 
  save(
    file = here("data/results/post-assess/cv/log_pop_cv.rda")
    )

log_pop_cv_preds |> 
  save(
    file = here("data/results/post-assess/cv/log_pop_cv_preds.rda")
  )

## log10_e_gdp ----
registerDoMC(cores = 8)

### cross-validate ----
log_gdp_cv <- ts_preproc |> 
  stretch_tsibble(.init = 1) |>
  model(
    ETS(log10_e_gdp),
    ARIMA(log10_e_gdp)
  )

### make cv predictions ----
log_gdp_cv_preds <- log_gdp_cv |> 
  forecast(h = 1)

### save results ----
log_gdp_cv |> 
  save(
    file = here("data/results/post-assess/cv/log_gdp_cv.rda")
    )

log_gdp_cv_preds |> 
  save(
    file = here("data/results/post-assess/cv/log_gdp_cv_preds.rda")
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

ets_better <- gdp_cv_pred_rmses |> 
  filter(diff > 0)

arima_better <- gdp_cv_pred_rmses |> 
  filter(diff < 0 | is.na(diff))

empty_set_check <- gdp_cv_pred_rmses |> 
  filter(diff == 0)


