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

## log10_e_gdppc ----
registerDoMC(cores = 8)

### cross-validate ----
log_gdppc_cv <- ts_preproc |> 
  stretch_tsibble(.init = 1) |>
  model(
    ETS(log10_e_gdppc),
    ARIMA(log10_e_gdppc)
  )

### make cv predictions ----
log_gdppc_cv_preds <- log_gdppc_cv |> 
  forecast(h = 1)

### save results ----
log_gdppc_cv |> 
  save(
    file = here("data/results/post-assess/cv/log_gdppc_cv.rda")
  )

log_gdppc_cv_preds |> 
  save(
    file = here("data/results/post-assess/cv/log_gdppc_cv_preds.rda")
  )
