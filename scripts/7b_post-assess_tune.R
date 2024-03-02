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
load(here("data/preprocessed/preproc_data.rda"))

# model section ----
## hr_score ----
registerDoMC(cores = 8)

### cross-validation
hrsc_cv <- ts_preproc |> 
  stretch_tsibble(.init = 1) |>
  model(
    ETS(hr_score),
    ARIMA(hr_score)
  )

#### save
hrsc_cv |> 
  save(
    file = here("data/results/post-assess/cv/hrsc_cv.rda")
  )

### make predictions ----
hrsc_cv_preds <- hrsc_cv |> 
  forecast(h = 1)

#### save
hrsc_cv_preds |> 
  save(
    file = here("data/results/post-assess/cv/hrsc_cv_preds.rda")
  )

### store prediction metrics ----
hrsc_cv_pred_mets <- hrsc_cv_preds |> 
  fabletools::accuracy(ts_preproc)

#### save
hrsc_cv_pred_mets |> 
  save(
    file = here("data/results/post-assess/cv/hrsc_cv_pred_mets.rda")
  )

### evaluate ----
#### compute rmse diffs ----
hrsc_cv_pred_rmses <- hrsc_cv_pred_mets |> 
  select(.model, cowcode, RMSE) |> 
  pivot_wider(
    names_from = .model,
    values_from = RMSE
  ) |> 
  rename(
    arima_rmse = "ARIMA(hr_score)",
    ets_rmse = "ETS(hr_score)"
  ) |> 
  mutate(diff = arima_rmse - ets_rmse)

#### locate best models ----
hrsc_ets_better <- hrsc_cv_pred_rmses |> 
  filter(diff > 0)
hrsc_arima_better <- hrsc_cv_pred_rmses |> 
  filter(diff < 0 | is.na(diff))
empty_set_check <- hrsc_cv_pred_rmses |> 
  filter(diff == 0)

##### save
hrsc_ets_better |> 
  save(
    file = here("data/results/post-assess/cv/hrsc_ets_better.rda")
    )

hrsc_arima_better |> 
  save(
    file = here("data/results/post-assess/cv/hrsc_arima_better.rda")
    )

#### compute proportion of cow_years for each cow in the preproc dataset ----
cow_prop <- preproc_data |> 
  group_by(cowcode) |> 
  summarize(
    prop = n()/nrow(preproc_data)
    )

#### create weighted average for rmse of each best model ----
hrsc_arima_better <- hrsc_arima_better |> 
  left_join(cow_prop) |> 
  mutate(
    wt_arima_rmse = arima_rmse*prop
    )
hrsc_ets_better <- hrsc_ets_better |> 
  left_join(cow_prop) |> 
  mutate(
    wt_ets_rmse = ets_rmse*prop
    )

#### sum weighted averages ----
sum(hrsc_arima_better$wt_arima_rmse) + sum(hrsc_ets_better$wt_ets_rmse)















## gdp ----
registerDoMC(cores = 8)

gdp_cv <- ts_preproc |> 
  stretch_tsibble(.init = 8) |>
  model(
    ETS(e_gdp),
    ARIMA(e_gdp)
    )

gdp_cv_preds <- gdp_cv |> 
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

ets_better <- gdp_cv_pred_rmses |> 
  filter(diff > 0)

arima_better <- gdp_cv_pred_rmses |> 
  filter(diff < 0 | is.na(diff))

empty_set_check <- gdp_cv_pred_rmses |> 
  filter(diff == 0)


