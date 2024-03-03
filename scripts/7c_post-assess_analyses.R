# Final Project ----
# Find best models, compute mean RMSEs, store predictions

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
load(here("data/preprocessed/preproc_data.rda"))
load(here("data/preprocessed/post-assess/ts_preproc.rda"))
load(here("data/results/post-assess/cv/hrsc_cv_preds.rda"))
load(here("data/results/post-assess/cv/log_pop_cv_preds.rda"))

# compute proportion of cow_years for each cow in the preproc dataset ----
cow_prop <- preproc_data |> 
  group_by(cowcode) |> 
  summarize(
    prop = n()/nrow(preproc_data)
  )

# hr_score ----
## store prediction metrics ----
hrsc_cv_pred_mets <- hrsc_cv_preds |> 
  fabletools::accuracy(ts_preproc)

## compute rmse diffs ----
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

## locate best models ----
hrsc_ets_better <- hrsc_cv_pred_rmses |> 
  filter(diff > 0)
hrsc_arima_better <- hrsc_cv_pred_rmses |> 
  filter(diff < 0 | is.na(diff))
empty_set_check <- hrsc_cv_pred_rmses |> 
  filter(diff == 0)

## create weighted average for rmse of each best model ----
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

## sum weighted averages ----
sum(hrsc_arima_better$wt_arima_rmse) + sum(hrsc_ets_better$wt_ets_rmse)

## save results ----
hrsc_cv_pred_mets |> 
  save(
    file = here("data/results/post-assess/cv/hrsc_cv_pred_mets.rda")
  )

hrsc_ets_better |> 
  save(
    file = here("data/results/post-assess/cv/hrsc_ets_better.rda")
  )

hrsc_arima_better |> 
  save(
    file = here("data/results/post-assess/cv/hrsc_arima_better.rda")
  )

# log10_e_pop ----
## store prediction metrics ----
log_pop_cv_pred_mets <- log_pop_cv_preds |> 
  fabletools::accuracy(ts_preproc)

## compute rmse diffs ----
log_pop_cv_pred_rmses <- log_pop_cv_pred_mets |> 
  select(.model, cowcode, RMSE) |> 
  pivot_wider(
    names_from = .model,
    values_from = RMSE
  ) |> 
  rename(
    arima_rmse = "ARIMA(log10_e_pop)",
    ets_rmse = "ETS(log10_e_pop)"
  ) |> 
  mutate(diff = arima_rmse - ets_rmse)

## locate best models ----
log_pop_ets_better <- log_pop_cv_pred_rmses |> 
  filter(diff > 0)
log_pop_arima_better <- log_pop_cv_pred_rmses |> 
  filter(diff < 0 | is.na(diff))
empty_set_check <- log_pop_cv_pred_rmses |> 
  filter(diff == 0)

## create weighted average for rmse of each best model ----
log_pop_arima_better <- log_pop_arima_better |> 
  left_join(cow_prop) |> 
  mutate(
    wt_arima_rmse = arima_rmse*prop
  )
log_pop_ets_better <- log_pop_ets_better |> 
  left_join(cow_prop) |> 
  mutate(
    wt_ets_rmse = ets_rmse*prop
  )

## sum weighted averages ----
sum(log_pop_arima_better$wt_arima_rmse) + sum(log_pop_ets_better$wt_ets_rmse)

## save results ----
log_pop_cv_pred_mets |> 
  save(
    file = here("data/results/post-assess/cv/log_pop_cv_pred_mets.rda")
  )

log_pop_ets_better |> 
  save(
    file = here("data/results/post-assess/cv/log_pop_ets_better.rda")
  )

log_pop_arima_better |> 
  save(
    file = here("data/results/post-assess/cv/log_pop_arima_better.rda")
  )

# log10_e_pop ----