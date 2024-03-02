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
load(here("data/results/post-assess/cv/hrsc_ets_better.rda"))
load(here("data/results/post-assess/cv/hrsc_arima_better.rda"))

# make time-series predictions ----
## hr_score ----
### ets ----
### BDK: we're running a longer prediction time for Serbia/Yugoslavia, which for some reason ends in 2018--not 2019--for hr_scores
hrsc_ets_predicts <- hrsc_ets_better |> 
  select(cowcode) |> 
  inner_join(ts_preproc) |> 
  filter(cowcode != 345) |> 
  as_tsibble(
    index = "year",
    key = "cowcode"
    ) |> 
  model(
    ETS(hr_score)
    ) |>
  forecast(h = "3 years")

hrsc_serb_predicts <- ts_preproc |> 
  filter(cowcode == 345) |> 
  model(
    ETS(hr_score)
    ) |>
  forecast(h = "4 years")

hrsc_ets_predicts <- bind_rows(
  hrsc_ets_predicts,
  hrsc_serb_predicts
  )

### arima ----
hrsc_arima_predicts <- hrsc_arima_better |> 
  select(cowcode) |> 
  inner_join(ts_preproc) |> 
  as_tsibble(
    index = "year",
    key = "cowcode"
  ) |> 
  model(
    ARIMA(hr_score)
  ) |>
  forecast(h = "3 years")

### combine ----
### BDK: filtering out cases that left the dataset decades ago
hrsc_ts_predicts <- bind_rows(
  hrsc_ets_predicts,
  hrsc_arima_predicts
  ) |> 
  filter(year > 2018)

#### save
hrsc_ts_predicts |> 
  save(
    file = here("data/results/post-assess/predictions/hrsc_ts_predicts.rda")
  )

### viz. ----
#### ---- usa (ets)
hrsc_ts_predicts |> 
  filter(cowcode == 2) |> 
  autoplot(
    ts_preproc |> filter(year > 1999)
    )

#### ---- germany (ets)
hrsc_ts_predicts |> 
  filter(cowcode == 255) |> 
  autoplot(
    ts_preproc |> filter(year > 1999)
    )

#### ---- mexico (arima)
hrsc_ts_predicts |> 
  filter(cowcode == 70) |> 
  autoplot(
    ts_preproc |> filter(year > 1999)
    )

#### ---- switzerland (arima)
hrsc_ts_predicts |> 
  filter(cowcode == 225) |> 
  autoplot(
    ts_preproc |> filter(year > 1999)
    )

## log10_e_pop ----
