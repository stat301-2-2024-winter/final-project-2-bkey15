# Final Project ----
# Separately predict hr_scores, log10_e_pop, log10_e_gdp, & log10_e_gdppc, 

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
## ts_preproc
load(here("data/preprocessed/post-assess/ts_preproc.rda"))

## ts_normal_stats
load(here("data/preprocessed/post-assess/ts_normal_stats.rda"))

## hr_score mods
load(here("data/results/post-assess/cv/hrsc_ets_better.rda"))
load(here("data/results/post-assess/cv/hrsc_arima_better.rda"))

## log_pop mods
load(here("data/results/post-assess/cv/log_pop_ets_better.rda"))
load(here("data/results/post-assess/cv/log_pop_arima_better.rda"))

## log_gdp mods
load(here("data/results/post-assess/cv/log_gdp_ets_better.rda"))
load(here("data/results/post-assess/cv/log_gdp_arima_better.rda"))

## log_gdppc mods
load(here("data/results/post-assess/cv/log_gdppc_ets_better.rda"))
load(here("data/results/post-assess/cv/log_gdppc_arima_better.rda"))

# make time-series predictions ----
## hr_score ----
### ets ----
### BDK: we're running a longer prediction time for Serbia/Yugoslavia (345), which for some reason ends in 2018--not 2019--for hr_scores and hence the dataset writ large.
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
  forecast(h = "4 years")

hrsc_serb_predicts <- ts_preproc |> 
  filter(cowcode == 345) |> 
  model(
    ETS(hr_score)
    ) |>
  forecast(h = "5 years")

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
  forecast(h = "4 years")

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
#### hr_score
hrsc_ts_predicts |> 
  ggplot(
    aes(x = .mean)
    ) +
  geom_density() +
  labs(
    x = "HR Score",
    y = "Density"
    ) +
  theme_solarized()

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
### ets ----
log_pop_ets_predicts <- log_pop_ets_better |> 
  select(cowcode) |> 
  inner_join(ts_preproc) |> 
  filter(cowcode != 345) |> 
  as_tsibble(
    index = "year",
    key = "cowcode"
  ) |> 
  model(
    ETS(log10_e_pop)
  ) |>
  forecast(h = "4 years")

log_pop_serb_predicts <- ts_preproc |> 
  filter(cowcode == 345) |> 
  model(
    ETS(log10_e_pop)
  ) |>
  forecast(h = "5 years")

log_pop_ets_predicts <- bind_rows(
  log_pop_ets_predicts,
  log_pop_serb_predicts
)

### arima ----
log_pop_arima_predicts <- log_pop_arima_better |> 
  select(cowcode) |> 
  inner_join(ts_preproc) |> 
  as_tsibble(
    index = "year",
    key = "cowcode"
  ) |> 
  model(
    ARIMA(log10_e_pop)
  ) |>
  forecast(h = "4 years")

### combine ----
log_pop_ts_predicts <- bind_rows(
  log_pop_ets_predicts,
  log_pop_arima_predicts
) |> 
  filter(year > 2018)

### un-normalize & un-log predicts ----
#### append xbar & s ----
#### BDK: see notes for underlying formula
log_pop_norm_xbar <- ts_normal_stats |> 
  filter(name == "log10_e_pop") |> 
  select(mean) |> 
  pull(mean) |> 
  rep(697) |> 
  as_tibble() |> 
  rename(log_pop_norm_xbar = value)

log_pop_norm_s <- ts_normal_stats |> 
  filter(name == "log10_e_pop") |> 
  select(sd) |> 
  pull(sd) |> 
  rep(697) |> 
  as_tibble() |> 
  rename(log_pop_norm_s = value)

log_pop_ts_predicts <- log_pop_ts_predicts |> 
  bind_cols(
    log_pop_norm_xbar,
    log_pop_norm_s
    )

#### un-normalize ----
log_pop_ts_predicts <- log_pop_ts_predicts |> 
  mutate(
    log10_e_pop_og_scale = .mean*log_pop_norm_s + log_pop_norm_xbar
    )

log_pop_ts_predicts |> 
  ggplot(aes(x = log10_e_pop_og_scale)) +
  geom_density()

#### un-log ----
log_pop_ts_predicts <- log_pop_ts_predicts |> 
  mutate(
    e_pop_og_scale = 10^(log10_e_pop_og_scale)
    )

#### save
log_pop_ts_predicts |> 
  save(
    file = here("data/results/post-assess/predictions/log_pop_ts_predicts.rda")
  )

### viz. ----
#### log_pop_ts_predicts
log_pop_ts_predicts |> 
  ggplot(
    aes(x = .mean)
  ) +
  geom_density() +
  labs(
    x = "Log Population (Normalized)",
    y = "Density"
  ) +
  theme_solarized()

## log10_e_gdp ----
### ets ----
log_gdp_ets_predicts <- log_gdp_ets_better |> 
  select(cowcode) |> 
  inner_join(ts_preproc) |> 
  filter(cowcode != 345) |> 
  as_tsibble(
    index = "year",
    key = "cowcode"
  ) |> 
  model(
    ETS(log10_e_gdp)
  ) |>
  forecast(h = "4 years")

log_gdp_serb_predicts <- ts_preproc |> 
  filter(cowcode == 345) |> 
  model(
    ETS(log10_e_gdp)
  ) |>
  forecast(h = "5 years")

log_gdp_ets_predicts <- bind_rows(
  log_gdp_ets_predicts,
  log_gdp_serb_predicts
)

### arima ----
log_gdp_arima_predicts <- log_gdp_arima_better |> 
  select(cowcode) |> 
  inner_join(ts_preproc) |> 
  as_tsibble(
    index = "year",
    key = "cowcode"
  ) |> 
  model(
    ARIMA(log10_e_gdp)
  ) |>
  forecast(h = "4 years")

### combine ----
log_gdp_ts_predicts <- bind_rows(
  log_gdp_ets_predicts,
  log_gdp_arima_predicts
) |> 
  filter(year > 2018)

### un-normalize & un-log predicts ----
#### append xbar & s ----
log_gdp_norm_xbar <- ts_normal_stats |> 
  filter(name == "log10_e_gdp") |> 
  select(mean) |> 
  pull(mean) |> 
  rep(697) |> 
  as_tibble() |> 
  rename(log_gdp_norm_xbar = value)

log_gdp_norm_s <- ts_normal_stats |> 
  filter(name == "log10_e_gdp") |> 
  select(sd) |> 
  pull(sd) |> 
  rep(697) |> 
  as_tibble() |> 
  rename(log_gdp_norm_s = value)

log_gdp_ts_predicts <- log_gdp_ts_predicts |> 
  bind_cols(
    log_gdp_norm_xbar,
    log_gdp_norm_s
    )

#### un-normalize ----
log_gdp_ts_predicts <- log_gdp_ts_predicts |> 
  mutate(
    log10_e_gdp_og_scale = .mean*log_gdp_norm_s + log_gdp_norm_xbar
    )

log_gdp_ts_predicts |> 
  ggplot(aes(x = log10_e_gdp_og_scale)) +
  geom_density()

#### un-log ----
log_gdp_ts_predicts <- log_gdp_ts_predicts |> 
  mutate(
    e_gdp_og_scale = 10^(log10_e_gdp_og_scale)
    )

#### save
log_gdp_ts_predicts |> 
  save(
    file = here("data/results/post-assess/predictions/log_gdp_ts_predicts.rda")
  )

### viz. ----
#### log_gdp_ts_predicts
log_gdp_ts_predicts |> 
  ggplot(
    aes(x = .mean)
  ) +
  geom_density() +
  labs(
    x = "Log GDP (Normalized)",
    y = "Density"
  ) +
  theme_solarized()

## log10_e_gdppc ----
### ets ----
log_gdppc_ets_predicts <- log_gdppc_ets_better |> 
  select(cowcode) |> 
  inner_join(ts_preproc) |> 
  filter(cowcode != 345) |> 
  as_tsibble(
    index = "year",
    key = "cowcode"
  ) |> 
  model(
    ETS(log10_e_gdppc)
  ) |>
  forecast(h = "4 years")

log_gdppc_serb_predicts <- ts_preproc |> 
  filter(cowcode == 345) |> 
  model(
    ETS(log10_e_gdppc)
  ) |>
  forecast(h = "5 years")

log_gdppc_ets_predicts <- bind_rows(
  log_gdppc_ets_predicts,
  log_gdppc_serb_predicts
)

### arima ----
log_gdppc_arima_predicts <- log_gdppc_arima_better |> 
  select(cowcode) |> 
  inner_join(ts_preproc) |> 
  as_tsibble(
    index = "year",
    key = "cowcode"
  ) |> 
  model(
    ARIMA(log10_e_gdppc)
  ) |>
  forecast(h = "4 years")

### combine ----
log_gdppc_ts_predicts <- bind_rows(
  log_gdppc_ets_predicts,
  log_gdppc_arima_predicts
) |> 
  filter(year > 2018)

### un-normalize & un-log predicts ----
#### append xbar & s ----
log_gdppc_norm_xbar <- ts_normal_stats |> 
  filter(name == "log10_e_gdppc") |> 
  select(mean) |> 
  pull(mean) |> 
  rep(697) |> 
  as_tibble() |> 
  rename(log_gdppc_norm_xbar = value)

log_gdppc_norm_s <- ts_normal_stats |> 
  filter(name == "log10_e_gdppc") |> 
  select(sd) |> 
  pull(sd) |> 
  rep(697) |> 
  as_tibble() |> 
  rename(log_gdppc_norm_s = value)

log_gdppc_ts_predicts <- log_gdppc_ts_predicts |> 
  bind_cols(
    log_gdppc_norm_xbar,
    log_gdppc_norm_s
    )

#### un-normalize ----
log_gdppc_ts_predicts <- log_gdppc_ts_predicts |> 
  mutate(
    log10_e_gdppc_og_scale = .mean*log_gdppc_norm_s + log_gdppc_norm_xbar
    )

log_gdppc_ts_predicts |> 
  ggplot(aes(x = log10_e_gdppc_og_scale)) +
  geom_density()

#### un-log ----
log_gdppc_ts_predicts <- log_gdppc_ts_predicts |> 
  mutate(
    e_gdppc_og_scale = 10^(log10_e_gdppc_og_scale)
    )

#### save
log_gdppc_ts_predicts |> 
  save(
    file = here("data/results/post-assess/predictions/log_gdppc_ts_predicts.rda")
    )

### viz. ----
#### log_gdppc_ts_predicts
log_gdppc_ts_predicts |> 
  ggplot(
    aes(x = .mean)
  ) +
  geom_density() +
  labs(
    x = "Log GDPpc (Normalized)",
    y = "Density"
  ) +
  theme_solarized()
