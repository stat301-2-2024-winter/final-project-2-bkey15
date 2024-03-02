# Final Project ----
# Preprocess to make predictions on missing data (hr_scores, pop, gdp, gdppc)
# Seed set for KNN imputation

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

# impute missing data (KNN, neighbors = 5)
## create preprocessing recipe ----
ts_imp_rec <- preproc_data |> 
  recipe(hr_score ~ .) |> 
  step_rm(PTS_A, PTS_H, PTS_S) |> 
  update_role(country_name, new_role = "id variable") |> 
  update_role(cow_year, new_role = "id variable") |> 
  step_mutate(
    cowcode = factor(cowcode),
    year = factor(year),
    log10_e_pop = log10(e_pop),
    log10_e_gdp = log10(e_gdp),
    log10_e_gdppc = log10(e_gdppc)
  ) |> 
  step_rm(e_pop, e_gdp, e_gdppc) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_zv(all_predictors()) |> 
  step_normalize(all_numeric_predictors()) |> 
  step_impute_knn(all_predictors()) |> 
  step_rm(
    starts_with("year"),
    starts_with("cowcode")
  )

## bake recipe
set.seed(1226)
ts_preproc <- ts_imp_rec |> 
  prep() |> 
  bake(new_data = NULL)

## create tsibble object
ts_preproc <- preproc_data |> 
  select(cowcode, year, cow_year) |> 
  left_join(ts_preproc) |> 
  relocate(
    hr_score,
    .after = country_name
    ) |> 
  as_tsibble(
    index = "year",
    key = "cowcode"
    )

## save ts_preproc
ts_preproc |> 
  save(
    file = here("data/preprocessed/post-assess/ts_preproc.rda")
    )
