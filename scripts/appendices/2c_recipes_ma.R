# Final Project ----
# Recipe creation, most multicollinearity averse

# Seed set and used for recipe check (imputation w/ bagged trees)

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

# handle common conflicts
tidymodels_prefer()

# load training set
load(here("data/splits/train.rda"))

# create recipes ----
## most averse ----
### knn neighbors = 5 (default) ----
rec_ma_imp_5 <- train |> 
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
  step_impute_knn(
    all_predictors()
    ) |> 
  step_rm(
    starts_with("year"),
    v2x_clphy,
    e_v2x_clphy_3C,
    e_v2x_clphy_4C,
    e_v2x_clphy_5C,
    e_v2xcl_rol_3C,
    e_v2xcl_rol_4C,
    e_v2xcl_rol_5C,
    e_v2x_civlib_3C,
    e_v2x_civlib_4C,
    e_v2x_civlib_5C
    )

### knn neighbors = 10 ----
rec_ma_imp_10 <- train |> 
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
  step_impute_knn(
    all_predictors(),
    neighbors = 10
    ) |> 
  step_rm(
    starts_with("year"),
    v2x_clphy,
    e_v2x_clphy_3C,
    e_v2x_clphy_4C,
    e_v2x_clphy_5C,
    e_v2xcl_rol_3C,
    e_v2xcl_rol_4C,
    e_v2xcl_rol_5C,
    e_v2x_civlib_3C,
    e_v2x_civlib_4C,
    e_v2x_civlib_5C
    )

### knn neighbors = 20
rec_ma_imp_20 <- train |> 
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
  step_impute_knn(
    all_predictors(),
    neighbors = 20
    ) |> 
  step_rm(
    starts_with("year"),
    v2x_clphy,
    e_v2x_clphy_3C,
    e_v2x_clphy_4C,
    e_v2x_clphy_5C,
    e_v2xcl_rol_3C,
    e_v2xcl_rol_4C,
    e_v2xcl_rol_5C,
    e_v2x_civlib_3C,
    e_v2x_civlib_4C,
    e_v2x_civlib_5C
    )

### rec check ----
#### set seed ----
set.seed(2612)

#### complete check ----
##### 5 neighbors ----
rec_check <- rec_ma_imp_5 |> 
  prep() |> 
  bake(new_data = NULL)

##### 10 neighbors ----
rec_check <- rec_ma_imp_10 |> 
  prep() |> 
  bake(new_data = NULL)

##### 20 neighbors ----
rec_check <- rec_ma_imp_20 |> 
  prep() |> 
  bake(new_data = NULL)

### write recs ----
rec_ma_imp_5 |> 
  save(
    file = here("data/recipes/appendices/rec_ma_imp_5.rda")
  )

rec_ma_imp_10 |> 
  save(
    file = here("data/recipes/appendices/rec_ma_imp_10.rda")
  )

rec_ma_imp_20 |> 
  save(
    file = here("data/recipes/appendices/rec_ma_imp_20.rda")
  )
