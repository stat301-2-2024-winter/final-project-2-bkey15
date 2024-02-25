# Final Project ----
# Recipe creation

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
## avg_kill_tort only ----
akt_rec <- train |> 
  recipe(hr_score ~ avg_kill_tort + cow_year) |> 
  update_role(cow_year, new_role = "id variable") |> 
  step_zv(all_predictors()) |> 
  step_normalize(all_predictors())

### rec check ----
akt_rec |> 
  prep() |> 
  bake(new_data = NULL)

### write rec ----
akt_rec |> 
  save(
    file = here("data/recipes/akt_rec.rda")
    )

## basic rec ----
### knn neighbors = 5 (default) ----
basic_rec_5_alt <- train |> 
  recipe(hr_score ~ .) |> 
  step_rm(
    v2x_clphy,
    e_v2x_clphy_3C,
    e_v2x_clphy_4C,
    e_v2x_clphy_5C,
    e_v2xcl_rol_3C,
    e_v2xcl_rol_4C,
    e_v2xcl_rol_5C,
    e_v2x_civlib_3C,
    e_v2x_civlib_4C,
    e_v2x_civlib_5C,
    PTS_A,
    PTS_H,
    PTS_S
    ) |> 
  update_role(country_name, new_role = "id variable") |> 
  update_role(year, new_role = "id variable") |> 
  update_role(cow_year, new_role = "id variable") |> 
  step_mutate(
    cowcode = factor(cowcode),
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
    )

### knn neighbors = 10 ----
basic_rec_10_alt <- train |> 
  recipe(hr_score ~ .) |> 
  step_rm(
    v2x_clphy,
    e_v2x_clphy_3C,
    e_v2x_clphy_4C,
    e_v2x_clphy_5C,
    e_v2xcl_rol_3C,
    e_v2xcl_rol_4C,
    e_v2xcl_rol_5C,
    e_v2x_civlib_3C,
    e_v2x_civlib_4C,
    e_v2x_civlib_5C,
    PTS_A,
    PTS_H,
    PTS_S
    ) |> 
  update_role(country_name, new_role = "id variable") |> 
  update_role(year, new_role = "id variable") |> 
  update_role(cow_year, new_role = "id variable") |> 
  step_mutate(
    cowcode = factor(cowcode),
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
    )

### knn neighbors = 20
basic_rec_20_alt <- train |> 
  recipe(hr_score ~ .) |> 
  step_rm(
    v2x_clphy,
    e_v2x_clphy_3C,
    e_v2x_clphy_4C,
    e_v2x_clphy_5C,
    e_v2xcl_rol_3C,
    e_v2xcl_rol_4C,
    e_v2xcl_rol_5C,
    e_v2x_civlib_3C,
    e_v2x_civlib_4C,
    e_v2x_civlib_5C,
    PTS_A,
    PTS_H,
    PTS_S
    ) |> 
  update_role(country_name, new_role = "id variable") |> 
  update_role(year, new_role = "id variable") |> 
  update_role(cow_year, new_role = "id variable") |> 
  step_mutate(
    cowcode = factor(cowcode),
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
    )

### rec check ----
#### set seed ----
set.seed(2612)

#### complete check ----
##### 5 neighbors ----
rec_check <- basic_rec_5_alt |> 
  prep() |> 
  bake(new_data = NULL)

##### 10 neighbors ----
rec_check <- basic_rec_10_alt |> 
  prep() |> 
  bake(new_data = NULL)

##### 20 neighbors ----
rec_check <- basic_rec_20_alt |> 
  prep() |> 
  bake(new_data = NULL)

### write recs ----
basic_rec_5_alt |> 
  save(
    file = here("data/recipes/basic_rec_5_alt.rda")
  )

basic_rec_10_alt |> 
  save(
    file = here("data/recipes/basic_rec_10_alt.rda")
  )

basic_rec_20_alt |> 
  save(
    file = here("data/recipes/basic_rec_20_alt.rda")
  )
