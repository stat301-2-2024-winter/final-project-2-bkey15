# Final Project ----
# Creation of feature-engineering recipes (interaction terms for basic, one-hot encoding for tree, removal of variant/input variables, etc.)

# Seed set and used for recipe check (imputation w/ knn)

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
library(corrplot)

# handle common conflicts
tidymodels_prefer()

# load training set
load(here("data/splits/train.rda"))

# create recipes ----
## main rec ----
### main1 (just interactions) ----
rec_fe_main1 <- train |> 
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
  step_impute_knn(
    all_predictors()
    ) |> 
  step_rm(
    starts_with("year")
    ) |> 
  step_interact(terms = ~ log10_e_gdppc:v2x_egal) |> 
  step_interact(terms = ~ log10_e_gdppc:starts_with("v2xeg")) |> 
  step_zv(all_predictors()) |> 
  step_normalize(all_numeric_predictors())

### main2 (interactions + removal of multicollinear/low corr vars) ----
#### check correlations ----
tmwr_cols <- colorRampPalette(c("#91CBD765", "#CA225E"))
train |> 
  mutate(
    log10_e_pop = log10(e_pop),
    log10_e_gdp = log10(e_gdp),
    log10_e_gdppc = log10(e_gdppc)
    ) |> 
  select(
    -c(
      cowcode,
      country_name,
      year,
      e_pop,
      e_gdp,
      e_gdppc,
      cow_year,
      PTS_A,
      PTS_H,
      PTS_S
      )
    ) |> 
  cor(use = "pairwise.complete.obs") |> 
  corrplot(
    col = tmwr_cols(200),
    tl.col = "black",
    method = "ellipse"
    ) +
  theme_solarized()

cor(train$hr_score, train$v2x_clphy)
cor(train$hr_score, train$avg_kill_tort)

#### recipe ----
rec_fe_main2 <- train |> 
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
    e_v2x_civlib_5C,
    v2x_suffr,
    log10_e_gdp
  ) |> 
  step_interact(terms = ~ log10_e_gdppc:v2x_egal) |> 
  step_interact(terms = ~ log10_e_gdppc:starts_with("v2xeg")) |> 
  step_zv(all_predictors()) |> 
  step_normalize(all_numeric_predictors())

## tree (removal of vars, one_hot = TRUE, no interactions) ----
rec_fe_tree <- train |> 
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
  step_dummy(
    all_nominal_predictors(),
    one_hot = TRUE
    ) |> 
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
    e_v2x_civlib_5C,
    v2x_suffr,
    log10_e_gdp
  ) |> 
  step_zv(all_predictors()) |> 
  step_normalize(all_numeric_predictors())

# rec check ----
## set seed ----
set.seed(2612)

## main ----
rec_check <- rec_fe_main1 |> 
  prep() |> 
  bake(new_data = NULL)

rec_check <- rec_fe_main2 |> 
  prep() |> 
  bake(new_data = NULL)

## tree ----
rec_check <- rec_fe_tree |> 
  prep() |> 
  bake(new_data = NULL)

# write recs ----
rec_fe_main1 |> 
  save(
    file = here("data/recipes/rec_fe_main1.rda")
    )

rec_fe_main2 |> 
  save(
    file = here("data/recipes/rec_fe_main2.rda")
    )

rec_fe_tree |> 
  save(
    file = here("data/recipes/rec_fe_tree.rda")
    )
