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
## preproc_data
load(here("data/preprocessed/preproc_data.rda"))

## vdem
vdem <- vdem |> 
  filter(year > 2019)

## pts ----
load(here("data/raw/PTS-2023.RData"))

## cow_codes ----
cow_codes <- read_csv("data/raw/COW-country-codes.csv")

# main preprocessing ----
## create preprocessing recipe ----
## BDK: remember - scale of imputations at this step is marginal, so shouldn't have a huge effect on outcome
ts_rec <- preproc_data |> 
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
    all_nominal_predictors()
  ) |> 
  step_impute_knn(
    all_predictors()
  ) |> 
  step_rm(
    starts_with("year")
  ) |> 
  step_zv(all_predictors()) |> 
  step_normalize(all_numeric_predictors())

## bake recipe ----
set.seed(1226)
ts_preproc <- ts_rec |> 
  prep() |> 
  bake(new_data = NULL)

### store mean & sd for pop, gdp, & gdppc ----
set.seed(1226)
ts_normal_stats <- preproc_data |> 
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
    all_nominal_predictors()
    ) |> 
  step_impute_knn(
    all_predictors()
    ) |> 
  step_rm(
    starts_with("year")
    ) |> 
  step_zv(all_predictors()) |> 
  prep() |> 
  bake(new_data = NULL) |> 
  select(
    starts_with("log")
    ) |> 
  pivot_longer(
    cols = 1:3
    ) |> 
  group_by(name) |> 
  summarize(
    mean = mean(value),
    sd = sd(value)
    )

## create tsibble object ----
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

## save results ----
ts_preproc |> 
  save(
    file = here("data/preprocessed/post-assess/ts_preproc.rda")
    )

ts_normal_stats |> 
  save(
    file = here("data/preprocessed/post-assess/ts_normal_stats.rda")
    )

# preprocessing for fit of final model on predicted vals ----
## pts ----
pts <- PTS_2023 |> 
  select(Year, COW_Code_N, PTS_A, PTS_H, PTS_S) |> 
  rename(
    year = Year,
    cowcode = COW_Code_N
  ) |> 
  filter(year > 2019) |> 
  mutate(
    cow_year = paste(
      cowcode, year, sep = "-"
    )
  ) |> 
  filter(
    !duplicated(cow_year)
  )

## cow_codes ----
cow_codes <- cow_codes |> 
  select(-StateAbb) |> 
  rename(
    cowcode = CCode,
    country_name_cow = StateNme
  ) |> 
  filter(
    !duplicated(cowcode)
  )

## vdem ----
preproc_final <- vdem |> 
  rename(
    cowcode = COWcode
  ) |> 
  mutate(
    cow_year = paste(
      cowcode, year, sep = "-"
    )
  ) |> 
  relocate(
    cowcode, year, cow_year
  )

## merge preproc & pts ----
preproc_final <- left_join(preproc_final, pts)

## merge preproc & cow_codes ----
## BDK: inner_join removes cases of missing vals for cow_code
preproc_final <- inner_join(preproc_final, cow_codes) |> 
  relocate(
    country_name_cow,
    .after = country_name
  )

## create avg_kill_tort ----
preproc_final <- preproc_final |> 
  mutate(
    avg_kill_tort = (v2clkill + v2cltort)/2
  ) |> 
  relocate(
    avg_kill_tort,
    .after = country_name_cow
  )

## select needed vars only ----
preproc_final <- preproc_final |> 
  select(
    cowcode,
    year,
    cow_year,
    country_name_cow,
    avg_kill_tort,
    v2x_clphy,
    e_v2x_clphy_3C,
    e_v2x_clphy_4C,
    e_v2x_clphy_5C,
    v2clkill,
    v2cltort,
    v2xcl_rol,
    e_v2xcl_rol_3C,
    e_v2xcl_rol_4C,
    e_v2xcl_rol_5C,
    v2x_civlib,
    e_v2x_civlib_3C,
    e_v2x_civlib_4C,
    e_v2x_civlib_5C,
    v2caviol,
    v2x_polyarchy,
    v2x_libdem,
    v2x_partipdem,
    v2x_delibdem,
    v2x_egaldem,
    v2x_api,
    v2x_mpi,
    v2x_freexp_altinf,
    v2x_frassoc_thick,
    v2x_suffr,
    v2xel_frefair,
    v2x_elecoff,
    v2x_liberal,
    v2x_jucon,
    v2xlg_legcon,
    v2x_partip,
    v2x_cspart,
    v2xdd_dd,
    v2xel_locelec,
    v2xel_regelec,
    v2xdl_delib,
    v2x_egal,
    v2xeg_eqprotec,
    v2xeg_eqaccess,
    v2xeg_eqdr,
    e_pop,
    e_gdp,
    e_gdppc,
    PTS_A,
    PTS_H,
    PTS_S
  ) |> 
  rename(
    country_name = country_name_cow
  )

## initial missingness check ----
gg_miss_var(preproc_final)

## BDK: vars w/ small-scale missingness: v2caviol, v2xlg_legcon, v2xel_loglec
## v2caviol: 4 cases - 910x2, 42x1, 52x1
## v2xlg_legcon: 7 cases - 625x2, 700x2, 531x2, 404x1
## v2xel_loglec: 13 cases - 696x3, 540x3, 482x3, 115x3, 700x1

## add NAs for hr_score ----
hr_score <- rep(NA, nrow(preproc_final))

preproc_final <- preproc_final |> 
  cbind(hr_score) |> 
  relocate(hr_score, .before = avg_kill_tort)

## save result ----
preproc_final |> 
  save(
    file = here("data/preprocessed/post-assess/preproc_final.rda")
  )
