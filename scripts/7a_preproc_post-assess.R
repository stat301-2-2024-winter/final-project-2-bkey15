# Final Project ----
# Make predictions on missing data

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
## vdem ----
vdem <- vdem |> 
  filter(year > 1945)

## hr_scores ----
hr_scores <- read_csv("data/raw/HumanRightsProtectionScores_v4.01.csv")

## pts ----
load(here("data/raw/PTS-2023.RData"))

## cow_codes ----
cow_codes <- read_csv("data/raw/COW-country-codes.csv")

# merge data ----
## prep merge
### vdem ----
vdem <- vdem |> 
  mutate(
    COWcode = if_else(COWcode == 315, 316, COWcode)
  )

### hr_scores ----
hr_scores <- hr_scores |> 
  select(YEAR, COW, theta_mean) |>
  rename(
    year = YEAR,
    COWcode = COW,
    hr_score = theta_mean
    )

### pts ----
### BDK: difference with original is removing filter(year < 2020) argument
pts <- PTS_2023 |> 
  select(Year, COW_Code_N, PTS_A, PTS_H, PTS_S) |> 
  rename(
    year = Year,
    cowcode = COW_Code_N
    ) |> 
  mutate(
    cow_year = paste(
      cowcode, year, sep = "-"
      )
    ) |> 
  filter(
    !duplicated(cow_year)
    )

### cow_codes ----
cow_codes <- cow_codes |> 
  select(-StateAbb) |> 
  rename(
    cowcode = CCode,
    country_name_cow = StateNme
  ) |> 
  filter(
    !duplicated(cowcode)
  )

## complete merge ----
### vdem & hr_scores ----
### BDK: note - difference with original is left_join instead of right_join, and removal of duplicates. Note that left_join eliminates the small states, because they appear in hr_scores but not in vdem
preproc_data <- left_join(vdem, hr_scores) |> 
  rename(
    cowcode = COWcode
  ) |> 
  mutate(
    cow_year = paste(
      cowcode, year, sep = "-"
    )
  ) |> 
  relocate(
    cowcode, year, cow_year, hr_score
  ) |> 
  filter(
    !is.na(cowcode)
  )

### merge preproc & pts ----
preproc_data <- left_join(preproc_data, pts)

### merge preproc & cow_codes ----
preproc_data <- left_join(preproc_data, cow_codes) |> 
  relocate(
    country_name_cow,
    .after = country_name
    )

## create avg_kill_tort ----
preproc_data <- preproc_data |> 
  mutate(
  avg_kill_tort = (v2clkill + v2cltort)/2
  ) |> 
  relocate(
    avg_kill_tort,
    .after = hr_score
    )

## select needed vars only ----
preproc_pa_init <- preproc_data |> 
  select(
    cowcode,
    year,
    cow_year,
    country_name_cow,
    hr_score,
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

duplicate_check <- preproc_pa_init |> 
  filter(
    duplicated(cow_year)
    )

preproc_pa_init |> 
  save(
  file = here("data/preprocessed/post-assess/preproc_pa_init.rda")
  )

# initial missingness check ----
gg_miss_var(preproc_data_pa)

# BDK: we have significant missingness in the economic statistics (specific to more recent years), so let's try to impute these with time series forecasting

# time series imputation (e_pop, e_gdp, e_gdppc) ----
## create tsibble object ----
ts_preproc <- preproc_pa_init |>
  select(
    cowcode,
    year,
    country_name,
    hr_score,
    e_pop,
    e_gdp,
    e_gdppc
    ) |> 
  filter(year < 2020) |> 
  as_tsibble(
    index = year,
    key = cowcode
    )

### save
ts_preproc |> 
  save(
    file = here("data/preprocessed/post-assess/ts_preproc.rda")
    )


registerDoMC(cores = 8)

e_pop_cv <- ts_dat |>
  stretch_tsibble(.init = 8) |>
  model(
    ETS(e_pop),
    ARIMA(e_pop)
  )

e_pop_cv_forecasts <- e_pop_cv |> 
  forecast(h = 1)

e_pop_cv_forecasts_perform_mets <- e_pop_cv_forecasts |> 
  fabletools::accuracy(ts_dat)

# final missingness check ----
preproc_miss_plot <- preproc_data |> 
  select(
    -starts_with("PTS")
    ) |> 
  gg_miss_var() +
  labs(
    title = "Plot: Missing Observations by Predictor in the Preprocessed Dataset",
    caption = "Source: V-Dem (2023)"
    ) +
  theme_solarized()

## save ----
ggsave(
  preproc_miss_plot,
  width = 2587,
  height = 1787,
  units = "px",
  file = here("plots/preproc_miss_plot.png")
)

# save preproc_data ----
preproc_data |> 
  save(
    file = here("data/preprocessed/preproc_data.rda")
    )
