# Final Project ----
# Initial merge; also create avg_kill_tort

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

# load data ----
## vdem ----
vdem <- vdem |> 
  filter(year > 1945 & year < 2020)

## hr_scores ----
hr_scores <- read_csv("data/raw/HumanRightsProtectionScores_v4.01.csv")

## pts ----
load(here("data/raw/PTS-2023.RData"))

# merge data ----
## prep merge
### hr_scores ----
hr_scores <- hr_scores |> 
  select(YEAR, COW, theta_mean) |>
  rename(
    year = YEAR,
    COWcode = COW,
    hr_score = theta_mean
    ) |> 
  filter(year > 1945)

### pts ----
### BDK: if you check by including Country in select(), duplicates are Gaza/Palestine, Somaliland, etc; similar to cases w/o hr_scores in initial preproc_data
pts <- PTS_2023 |> 
  select(Year, COW_Code_N, PTS_A, PTS_H, PTS_S) |> 
  rename(
    year = Year,
    cowcode = COW_Code_N
    ) |> 
  filter(year < 2020) |> 
  mutate(
    cow_year = paste(
      cowcode, year, sep = "-"
      )
    ) |> 
  filter(!duplicated(cow_year))

## complete merge ----
### vdem & hr_scores ----
preproc_data <- right_join(vdem, hr_scores) |> 
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
  )

### BDK: check for duplicates - shouldn't have any (i.e., should get an empty set)
duplicate_test <- preproc_data |> 
  filter(duplicated(cow_year))

### merge preproc & pts ----
preproc_data <- left_join(preproc_data, pts)

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
preproc_data <- preproc_data |> 
  select(
    cowcode,
    year,
    cow_year,
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
    )

## save merge ----
preproc_data |> 
  save(
    file = here("data/preprocessed/preproc_data.rda")
    )

## BDK: skip this for now

## prep cow_codes for merge
cow_codes <- cow_codes |> 
  select(-StateAbb) |> 
  rename(
    cowcode = CCode,
    country_name_cow = StateNme
  )