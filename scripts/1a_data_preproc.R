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

hr_scores |> 
  ggplot(aes(x = theta_mean)) +
  geom_density()

## pts ----
load(here("data/raw/PTS-2023.RData"))

## cow_codes ----
## BDK: this is more for reference
cow_codes <- read_csv("data/raw/COW-country-codes.csv")

# merge data ----
## prep merge
### vdem ----
### BDK: it's essential to recode Czechia's cow_code early on, as it's much more complicated to rectify the missingness that would arise in this instance later on in the process
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
  filter(
    !duplicated(cow_year)
    )

### cow_codes ----
### BDK: we're including and merging this b/c v-dem is missing a lot of country names
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
  filter(
    duplicated(cow_year)
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
## BDK: a quick check shows no missingness in country_name_cow; so include this rather than v-dem's country_name
preproc_data <- preproc_data |> 
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

# initial missingness check ----
gg_miss_var(preproc_data)

## BDK: we have missingness in a lot of the v-dem vars. Let's check where avg_kill_tort is missing:
dattbl_vdem_miss <- preproc_data |> 
  select(cowcode, country_name, avg_kill_tort) |> 
  summarize(
    n_miss = n_miss(avg_kill_tort),
    .by = c(cowcode, country_name)
    ) |> 
  arrange(
    desc(n_miss)
    ) |> 
  datatable(
    colnames = c(
      "COW Code", "Country Name", "N Missing"
    ),
    style = "bootstrap4"
    )

## BDK: microstates + post-Soviet/WWII states. The microstates basically have no data whatsoever, so we may need to remove them entirely. We can also remove missing rows for the latter; see notes, but these generally result from coding gaps, coding start dates, etc.

## save data table ----
dattbl_vdem_miss |> 
  save(
    file = here("data/preprocessed/dattbl_vdem_miss.rda")
    )

dattbl_vdem_miss

# remove missing rows ----
preproc_data <- preproc_data |> 
  filter(
    !is.na(avg_kill_tort)
    )

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
