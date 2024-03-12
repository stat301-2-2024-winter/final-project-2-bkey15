# Final Project ----
# Initial data quality check

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
vdem <- vdem
hr_scores <- read_csv("data/raw/HumanRightsProtectionScores_v4.01.csv")
load(here("data/preprocessed/preproc_data.rda"))

# vdem obs counts ----
vdem |> 
  filter(year > 1945 & year < 2020) |> 
  summarize(n = n())

vdem |> 
  filter(year > 2019) |> 
  summarize(n = n())

# vdem missingness ----
## overall (non-ids) ----
vdem |> 
  filter(year > 1945 & year < 2020) |> 
  select(23:4607) |> 
  pct_miss()

## main & mid-level indices ----
vdem |> 
  filter(year > 1945 & year < 2020) |> 
  select(
    v2x_polyarchy,
    v2x_libdem,
    v2x_partipdem,
    v2x_delibdem,
    v2x_egaldem
    ) |> 
  pct_miss()

vdem |> 
  filter(year > 1945 & year < 2020) |> 
  select(
    v2x_api,
    v2x_mpi,
    v2x_freexp_altinf,
    v2x_frassoc_thick,
    v2x_suffr,
    v2xel_frefair,
    v2x_elecoff,
    v2x_liberal,
    v2xcl_rol,
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
    v2xeg_eqdr
  ) |> 
  pct_miss()

# preproc missingness ----
preproc_data |> 
  select(5:49) |> 
  pct_miss()

# viz. hr_scores ----
## OG obs ----
### density plot ----
outcome_OG_dens_plot <- hr_scores |> 
  ggplot(
    aes(
      x = theta_mean
    )
  ) +
  geom_density() +
  labs(
    x = "HR Score",
    y = "Density"
  ) +
  theme_solarized()

### boxplot ----
outcome_OG_boxplot <- hr_scores |> 
  ggplot(
    aes(
      x = theta_mean
    )
  ) +
  geom_boxplot(
    fill = NA,
    alpha = 0.25
  ) +
  scale_y_discrete() +
  labs(
    x = "HR Score"
  ) +
  theme_solarized()

### combine ----
hr_score_OG_dist_plots <- outcome_OG_dens_plot / outcome_OG_boxplot + plot_annotation(
  title = "Visualizing the Distribution of Human Rights Scores",
  subtitle = "Density Plot (Top) & Boxplot (Bottom)",
  caption = "Source: HR Scores (Fariss, 2020)",
  theme = theme_solarized()
)

### save ----
ggsave(
  hr_score_OG_dist_plots,
  width = 2587,
  height = 1787,
  units = "px",
  file = here("plots/hr_score_OG_dist_plots.png")
)

## preprocesssed obs ----
### density plot ----
outcome_dens_plot <- preproc_data |> 
  ggplot(
    aes(
      x = hr_score
      )
    ) +
  geom_density() +
  labs(
    x = "HR Score",
    y = "Density"
    ) +
  theme_solarized()

### boxplot ----
outcome_boxplot <- preproc_data |> 
  ggplot(
    aes(
      x = hr_score
      )
    ) +
  geom_boxplot(
    fill = NA,
    alpha = 0.25
    ) +
  scale_y_discrete() +
  labs(
    x = "HR Score"
    ) +
  theme_solarized()

### combine ----
hr_score_dist_plots <- outcome_dens_plot / outcome_boxplot + plot_annotation(
  title = "Distribution of Human Rights Scores after Preprocessing",
    subtitle = "Density Plot (Top) & Boxplot (Bottom)",
    caption = "Source: HR Scores (Fariss, 2020)",
    theme = theme_solarized()
  )

### save ----
ggsave(
  hr_score_dist_plots,
  width = 2587,
  height = 1787,
  units = "px",
  file = here("plots/hr_score_dist_plots.png")
)

# check skew for pop, gdp, & gdppc ----
preproc_data |> 
  ggplot(
    aes(x = e_pop)
    ) + 
  geom_density()

preproc_data |> 
  ggplot(
    aes(x = e_gdp)
  ) + 
  geom_density()

preproc_data |> 
  ggplot(
    aes(x = e_gdppc)
  ) + 
  geom_density()
