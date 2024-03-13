# Final Project ----
# Predict hr_score with estimates of log10_e_pop, log10_e_gdp, & log10_e_gdppc; compare w/ pure time-series predictions of hr_score

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

## preproc_final
load(here("data/preprocessed/post-assess/preproc_final.rda"))

preproc_final <- preproc_final |> 
  arrange(cowcode)

## predictions
load(here("data/results/post-assess/predictions/hrsc_ts_predicts.rda"))
load(here("data/results/post-assess/predictions/log_pop_ts_predicts.rda"))
load(here("data/results/post-assess/predictions/log_gdp_ts_predicts.rda"))
load(here("data/results/post-assess/predictions/log_gdppc_ts_predicts.rda"))

## final fit
load(here("data/results/final/final_fit.rda"))

# prep for final predictions ----
## merge preproc_final w/ new predictions ----
### extract predictions on original scale ----
hr_score <- hrsc_ts_predicts |> 
  rename(hr_score_est = .mean) |> 
  filter(year != 2019) |> 
  pull(hr_score_est)

e_pop <- log_pop_ts_predicts |> 
  filter(year != 2019) |> 
  pull(e_pop_og_scale)

e_gdp <- log_gdp_ts_predicts |> 
  filter(year != 2019) |> 
  pull(e_gdp_og_scale)

e_gdppc <- log_gdppc_ts_predicts |> 
  filter(year != 2019) |> 
  pull(e_gdppc_og_scale)

### append t-series predicts to preproc_final ----
preproc_final <- preproc_final |> 
  select(
    -c(
      hr_score,
      e_pop,
      e_gdp,
      e_gdppc
      )
    ) |> 
  cbind(
    hr_score,
    e_pop,
    e_gdp,
    e_gdppc
    )

## merge preproc_final w/ preproc_data ----
preproc_full <- preproc_data |> 
  full_join(preproc_final)

# predict w/ final fit on 2020+ observations ----
## BDK: no need to use preproc_full; will generate same results
final_predicts <- preproc_final |> 
  select(cowcode, year, cow_year, country_name, hr_score) |> 
  bind_cols(
    predict(
      final_fit,
      new_data = preproc_final
    )
  )

## rename pred vars for clarity ----
final_predicts <- final_predicts |> 
  rename(
    ts_preds = hr_score,
    knn_preds = .pred
    )

## save preds ----
final_predicts |> 
  save(file = here("data/results/post-assess/predictions/final_predicts.rda"))

# plot preds ----
scatt_plot_ts_knn <- final_predicts |> 
  ggplot(
    aes(
      x = ts_preds,
      y = knn_preds
    )
  ) +
  geom_abline(lty = 2) +
  geom_point(alpha = 0.5) +
  coord_obs_pred() +
  labs(
    x = "HR Score (Time-Series)",
    y = "HR Score (KNN)",
    title = "Scatterplot: Time Series vs. KNN Model Predictions",
    subtitle = "Values are for country-years from 2020-2023"
  ) +
  theme_solarized()

ggsave(
  scatt_plot_ts_knn,
  width = 2587,
  height = 1787,
  units = "px",
  file = here("plots/scatt_plot_ts_knn.png")
  )

dens_plot_ts_knn <- final_predicts |> 
  pivot_longer(
    cols = 5:6,
    names_to = "pred_type",
    values_to = "pred_val"
    ) |> 
  ggplot(
    aes(
      x = pred_val,
      color = pred_type
      )
    ) +
  geom_density(linewidth = 0.75) +
  scale_color_manual(
    labels = c(
      "KNN",
      "Time Series"
      ),
    values = c(
      "blue3",
      "orange1")
    ) +
  labs(
    x = "HR Score Predictions",
    y = "Density",
    color = "Method",
    title = "Density Plots: Time Series vs. KNN Model Predictions",
    subtitle = "Values from country-years (2020-2022)"
    ) +
  theme_solarized()

ggsave(
  dens_plot_ts_knn,
  width = 2587,
  height = 1787,
  units = "px",
  file = here("plots/dens_plot_ts_knn.png")
)

