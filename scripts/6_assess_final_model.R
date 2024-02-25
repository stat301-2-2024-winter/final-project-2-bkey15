# Final Project ----
# Assess final model

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
library(knitr)

# handle common conflicts
tidymodels_prefer()

# load testing set ----
load(here("data/splits/test.rda"))

# load final fit ----
load(here("data/results/final/final_fit.rda"))

# predict w/ final fit on testing ----
final_predicts <- test |> 
  select(cowcode, year, cow_year, country_name, hr_score) |> 
  bind_cols(
    predict(
      final_fit,
      new_data = test
    )
  )

## save final predictions ----
final_predicts |> 
  save(
    file = here("data/results/final/final_predicts.rda")
    )

# create performance stats kbl ----
## create metric_set
perform_stats <- metric_set(rmse, mae, mape, rsq)

final_perform_stats <- perform_stats(
  final_predicts,
  truth = hr_score,
  estimate = .pred
) |> 
  select(-.estimator)

## create & write out kbl ----
final_perform_stats_kbl <- final_perform_stats |> 
  knitr::kable(
    col.names = c("Metric", "Estimate")
  )

final_perform_stats_kbl |> 
  save(
    file = here("data/results/final/final_perform_stats_kbl.rda")
  )

# plot predictions vs. actual ----
scatt_plot_final_predicts <- final_predicts |> 
  ggplot(
    aes(
      x = hr_score,
      y = .pred
    )
  ) +
  geom_abline(lty = 2) +
  geom_point(alpha = 0.5) +
  coord_obs_pred() +
  labs(
    x = "Actual",
    y = "Predicted",
    title = "Scatterplot: HR Score, Actual vs. Predicted",
    subtitle = "Predictions generated from a KNN model",
    caption = "Sources: HR Scores (2020) & V-Dem (2023)"
  ) +
  theme_solarized()

## save plot
ggsave(
  scatt_plot_final_predicts,
  width = 2587,
  height = 1787,
  units = "px",
  file = here("plots/scatt_plot_final_predicts.png")
)


