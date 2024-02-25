# Final Project ----
# Model selection/comparison & analysis

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

# load fits ----
load(here("data/results/fits_cv/tuned/ridge_tuned.rda"))
load(here("data/results/fits_cv/tuned/lasso_tuned.rda"))
load(here("data/results/fits_cv/tuned/en_tuned.rda"))
load(here("data/results/fits_cv/tuned/knn_tuned.rda"))
load(here("data/results/fits_cv/tuned/rf_tuned.rda"))
load(here("data/results/fits_cv/tuned/bt_tuned.rda"))

# locate best models ----
## ridge ----
ridge_best <- ridge_tuned |> 
  show_best(metric = "rmse") |> 
  slice(1) |> 
  select(mean, std_err) |> 
  mutate(model = "ridge") |> 
  relocate(model)

## lasso ----
lasso_best <- lasso_tuned |> 
  show_best(metric = "rmse") |> 
  slice(1) |> 
  select(mean, std_err) |> 
  mutate(model = "lasso") |> 
  relocate(model)

## en ----
en_best <- en_tuned |> 
  show_best(metric = "rmse") |> 
  slice(1) |> 
  select(mean, std_err) |> 
  mutate(model = "en") |> 
  relocate(model)

## knn ----
knn_best <- knn_tuned |> 
  show_best(metric = "rmse") |> 
  slice(1) |> 
  select(mean, std_err) |> 
  mutate(model = "knn") |> 
  relocate(model)

## rf ----
rf_best <- rf_tuned |> 
  show_best(metric = "rmse") |> 
  slice(1) |> 
  select(mean, std_err) |> 
  mutate(model = "rf") |> 
  relocate(model)

## bt ----
bt_best <- bt_tuned |> 
  show_best(metric = "rmse") |> 
  slice(1) |> 
  select(mean, std_err) |> 
  mutate(model = "bt") |> 
  relocate(model)

# create best models tbl ----
rmse_tbl_best_tuned <- rbind(
  ridge_best,
  lasso_best,
  en_best,
  knn_best,
  rf_best,
  bt_best
) |> 
  arrange(mean)

# create best models kbl ----
rmse_kbl_best_tuned <- rmse_tbl_best_tuned |> 
  kable(
    col.names = c("Model", "Mean RMSE", "Std. Error")
  )

## save best models kbl ----
rmse_kbl_best_tuned |> 
  save(
    file = here("data/results/fits_cv/tuned/rmse_kbl_best_tuned.rda")
    )

# plot CIs ----
ci_plot_mod_rank <- rmse_tbl_best_tuned |> 
  ggplot(
    aes(
      x = reorder(
        model,
        mean
      ),
      y = mean
    )
  ) +
  geom_point() +
  geom_errorbar(
    aes(
      ymin = mean - 1.96*std_err,
      ymax = mean + 1.96*std_err
    ),
    width = 0.2
  ) +
  labs(
    x = "Model",
    y = "RMSE (Mean)",
    title = "Plot: Model Ranking by Best Mean RMSE",
    subtitle = "Bounds = 95% confidence interval",
  ) +
  theme_bw()
