# Final Project ----
# Model selection/comparison & analysis - minimal fe

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
load(here("data/results/fits_cv/tuned/ridge_tuned_1.rda"))
load(here("data/results/fits_cv/tuned/lasso_tuned_1.rda"))
load(here("data/results/fits_cv/tuned/en_tuned_1.rda"))
load(here("data/results/fits_cv/tuned/knn_tuned_1.rda"))
load(here("data/results/fits_cv/tuned/rf_tuned_1.rda"))
load(here("data/results/fits_cv/tuned/bt_tuned_1.rda"))

# locate best models ----
## ridge ----
ridge_best <- ridge_tuned_1 |> 
  show_best(metric = "rmse") |> 
  slice(1) |> 
  select(mean, std_err) |> 
  mutate(model = "ridge") |> 
  relocate(model)

ridge_tuned_1 |> 
  select_best(metric = "rmse")

## lasso ----
lasso_best <- lasso_tuned_1 |> 
  show_best(metric = "rmse") |> 
  slice(1) |> 
  select(mean, std_err) |> 
  mutate(model = "lasso") |> 
  relocate(model)

lasso_tuned_1 |> 
  select_best(metric = "rmse")

## en ----
en_best <- en_tuned_1 |> 
  show_best(metric = "rmse") |> 
  slice(1) |> 
  select(mean, std_err) |> 
  mutate(model = "en") |> 
  relocate(model)

en_tuned_1 |> 
  select_best(metric = "rmse")

## knn ----
knn_best <- knn_tuned_1 |> 
  show_best(metric = "rmse") |> 
  slice(1) |> 
  select(mean, std_err) |> 
  mutate(model = "knn") |> 
  relocate(model)

knn_tuned_1 |> 
  select_best(metric = "rmse")

## rf ----
rf_best <- rf_tuned_1 |> 
  show_best(metric = "rmse") |> 
  slice(1) |> 
  select(mean, std_err) |> 
  mutate(model = "rf") |> 
  relocate(model)

rf_tuned_1 |> 
  select_best(metric = "rmse")

## bt ----
bt_best <- bt_tuned_1 |> 
  show_best(metric = "rmse") |> 
  slice(1) |> 
  select(mean, std_err) |> 
  mutate(model = "bt") |> 
  relocate(model)

bt_tuned_1 |> 
  select_best(metric = "rmse")

# create best models tbl ----
rmse_tbl_best_tuned1 <- rbind(
  ridge_best,
  lasso_best,
  en_best,
  knn_best,
  rf_best,
  bt_best
) |> 
  arrange(mean)

# create best models kbl ----
rmse_kbl_best_tuned1 <- rmse_tbl_best_tuned1 |> 
  kable(
    col.names = c("Model", "Mean RMSE", "Std. Error")
  )

## save best models kbl ----
rmse_kbl_best_tuned1 |> 
  save(
    file = here("data/results/fits_cv/tuned/rmse_kbl_best_tuned1.rda")
    )

# plot CIs ----
ci_plot_best_tuned1 <- rmse_tbl_best_tuned1 |> 
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
  theme_solarized()

## save plot ----
ggsave(
  ci_plot_best_tuned1,
  width = 2587,
  height = 1787,
  units = "px",
  file = here("plots/ci_plot_best_tuned1.png")
)
