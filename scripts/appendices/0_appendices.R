# Final Project ----
# Appendices

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

# load data ----
load(here("data/preprocessed/preproc_data.rda"))
hr_scores <- read_csv("data/raw/HumanRightsProtectionScores_v4.01.csv")

# Yeo-Johnson transform hr_score? ----
yj_hr <- hr_scores |> 
  recipe(theta_mean ~ .) |> 
  step_YeoJohnson(theta_mean) |> 
  prep() |> 
  bake(new_data = NULL) |> 
  select(theta_mean)

## density plot ----
yj_dens_plot <- yj_hr |> 
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

## boxplot ----
yj_boxplot <- yj_hr |> 
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

## combine ----
yj_dist_plots <- yj_dens_plot / yj_boxplot + plot_annotation(
  title = "Distribution of Human Rights Scores, Yeo-Johnson Transformed",
  subtitle = "Density Plot (Top) & Boxplot (Bottom)",
  caption = "Source: HR Scores (Fariss, 2020)",
  theme = theme_solarized()
)

ggsave(
  yj_dist_plots,
  width = 2587,
  height = 1787,
  units = "px",
  file = here("plots/appendices/yj_dist_plots.png")
)

# multicollinearity issue with avg_kill_tort and v2x_clphy? ----
scatt_plot_pvi <- preproc_data |> 
  ggplot(
    aes(
      x = avg_kill_tort,
      y = v2x_clphy
      )
    ) +
  geom_point(alpha = 0.25) +
  labs(
    x = "PVI (Unscaled)",
    y = "PVI (Scaled)",
    title = "Scatterplot: Physical Violence Indices, Scaled vs. Unscaled",
    subtitle = "Is the strong relationship a cause for concern?",
    caption = "Source: V-Dem (2023)"
    ) +
  theme_solarized()

## save plot ----
ggsave(
  scatt_plot_pvi,
  width = 2587,
  height = 1787,
  units = "px",
  file = here("plots/scatt_plot_pvi.png")
)

# dist of pop, gdp, and gdppc ----
## pop ----
preproc_data |> 
  ggplot(
    aes(x = e_pop)
    ) +
  geom_density()

## gdp ----
preproc_data |> 
  ggplot(
    aes(x = e_gdp)
  ) +
  geom_density()

## gdppc ----
preproc_data |> 
  ggplot(
    aes(x = e_gdppc)
  ) +
  geom_density()

# mean sd of hr_score ----
mean(hr_scores$theta_sd)

