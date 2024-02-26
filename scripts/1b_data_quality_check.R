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
load(here("data/preprocessed/preproc_data.rda"))

# viz. hr_scores ----
## density plot ----
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

## boxplot ----
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

## combine ----
hr_score_dist_plots <- outcome_dens_plot / outcome_boxplot + plot_annotation(
  title = "Visualizing the Distribution of Human Rights Scores",
    subtitle = "Density Plot (Top) & Boxplot (Bottom)",
    caption = "Source: HR Scores (Fariss, 2020)",
    theme = theme_solarized()
  )

ggsave(
  hr_score_dist_plots,
  width = 2587,
  height = 1787,
  units = "px",
  file = here("plots/hr_score_dist_plots.png")
)

preproc_data |> 
  ggplot(
    aes(
      x = log10(e_gdppc)
    )
  ) +
  geom_density()

