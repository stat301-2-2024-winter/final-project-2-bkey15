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
load(here("data/preprocessed/merge_data.rda"))

# missingness hr_scores ----


# viz. hr_scores ----
## density plot ----
outcome_dens_plot <- merge_data |> 
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
outcome_boxplot <- merge_data |> 
  ggplot(
    aes(
      x = hr_score
      )
    ) +
  geom_boxplot() +
  scale_y_discrete() +
  labs(
    x = "HR Score"
    ) +
  theme_solarized()

## combine ----
outcome_dens_plot / outcome_boxplot + plot_annotation(
  title = "Visualizing the Distribution of Human Rights Scores",
    subtitle = "Density Plot (Top) & Boxplot (Bottom)",
    caption = "Source: HR Scores (2020)",
    theme = theme_solarized()
  )

