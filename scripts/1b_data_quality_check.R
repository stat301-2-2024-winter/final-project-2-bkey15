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
hr_scores <- read_csv("data/raw/HumanRightsProtectionScores_v4.01.csv")
load(here("data/preprocessed/preproc_data.rda"))

library(corrplot)
load(here("data/splits/train.rda"))
tmwr_cols <- colorRampPalette(c("#91CBD765", "#CA225E"))
train |> 
  select(-c(cowcode, country_name, year, cow_year, PTS_A, PTS_H, PTS_S)) |> 
  cor(use = "pairwise.complete.obs") |> 
  corrplot(col = tmwr_cols(200), tl.col = "black", method = "ellipse")

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
