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

# multicollinearity issue with avg_kill_tort and v2x_clphy? ----
scatt_plot_pvi <- preproc_data |> 
  ggplot(
    aes(
      x = avg_kill_tort,
      y = v2x_clphy
      )
    ) +
  geom_point() +
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

# mean sd of hr_score ----
mean(hr_scores$theta_sd)

