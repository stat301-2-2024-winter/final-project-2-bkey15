# Final Project ----
# Tune ARIMA & ETS

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
load(here("data/results/post-assess/cv/hrsc_cv.rda"))

# make predictions ----
## hr_score ----
hrsc_cv_preds <- hrsc_cv |> 
  forecast(h = 4)
