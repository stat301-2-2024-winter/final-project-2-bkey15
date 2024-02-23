# Final Project ----
# Initial data split

# Random process in script; seed set right before it

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

# split data ----
## load merge_data ----
load(here("data/preprocessed/preproc_data.rda"))

## set seed ----
set.seed(2612)

## complete split ----
split <- preproc_data |> 
  initial_split(
    prop = 0.75,
    strata = hr_score
  )

train <- training(split)
test <- testing(split)

## save splits ----
train |> 
  save(
    file = here("data/splits/train.rda")
  )
test |> 
  save(
    file = here("data/splits/test.rda")
  )

# fold training data ----
## load training data ----
load(here("data/splits/train.rda"))

## register cores ----
registerDoMC(cores = 8)

## set seed ----
set.seed(2612)

## fold training data ----
train_folds <- train |> 
  vfold_cv(
    v = 10,
    repeats = 5,
    strata = hr_score
  )

## save folds ----
train_folds |> 
  save(
    file = here("data/splits/train_folds.rda")
  )
