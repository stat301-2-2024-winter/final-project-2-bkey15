# Final Project ----
# Initial data split

# Random process in script, seed set right before it

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
load(here("data/preprocessed/merge_data.rda"))

## set seed ----
set.seed(2612)

## complete split ----
merge_split <- merge_data |> 
  initial_split(
    prop = 0.75,
    strata = hr_score
  )

merge_train <- training(merge_split)
merge_test <- testing(merge_split)

## save splits ----
merge_train |> 
  save(
    file = here("data/data_splits/merge_train.rda")
  )
merge_test |> 
  save(
    file = here("data/data_splits/merge_test.rda")
  )

# fold training data ----
## load training data ----
load(here("data/data_splits/merge_train.rda"))

## register cores ----
registerDoMC(cores = 8)

## set seed ----
set.seed(2612)

## fold training data ----
merge_train_folds <- merge_train |> 
  vfold_cv(
    v = 10,
    repeats = 5,
    strata = hr_score
  )

## save folds ----
merge_train_folds |> 
  save(
    file = here("data/data_splits/merge_train_folds.rda")
  )
