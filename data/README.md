## Overview

This subfolder contains all data objects used and produced for this project.

## Directory

- `preprocessed/`: Subfolder containing:
  - `preproc_data.rda`: The main preprocessed dataset.
  - `dattbl_vdem_miss.rda`: The table displaying the number of observations missing V-Dem data by country for the preprocessed dataset.
  - `post_assess/`: Subfolder containing preprocessed datasets for use in the time-series models.
- `raw/`: Subfolder containing the raw datasets used for preprocessing, in addition to their codebooks.
- `recipes/`: Subfolder containing the recipes underpinning my models.
  - `appendices/`: Subfolder containing preliminary recipes used in my second progress memo.
- `results/`: Subfolder containing:
  - `final/`: Subfolder containing the results of the final fit.
  - `fits_cv/`: Subfolder containing the results of the cross-validation fits, organized by baseline (`baselines/`) and tuned (`tuned/`) fits.
  -`post-assess/`: Subfolder containing the results of the time series cross-validation fits (`cv/`) and predictions informed exclusively or in part by time-series modelling.
- `splits/`: Subfolder containing the training and testing data splits, as well as the training-set folds.