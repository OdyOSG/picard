################################################################
# Code to Run
# Written by: Martin Lavallee Odysseus Data Services Inc
# Questions contact martin.lavallee@odysseusinc.com
################################################################

# Step 0: Load libraries

library(config)
library(keyring)
library(tidyverse, quietly = TRUE)
library(picard)
options(dplyr.summarise.inform = FALSE)


# prepare the study with options optum, ckd, or mrktscan
database <- "example" #initialize database

setup_study(database)

#create execution settings
execution_settings <- picard::get_execution_settings(database)

# Step 1: Build cohorts

cohorts <- picard::build_cohorts(
  execution_settings,
  input_path = here::here("input/cohorts_to_create"),
  output_path = here::here("output/cohort_build")
)

# Step 2: Run cohort Diagnostics

diagnostics <- picard::get_diagnostics(
  execution_settings = execution_settings,
  cohort_meta = cohorts,
  output_path = here::here("output/diagnostics")
)

