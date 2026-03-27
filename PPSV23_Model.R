rm(list=ls())

#################################################################################################
## FRONTEND MODULE: PPSV23 MODEL
## Author: [BLINDED FOR REVIEW]
## [BLINDED FOR REVIEW]
## [BLINDED FOR REVIEW]
#################################################################################################

##******************************************************************************
## Load required R packages
##******************************************************************************
ppsv23_packages <- c(
  "data.tree", "DiagrammeR", "DiagrammeRsvg", "rsvg", "dampack", "readxl", 
  "data.table", "truncnorm", "MASS", "dplyr", "knitr", "tidyr", "tidyverse", "purrr", 
  "boot", "ggplot2", "scales", "ggsci", "flextable", "officer", "magrittr", 
  "patchwork", "akima", "Hmisc", "robustbase"
)

##******************************************************************************
##* Directories
##******************************************************************************
## Parameters file path
ppsv23_model_dir <- "~/model"
ppsv23_library_dir <- file.path(ppsv23_model_dir, "library")

##******************************************************************************
## Input parameters
##******************************************************************************
## Parameters file
ppsv23_params_fname <- "inputs/ppsv23_params.xlsx"
ppsv23_params_path <- file.path(ppsv23_model_dir, ppsv23_params_fname)

##******************************************************************************
## Model switches
##******************************************************************************
## plot flow diagram
flow_diag <- TRUE

## reference year for inflation adjustment
ref_year <- 2025

## table switches
create_table1 <- TRUE
create_table2 <- TRUE
create_tableS1 <- TRUE

## simulation switches
base_case_switch <- TRUE
print_base_case_results <- TRUE
owsa_switch <- TRUE
psa_switch <- TRUE

## set seed for simulations (for reproducibility)
set.seed(1234)

## number of PSA simulations
n_sim <- 1000
n_boot_reps <- 500

##******************************************************************************
##* Scenario Analyses
##******************************************************************************
## AMC payment
amc_switch <- TRUE
scenario_c_amc_vaccine_ppsv23 <- c(10.46, 9.58, 7.70, 3.30)

## buffer stock
buffer_switch <- TRUE
buffer_stock_values <- c(0, 0.05, 0.10, 0.25)

##******************************************************************************
##* Begin simulations
##******************************************************************************
source(file.path(ppsv23_library_dir, "PPSV_Settings.R"))

cat("End of Simulations\n")
#################################################################################################
## END OF MODULE
#################################################################################################
