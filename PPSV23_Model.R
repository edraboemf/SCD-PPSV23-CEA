rm(list=ls())

#################################################################################################
## FRONTEND MODULE: PPSV23 MODEL
## Author: Emmanuel F. Drabo, MPhil, PhD
## Associate Professor of Health Economic Evaluation
## Department of Health Technology Assessment (HTA)
## Erasmus School of Health Policy & Management (ESHPM)
## Rotterdam, Netherlands
## Email: drabo@eshpm.eur.nl
#################################################################################################

##******************************************************************************
## Load required R packages
##******************************************************************************
ppsv23_packages <- c(
  "xfun", "data.tree", "DiagrammeR", "DiagrammeRsvg", "rsvg", "dampack", "readxl", 
  "data.table", "truncnorm", "MASS", "dplyr", "knitr", "tidyr", "tidyverse", "purrr", 
  "boot", "ggplot2", "scales", "ggsci", "flextable", "officer", "magrittr", 
  "patchwork", "akima", "Hmisc", "robustbase"
)

##******************************************************************************
##* Directories
##******************************************************************************
## Parameters file path
ppsv23_model_dir <- "/Users/emmanuel/Dropbox/vaccines/VBC - SCD/PPSV23 model/PPSV23 CEM Thesis/Value in Health/Submission Materials/RR2/model"
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
flow_diag <- FALSE #TRUE

## reference year for inflation adjustment
ref_year <- 2025

## table switches
create_table1 <- TRUE
create_table2 <- TRUE
create_tableS1 <- TRUE

## simulation switches
base_case_switch <- TRUE
print_base_case_results <- TRUE
owsa_switch <- FALSE #TRUE
psa_switch <- TRUE

## set seed for simulations (for reproducibility)
set.seed(1234)

## number of PSA simulations (parameter uncertainty)
n_sim <- 1000

## bootstrap (sampling uncertainty in PSA summaries)
## optional second-order uncertainty
n_boot_reps <- 500

##******************************************************************************
##* Scenario Analyses
##******************************************************************************
## AMC payment
amc_switch <- FALSE #TRUE
scenario_c_amc_vaccine_ppsv23 <- c(10.46, 9.58, 7.70, 3.30)

## buffer stock
buffer_switch <- FALSE #TRUE
buffer_stock_values <- c(0, 0.05, 0.10, 0.25)

##******************************************************************************
##* Begin simulations
##******************************************************************************
source(file.path(ppsv23_library_dir, "PPSV_Settings.R"))

cat("End of Simulations\n")
#################################################################################################
## END OF MODULE
#################################################################################################