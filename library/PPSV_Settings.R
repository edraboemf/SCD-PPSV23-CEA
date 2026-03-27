#################################################################################################
## SETTINGS
#################################################################################################

##******************************************************************************
## Directories
##******************************************************************************
ppsv23_inputs_dir <- file.path(ppsv23_model_dir, "inputs")
ppsv23_outputs_dir <- file.path(ppsv23_model_dir, "outputs")
ppsv23_figures_dir <- file.path(ppsv23_outputs_dir, "figures")
ppsv23_tables_dir <- file.path(ppsv23_outputs_dir, "tables")

##******************************************************************************
## Functions
##******************************************************************************
source(file.path(ppsv23_library_dir, "PPSV_Functions.R"))

##******************************************************************************
## Packages
##******************************************************************************
ipak(pkg = ppsv23_packages)

##******************************************************************************
## Decision-tree model diagram
##******************************************************************************
if(isTRUE(flow_diag)){
  source(file.path(ppsv23_library_dir, "PPSV_Flow_Diag.R"))
}

##******************************************************************************
## Data
##******************************************************************************
source(file.path(ppsv23_library_dir, "PPSV_Data.R"))

##******************************************************************************
## Base Case CEA
##******************************************************************************
## payment models
payment_models <- c("payg", "netflix", "disney", "costco", "mbp", "obp")
names(payment_models) <- c(
  "Pay as you go", "Subscription", "Two-part-tariff", 
  "Pass-through", "Mortgage-based", "Outcome-based"
)

if(isTRUE(base_case_switch)){
  source(file.path(ppsv23_library_dir, "PPSV_Base_Case_CEA.R"))
}

# 33756*12.76035
# mortgage_func(principal=33756*12.76035, interest=0.07, periods=5)
# 118377.5

##******************************************************************************
## Sensitivity Analyses
##******************************************************************************
if(isTRUE(owsa_switch)){
  source(file.path(ppsv23_library_dir, "PPSV_OWSA.R"))
}

if(isTRUE(psa_switch)){
  source(file.path(ppsv23_library_dir, "PPSV_PSA.R"))
}

##******************************************************************************
##* Scenario Analyses
##******************************************************************************
tail_price_range <- sort(c(3.30, 10.46, 17.94, seq(from = 0.00, to = 18, by = 1)))
copay_range <- seq(from = 0, to = 0.50, by = 0.05)
amc_dose_subsidy_range <- c(0.10, 0.20, 0.30)

amc_sensi_vals <- expand_grid(
  tail_price = tail_price_range,
  copay = copay_range,
  amc_dose_subsidy = amc_dose_subsidy_range
)

## AMC Payment
if(isTRUE(amc_switch)){
  source(file.path(ppsv23_library_dir, "PPSV_AMC_Scenario_Analyses.R"))
}

## Buffer Stock
if(isTRUE(buffer_switch)){
  source(file.path(ppsv23_library_dir, "PPSV_Buffer_Stock_Scenario_Analyses.R"))
}

##******************************************************************************
##* Tables
##******************************************************************************
create_tables <- ifelse(any(c(create_table1, create_table2)), TRUE, FALSE)

if(isTRUE(create_tables)){
  source(file.path(ppsv23_library_dir, "PPSV_Tables.R"))
}

#################################################################################################
## END OF MODULE
#################################################################################################