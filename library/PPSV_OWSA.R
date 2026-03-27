#################################################################################################
## ONE-WAY SENSITIVITY ANALYSIS (OWSA)
#################################################################################################

## base case, lower bound, and upper bound parameters
base_params <- ppsv23_params_value
low_params <- ppsv23_params_lower
up_params <- ppsv23_params_upper

## run the OWSA
owsa_output_3gdp <- run_owsa(
  base_params = ppsv23_params_value,
  low_params = ppsv23_params_lower,
  up_params = ppsv23_params_upper,
  cpi = ppsv23_params_cpi, 
  gdp = bfa_gdp,
  gdp_mult = 3,
  ref_yr = ref_year,
  outcome = c("icer_hc", "inmb_who_gdp_hc", "inhb_who_gdp_hc")[1],
  add_pcv13 = FALSE,
  amc = FALSE,
  time = 0,
  nyears = 5,
  birth_cohort_update = NULL,
  alive_pop = NULL
)

## produce Tornado Plot
source(file.path(ppsv23_library_dir, "PPSV_TornadoPlot.R"))

#################################################################################################
## END OF MODULE
#################################################################################################