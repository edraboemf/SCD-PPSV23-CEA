##******************************************************************************
## One-way sensitivity analysis (OWSA)
##******************************************************************************

## Extract base outcome (ICER or INMB or INHB)
extract_outcome <- function(result, outcome) {
  if (outcome == "icer_hc") {
    val <- tryCatch({
      result %>%
        filter(strategy == 2) %>%
        pull(icer_hc)
    }, error = function(e) NA)
  } else {
    if (outcome == "inmb_who_gdp_hc") {
      val <- tryCatch({
        result %>%
          filter(strategy == 2) %>%
          pull(inmb_who_gdp_hc)
      }, error = function(e) NA)
    } else {
      val <- tryCatch({
        result %>%
          filter(strategy == 2) %>%
          pull(inhb_who_gdp_hc)
      }, error = function(e) NA)
    }
  }
  return(val)
}

## inner OWSA function
inner_owsa_func <- function(
  param,
  base_params, 
  low_params, 
  up_params,
  base_val,
  cpi = ppsv23_params_cpi, 
  gdp = bfa_gdp,
  gdp_mult = 3,
  ref_yr = ref_year,
  strategies = c("PCV13", "PPSV23"),
  ref_strategy = "PCV13",
  outcome = c("icer_hc", "inmb_who_gdp_hc", "inhb_who_gdp_hc"),
  add_pcv13 = FALSE,
  amc = FALSE,
  time = 0,
  nyears = 5,
  birth_cohort_update = NULL,
  alive_pop = NULL
) {

  base_val_param <- base_params[param]
  low_val_param  <- low_params[param]
  high_val_param <- up_params[param]
  
  if (!is.numeric(base_val_param) || is.na(low_val_param) || is.na(high_val_param)) return(NULL)
  if (low_val_param == high_val_param) return(NULL)
  
  ## lower bound
  low_result <- cea_func(
    params = replace(base_params, param, low_val_param), 
    cpi = cpi, 
    gdp = gdp,
    gdp_mult = gdp_mult,
    ref_yr = ref_yr,
    strategies = strategies, 
    ref_strategy = ref_strategy,
    add_pcv13 = add_pcv13,
    amc = amc,
    time = time,
    nyears = nyear,
    birth_cohort_update = birth_cohort_update,
    alive_pop = alive_pop
  )
  
  low_val_outcome  <- extract_outcome(low_result, outcome)
  
  ## upper bound
  high_result <- cea_func(
    params = replace(base_params, param, high_val_param), 
    cpi = cpi, 
    gdp = gdp,
    gdp_mult = gdp_mult,
    ref_yr = ref_yr,
    strategies = strategies, 
    ref_strategy = ref_strategy,
    add_pcv13 = add_pcv13,
    amc = amc,
    time = time,
    nyears = nyear,
    birth_cohort_update = birth_cohort_update,
    alive_pop = alive_pop
  )
  
  high_val_outcome <- extract_outcome(high_result, outcome)
  
  ## output value
  val <- data.frame(
    parameter = param,
    base = base_val,
    low = low_val_outcome,
    high = high_val_outcome,
    low_val = low_val_param,
    high_val = high_val_param
  )
  
  ## return output
  return(val)
}

## Function for one-way sensitivity analysis
run_owsa <- function(
  base_params, 
  low_params, 
  up_params,
  cpi = ppsv23_params_cpi, 
  gdp = bfa_gdp,
  gdp_mult = 3,
  ref_yr = ref_year,
  strategies = c("PCV13", "PPSV23"),
  ref_strategy = "PCV13",
  outcome = c("icer_hc", "inmb_who_gdp_hc", "inhb_who_gdp_hc"),
  add_pcv13 = FALSE,
  amc = FALSE,
  time = 0,
  nyears = 5,
  birth_cohort_update = NULL,
  alive_pop = NULL
) {
  
  outcome <- match.arg(outcome)
  
  ## Base result to use for reference line
  base_result <- cea_func(
    params = base_params,
    cpi = cpi,
    gdp = gdp,
    gdp_mult = gdp_mult,
    ref_yr = ref_yr,
    strategies = strategies,
    ref_strategy = ref_strategy,
    add_pcv13 = add_pcv13,
    amc = amc,
    time = time,
    nyears = nyear,
    birth_cohort_update = birth_cohort_update,
    alive_pop = alive_pop
  )

  base_val <- extract_outcome(base_result, outcome)
  
  ## Run OWSA
  parameter_names <- intersect(intersect(names(base_params), names(low_params)), names(up_params))
  
  results <- lapply(
    X = parameter_names, 
    FUN = inner_owsa_func,
    base_params = base_params, 
    low_params = low_params, 
    up_params = up_params,
    base_val = base_val,
    cpi = cpi, 
    gdp = gdp,
    gdp_mult = gdp_mult,
    ref_yr = ref_yr,
    strategies = strategies,
    ref_strategy = ref_strategy,
    outcome = outcome,
    add_pcv13 = add_pcv13,
    amc = amc,
    time = time,
    nyears = nyears,
    birth_cohort_update = birth_cohort_update,
    alive_pop = alive_pop
  )

  owsa_df <- do.call(rbind, results)  

  ## output value
  val <- list(
    owsa_table = owsa_df,
    base_result = base_result
  )

  ## output
  return(val)
}

#################################################################################################
## END OF MODULE
#################################################################################################