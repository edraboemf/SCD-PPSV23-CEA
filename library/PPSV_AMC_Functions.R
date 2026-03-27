#################################################################################################
## AMC PAYMENT FUNCTIONS
#################################################################################################

##******************************************************************************
## Scenario Analysis: AMC Payment
##******************************************************************************
## cea
amc_pay_cea_func <- function(
  c_amc_vaccine = scenario_c_amc_vaccine_ppsv23[1],
  params = ppsv23_params_value,
  cpi = ppsv23_params_cpi, 
  gdp = bfa_gdp,
  gdp_mult = 3,
  ref_yr = ref_year,
  strategies = c("PCV13", "PPSV23"),
  ref_strategy = "PCV13",
  add_pcv13 = FALSE,
  amc = TRUE,
  time = 0,
  nyears = 5,
  birth_cohort_update = NULL,
  alive_pop = NULL
){
  
  if(isTRUE(amc)){
    params["c_amc_vaccine_ppsv23"] <- c_amc_vaccine
  } 
  
  val <- cea_func(
    params = params,
    cpi = cpi, 
    gdp = gdp,
    gdp_mult = gdp_mult,
    ref_yr = ref_yr,
    strategies = strategies,
    ref_strategy = ref_strategy,
    add_pcv13 = add_pcv13,
    amc = amc,
    time = time,
    nyears = nyears,
    birth_cohort_update = birth_cohort_update,
    alive_pop = alive_pop
  )
  val$amc_pay <- c_amc_vaccine
  return(val)
}


## Function to reshape and prepare results
prepare_amc_table_data <- function(df) {
  df %>% 
    mutate(
      `AMC`= amc_pay,
      `Costs` = cost,
      `DALYs` = dalys,
      `NMB` = nmb,
      `NHB` = nhb,
      `div`= " ",
      `ΔCosts` = inc_cost,
      `ΔDALYs` = inc_dalys,
      `ΔNMB` = inmb,
      `ΔNHB` = inhb,
      ICER = icer
    ) %>%
    dplyr::select(
      `AMC`, Strategy = strategy,
      `Costs`, `DALYs`, `NMB`, `NHB`,
      `div`,
      `ΔCosts`, `ΔDALYs`, `ΔNMB`, `ΔNHB`, ICER
    )
}

##******************************************************************************
## Scenario Analysis: AMC Payment Plot
##******************************************************************************
amc_pay_cea_plot_func <- function(
  X = 1,
  sensi_params = sensi_vals,
  params = ppsv23_params_value,
  cpi = ppsv23_params_cpi, 
  gdp = bfa_gdp,
  gdp_mult = 3,
  ref_yr = ref_year,
  strategies = c("PCV13", "PPSV23"),
  ref_strategy = "PCV13",
  add_pcv13 = FALSE,
  amc = TRUE,
  time = 0,
  nyears = 5,
  birth_cohort_update = NULL,
  alive_pop = NULL
){

  if(isTRUE(amc)){
    params["c_amc_vaccine_ppsv23"] <- as.numeric(sensi_params$tail_price[X])
    params["c_amc_vaccine_copay_ppsv23"] <- as.numeric(sensi_params$copay[X])
    params["prop_amc_pay_ppsv23"] <- as.numeric(sensi_params$amc_dose_subsidy[X])
  } 
  
  val <- cea_func(
    params = params,
    cpi = cpi, 
    gdp = gdp,
    gdp_mult = gdp_mult,
    ref_yr = ref_yr,
    strategies = strategies,
    ref_strategy = ref_strategy,
    add_pcv13 = add_pcv13,
    amc = amc,
    time = time,
    nyears = nyears,
    birth_cohort_update = birth_cohort_update,
    alive_pop = alive_pop
  )
  
  val$tail_price <- as.numeric(sensi_params$tail_price[X])
  val$copay <- as.numeric(sensi_params$copay[X])
  val$amc_dose_subsidy <- as.numeric(sensi_params$amc_dose_subsidy[X])
  
  return(val)
}

## interpolation function
interp_one_subsidy <- function(df_subsidy) {
  n_obs <- nrow(df_subsidy)

  # not enough data to interpolate
  if (
    length(unique(df_subsidy$tail_price)) < 3 ||
    length(unique(df_subsidy$copay)) < 3
  ) {
    warning("Not enough unique points for interpolation. Returning raw data.")
    return(df_subsidy %>%
      dplyr::select(tail_price, copay, icer, amc_dose_subsidy))
  }

  # add minimal jitter to break collinearity
  df_subsidy <- df_subsidy %>%
    mutate(
      tail_price = tail_price + rnorm(n_obs, mean = 0, sd = 0.001),
      copay = copay + rnorm(n_obs, mean = 0, sd = 0.001)
    )

  # try interpolation
  interp_data <- tryCatch(
    akima::interp(
      x = df_subsidy$tail_price,
      y = df_subsidy$copay,
      z = df_subsidy$icer,
      xo = seq(min(df_subsidy$tail_price), max(df_subsidy$tail_price), length = 100),
      yo = seq(min(df_subsidy$copay), max(df_subsidy$copay), length = 100),
      duplicate = "mean",
      extrap = TRUE
    ),
    error = function(e) {
      warning("Interpolation failed: ", conditionMessage(e), ". Returning raw data.")
      return(NULL)
    }
  )

  # fallback if interp failed
  if (is.null(interp_data)) {
    return(df_subsidy %>%
      dplyr::select(tail_price, copay, icer, amc_dose_subsidy))
  }

  # build grid
  val <- expand_grid(
    tail_price = interp_data$x,
    copay = interp_data$y
  ) %>%
    mutate(
      icer = as.vector(interp_data$z),
      amc_dose_subsidy = unique(df_subsidy$amc_dose_subsidy)
    )

  # check NA proportion
  na_frac <- mean(is.na(val$icer))
  if (na_frac > 0.5) {
    warning("More than 50% of interpolated values are NA. Returning raw data instead.")
    return(df_subsidy %>%
      dplyr::select(tail_price, copay, icer, amc_dose_subsidy))
  }

  return(val)
}



#################################################################################################
## END OF MODULE
#################################################################################################