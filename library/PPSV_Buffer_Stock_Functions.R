#################################################################################################
## BUFFER STOCK SCENARIOS FUNCTIONS
#################################################################################################

##******************************************************************************
## Scenario Analysis: Buffer stock
##******************************************************************************
buffer_cea_func <- function(
  buffer_val = buffer_stock_values[1],
  params = ppsv23_params_value,
  cpi = ppsv23_params_cpi, 
  gdp = bfa_gdp,
  gdp_mult = 3,
  ref_yr = ref_year,
  strategies = c("PCV13", "PPSV23"),
  ref_strategy = "PCV13",
  add_pcv13 = FALSE,
  amc = FALSE,
  time = 0,
  nyears = 5,
  birth_cohort_update = NULL,
  alive_pop = NULL
){
  
  params["prop_buffer_stock_ppsv23"] <- buffer_val
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
    nyears = nyear,
    birth_cohort_update = birth_cohort_update,
    alive_pop = alive_pop
  )
  
  val$buffer_stock <- buffer_val

  ## output 
  return(val)
}

##******************************************************************************
## Function to reshape and prepare results
##******************************************************************************
prepare_buffer_table_data <- function(df) {
  df %>% 
    dplyr::mutate(
      `Buffer Stock` = buffer_stock,
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
      `Buffer Stock`, Strategy = strategy,
      `Costs`, `DALYs`, `NMB`, `NHB`,
      `div`,
      `ΔCosts`, `ΔDALYs`, `ΔNMB`, `ΔNHB`, ICER
    )
}

#################################################################################################
## END OF MODULE
#################################################################################################
