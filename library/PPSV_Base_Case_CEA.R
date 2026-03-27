#################################################################################################
## BASE CASE CEA
#################################################################################################
base_cea_results_3gdp <- cea_func(
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
)

## print results
if(isTRUE(print_base_case_results)){
  base_cea_results_3gdp %>%
    pivot_longer(!strategy, names_to = "variable", values_to = "estimate") %>%
    pivot_wider(names_from = "strategy", values_from = "estimate") %>%
      set_names(c("variable", "NoPPSV23", "PPSV23")) %>% 
      print(n = Inf, row.names = FALSE)
  
    scales::comma_format()(base_cea_results_3gdp$cost_vacc)
    scales::comma_format()(base_cea_results_3gdp$scd_birth_cohort)
    scales::comma_format()(base_cea_results_3gdp$cost_coi_hc_num)
    scales::comma_format()(base_cea_results_3gdp$cost_coi_vsly_num)
    scales::comma_format()(base_cea_results_3gdp$wtp_coi_hc) 
    scales::comma_format()(base_cea_results_3gdp$wtp_who_vsly)
    
    scales::comma_format()(base_cea_results_3gdp$wtp_who_gdp / base_cea_results_3gdp$wtp_coi_hc) 
  
    scales::comma_format()(base_cea_results_3gdp$wtp_who_vsly / base_cea_results_3gdp$wtp_who_gdp)
    scales::comma_format()(base_cea_results_3gdp$wtp_who_gdp / base_cea_results_3gdp$wtp_coi_hc) 
    scales::comma_format()(base_cea_results_3gdp$cost_coi_hc_num / (base_cea_results_3gdp$dalys / base_cea_results_3gdp$cases))
    
    scales::comma_format()(base_cea_results_3gdp$inc_hosp)
    100*(base_cea_results_3gdp$inc_hosp[2]/base_cea_results_3gdp$hosp[1])
    
    scales::comma_format()(base_cea_results_3gdp$inc_deaths)
    100*(base_cea_results_3gdp$inc_deaths[2]/base_cea_results_3gdp$deaths[1])
    
    base_cea_results_3gdp$deaths[2]
  
    465466 / (base_cea_results_3gdp$wtp_who_gdp/3)[1]  
    120787 / (base_cea_results_3gdp$wtp_who_gdp/3)[1]  
    977912 / (base_cea_results_3gdp$wtp_who_gdp/3)[1]  
  
  
    scales::comma_format()(23124729853.0218 * (1 + 0.027) * (1 + 0.047))
    scales::comma_format()(23124729853.0218 * (1 + 0.027) * (1 + 0.047) * 0.0779943228)
  
    round(100 * base_cea_results_3gdp$cost_vacc[1] / (23124729853.0218 * (1 + 0.027) * (1 + 0.047) * 0.0779943228), 5)
  
    base_cea_results_3gdp$cost_vacc[2]  
  
    124*1.007*1.004*1.015*1.020*0.968*1.019*1.037*1.143*1.007*1.042
}

scales::comma_format()(204395 *500) 

diff(base_cea_results_3gdp$deaths)

base_cea_results_3gdp$inc_hosp
base_cea_results_3gdp$inc_deaths

c(758876, 712005, 806586)/12977

100*(c(758876, 712005, 806586)/12977)/1005


names(base_cea_results_3gdp)
#################################################################################################
## END OF MODULE
#################################################################################################