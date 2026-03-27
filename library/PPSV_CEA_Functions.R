#################################################################################################
## CEA MODEL
#################################################################################################
## AMC vaccine cost adjustment
amc_vacc_cost_func <- function(theta, copay, tail) {
  val <- min(1, (theta * 1)) * copay + max(0, 1 - (theta * 1)) * tail
  return(val)
}

## Indirect costs, productive time: human capital approach
ind_cost_hc_func <- function(pc_gdp, lis, lfpr, pls){
  val <- pc_gdp * lis * lfpr * pls
  return(val)
}

## Indirect costs, leisure: human capital approach
ind_cost_leis_hc_func <- function(pc_gdp, lis, lfpr, pls, p_leis){
  cost_leis <- ind_cost_hc_func(pc_gdp = pc_gdp, lis = lis, lfpr = lfpr, pls = pls)
  val <- cost_leis * p_leis
  return(val)
}

## Indirect costs: VSLY approach
ind_cost_vsly_func <- function(pc_gdp, vsly_gdp_pc, inf_vsly_mult, dalys){
  val <- pc_gdp * vsly_gdp_pc * inf_vsly_mult * dalys
  return(val)
}

##******************************************************************************
## Decision-tree model
##******************************************************************************
eval_strategy <- function(
  strategy = "PPSV23",
  params = ppsv23_params_value,
  cpi = ppsv23_params_cpi,
  gdp = bfa_gdp,
  gdp_mult = 3,
  ref_yr = ref_year,
  add_pcv13 = FALSE,
  amc = FALSE,
  time = 0,
  nyears = 5,
  birth_cohort_update = NULL,
  alive_pop = NULL
) {

  ## cpi adjustments
  cpi_adj_params <- as.numeric(params[names(params)]) * as.numeric(cpi[names(params)])
  names(cpi_adj_params) <- names(params)

  with(as.list(cpi_adj_params), {
    
    ##**************************************************************************
    ## proportions and probabilities
    ##**************************************************************************    
    prop_severe_nmnp <- n_severe_nmnp / (n_severe_nmnp + n_nonsevere_nmnp)
    cfr_nonsevere_nmnp <- (deaths_nmnp - (cfr_severe_nmnp * n_severe_nmnp)) / n_nonsevere_nmnp
    # p_death_nmnp <- prop_severe_nmnp * cfr_severe_nmnp + (1 - prop_severe_nmnp) * cfr_nonsevere_nmnp
    p_death_nmnp <- cfr_severe_nmnp
    
    ## Assumed to be zero due to low disease severity
    p_death_nipd_pneum_tx <- 0
    p_death_nipd_pneum_notx <- 0
    p_death_aom_tx <- 0
    p_death_aom_notx <- 0
    
    ##**************************************************************************
    ## WTP estimates
    ##**************************************************************************
    ## GDP per capita
    gdp_per_capita_base <- as.numeric(gdp[as.character(ref_yr - 1)])
    gdp_per_capita <- (1 + gdp_growth_rate) * gdp_per_capita_base 
    
    ##**********************************************
    ## WHO approach
    ##**********************************************
    wtp <- gdp_per_capita * gdp_mult
    
    ##**********************************************
    ## Human capital approach
    ##**********************************************
    hc_gdp_multiplier <- labor_income_gdp_share * lfpr
    leisure_multiplier <- labor_income_gdp_share * prop_leisure_time
    
    ##**********************************************
    ## VSL approach
    ##**********************************************
    ## VSLY multiplier
    w_med_vsly <- Hmisc::wtd.quantile(
      x = c(adult_vsly_wtp_small, adult_vsly_wtp_large), 
      weights = c(sample_vsly_wtp_small, sample_vsly_wtp_large),
      probs = 0.5
    )
    vsly_gdp_multiplier <- (as.vector(w_med_vsly) / base_gdp_per_capita)
    wtp_who_vsly <- gdp_per_capita * vsly_gdp_multiplier

    ##**************************************************************************
    ## Buffer stock 
    ##**************************************************************************
    if(is.na(prop_buffer_stock) | (prop_buffer_stock < prop_wastage)){
      prop_buffer_stock <- prop_wastage
    }
    
    if(is.na(prop_buffer_stock_ppsv23) | (prop_buffer_stock_ppsv23 < prop_wastage_ppsv23)){
      prop_buffer_stock_ppsv23 <- prop_wastage_ppsv23
    }
    
    ##**************************************************************************
    ## AMC vaccine cost
    ##**************************************************************************
    if(isTRUE(amc)){
      prop_adj_amc_pay <- prop_amc_pay
      c_adj_vaccine <- c_amc_vaccine
      c_adj_vaccine_copay <- c_amc_vaccine_copay
      prop_adj_amc_pay_ppsv23 <- prop_amc_pay_ppsv23
      c_adj_subscription_fee_vaccine_ppsv23 <- 0
      c_adj_vaccine_ppsv23 <- c_amc_vaccine_ppsv23
      c_adj_vaccine_copay_ppsv23 <- c_amc_vaccine_copay_ppsv23
    } else {
      prop_adj_amc_pay <- 0
      c_adj_vaccine <- c_vaccine
      c_adj_vaccine_copay <- c_vaccine
      prop_adj_amc_pay_ppsv23 <- 0
      c_adj_subscription_fee_vaccine_ppsv23 <- 0
      c_adj_vaccine_ppsv23 <- c_vaccine_ppsv23
      c_adj_vaccine_copay_ppsv23 <- c_vaccine_ppsv23
    }
    
    ## birth cohort
    if(time == 0){
      scd_birth_cohort <- cohort_size * prop_scd * p_uptake_pcv13
      cohort_alive <- scd_birth_cohort
    }else{
      scd_birth_cohort <- as.numeric(birth_cohort_update[strategy])
      cohort_alive <- as.numeric(alive_pop[strategy])
    }
    
    #****************************
    ## vaccination costs
    #****************************
    ## pcv13 costs
    prop_excess_demand_pcv13 <- prop_buffer_stock - prop_wastage
    amc_adj_vaccine_dose_cost_pcv13 <- amc_vacc_cost_func(theta = prop_adj_amc_pay, copay = c_adj_vaccine_copay, tail = c_adj_vaccine)
    c_per_dose_used_pcv13 <- c_adj_vaccine * prop_freight_cost + c_cold_chain + c_safety_box + amc_adj_vaccine_dose_cost_pcv13 + c_syringe + c_nurses_fees
    c_per_dose_wasted_pcv13 <- c_adj_vaccine * prop_freight_cost + c_cold_chain + c_safety_box + amc_adj_vaccine_dose_cost_pcv13
    c_full_schedule_pcv13 <- c_per_dose_used_pcv13 * (1 + prop_excess_demand_pcv13) + c_per_dose_wasted_pcv13 * prop_wastage
    
    ## ppsv23 costs
    prop_excess_demand_ppsv23 <- prop_buffer_stock_ppsv23 - prop_wastage_ppsv23
    amc_adj_vaccine_dose_cost_ppsv23 <-  amc_vacc_cost_func(theta = prop_adj_amc_pay_ppsv23, copay = c_adj_vaccine_copay_ppsv23, tail = c_adj_vaccine_ppsv23)
    c_fixed_costs_ppsv23 <-  sum(c(amc_adj_vaccine_dose_cost_ppsv23, c_adj_vaccine_ppsv23 * prop_freight_cost_ppsv23, c_cold_chain_ppsv23, c_safety_box_ppsv23, c_syringe_ppsv23))
        
    c_per_dose_used_ppsv23 <- sum(c(c_fixed_costs_ppsv23, c_nurses_fees_ppsv23))
    c_per_dose_wasted_ppsv23 <- c_fixed_costs_ppsv23 
    c_full_schedule_ppsv23 <- c_per_dose_used_ppsv23 * (1 + prop_excess_demand_ppsv23) + prop_wastage_ppsv23 * c_per_dose_wasted_ppsv23

    ##**************************************************************************
    ## PROBALIBILITIES
    ##**************************************************************************
    ## PCV13 probabilities
    inc_aom_pcv13 <- prop_aom_pneumonia * inc_all_cause_aom
    inc_nbp_pcv13 <- (1 - prop_ipd_pneumonia) * inc_pp_pcv13  
    inc_nipd_pcv13 <- inc_aom_pcv13 + inc_nbp_pcv13
    
    p_ipd_pcv13 <- (inc_ipd_pcv13 / 100000) * (rr_ipd_scd_hbss * prop_scd_hbss + rr_ipd_scd_hbsc * (1 - prop_scd_hbss))
    p_nipd_pcv13 <- (inc_nipd_pcv13 / 100000) * (rr_ipd_scd_hbss * prop_scd_hbss + rr_ipd_scd_hbsc * (1 - prop_scd_hbss))
    p_noinf_pcv13 <- 1 - (p_ipd_pcv13 + p_nipd_pcv13)

    p_meningitis_pcv13 <- prop_meningitis_pcv13
    p_ipd_pneum_pcv13 <- prop_pneumonia
    p_nmnp_pcv13 <- 1 - (p_meningitis_pcv13 + p_ipd_pneum_pcv13)

    p_aom_pcv13 <- inc_aom_pcv13 / (inc_aom_pcv13 + inc_nbp_pcv13)
    p_nipd_pneum_pcv13 <- 1 - p_aom_pcv13

    ## PPSV23 probabilities
    ipd_risk_reduction <- (1 - eff_ipd_ppsv23 * prop_ipd_cov_by_ppsv23)  
    p_ipd_ppsv23 <- ipd_risk_reduction * p_ipd_pcv13
    p_nipd_ppsv23 <- p_nipd_pcv13
    p_noinf_ppsv23 <- 1 - (p_ipd_ppsv23 + p_nipd_ppsv23)

    p_meningitis_ppsv23 <- p_meningitis_pcv13
    p_ipd_pneum_ppsv23 <- p_ipd_pneum_pcv13
    p_nmnp_ppsv23 <- 1 - (p_meningitis_ppsv23 + p_ipd_pneum_ppsv23)

    p_aom_ppsv23 <- p_aom_pcv13 
    p_nipd_pneum_ppsv23 <- 1 - p_aom_ppsv23
    
    ## outcome probabilities
    p_hosp_nipd_pneum_notx <- 1 - p_hosp_nipd_pneum_tx
    p_hosp_aom_notx <- 1 - p_hosp_aom_tx
    
    p_alive_meningitis_nosequelae <- 1 - (p_death_meningitis + p_alive_meningitis_sequelae)
    
    ##************************************************
    ## transition probabilities
    ##************************************************
    p_tree <- list(
      PPSV23 = list(
        vaccinated = list(
          invasive = p_ipd_ppsv23,
          non_invasive = p_nipd_ppsv23,
          no_infection = p_noinf_ppsv23,
          meningitis = p_meningitis_ppsv23,
          pneumonia_invasive = p_ipd_pneum_ppsv23,
          nmn_pneumonia = p_nmnp_ppsv23,
          hosp_meningitis = p_hosp_meningitis,
          death_meningitis = p_death_meningitis,
          sequelae = p_alive_meningitis_sequelae,
          hosp_pneumonia_invasive = p_hosp_ipd_pneum,
          death_pneumonia_invasive = p_death_ipd_pneum,
          hosp_nmn = p_hosp_nmnp,
          death_nmn = p_death_nmnp,
          pneumonia_noninv = p_nipd_pneum_ppsv23,
          nipd_aom_media = p_aom_ppsv23,
          hosp_pneumonia_noninv = p_hosp_nipd_pneum_tx,
          hosp_pneumonia_nonipd_death = p_death_nipd_pneum_tx,
          no_treated_pneumonia = p_hosp_nipd_pneum_notx,
          untreated_pneumonia_death = p_death_nipd_pneum_notx,
          treat_otitis = p_hosp_aom_tx
        ),
        not_vaccinated = list(
          invasive = p_ipd_pcv13,
          non_invasive = p_nipd_pcv13,
          no_infection = p_noinf_pcv13,
          meningitis = p_meningitis_pcv13,
          pneumonia_invasive = p_ipd_pneum_pcv13,
          nmn_pneumonia = p_nmnp_pcv13,
          hosp_meningitis = p_hosp_meningitis,
          death_meningitis = p_death_meningitis,
          sequelae = p_alive_meningitis_sequelae,
          hosp_pneumonia_invasive = p_hosp_ipd_pneum,
          death_pneumonia_invasive = p_death_ipd_pneum,
          hosp_nmn = p_hosp_nmnp,
          death_nmn = p_death_nmnp,
          pneumonia_noninv = p_nipd_pneum_pcv13,
          nipd_aom_media = p_aom_pcv13,
          hosp_pneumonia_noninv = p_hosp_nipd_pneum_tx,
          hosp_pneumonia_nonipd_death = p_death_nipd_pneum_tx,
          no_treated_pneumonia = p_hosp_nipd_pneum_notx,
          untreated_pneumonia_death = p_death_nipd_pneum_notx,
          treat_otitis = p_hosp_aom_tx
        )
      ),
      PCV13 = list(
        invasive = p_ipd_pcv13,
        non_invasive = p_nipd_pcv13,
        no_infection = p_noinf_pcv13,
        meningitis = p_meningitis_pcv13,
        pneumonia_invasive = p_ipd_pneum_pcv13,
        nmn_pneumonia = p_nmnp_pcv13,
        hosp_meningitis = p_hosp_meningitis,
        death_meningitis = p_death_meningitis,
        sequelae = p_alive_meningitis_sequelae,
        hosp_pneumonia_invasive = p_hosp_ipd_pneum,
        death_pneumonia_invasive = p_death_ipd_pneum,
        hosp_nmn = p_hosp_nmnp,
        death_nmn = p_death_nmnp,
        pneumonia_noninv = p_nipd_pneum_pcv13,
        nipd_aom_media = p_aom_pcv13,
        hosp_pneumonia_noninv = p_hosp_nipd_pneum_tx,
        hosp_pneumonia_nonipd_death = p_death_nipd_pneum_tx,
        no_treated_pneumonia = p_hosp_nipd_pneum_notx,
        untreated_pneumonia_death = p_death_nipd_pneum_notx,
        treat_otitis = p_hosp_aom_tx
      )
    )
    
    ## Vary the probabilities of vaccination by strategy
    if (strategy == "PPSV23") {
      p_vac <- p_uptake_ppsv23
      probs_vac <- p_tree$PPSV23$vaccinated
      probs_novac <- p_tree$PPSV23$not_vaccinated
    } else if (strategy == "PCV13") {
      p_vac <- 1
      probs_vac <- probs_novac <- p_tree$PCV13
    }
    
    ## Invasive Meningitis
    ipd_men_alive_seq_vac <- with(probs_vac, p_vac * invasive * meningitis * hosp_meningitis * (1 - death_meningitis) * sequelae)
    ipd_men_alive_seq_novac <- with(probs_novac, (1 - p_vac) * invasive * meningitis * hosp_meningitis * (1 - death_meningitis) * sequelae)
    ipd_men_alive_seq <- ipd_men_alive_seq_vac + ipd_men_alive_seq_novac
    
    ipd_men_alive_noseq_vac <- with(probs_vac, p_vac * invasive * meningitis * hosp_meningitis * (1 - death_meningitis) * (1 - sequelae))
    ipd_men_alive_noseq_novac <- with(probs_novac, (1 - p_vac) * invasive * meningitis * hosp_meningitis * (1 - death_meningitis) * (1 - sequelae))
    ipd_men_alive_noseq <- ipd_men_alive_noseq_vac + ipd_men_alive_noseq_novac
    
    ipd_men_dead_vac <- with(probs_vac, p_vac * invasive * meningitis * hosp_meningitis * death_meningitis)
    ipd_men_dead_novac <- with(probs_novac, (1 - p_vac) * invasive * meningitis * hosp_meningitis * death_meningitis)
    ipd_men_dead <- ipd_men_dead_vac + ipd_men_dead_novac
    
    ## Invasive Pneumonia (IPD)
    ipd_pneum_alive_vac <- with(probs_vac, p_vac * invasive * pneumonia_invasive * hosp_pneumonia_invasive * (1 - death_pneumonia_invasive))
    ipd_pneum_alive_novac <- with(probs_novac, (1 - p_vac) * invasive * pneumonia_invasive * hosp_pneumonia_invasive * (1 - death_pneumonia_invasive))
    ipd_pneum_alive <- ipd_pneum_alive_vac + ipd_pneum_alive_novac
    
    ipd_pneum_dead_vac <- with(probs_vac, p_vac * invasive * pneumonia_invasive * hosp_pneumonia_invasive * death_pneumonia_invasive)
    ipd_pneum_dead_novac <- with(probs_novac, (1 - p_vac) * invasive * pneumonia_invasive * hosp_pneumonia_invasive * death_pneumonia_invasive)
    ipd_pneum_dead <- ipd_pneum_dead_vac + ipd_pneum_dead_novac
    
    ## Non-meningitis invasive pneumonia (NMNP)
    ipd_nmn_alive_vac <- with(probs_vac, p_vac * invasive * nmn_pneumonia * hosp_nmn * (1 - death_nmn))
    ipd_nmn_alive_novac <- with(probs_novac, (1 - p_vac) * invasive * nmn_pneumonia * hosp_nmn * (1 - death_nmn))
    ipd_nmn_alive <- ipd_nmn_alive_vac + ipd_nmn_alive_novac
    
    ipd_nmn_dead_vac <- with(probs_vac, p_vac * invasive * nmn_pneumonia * hosp_nmn * death_nmn)
    ipd_nmn_dead_novac <- with(probs_novac, (1 - p_vac) * invasive * nmn_pneumonia * hosp_nmn * death_nmn)
    ipd_nmn_dead <- ipd_nmn_dead_vac + ipd_nmn_dead_novac 
    
    ## Non-invasive pneumonia (hospital treated)
    nipd_pneum_hosp_alive_vac <- with(probs_vac, p_vac * non_invasive * pneumonia_noninv * hosp_pneumonia_noninv * (1 - hosp_pneumonia_nonipd_death))
    nipd_pneum_hosp_alive_novac <- with(probs_novac, (1 - p_vac) * non_invasive * pneumonia_noninv * hosp_pneumonia_noninv * (1 - hosp_pneumonia_nonipd_death))
    nipd_pneum_hosp_alive <- nipd_pneum_hosp_alive_vac + nipd_pneum_hosp_alive_novac
    
    nipd_pneum_hosp_dead_vac <- with(probs_vac, p_vac * non_invasive * pneumonia_noninv * hosp_pneumonia_noninv * hosp_pneumonia_nonipd_death)
    nipd_pneum_hosp_dead_novac <- with(probs_novac, (1 - p_vac) * non_invasive * pneumonia_noninv * hosp_pneumonia_noninv * hosp_pneumonia_nonipd_death)
    nipd_pneum_hosp_dead <- nipd_pneum_hosp_dead_vac + nipd_pneum_hosp_dead_novac
    
    ## Non-invasive pneumonia (untreated)
    nipd_pneum_nohosp_alive_vac <- with(probs_vac, p_vac * non_invasive * pneumonia_noninv * no_treated_pneumonia * (1 - untreated_pneumonia_death))
    nipd_pneum_nohosp_alive_novac <- with(probs_novac, (1 - p_vac) * non_invasive * pneumonia_noninv * no_treated_pneumonia * (1 - untreated_pneumonia_death))
    nipd_pneum_nohosp_alive <- nipd_pneum_nohosp_alive_vac + nipd_pneum_nohosp_alive_novac
    
    nipd_pneum_nohosp_dead_vac <- with(probs_vac, p_vac * non_invasive * pneumonia_noninv * no_treated_pneumonia * untreated_pneumonia_death)
    nipd_pneum_nohosp_dead_novac <- with(probs_novac, (1 - p_vac) * non_invasive * pneumonia_noninv * no_treated_pneumonia * untreated_pneumonia_death)
    nipd_pneum_nohosp_dead <- nipd_pneum_nohosp_dead_vac + nipd_pneum_nohosp_dead_novac
    
    ## Otitis media (treated)
    nipd_aom_treated_alive_vac <- with(probs_vac, p_vac * non_invasive * nipd_aom_media * treat_otitis)
    nipd_aom_treated_alive_novac <- with(probs_novac, (1 - p_vac) * non_invasive * nipd_aom_media * treat_otitis)
    nipd_aom_treated_alive <- nipd_aom_treated_alive_vac + nipd_aom_treated_alive_novac
    
    ## Otitis media (untreated)
    nipd_aom_untreated_alive_vac <- with(probs_vac, p_vac * non_invasive * nipd_aom_media * (1 - treat_otitis))
    nipd_aom_untreated_alive_novac <- with(probs_novac, (1 - p_vac) * non_invasive * nipd_aom_media * (1 - treat_otitis))
    nipd_aom_untreated_alive <- nipd_aom_untreated_alive_vac + nipd_aom_untreated_alive_novac
    
    ## No infection
    noinf_vac <- with(probs_vac, p_vac * no_infection) 
    noinf_novac <- with(probs_novac, (1 - p_vac) * no_infection)
    noinf <- noinf_vac + noinf_novac

    ##*************************************************************
    ## coi cost weights (probabilities)
    ##************************************************************* 
    ## Invasive Meningitis
    ipd_men_alive_seq_coi <- with(probs_vac, (invasive / (invasive + non_invasive)) * meningitis * hosp_meningitis * (1 - death_meningitis) * sequelae)
    ipd_men_alive_noseq_coi <- with(probs_vac, (invasive / (invasive + non_invasive)) * meningitis * hosp_meningitis * (1 - death_meningitis) * (1 - sequelae))
    ipd_men_dead_coi <- with(probs_vac, (invasive / (invasive + non_invasive)) * meningitis * hosp_meningitis * death_meningitis)
    
    ## Invasive Pneumonia (IPD)
    ipd_pneum_alive_coi <- with(probs_vac, (invasive / (invasive + non_invasive)) * pneumonia_invasive * hosp_pneumonia_invasive * (1 - death_pneumonia_invasive))
    ipd_pneum_dead_coi <- with(probs_vac, (invasive / (invasive + non_invasive)) * pneumonia_invasive * hosp_pneumonia_invasive * death_pneumonia_invasive)
    
    ## Non-meningitis invasive pneumonia (NMNP)
    ipd_nmn_alive_coi <- with(probs_vac, (invasive / (invasive + non_invasive)) * nmn_pneumonia * hosp_nmn * (1 - death_nmn))
    ipd_nmn_dead_coi <- with(probs_vac, (invasive / (invasive + non_invasive)) * nmn_pneumonia * hosp_nmn * death_nmn)
    
    ## Non-invasive pneumonia (hospital treated)
    nipd_pneum_hosp_alive_coi <- with(probs_vac, (non_invasive / (invasive + non_invasive)) * pneumonia_noninv * hosp_pneumonia_noninv * (1 - hosp_pneumonia_nonipd_death))
    nipd_pneum_hosp_dead_coi <- with(probs_vac, (non_invasive / (invasive + non_invasive)) * pneumonia_noninv * hosp_pneumonia_noninv * hosp_pneumonia_nonipd_death)
    
    ## Non-invasive pneumonia (untreated)
    nipd_pneum_nohosp_alive_coi <- with(probs_vac, (non_invasive / (invasive + non_invasive)) * pneumonia_noninv * no_treated_pneumonia * (1 - untreated_pneumonia_death))
    nipd_pneum_nohosp_dead_coi <- with(probs_vac, (non_invasive / (invasive + non_invasive)) * pneumonia_noninv * no_treated_pneumonia * untreated_pneumonia_death)
    
    ## Otitis media (treated)
    nipd_aom_treated_alive_coi <- with(probs_vac, (non_invasive / (invasive + non_invasive)) * nipd_aom_media * treat_otitis)
    
    ## Otitis media (untreated)
    nipd_aom_untreated_alive_coi <- with(probs_vac, (non_invasive / (invasive + non_invasive)) * nipd_aom_media * (1 - treat_otitis))
    
    ##*************************************************************
    ## Compute totals
    ##*************************************************************
    
    ###############################
    ## Infections
    ###############################
    ## total, infected, alive
    p_total_infected_alive <- sum(
      c(
        ipd_men_alive_seq_vac, ipd_men_alive_noseq_vac, 
        ipd_pneum_alive_vac, ipd_nmn_alive_vac, nipd_pneum_hosp_alive_vac, 
        nipd_pneum_nohosp_alive_vac, nipd_aom_treated_alive_vac, 
        nipd_aom_untreated_alive_vac
      )
    ) +
    sum(
      c(
        ipd_men_alive_seq_novac, ipd_men_alive_noseq_novac,
        ipd_pneum_alive_novac, ipd_nmn_alive_novac,
        nipd_pneum_hosp_alive_novac,
        nipd_pneum_nohosp_alive_novac
      )
    )
    
    total_infected_alive <- p_total_infected_alive * cohort_alive
    
    total_prob <- ipd_men_alive_seq_vac + ipd_men_alive_noseq_vac + ipd_men_dead_vac +
      ipd_pneum_alive_vac + ipd_pneum_dead_vac +
      ipd_nmn_alive_vac + ipd_nmn_dead_vac +
      nipd_pneum_hosp_alive_vac + nipd_pneum_hosp_dead_vac +
      nipd_pneum_nohosp_alive_vac + nipd_pneum_nohosp_dead_vac +
      nipd_aom_treated_alive_vac + nipd_aom_untreated_alive_vac + noinf_vac
    
    ## total, infected, deceased
    p_total_infected_dead <- sum(
      c(
        ipd_men_dead_vac, ipd_pneum_dead_vac, ipd_nmn_dead_vac,
        nipd_pneum_hosp_dead_vac, nipd_pneum_nohosp_dead_vac
      )
    ) +
    sum(
      c(
        ipd_men_dead_novac, ipd_pneum_dead_novac, ipd_nmn_dead_novac,
        nipd_pneum_hosp_dead_novac, nipd_pneum_nohosp_dead_novac
      )
    )
    
    total_infected_dead <- p_total_infected_dead * cohort_alive
    
    ## total, infected
    total_infected <- total_infected_alive + total_infected_dead
    
    ## total, uninfected, alive
    total_uninfected_alive <- noinf * cohort_alive
    
    ## total, uninfected, deceased
    total_uninfected_dead <- 0
    
    ## total, uninfected
    total_uninfected <- total_uninfected_alive + total_uninfected_dead
    
    ###############################
    ## VACCINATED
    ###############################
    ## vaccinated
    p_total_vac <- sum(
      c(
        ipd_men_alive_seq_vac, ipd_men_alive_noseq_vac, ipd_men_dead_vac,
        ipd_pneum_alive_vac, ipd_pneum_dead_vac,
        ipd_nmn_alive_vac, ipd_nmn_dead_vac,
        nipd_pneum_hosp_alive_vac, nipd_pneum_hosp_dead_vac,
        nipd_pneum_nohosp_alive_vac, nipd_pneum_nohosp_dead_vac,
        nipd_aom_treated_alive_vac, nipd_aom_untreated_alive_vac, 
        noinf_vac
      )
    )
    total_vaccinated <- p_total_vac * cohort_alive
    
    ## vaccinated, alive
    p_total_vac_alive <- sum(
      c(
        ipd_men_alive_seq_vac, ipd_men_alive_noseq_vac,
        ipd_pneum_alive_vac, ipd_nmn_alive_vac,
        nipd_pneum_hosp_alive_vac, nipd_pneum_nohosp_alive_vac,
        nipd_aom_treated_alive_vac, nipd_aom_untreated_alive_vac, 
        noinf_vac
      )
    )
    total_vaccinated_alive <- p_total_vac_alive * cohort_alive
    
    ## vaccinated, deceased
    p_total_vac_dead <- sum(
      c(
        ipd_men_dead_vac, ipd_pneum_dead_vac, ipd_nmn_dead_vac,
        nipd_pneum_hosp_dead_vac, nipd_pneum_nohosp_dead_vac
      )
    )
    total_vaccinated_dead <- p_total_vac_dead * cohort_alive
    
    ## vaccinated, infected, alive and dead
    p_inf_vac <- sum(
      c(
        ipd_men_alive_seq_vac, ipd_men_alive_noseq_vac, ipd_men_dead_vac,
        ipd_pneum_alive_vac, ipd_pneum_dead_vac, ipd_nmn_alive_vac, ipd_nmn_dead_vac,
        nipd_pneum_hosp_alive_vac, nipd_pneum_hosp_dead_vac,
        nipd_pneum_nohosp_alive_vac, nipd_pneum_nohosp_dead_vac,
        nipd_aom_treated_alive_vac, nipd_aom_untreated_alive_vac
      )
    )
    total_infections_vaccinated <- p_inf_vac * cohort_alive
    
    ## vaccinated, uninfected, alive and dead
    p_uninf_vac <- noinf_vac
    total_uninfected_vaccinated <- p_uninf_vac * cohort_alive
    
    ## vaccinated, infected, alive
    p_inf_vac_alive <- sum(
      c(
        ipd_men_alive_seq_vac, ipd_men_alive_noseq_vac,
        ipd_pneum_alive_vac, ipd_nmn_alive_vac,
        nipd_pneum_hosp_alive_vac,
        nipd_pneum_nohosp_alive_vac,
        nipd_aom_treated_alive_vac, nipd_aom_untreated_alive_vac
      )
    )
    total_infections_vaccinated_alive <- p_inf_vac_alive * cohort_alive
    
    ## vaccinated, uninfected, alive
    p_uninf_vac_alive <- noinf_vac
    total_uninfected_vaccinated_alive <- p_uninf_vac_alive * cohort_alive
    
    ## vaccinated, infected, deceased
    p_inf_vac_dead <- sum(
      c(
        ipd_men_dead_vac, ipd_pneum_dead_vac, ipd_nmn_dead_vac,
        nipd_pneum_hosp_dead_vac, nipd_pneum_nohosp_dead_vac
      )
    )
    total_infections_vaccinated_dead <- p_inf_vac_dead * cohort_alive
    
    ## vaccinated, uninfected, deceased
    p_uninf_vac_dead <- 0
    total_uninfected_vaccinated_dead <- p_uninf_vac_dead * cohort_alive
    
    ###############################
    ## UNVACCINATED
    ###############################
    ## unvaccinated
    p_total_novac <- sum(
      c(
        ipd_men_alive_seq_novac, ipd_men_alive_noseq_novac, ipd_men_dead_novac,
        ipd_pneum_alive_novac, ipd_pneum_dead_novac, ipd_nmn_alive_novac, ipd_nmn_dead_novac,
        nipd_pneum_hosp_alive_novac, nipd_pneum_hosp_dead_novac,
        nipd_pneum_nohosp_alive_novac, nipd_pneum_nohosp_dead_novac,
        nipd_aom_treated_alive_novac, nipd_aom_untreated_alive_novac, 
        noinf_novac
      )
    )
    total_unvaccinated <- p_total_novac * cohort_alive
    
    ## unvaccinated, alive
    p_total_novac_alive <- sum(
      c(
        ipd_men_alive_seq_novac, ipd_men_alive_noseq_novac,
        ipd_pneum_alive_novac, ipd_nmn_alive_novac,
        nipd_pneum_hosp_alive_novac,
        nipd_pneum_nohosp_alive_novac,
        nipd_aom_treated_alive_novac, nipd_aom_untreated_alive_novac, 
        noinf_novac
      )
    )
    total_unvaccinated_alive <- p_total_novac_alive * cohort_alive
    
    ## unvaccinated, deceased
    p_total_novac_dead <- sum(
      c(
        ipd_men_dead_novac,
        ipd_pneum_dead_novac, ipd_nmn_dead_novac,
        nipd_pneum_hosp_dead_novac,
        nipd_pneum_nohosp_dead_novac
      )
    )
    total_unvaccinated_dead <- p_total_novac_dead * cohort_alive
    
    ## unvaccinated, infected, alive and dead
    p_inf_novac <- sum(
      c(
        ipd_men_alive_seq_novac, ipd_men_alive_noseq_novac, ipd_men_dead_novac,
        ipd_pneum_alive_novac, ipd_pneum_dead_novac, ipd_nmn_alive_novac, ipd_nmn_dead_novac,
        nipd_pneum_hosp_alive_novac, nipd_pneum_hosp_dead_novac,
        nipd_pneum_nohosp_alive_novac, nipd_pneum_nohosp_dead_novac,
        nipd_aom_treated_alive_novac, nipd_aom_untreated_alive_novac
      )
    )
    total_infections_unvaccinated <- p_inf_novac * cohort_alive
    
    ## unvaccinated, uninfected, alive and dead
    p_uninf_novac <- noinf_novac
    total_uninfected_unvaccinated <- p_uninf_novac * cohort_alive
    
    ## unvaccinated, infected, alive
    p_inf_novac_alive <- sum(
      c(
        ipd_men_alive_seq_novac, ipd_men_alive_noseq_novac,
        ipd_pneum_alive_novac, ipd_nmn_alive_novac,
        nipd_pneum_hosp_alive_novac,
        nipd_pneum_nohosp_alive_novac,
        nipd_aom_treated_alive_novac, nipd_aom_untreated_alive_novac, 
        noinf_novac
      )
    )
    total_infections_unvaccinated_alive <- p_inf_novac_alive * cohort_alive
    
    ## unvaccinated, uninfected, alive
    p_uninf_novac_alive <- noinf_novac
    total_uninfected_unvaccinated_alive <- p_uninf_novac_alive * cohort_alive
    
    ## unvaccinated, infected, deceased
    p_inf_novac_dead <- sum(
      c(
        ipd_men_dead_novac, ipd_pneum_dead_novac, ipd_nmn_dead_novac,
        nipd_pneum_hosp_dead_novac, nipd_pneum_nohosp_dead_novac
      )
    )
    total_infections_unvaccinated_dead <- p_inf_novac_dead * cohort_alive
    
    ## unvaccinated, uninfected, deceased
    p_uninf_novac_dead <- 0
    total_uninfected_unvaccinated_dead <- p_uninf_novac_dead * cohort_alive
    
    ###############################
    ## Infections for COI 
    ###############################
    ## total coi
    total_prob_coi <- sum(
      c(
        ipd_men_alive_seq_coi, ipd_men_alive_noseq_coi, ipd_men_dead_coi,
        ipd_pneum_alive_coi, ipd_pneum_dead_coi, 
        ipd_nmn_alive_coi, ipd_nmn_dead_coi, 
        nipd_pneum_hosp_alive_coi, nipd_pneum_hosp_dead_coi,
        nipd_pneum_nohosp_alive_coi, nipd_pneum_nohosp_dead_coi,
        nipd_aom_treated_alive_coi, 
        nipd_aom_untreated_alive_coi
      )
    ) 

    ###############################
    ## CASES
    ###############################
    ## Lives (cases)
    p_cases <- sum(
      c(
        ipd_men_alive_seq, ipd_men_alive_noseq, ipd_men_dead,
        ipd_pneum_alive, ipd_pneum_dead, ipd_nmn_alive, ipd_nmn_dead,
        nipd_pneum_hosp_alive, nipd_pneum_hosp_dead,
        nipd_pneum_nohosp_alive, nipd_pneum_nohosp_dead,
        nipd_aom_treated_alive, nipd_aom_untreated_alive
      )
    )
    total_cases <- p_cases * cohort_alive
    
    p_hosp <- sum(
      c(
        ipd_men_alive_seq, ipd_men_alive_noseq, ipd_men_dead,
        ipd_pneum_alive, ipd_pneum_dead, ipd_nmn_alive, ipd_nmn_dead,
        nipd_pneum_hosp_alive, nipd_pneum_hosp_dead
      )
    )
    total_hosp <- p_hosp * cohort_alive
    
    p_deaths <- sum(
      c(
        ipd_men_dead, ipd_pneum_dead, ipd_nmn_alive, ipd_nmn_dead,
        nipd_pneum_hosp_dead, nipd_pneum_nohosp_dead
      )
    )
    total_deaths <- p_deaths * cohort_alive
    
    ##**************************************************************************
    ## utilities
    ##**************************************************************************
    dalys <- list(
      Alive_meningitis_nosequelae = dw_meningitis_nosequelae * (los_days_meningitis / 365),
      Alive_meningitis_sequelae = sum(npv_calc(cash_flow = dw_meningitis_nosequelae, disc_rate = r_discount, time = time:(los_days_meningitis / 365))) +
        sum(npv_calc(cash_flow = dw_meningitis_sequelae, disc_rate = r_discount, time = (los_days_meningitis / 365):ceiling(le_age3))),
      Dead_meningitis = sum(npv_calc(cash_flow = 1, disc_rate = r_discount, time = time:ceiling(le_age3))),
      
      Alive_ipd_pneumo = dw_ipd_pneum * (los_days_ipd_pneum / 365),
      Dead_ipd_pneumo = sum(npv_calc(cash_flow = 1, disc_rate = r_discount, time = time:ceiling(le_age3))),
                  
      Alive_nmnp = dw_npnm * (los_days_nmnp / 365),
      Dead_nmnp = sum(npv_calc(cash_flow = 1, disc_rate = r_discount, time = time:ceiling(le_age3))),
                  
      Alive_nipd_pneumo_tx = dw_nipd_pneum * (los_days_nipd_pneum_tx / 365),
      Dead_nipd_pneumo_tx = sum(npv_calc(cash_flow = 1, disc_rate = r_discount, time = time:ceiling(le_age3))),
                  
      Alive_nipd_pneumo_notx = dw_nipd_pneum * (sick_days_nipd_pneum_notx / 365),
      Dead_nipd_pneumo_notx = sum(npv_calc(cash_flow = 1, disc_rate = r_discount, time = time:ceiling(le_age3))),
                  
      Alive_aom_tx = dw_aom * (sick_days_aom_tx / 365),
      Alive_aom_notx = dw_aom * (sick_days_aom_notx / 365)
    )
    
    ##**************************************************************************
    ## costs
    ##**************************************************************************
    ## https://www.aeaweb.org/articles?id=10.1257/pandp.20201017
    ## Q -- total number of doses procured
    ## Qmax -- the contracted amount eligible for the AMC subsidy with the manufacturer (not always equal to Q)
    ## theta = 0.20 --  fraction of doses eligible for AMC subsidy -- prop_amc_pay
    ## Pco (c_vaccine_copay) = country co-payment per subsidized dose (e.g., US$0.20: https://www.gavi.org/programmes-impact/programmatic-policies/co-financing-policy)
    ## ~$1.00-- https://pmc.ncbi.nlm.nih.gov/articles/PMC2647434/
    ## Ptail = price per dose after the subsidy ends (e.g., US$3.50 or less, negotiated under AMC terms): c_vaccine
    #Total country payment = min(Q, theta*Qmax)*Pco + max(0, Q - theta*Qmax)*Ptail
    
    ##************************************************
    ## direct costs
    ##************************************************
    ## vaccination costs
    c_vacc <- ifelse(isTRUE(add_pcv13), c_full_schedule_pcv13, 0) + 
      ifelse(strategy == "PPSV23", c_full_schedule_ppsv23, 0)
        
    #****************************
    ## direct medical care costs
    #****************************
    ## invasive pneumonococcal disease (ipd) meningitis
    c_hosp_meningitis <- los_days_meningitis * c_hosp_day_meningitis
    
    ## invasive pneumonococcal disease (ipd) pneumonia
    c_hosp_ipd_pneum <- los_days_ipd_pneum * c_hosp_day_ipd_pneum
    
    ## non-meningitis non-pneumonia (nmnp)
    c_hosp_nmnp <- los_days_nmnp * c_hosp_day_nmnp
    
    ## non-invasive pneumonococcal disease (nipd) pneumonia (non-invasive pneumonia), treated
    c_hosp_nipd_pneum_tx <- los_days_nipd_pneum_tx * c_hosp_day_pneum
    
    ## non-invasive pneumonococcal disease (nipd) pneumonia (non-invasive pneumonia), untreated
    c_hosp_nipd_pneum_notx <- c_direct_homecare_nipd_pneum 
    
    ## acute otitis media (aom), treated
    c_consult_aom_tx <- c_hosp_day_aom_consult
    
    ## acute otitis media (aom), untreated (0 by assumption)
    c_consult_aom_notx <- 0 
    
    #****************************
    ## direct non-medical costs; estimate these from the Ghana study
    #****************************
    ## invasive pneumonococcal disease (ipd) meningitis
    c_nonmed_meningitis <- c_direct_nonmed_meningitis
    
    ## invasive pneumonococcal disease (ipd) pneumonia
    c_nonmed_ipd_pneum <- c_direct_nonmed_inpatient_pneum
    
    ## non-meningitis non-pneumonia (nmnp) (0 by assumption)
    c_nonmed_nmnp <- 0 
    
    ## non-invasive pneumonococcal disease (nipd) pneumonia (non-invasive pneumonia), treated
    c_nonmed_nipd_pneum_tx <- c_direct_nonmed_inpatient_pneum
    
    ## non-invasive pneumonococcal disease (nipd) pneumonia (non-invasive pneumonia), untreated
    c_nonmed_nipd_pneum_notx <- c_direct_nonmed_homecare_pneum 
    
    ## acute otitis media (aom), treated (0 by assumption)
    c_nonmed_aom_tx <- 0
    
    ## acute otitis media (aom), untreated (0 by assumption)
    c_nonmed_aom_notx <- 0 
    
    ##****************************
    ## total direct costs
    ##****************************
    ## invasive pneumonococcal disease (ipd) meningitis
    c_direct_meningitis <- c_hosp_meningitis + c_nonmed_meningitis
    
    ## invasive pneumonococcal disease (ipd) pneumonia
    c_direct_ipd_pneum <- c_hosp_ipd_pneum + c_nonmed_ipd_pneum
    
    ## non-meningitis non-pneumonia (nmnp)
    c_direct_nmnp <- c_hosp_nmnp + c_nonmed_nmnp
    
    ## non-invasive pneumonococcal disease (nipd) pneumonia (non-invasive pneumonia), treated
    c_direct_nipd_pneum_tx <- c_hosp_nipd_pneum_tx + c_nonmed_nipd_pneum_tx
    
    ## non-invasive pneumonococcal disease (nipd) pneumonia (non-invasive pneumonia), untreated
    c_direct_nipd_pneum_notx <- c_hosp_nipd_pneum_notx + c_nonmed_nipd_pneum_notx
    
    ## acute otitis media (aom), treated
    c_direct_aom_tx <- c_consult_aom_tx + c_nonmed_aom_tx
    
    ## acute otitis media (aom), untreated
    c_direct_aom_notx <- c_consult_aom_notx + c_nonmed_aom_notx
    
    ##************************************************
    ## indirect costs
    ##************************************************
    ##****************************
    ## caregiving costs: 
    ## 1) lost work time for caregivers during illness/recovery
    ## 2) lost work time for caregivers during child's lifetime (only for caregivers of those with sequelae)
    ##    i.e., productivity loss by caregivers (for children with lifetime disability) 
    ##****************************
    ## invasive pneumonococcal disease (ipd) meningitis, with sequelae
    c_caregiver_meningitis_seq <- array(data = c_caregiver_meningitis, dim = length(0:ceiling(le_age3)))
    c_caregiver_meningitis_seq <- sum(npv_calc(cash_flow = c_caregiver_meningitis_seq, disc_rate = r_discount, time = 0:ceiling(le_age3)))
    
    ## invasive pneumonococcal disease (ipd) meningitis, without sequelae
    c_caregiver_meningitis_noseq <- c_caregiver_meningitis_noseq

    ## invasive pneumonococcal disease (ipd) pneumonia
    c_caregiver_ipd_pneum <- c_caregiver_ipd_pneum

    ## non-meningitis non-pneumonia (nmnp) (0 by assumption)
    c_caregiver_nmnp <- 0 
    
    ## non-invasive pneumonococcal disease (nipd) pneumonia, treated
    c_caregiver_nipd_pneum_tx <- c_caregiver_nipd_pneum
    
    ## non-invasive pneumonococcal disease (nipd) pneumonia, untreated
    c_caregiver_nipd_pneum_notx <- c_caregiver_nipd_pneum
    
    ## acute otitis media (aom), treated (0 by assumption)
    c_caregiver_aom_tx <- 0
    
    ## acute otitis media (aom), untreated (0 by assumption)
    c_caregiver_aom_notx <- 0
    
    ##****************************
    ## productivity costs, human capital approach:
    ## 1) Reduced earning potential due to sequelae (child; kicks in after legal work age)
    ## 2) Lost future earnings due to early death (lifetime earnings lost for the child; kicks in after legal work age)
    ##****************************
    work_age_index <- seq(from = (age_legal_work_start - 3), to = (age_retirement - 3), by = 1)
    leis_age_index <- c(seq(from = 1, to = (age_legal_work_start - 3), by = 1), seq(from = (age_retirement - 3), to = ceiling(le_age3), by = 1))

    ## invasive pneumonococcal disease (ipd) meningitis, with sequelae
    c_prod_meningitis_hc_seq <- array(data = 0, dim = length(0:ceiling(le_age3)))
    c_prod_meningitis_hc_seq[work_age_index] <- ind_cost_hc_func(pc_gdp = gdp_per_capita, lis = labor_income_gdp_share, lfpr = lfpr, pls = prop_prod_loss_seq)
    c_prod_meningitis_hc_seq[leis_age_index] <- ind_cost_leis_hc_func(pc_gdp = gdp_per_capita, lis = labor_income_gdp_share, lfpr = lfpr, pls = prop_prod_loss_seq, p_leis = prop_leisure_time)
    c_prod_meningitis_hc_seq <- sum(npv_calc(cash_flow = c_prod_meningitis_hc_seq, disc_rate = r_discount, time = 0:ceiling(le_age3)))
    
    ## invasive pneumonococcal disease (ipd) meningitis, without sequelae
    c_prod_meningitis_hc_noseq <- ind_cost_leis_hc_func(pc_gdp = gdp_per_capita, lis = labor_income_gdp_share, lfpr = lfpr, pls = prop_prod_loss_seq, p_leis = prop_leisure_time)
    ##print(c(c_caregiver_meningitis_seq, c_caregiver_meningitis_noseq, c_prod_meningitis_hc_seq, c_prod_meningitis_hc_noseq))
    
    ## invasive pneumonococcal disease (ipd) meningitis, deceased
    c_prod_meningitis_hc_death <- array(data = 0, dim = length(0:ceiling(le_age3)))
    c_prod_meningitis_hc_death[work_age_index] <- ind_cost_hc_func(pc_gdp = gdp_per_capita, lis = labor_income_gdp_share, lfpr = lfpr, pls = prop_prod_loss_seq)
    c_prod_meningitis_hc_death[leis_age_index] <- ind_cost_leis_hc_func(pc_gdp = gdp_per_capita, lis = labor_income_gdp_share, lfpr = lfpr, pls = prop_prod_loss_seq, p_leis = prop_leisure_time)
    c_prod_meningitis_hc_death <- sum(npv_calc(cash_flow = c_prod_meningitis_hc_death, disc_rate = r_discount, time = 0:ceiling(le_age3)))
    
    ## invasive pneumonococcal disease (ipd) pneumonia, alive
    c_prod_ipd_pneum_hc_alive <- ind_cost_leis_hc_func(pc_gdp = gdp_per_capita, lis = labor_income_gdp_share, lfpr = lfpr, pls = prop_prod_loss_seq, p_leis = prop_leisure_time)
    
    ## invasive pneumonococcal disease (ipd) pneumonia, deceased
    c_prod_ipd_pneum_hc_death <- array(data = 0, dim = length(0:ceiling(le_age3)))
    c_prod_ipd_pneum_hc_death[work_age_index] <- ind_cost_hc_func(pc_gdp = gdp_per_capita, lis = labor_income_gdp_share, lfpr = lfpr, pls = prop_prod_loss_seq)
    c_prod_ipd_pneum_hc_death[leis_age_index] <- ind_cost_leis_hc_func(pc_gdp = gdp_per_capita, lis = labor_income_gdp_share, lfpr = lfpr, pls = prop_prod_loss_seq, p_leis = prop_leisure_time)
    c_prod_ipd_pneum_hc_death <- sum(npv_calc(cash_flow = c_prod_ipd_pneum_hc_death, disc_rate = r_discount, time = 0:ceiling(le_age3)))
    
    ## non-meningitis non-pneumonia (nmnp), alive
    c_prod_nmnp_hc_alive <- ind_cost_leis_hc_func(pc_gdp = gdp_per_capita, lis = labor_income_gdp_share, lfpr = lfpr, pls = prop_prod_loss_seq, p_leis = prop_leisure_time)
    
    ## non-meningitis non-pneumonia (nmnp), deceased
    c_prod_nmnp_hc_death <- array(data = 0, dim = length(0:ceiling(le_age3)))
    c_prod_nmnp_hc_death[work_age_index] <- ind_cost_hc_func(pc_gdp = gdp_per_capita, lis = labor_income_gdp_share, lfpr = lfpr, pls = prop_prod_loss_seq)
    c_prod_nmnp_hc_death[leis_age_index] <- ind_cost_leis_hc_func(pc_gdp = gdp_per_capita, lis = labor_income_gdp_share, lfpr = lfpr, pls = prop_prod_loss_seq, p_leis = prop_leisure_time)
    c_prod_nmnp_hc_death <- sum(npv_calc(cash_flow = c_prod_nmnp_hc_death, disc_rate = r_discount, time = 0:ceiling(le_age3)))
    
    ## non-invasive pneumonococcal disease (nipd) pneumonia, treated, alive
    c_prod_nipd_pneum_hc_tx_alive <- ind_cost_leis_hc_func(pc_gdp = gdp_per_capita, lis = labor_income_gdp_share, lfpr = lfpr, pls = prop_prod_loss_seq, p_leis = prop_leisure_time)
    
    ## non-invasive pneumonococcal disease (nipd) pneumonia, treated, deceased
    c_prod_nipd_pneum_hc_tx_death <- array(data = 0, dim = length(0:ceiling(le_age3)))
    c_prod_nipd_pneum_hc_tx_death[work_age_index] <- ind_cost_hc_func(pc_gdp = gdp_per_capita, lis = labor_income_gdp_share, lfpr = lfpr, pls = prop_prod_loss_seq)
    c_prod_nipd_pneum_hc_tx_death[leis_age_index] <- ind_cost_leis_hc_func(pc_gdp = gdp_per_capita, lis = labor_income_gdp_share, lfpr = lfpr, pls = prop_prod_loss_seq, p_leis = prop_leisure_time)
    c_prod_nipd_pneum_hc_tx_death <- sum(npv_calc(cash_flow = c_prod_nipd_pneum_hc_tx_death, disc_rate = r_discount, time = 0:ceiling(le_age3)))
    
    ## non-invasive pneumonococcal disease (nipd) pneumonia, untreated, alive
    c_prod_nipd_pneum_hc_notx_alive <- ind_cost_leis_hc_func(pc_gdp = gdp_per_capita, lis = labor_income_gdp_share, lfpr = lfpr, pls = prop_prod_loss_seq, p_leis = prop_leisure_time)
    
    ## non-invasive pneumonococcal disease (nipd) pneumonia, untreated, deceased
    c_prod_nipd_pneum_hc_notx_death <- array(data = 0, dim = length(0:ceiling(le_age3)))
    c_prod_nipd_pneum_hc_notx_death[work_age_index] <- ind_cost_hc_func(pc_gdp = gdp_per_capita, lis = labor_income_gdp_share, lfpr = lfpr, pls = prop_prod_loss_seq)
    c_prod_nipd_pneum_hc_notx_death[leis_age_index] <- ind_cost_leis_hc_func(pc_gdp = gdp_per_capita, lis = labor_income_gdp_share, lfpr = lfpr, pls = prop_prod_loss_seq, p_leis = prop_leisure_time)
    c_prod_nipd_pneum_hc_notx_death <- sum(npv_calc(cash_flow = c_prod_nipd_pneum_hc_notx_death, disc_rate = r_discount, time = 0:ceiling(le_age3)))
    
    ## acute otitis media (aom), treated, alive
    c_prod_aom_hc_tx <- ind_cost_leis_hc_func(pc_gdp = gdp_per_capita, lis = labor_income_gdp_share, lfpr = lfpr, pls = prop_prod_loss_seq, p_leis = prop_leisure_time)
    
    ## acute otitis media (aom), untreated, alive
    c_prod_aom_hc_notx <- ind_cost_leis_hc_func(pc_gdp = gdp_per_capita, lis = labor_income_gdp_share, lfpr = lfpr, pls = prop_prod_loss_seq, p_leis = prop_leisure_time)
    
    ##****************************
    ## productivity costs, VSLY approach
    ##****************************
    ## invasive pneumonococcal disease (ipd) meningitis, with sequelae
    c_prod_meningitis_vsly_seq_y <- ind_cost_vsly_func(pc_gdp = gdp_per_capita, vsly_gdp_pc = vsly_gdp_multiplier, inf_vsly_mult = vsly_infant_multiplier, dalys = dw_meningitis_sequelae)
    c_prod_meningitis_vsly_seq <- array(data = c_prod_meningitis_vsly_seq_y, dim = length(0:ceiling(le_age3)))
    c_prod_meningitis_vsly_seq <- sum(npv_calc(cash_flow = c_prod_meningitis_vsly_seq, disc_rate = r_discount, time = 0:ceiling(le_age3)))
    
    ## invasive pneumonococcal disease (ipd) meningitis, without sequelae
    c_prod_meningitis_vsly_noseq <- ind_cost_vsly_func(pc_gdp = gdp_per_capita, vsly_gdp_pc = vsly_gdp_multiplier, inf_vsly_mult = vsly_infant_multiplier, dalys = dw_meningitis_nosequelae * (los_days_meningitis/365))
    
    ## invasive pneumonococcal disease (ipd) meningitis, death
    c_prod_meningitis_vsly_death_y <- ind_cost_vsly_func(pc_gdp = gdp_per_capita, vsly_gdp_pc = vsly_gdp_multiplier, inf_vsly_mult = vsly_infant_multiplier, dalys = 1)
    c_prod_meningitis_vsly_death <- array(data = c_prod_meningitis_vsly_death_y, dim = length(0:ceiling(le_age3)))
    c_prod_meningitis_vsly_death <- sum(npv_calc(cash_flow = c_prod_meningitis_vsly_death, disc_rate = r_discount, time = 0:ceiling(le_age3)))
    
    ## invasive pneumonococcal disease (ipd) pneumonia, alive
    c_prod_ipd_pneum_vsly_alive <- ind_cost_vsly_func(pc_gdp = gdp_per_capita, vsly_gdp_pc = vsly_gdp_multiplier, inf_vsly_mult = vsly_infant_multiplier, dalys = dw_ipd_pneum * (los_days_ipd_pneum/365))
    
    ## invasive pneumonococcal disease (ipd) pneumonia, death
    c_prod_ipd_pneum_vsly_death_y <- ind_cost_vsly_func(pc_gdp = gdp_per_capita, vsly_gdp_pc = vsly_gdp_multiplier, inf_vsly_mult = vsly_infant_multiplier, dalys = 1)
    c_prod_ipd_pneum_vsly_death <- array(data = c_prod_ipd_pneum_vsly_death_y, dim = length(0:ceiling(le_age3)))
    c_prod_ipd_pneum_vsly_death <- sum(npv_calc(cash_flow = c_prod_ipd_pneum_vsly_death, disc_rate = r_discount, time = 0:ceiling(le_age3)))
    
    ## non-meningitis non-pneumonia (nmnp), alive
    c_prod_nmnp_vsly_alive <- ind_cost_vsly_func(pc_gdp = gdp_per_capita, vsly_gdp_pc = vsly_gdp_multiplier, inf_vsly_mult = vsly_infant_multiplier, dalys = dw_npnm * (los_days_nmnp/365))
    
    ## non-meningitis non-pneumonia (nmnp), death
    c_prod_nmnp_vsly_death_y <- ind_cost_vsly_func(pc_gdp = gdp_per_capita, vsly_gdp_pc = vsly_gdp_multiplier, inf_vsly_mult = vsly_infant_multiplier, dalys = 1)
    c_prod_nmnp_vsly_death <- array(data = c_prod_nmnp_vsly_death_y, dim = length(0:ceiling(le_age3)))
    c_prod_nmnp_vsly_death <- sum(npv_calc(cash_flow = c_prod_nmnp_vsly_death, disc_rate = r_discount, time = 0:ceiling(le_age3)))
    
    ## non-invasive pneumonococcal disease (nipd) pneumonia, treated, alive
    c_prod_nipd_pneum_vsly_tx_alive <- ind_cost_vsly_func(pc_gdp = gdp_per_capita, vsly_gdp_pc = vsly_gdp_multiplier, inf_vsly_mult = vsly_infant_multiplier, dalys = dw_nipd_pneum * (los_days_nipd_pneum_tx/365))
    
    ## non-invasive pneumonococcal disease (nipd) pneumonia, treated, deceased
    c_prod_nipd_pneum_vsly_tx_death_y <- ind_cost_vsly_func(pc_gdp = gdp_per_capita, vsly_gdp_pc = vsly_gdp_multiplier, inf_vsly_mult = vsly_infant_multiplier, dalys = 1)
    c_prod_nipd_pneum_vsly_tx_death <- array(data = c_prod_nipd_pneum_vsly_tx_death_y, dim = length(0:ceiling(le_age3)))
    c_prod_nipd_pneum_vsly_tx_death <- sum(npv_calc(cash_flow = c_prod_nipd_pneum_vsly_tx_death, disc_rate = r_discount, time = 0:ceiling(le_age3)))
    
    ## non-invasive pneumonococcal disease (nipd) pneumonia, untreated, alive
    c_prod_nipd_pneum_vsly_notx_alive <- ind_cost_vsly_func(pc_gdp = gdp_per_capita, vsly_gdp_pc = vsly_gdp_multiplier, inf_vsly_mult = vsly_infant_multiplier, dalys = dw_nipd_pneum * (sick_days_nipd_pneum_notx/365))
    
    ## non-invasive pneumonococcal disease (nipd) pneumonia, untreated, deceased
    c_prod_nipd_pneum_vsly_notx_death_y <- ind_cost_vsly_func(pc_gdp = gdp_per_capita, vsly_gdp_pc = vsly_gdp_multiplier, inf_vsly_mult = vsly_infant_multiplier, dalys = 1)
    c_prod_nipd_pneum_vsly_notx_death <- array(data = c_prod_nipd_pneum_vsly_notx_death_y, dim = length(0:ceiling(le_age3)))
    c_prod_nipd_pneum_vsly_notx_death <- sum(npv_calc(cash_flow = c_prod_nipd_pneum_vsly_notx_death, disc_rate = r_discount, time = 0:ceiling(le_age3)))
    
    ## acute otitis media (aom), treated, alive
    c_prod_aom_vsly_tx <- ind_cost_vsly_func(pc_gdp = gdp_per_capita, vsly_gdp_pc = vsly_gdp_multiplier, inf_vsly_mult = vsly_infant_multiplier, dalys = dw_aom * (sick_days_aom_tx/365))
    
    ## acute otitis media (aom), untreated, alive
    c_prod_aom_vsly_notx <- ind_cost_vsly_func(pc_gdp = gdp_per_capita, vsly_gdp_pc = vsly_gdp_multiplier, inf_vsly_mult = vsly_infant_multiplier, dalys = dw_aom * (sick_days_aom_notx/365))
    
    ##****************************
    ## total indirect costs: human capital approach
    ##****************************
    ## invasive pneumonococcal disease (ipd) meningitis, with sequelae
    c_indirect_meningitis_hc_seq <- c_caregiver_meningitis_seq + c_prod_meningitis_hc_seq
    
    ## invasive pneumonococcal disease (ipd) meningitis, without sequelae
    c_indirect_meningitis_hc_noseq <- c_caregiver_meningitis_noseq + c_prod_meningitis_hc_noseq
    
    ## invasive pneumonococcal disease (ipd) meningitis, deceased
    c_indirect_meningitis_hc_death <- c_caregiver_meningitis_seq + c_prod_meningitis_hc_death
    
    ## invasive pneumonococcal disease (ipd) pneumonia, alive
    c_indirect_ipd_pneum_hc_alive <- c_caregiver_ipd_pneum + c_prod_ipd_pneum_hc_alive
    
    ## invasive pneumonococcal disease (ipd) pneumonia, deceased
    c_indirect_ipd_pneum_hc_death <- c_caregiver_ipd_pneum + c_prod_ipd_pneum_hc_death
    
    ## non-meningitis non-pneumonia (nmnp), alive
    c_indirect_nmnp_hc_alive <- c_caregiver_nmnp + c_prod_nmnp_hc_alive
    
    ## non-meningitis non-pneumonia (nmnp), deceased
    c_indirect_nmnp_hc_death <- c_caregiver_nmnp + c_prod_nmnp_hc_death
    
    ## non-invasive pneumonococcal disease (nipd) pneumonia, treated, alive
    c_indirect_nipd_pneum_hc_tx_alive <- c_caregiver_nipd_pneum_tx + c_prod_nipd_pneum_hc_tx_alive
    
    ## non-invasive pneumonococcal disease (nipd) pneumonia, treated, deceased
    c_indirect_nipd_pneum_hc_tx_death <- c_caregiver_nipd_pneum_tx + c_prod_nipd_pneum_hc_tx_death
    
    ## non-invasive pneumonococcal disease (nipd) pneumonia, untreated, alive
    c_indirect_nipd_pneum_hc_notx_alive <- c_caregiver_nipd_pneum_notx + c_prod_nipd_pneum_hc_notx_alive
    
    ## non-invasive pneumonococcal disease (nipd) pneumonia, untreated, deceased
    c_indirect_nipd_pneum_hc_notx_death <- c_caregiver_nipd_pneum_notx + c_prod_nipd_pneum_hc_notx_death
    
    ## acute otitis media (aom), treated, alive
    c_indirect_aom_hc_tx_alive <- c_caregiver_aom_tx + c_prod_aom_hc_tx
    
    ## acute otitis media (aom), untreated, alive
    c_indirect_aom_hc_notx_alive <- c_caregiver_aom_notx + c_prod_aom_hc_notx
    
    ##****************************
    ## total indirect costs: vsly approach
    ##****************************
    ## invasive pneumonococcal disease (ipd) meningitis, with sequelae
    c_indirect_meningitis_vsly_seq <- c_caregiver_meningitis_seq + c_prod_meningitis_vsly_seq
    
    ## invasive pneumonococcal disease (ipd) meningitis, without sequelae
    c_indirect_meningitis_vsly_noseq <- c_caregiver_meningitis_noseq + c_prod_meningitis_vsly_noseq
    
    ## invasive pneumonococcal disease (ipd) meningitis, deceased
    c_indirect_meningitis_vsly_death <- c_caregiver_meningitis_seq + c_prod_meningitis_vsly_death
    
    ## invasive pneumonococcal disease (ipd) pneumonia, alive
    c_indirect_ipd_pneum_vsly_alive <- c_caregiver_ipd_pneum + c_prod_ipd_pneum_vsly_alive
    
    ## invasive pneumonococcal disease (ipd) pneumonia, deceased
    c_indirect_ipd_pneum_vsly_death <- c_caregiver_ipd_pneum + c_prod_ipd_pneum_vsly_death
    
    ## non-meningitis non-pneumonia (nmnp), alive
    c_indirect_nmnp_vsly_alive <- c_caregiver_nmnp + c_prod_nmnp_vsly_alive
    
    ## non-meningitis non-pneumonia (nmnp), deceased
    c_indirect_nmnp_vsly_death <- c_caregiver_nmnp + c_prod_nmnp_vsly_death
    
    ## non-invasive pneumonococcal disease (nipd) pneumonia, treated, alive
    c_indirect_nipd_pneum_vsly_tx_alive <- c_caregiver_nipd_pneum_tx + c_prod_nipd_pneum_vsly_tx_alive
    
    ## non-invasive pneumonococcal disease (nipd) pneumonia, treated, deceased
    c_indirect_nipd_pneum_vsly_tx_death <- c_caregiver_nipd_pneum_tx + c_prod_nipd_pneum_vsly_tx_death
    
    ## non-invasive pneumonococcal disease (nipd) pneumonia, untreated, alive
    c_indirect_nipd_pneum_vsly_notx_alive <- c_caregiver_nipd_pneum_notx + c_prod_nipd_pneum_vsly_notx_alive
    
    ## non-invasive pneumonococcal disease (nipd) pneumonia, untreated, deceased
    c_indirect_nipd_pneum_vsly_notx_death <- c_caregiver_nipd_pneum_notx + c_prod_nipd_pneum_vsly_notx_death
    
    ## acute otitis media (aom), treated, alive
    c_indirect_aom_vsly_tx_alive <- c_caregiver_aom_tx + c_prod_aom_vsly_tx
    
    ## acute otitis media (aom), untreated, alive
    c_indirect_aom_vsly_notx_alive <- c_caregiver_aom_notx + c_prod_aom_vsly_notx
    
    ##************************************************
    ## Costs list
    ##************************************************
    ## vaccination costs
    total_vacc_costs <- list(
      Alive_meningitis_nosequelae = c_vacc,
      Alive_meningitis_sequelae = c_vacc,
      Dead_meningitis = c_vacc,
      ##
      Alive_ipd_pneum = c_vacc,
      Dead_ipd_pneum = c_vacc,
      ##
      Alive_nmnp = c_vacc,
      Dead_nmnp = c_vacc,
      ##
      Alive_nipd_pneum_tx = c_vacc,
      Dead_nipd_pneum_tx = c_vacc,
      ##
      Alive_nipd_pneum_notx = c_vacc,
      Dead_nipd_pneum_notx = c_vacc,
      ##
      Alive_aom_tx = c_vacc,
      Alive_aom_notx = c_vacc
    )
    
    ## direct costs
    total_direct_costs <- list(
      Alive_meningitis_nosequelae = c_direct_meningitis,
      Alive_meningitis_sequelae = c_direct_meningitis,
      Dead_meningitis = c_direct_meningitis,
      ##
      Alive_ipd_pneum = c_direct_ipd_pneum,
      Dead_ipd_pneum = c_direct_ipd_pneum,
      ##
      Alive_nmnp = c_direct_nmnp,
      Dead_nmnp = c_direct_nmnp,
      ##
      Alive_nipd_pneum_tx = c_direct_nipd_pneum_tx,
      Dead_nipd_pneum_tx = c_direct_nipd_pneum_tx,
      ##
      Alive_nipd_pneum_notx = c_direct_nipd_pneum_notx,
      Dead_nipd_pneum_notx = c_direct_nipd_pneum_notx,
      ##
      Alive_aom_tx = c_direct_aom_tx,
      Alive_aom_notx = c_direct_aom_notx
    )
    
    ## indirect costs, human capital
    total_indirect_costs_hc <- list(
      Alive_meningitis_nosequelae = c_indirect_meningitis_hc_noseq,
      Alive_meningitis_sequelae = c_indirect_meningitis_hc_seq,
      Dead_meningitis = c_indirect_meningitis_hc_death,
      ##
      Alive_ipd_pneum = c_indirect_ipd_pneum_hc_alive,
      Dead_ipd_pneum = c_indirect_ipd_pneum_hc_death,
      ##
      Alive_nmnp = c_indirect_nmnp_hc_alive,
      Dead_nmnp = c_indirect_nmnp_hc_death,
      ##
      Alive_nipd_pneum_tx = c_indirect_nipd_pneum_hc_tx_alive,
      Dead_nipd_pneum_tx = c_indirect_nipd_pneum_hc_tx_death,
      ##
      Alive_nipd_pneum_notx = c_indirect_nipd_pneum_hc_notx_alive,
      Dead_nipd_pneum_notx = c_indirect_nipd_pneum_hc_notx_death,
      ##
      Alive_aom_tx = c_indirect_aom_hc_tx_alive,
      Alive_aom_notx = c_indirect_aom_hc_notx_alive
    )
    
    ## indirect costs, vsly
    total_indirect_costs_vsly <- list(
      Alive_meningitis_nosequelae = c_indirect_meningitis_vsly_noseq,
      Alive_meningitis_sequelae = c_indirect_meningitis_vsly_seq,
      Dead_meningitis = c_indirect_meningitis_vsly_death,
      ##
      Alive_ipd_pneum = c_indirect_ipd_pneum_vsly_alive,
      Dead_ipd_pneum = c_indirect_ipd_pneum_vsly_death,
      ##
      Alive_nmnp = c_indirect_nmnp_vsly_alive,
      Dead_nmnp = c_indirect_nmnp_vsly_death,
      ##
      Alive_nipd_pneum_tx = c_indirect_nipd_pneum_vsly_tx_alive,
      Dead_nipd_pneum_tx = c_indirect_nipd_pneum_vsly_tx_death,
      ##
      Alive_nipd_pneum_notx = c_indirect_nipd_pneum_vsly_notx_alive,
      Dead_nipd_pneum_notx = c_indirect_nipd_pneum_vsly_notx_death,
      ##
      Alive_aom_tx = c_indirect_aom_vsly_tx_alive,
      Alive_aom_notx = c_indirect_aom_vsly_notx_alive
    )

    ## human capital
    total_costs_hc <- list(
      Alive_meningitis_nosequelae = sum(c(c_vacc, c_direct_meningitis, c_indirect_meningitis_hc_noseq)),
      Alive_meningitis_sequelae = sum(c(c_vacc, c_direct_meningitis, c_indirect_meningitis_hc_seq)),
      Dead_meningitis = sum(c(c_vacc, c_direct_meningitis, c_indirect_meningitis_hc_death)),
      ##
      Alive_ipd_pneum = sum(c(c_vacc, c_direct_ipd_pneum, c_indirect_ipd_pneum_hc_alive)),
      Dead_ipd_pneum = sum(c(c_vacc, c_direct_ipd_pneum, c_indirect_ipd_pneum_hc_death)),
      ##
      Alive_nmnp = sum(c(c_vacc, c_direct_nmnp, c_indirect_nmnp_hc_alive)),
      Dead_nmnp = sum(c(c_vacc, c_direct_nmnp, c_indirect_nmnp_hc_death)),
      ##
      Alive_nipd_pneum_tx = sum(c(c_vacc, c_direct_nipd_pneum_tx, c_indirect_nipd_pneum_hc_tx_alive)),
      Dead_nipd_pneum_tx = sum(c(c_vacc, c_direct_nipd_pneum_tx, c_indirect_nipd_pneum_hc_tx_death)),
      ##
      Alive_nipd_pneum_notx = sum(c(c_vacc, c_direct_nipd_pneum_notx, c_indirect_nipd_pneum_hc_notx_alive)),
      Dead_nipd_pneum_notx = sum(c(c_vacc, c_direct_nipd_pneum_notx, c_indirect_nipd_pneum_hc_notx_death)),
      ##
      Alive_aom_tx = sum(c(c_vacc, c_direct_aom_tx, c_indirect_aom_hc_tx_alive)),
      Alive_aom_notx = sum(c(c_vacc, c_direct_aom_notx, c_indirect_aom_hc_notx_alive))
    )
    
    ## vsly
    total_costs_vsly <- list(
      Alive_meningitis_nosequelae = sum(c(c_vacc, c_direct_meningitis, c_indirect_meningitis_vsly_noseq)),
      Alive_meningitis_sequelae = sum(c(c_vacc, c_direct_meningitis, c_indirect_meningitis_vsly_seq)),
      Dead_meningitis = sum(c(c_vacc, c_direct_meningitis, c_indirect_meningitis_vsly_death)),
      ##
      Alive_ipd_pneum = sum(c(c_vacc, c_direct_ipd_pneum, c_indirect_ipd_pneum_vsly_alive)),
      Dead_ipd_pneum = sum(c(c_vacc, c_direct_ipd_pneum, c_indirect_ipd_pneum_vsly_death)),
      ##
      Alive_nmnp = sum(c(c_vacc, c_direct_nmnp, c_indirect_nmnp_vsly_alive)),
      Dead_nmnp = sum(c(c_vacc, c_direct_nmnp, c_indirect_nmnp_vsly_death)),
      ##
      Alive_nipd_pneum_tx = sum(c(c_vacc, c_direct_nipd_pneum_tx, c_indirect_nipd_pneum_vsly_tx_alive)),
      Dead_nipd_pneum_tx = sum(c(c_vacc, c_direct_nipd_pneum_tx, c_indirect_nipd_pneum_vsly_tx_death)),
      ##
      Alive_nipd_pneum_notx = sum(c(c_vacc, c_direct_nipd_pneum_notx, c_indirect_nipd_pneum_vsly_notx_alive)),
      Dead_nipd_pneum_notx = sum(c(c_vacc, c_direct_nipd_pneum_notx, c_indirect_nipd_pneum_vsly_notx_death)),
      ##
      Alive_aom_tx = sum(c(c_vacc, c_direct_aom_tx, c_indirect_aom_vsly_tx_alive)),
      Alive_aom_notx = sum(c(c_vacc, c_direct_aom_notx, c_indirect_aom_vsly_notx_alive))
    )
    
    ##*************************************************************
    ## Compute totals
    ##*************************************************************
    ## vaccination costs
    total_cost_vacc <- (
      ipd_men_alive_seq_vac * total_vacc_costs$Alive_meningitis_sequelae +
      ipd_men_alive_noseq_vac * total_vacc_costs$Alive_meningitis_nosequelae +
      ipd_men_dead_vac * total_vacc_costs$Dead_meningitis +
      ipd_pneum_alive_vac * total_vacc_costs$Alive_ipd_pneum +
      ipd_pneum_dead_vac * total_vacc_costs$Dead_ipd_pneum +
      ipd_nmn_alive_vac * total_vacc_costs$Alive_nmnp +
      ipd_nmn_dead_vac * total_vacc_costs$Dead_nmnp +
      nipd_pneum_hosp_alive_vac * total_vacc_costs$Alive_nipd_pneum_tx +
      nipd_pneum_hosp_dead_vac * total_vacc_costs$Dead_nipd_pneum_tx +
      nipd_pneum_nohosp_alive_vac * total_vacc_costs$Alive_nipd_pneum_notx +
      nipd_pneum_nohosp_dead_vac * total_vacc_costs$Dead_nipd_pneum_notx +
      nipd_aom_treated_alive_vac * total_vacc_costs$Alive_aom_tx +
      nipd_aom_untreated_alive_vac * total_vacc_costs$Alive_aom_notx +
      noinf_vac * total_vacc_costs$Alive_aom_notx
    ) * scd_birth_cohort + 
      c_adj_subscription_fee_vaccine_ppsv23
    
    ## other direct costs
    total_cost_direct <- (
      ipd_men_alive_seq * total_direct_costs$Alive_meningitis_sequelae +
      ipd_men_alive_noseq * total_direct_costs$Alive_meningitis_nosequelae +
      ipd_men_dead * total_direct_costs$Dead_meningitis +
      ipd_pneum_alive * total_direct_costs$Alive_ipd_pneum +
      ipd_pneum_dead * total_direct_costs$Dead_ipd_pneum +
      ipd_nmn_alive * total_direct_costs$Alive_nmnp +
      ipd_nmn_dead * total_direct_costs$Dead_nmnp +
      nipd_pneum_hosp_alive * total_direct_costs$Alive_nipd_pneum_tx +
      nipd_pneum_hosp_dead * total_direct_costs$Dead_nipd_pneum_tx +
      nipd_pneum_nohosp_alive * total_direct_costs$Alive_nipd_pneum_notx +
      nipd_pneum_nohosp_dead * total_direct_costs$Dead_nipd_pneum_notx +
      nipd_aom_treated_alive * total_direct_costs$Alive_aom_tx +
      nipd_aom_untreated_alive * total_direct_costs$Alive_aom_notx +
      noinf * total_direct_costs$Alive_aom_notx
    ) * cohort_alive

    ## other direct costs, coi
    expected_cost_direct_coi <- (
      ipd_men_alive_seq_coi * total_direct_costs$Alive_meningitis_sequelae +
      ipd_men_alive_noseq_coi * total_direct_costs$Alive_meningitis_nosequelae +
      ipd_men_dead_coi * total_direct_costs$Dead_meningitis +
      ipd_pneum_alive_coi * total_direct_costs$Alive_ipd_pneum +
      ipd_pneum_dead_coi * total_direct_costs$Dead_ipd_pneum +
      ipd_nmn_alive_coi * total_direct_costs$Alive_nmnp +
      ipd_nmn_dead_coi * total_direct_costs$Dead_nmnp +
      nipd_pneum_hosp_alive_coi * total_direct_costs$Alive_nipd_pneum_tx +
      nipd_pneum_hosp_dead_coi * total_direct_costs$Dead_nipd_pneum_tx +
      nipd_pneum_nohosp_alive_coi * total_direct_costs$Alive_nipd_pneum_notx +
      nipd_pneum_nohosp_dead_coi * total_direct_costs$Dead_nipd_pneum_notx +
      nipd_aom_treated_alive_coi * total_direct_costs$Alive_aom_tx +
      nipd_aom_untreated_alive_coi * total_direct_costs$Alive_aom_notx
    )

    ## indirect costs (human capital approach)
    total_cost_indirect_hc <- (
      ipd_men_alive_seq * total_indirect_costs_hc$Alive_meningitis_sequelae +
      ipd_men_alive_noseq * total_indirect_costs_hc$Alive_meningitis_nosequelae +
      ipd_men_dead * total_indirect_costs_hc$Dead_meningitis +
      ipd_pneum_alive * total_indirect_costs_hc$Alive_ipd_pneum +
      ipd_pneum_dead * total_indirect_costs_hc$Dead_ipd_pneum +
      ipd_nmn_alive * total_indirect_costs_hc$Alive_nmnp +
      ipd_nmn_dead * total_indirect_costs_hc$Dead_nmnp +
      nipd_pneum_hosp_alive * total_indirect_costs_hc$Alive_nipd_pneum_tx +
      nipd_pneum_hosp_dead * total_indirect_costs_hc$Dead_nipd_pneum_tx +
      nipd_pneum_nohosp_alive * total_indirect_costs_hc$Alive_nipd_pneum_notx +
      nipd_pneum_nohosp_dead * total_indirect_costs_hc$Dead_nipd_pneum_notx +
      nipd_aom_treated_alive * total_indirect_costs_hc$Alive_aom_tx +
      nipd_aom_untreated_alive * total_indirect_costs_hc$Alive_aom_notx +
      noinf * total_indirect_costs_hc$Alive_aom_notx
    ) * cohort_alive

    ## expected indirect costs, coi (human capital approach)
    expected_cost_indirect_coi_hc <- (
      ipd_men_alive_seq_coi * total_indirect_costs_hc$Alive_meningitis_sequelae +
      ipd_men_alive_noseq_coi * total_indirect_costs_hc$Alive_meningitis_nosequelae +
      ipd_men_dead_coi * total_indirect_costs_hc$Dead_meningitis +
      ipd_pneum_alive_coi * total_indirect_costs_hc$Alive_ipd_pneum +
      ipd_pneum_dead_coi * total_indirect_costs_hc$Dead_ipd_pneum +
      ipd_nmn_alive_coi * total_indirect_costs_hc$Alive_nmnp +
      ipd_nmn_dead_coi * total_indirect_costs_hc$Dead_nmnp +
      nipd_pneum_hosp_alive_coi * total_indirect_costs_hc$Alive_nipd_pneum_tx +
      nipd_pneum_hosp_dead_coi * total_indirect_costs_hc$Dead_nipd_pneum_tx +
      nipd_pneum_nohosp_alive_coi * total_indirect_costs_hc$Alive_nipd_pneum_notx +
      nipd_pneum_nohosp_dead_coi * total_indirect_costs_hc$Dead_nipd_pneum_notx +
      nipd_aom_treated_alive_coi * total_indirect_costs_hc$Alive_aom_tx +
      nipd_aom_untreated_alive_coi * total_indirect_costs_hc$Alive_aom_notx 
    )
 
    ## indirect costs (vsly approach)
    total_cost_indirect_vsly <- (
      ipd_men_alive_seq * total_indirect_costs_vsly$Alive_meningitis_sequelae +
      ipd_men_alive_noseq * total_indirect_costs_vsly$Alive_meningitis_nosequelae +
      ipd_men_dead * total_indirect_costs_vsly$Dead_meningitis +
      ipd_pneum_alive * total_indirect_costs_vsly$Alive_ipd_pneum +
      ipd_pneum_dead * total_indirect_costs_vsly$Dead_ipd_pneum +
      ipd_nmn_alive * total_indirect_costs_vsly$Alive_nmnp +
      ipd_nmn_dead * total_indirect_costs_vsly$Dead_nmnp +
      nipd_pneum_hosp_alive * total_indirect_costs_vsly$Alive_nipd_pneum_tx +
      nipd_pneum_hosp_dead * total_indirect_costs_vsly$Dead_nipd_pneum_tx +
      nipd_pneum_nohosp_alive * total_indirect_costs_vsly$Alive_nipd_pneum_notx +
      nipd_pneum_nohosp_dead * total_indirect_costs_vsly$Dead_nipd_pneum_notx +
      nipd_aom_treated_alive * total_indirect_costs_vsly$Alive_aom_tx +
      nipd_aom_untreated_alive * total_indirect_costs_vsly$Alive_aom_notx +
      noinf * total_indirect_costs_vsly$Alive_aom_notx
    ) * cohort_alive

    ## expected indirect costs, coi (vsly approach)
    expected_cost_indirect_coi_vsly <- (
      ipd_men_alive_seq_coi * total_indirect_costs_vsly$Alive_meningitis_sequelae +
      ipd_men_alive_noseq_coi * total_indirect_costs_vsly$Alive_meningitis_nosequelae +
      ipd_men_dead_coi * total_indirect_costs_vsly$Dead_meningitis +
      ipd_pneum_alive_coi * total_indirect_costs_vsly$Alive_ipd_pneum +
      ipd_pneum_dead_coi * total_indirect_costs_vsly$Dead_ipd_pneum +
      ipd_nmn_alive_coi * total_indirect_costs_vsly$Alive_nmnp +
      ipd_nmn_dead_coi * total_indirect_costs_vsly$Dead_nmnp +
      nipd_pneum_hosp_alive_coi * total_indirect_costs_vsly$Alive_nipd_pneum_tx +
      nipd_pneum_hosp_dead_coi * total_indirect_costs_vsly$Dead_nipd_pneum_tx +
      nipd_pneum_nohosp_alive_coi * total_indirect_costs_vsly$Alive_nipd_pneum_notx +
      nipd_pneum_nohosp_dead_coi * total_indirect_costs_vsly$Dead_nipd_pneum_notx +
      nipd_aom_treated_alive_coi * total_indirect_costs_vsly$Alive_aom_tx +
      nipd_aom_untreated_alive_coi * total_indirect_costs_vsly$Alive_aom_notx
    )
    
    ## total costs (human capital approach)
    total_cost_hc <- (
      ipd_men_alive_seq * total_costs_hc$Alive_meningitis_sequelae +
      ipd_men_alive_noseq * total_costs_hc$Alive_meningitis_nosequelae +
      ipd_men_dead * total_costs_hc$Dead_meningitis +
      ipd_pneum_alive * total_costs_hc$Alive_ipd_pneum +
      ipd_pneum_dead * total_costs_hc$Dead_ipd_pneum +
      ipd_nmn_alive * total_costs_hc$Alive_nmnp +
      ipd_nmn_dead * total_costs_hc$Dead_nmnp +
      nipd_pneum_hosp_alive * total_costs_hc$Alive_nipd_pneum_tx +
      nipd_pneum_hosp_dead * total_costs_hc$Dead_nipd_pneum_tx +
      nipd_pneum_nohosp_alive * total_costs_hc$Alive_nipd_pneum_notx +
      nipd_pneum_nohosp_dead * total_costs_hc$Dead_nipd_pneum_notx +
      nipd_aom_treated_alive * total_costs_hc$Alive_aom_tx +
      nipd_aom_untreated_alive * total_costs_hc$Alive_aom_notx +
      noinf * total_costs_hc$Alive_aom_notx
    ) * cohort_alive

    ## total costs (vsly approach)
    total_cost_vsly <- (
      ipd_men_alive_seq * total_costs_vsly$Alive_meningitis_sequelae +
      ipd_men_alive_noseq * total_costs_vsly$Alive_meningitis_nosequelae +
      ipd_men_dead * total_costs_vsly$Dead_meningitis +
      ipd_pneum_alive * total_costs_vsly$Alive_ipd_pneum +
      ipd_pneum_dead * total_costs_vsly$Dead_ipd_pneum +
      ipd_nmn_alive * total_costs_vsly$Alive_nmnp +
      ipd_nmn_dead * total_costs_vsly$Dead_nmnp +
      nipd_pneum_hosp_alive * total_costs_vsly$Alive_nipd_pneum_tx +
      nipd_pneum_hosp_dead * total_costs_vsly$Dead_nipd_pneum_tx +
      nipd_pneum_nohosp_alive * total_costs_vsly$Alive_nipd_pneum_notx +
      nipd_pneum_nohosp_dead * total_costs_vsly$Dead_nipd_pneum_notx +
      nipd_aom_treated_alive * total_costs_vsly$Alive_aom_tx +
      nipd_aom_untreated_alive * total_costs_vsly$Alive_aom_notx +
      noinf * total_costs_vsly$Alive_aom_notx
    ) * cohort_alive
    
    ## DALYs
    total_dalys <- (
      ipd_men_alive_seq * dalys$Alive_meningitis_sequelae +
      ipd_men_alive_noseq * dalys$Alive_meningitis_nosequelae +
      ipd_men_dead * dalys$Dead_meningitis +
      ipd_pneum_alive * dalys$Alive_ipd_pneumo +
      ipd_pneum_dead * dalys$Dead_ipd_pneumo +
      ipd_nmn_alive * dalys$Alive_nmnp +
      ipd_nmn_dead * dalys$Dead_nmnp +
      nipd_pneum_hosp_alive * dalys$Alive_nipd_pneumo_tx +
      nipd_pneum_hosp_dead * dalys$Dead_nipd_pneumo_tx +
      nipd_pneum_nohosp_alive * dalys$Alive_nipd_pneumo_notx +
      nipd_pneum_nohosp_dead * dalys$Dead_nipd_pneumo_notx +
      nipd_aom_treated_alive * dalys$Alive_aom_tx +
      nipd_aom_untreated_alive * dalys$Alive_aom_notx +
      noinf * dalys$Alive_aom_notx
    ) * cohort_alive
    
    ##*************************************************************
    ## output
    ##*************************************************************
    val <- c(
      p_cases = p_cases, 
      p_hosp = p_hosp, 
      p_deaths = p_deaths,
      scd_birth_cohort = scd_birth_cohort,
      cohort_alive = cohort_alive,
      ##
      infected_total = total_infected,
      infected_total_alive = total_infected_alive,
      infected_total_dead = total_infected_dead,
      uninfected_total = total_uninfected,
      uninfected_total_alive = total_uninfected_alive,
      uninfected_total_dead = total_uninfected_dead,
      ##
      vacc_total = total_vaccinated,
      vacc_total_alive = total_vaccinated_alive,
      vacc_total_dead = total_vaccinated_dead,
      vacc_infected = total_infections_vaccinated,
      vacc_infected_alive = total_infections_vaccinated_alive,
      vacc_infected_dead = total_infections_vaccinated_dead,
      vacc_uninfected = total_uninfected_vaccinated,
      vacc_uninfected_alive = total_uninfected_vaccinated_alive,
      vacc_uninfected_dead = total_uninfected_vaccinated_dead,
      ##
      unvacc_total = total_unvaccinated,
      unvacc_total_alive = total_unvaccinated_alive,
      unvacc_total_dead = total_unvaccinated_dead,
      unvacc_infected = total_infections_unvaccinated,
      unvacc_infected_alive = total_infections_unvaccinated_alive,
      unvacc_infected_dead = total_infections_unvaccinated_dead,
      unvacc_uninfected = total_uninfected_unvaccinated,
      unvacc_uninfected_alive = total_uninfected_unvaccinated_alive,
      unvacc_uninfected_dead = total_uninfected_unvaccinated_dead,
      ##
      cases = total_cases, 
      hosp = total_hosp, 
      deaths = total_deaths,
      ##
      cost_vacc = total_cost_vacc,
      ##
      cost_direct = total_cost_direct,
      cost_direct_coi = expected_cost_direct_coi,
      ##
      cost_indirect_hc = total_cost_indirect_hc,
      cost_indirect_coi_hc = expected_cost_indirect_coi_hc,
      ##
      cost_indirect_vsly = total_cost_indirect_vsly,
      cost_indirect_coi_vsly = expected_cost_indirect_coi_vsly,
      ##
      cost_hc = total_cost_hc,
      cost_vsly = total_cost_vsly,
      ##
      dalys = total_dalys,
      ##
      wtp_who_gdp = wtp,
      wtp_who_vsly = wtp_who_vsly
    )

    ## return output
    return(val)
  })
}

##******************************************************************************
## Evaluate both strategies and conduct CEA
##******************************************************************************
## CEA function
cea_func <- function(
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
) {
  
  ##**********************************************
  ## Results
  ##**********************************************
  results <- do.call(
    rbind, 
    lapply(
      X = strategies,
      FUN = eval_strategy, 
      params = params, 
      cpi = cpi,
      gdp = gdp,
      gdp_mult = gdp_mult,
      ref_yr = ref_yr,
      add_pcv13 = add_pcv13,
      amc = amc,
      time = time,
      nyears = nyears,
      birth_cohort_update = birth_cohort_update,
      alive_pop = alive_pop
    )
  ) %>%
    as.data.frame() %>%
    mutate(
      ## strategy
      strategy = rownames(.),

      ## cfr
      cfr = deaths[1] / cases[1],
      
      ## cases
      inc_cases = cases - cases[match(ref_strategy, strategies)],
      inc_hosp = hosp - hosp[match(ref_strategy, strategies)],
      inc_deaths = deaths - deaths[match(ref_strategy, strategies)],
      
      ## incremental values
      inc_cost_hc = cost_hc - cost_hc[match(ref_strategy, strategies)],
      inc_cost_vsly = cost_vsly - cost_vsly[match(ref_strategy, strategies)],
      inc_dalys = - (dalys - dalys[match(ref_strategy, strategies)]),
      
      ## icer
      icer_hc = (inc_cost_hc / inc_dalys),
      icer_vsly = (inc_cost_vsly / inc_dalys),

      ## denom
      daly_denom = dalys[match(ref_strategy, strategies)] / cases[match(ref_strategy, strategies)],
      cost_coi_hc_num = (cost_direct_coi[match(ref_strategy, strategies)] + cost_indirect_coi_hc[match(ref_strategy, strategies)]),      
      cost_coi_vsly_num = (cost_direct_coi[match(ref_strategy, strategies)] + cost_indirect_coi_vsly[match(ref_strategy, strategies)]),
      
      ## wtps
      wtp_coi_hc = cost_coi_hc_num / daly_denom,
      wtp_coi_vsly = cost_coi_vsly_num / daly_denom,

      ## nmb
      nmb_who_gdp_hc = - (dalys * wtp_who_gdp + cost_hc),
      nmb_who_vsly_hc = - (dalys * wtp_who_vsly + cost_hc), ## We don't want this
      nmb_coi_hc_hc = - (dalys * wtp_coi_hc + cost_hc),
      nmb_coi_vsly_hc = - (dalys * wtp_coi_vsly + cost_hc), ## we don't want this

      nmb_who_gdp_vsly = - (dalys * wtp_who_gdp + cost_vsly),
      nmb_who_vsly_vsly = - (dalys * wtp_who_vsly + cost_vsly),
      nmb_coi_hc_vsly = - (dalys * wtp_coi_hc + cost_vsly),   ## we don't want
      nmb_coi_vsly_vsly = - (dalys * wtp_coi_vsly + cost_vsly),
      
      ## nhb
      nhb_who_gdp_hc = - (dalys + cost_hc / wtp_who_gdp),
      nhb_who_vsly_hc = - (dalys + cost_hc / wtp_who_vsly), ## We don't want this
      nhb_coi_hc_hc = - (dalys + cost_hc / wtp_coi_hc),
      nhb_coi_vsly_hc = - (dalys + cost_hc / wtp_coi_vsly), ## we don't want this

      nhb_who_gdp_vsly = - (dalys + cost_vsly / wtp_who_gdp),
      nhb_who_vsly_vsly = - (dalys + cost_vsly / wtp_who_vsly),
      nhb_coi_hc_vsly = - (dalys + cost_vsly / wtp_coi_hc),  ## we don't want this
      nhb_coi_vsly_vsly = - (dalys + cost_vsly / wtp_coi_vsly),

      ## inmb
      inmb_who_gdp_hc = (inc_dalys * wtp_who_gdp) - inc_cost_hc,
      inmb_who_vsly_hc = (inc_dalys * wtp_who_vsly) - inc_cost_hc, ## we don't want this
      inmb_coi_hc_hc = (inc_dalys * wtp_coi_hc) - inc_cost_hc,
      inmb_coi_vsly_hc = (inc_dalys * wtp_coi_vsly) - inc_cost_hc,  ## we don't want this

      inmb_who_gdp_vsly = (inc_dalys * wtp_who_gdp) - inc_cost_vsly,
      inmb_who_vsly_vsly = (inc_dalys * wtp_who_vsly) - inc_cost_vsly, 
      inmb_coi_hc_vsly = (inc_dalys * wtp_coi_hc) - inc_cost_vsly, ## we don't want this
      inmb_coi_vsly_vsly = (inc_dalys * wtp_coi_vsly) - inc_cost_vsly,

      ## inhb
      inhb_who_gdp_hc = inc_dalys - (inc_cost_hc / wtp_who_gdp),
      inhb_who_vsly_hc = inc_dalys - (inc_cost_hc / wtp_who_vsly),  ## we don't want this
      inhb_coi_hc_hc = inc_dalys - (inc_cost_hc / wtp_coi_hc),
      inhb_coi_vsly_hc = inc_dalys - (inc_cost_hc / wtp_coi_vsly),  ## we don't want this

      inhb_who_gdp_vsly = inc_dalys - (inc_cost_vsly / wtp_who_gdp),
      inhb_who_vsly_vsly = inc_dalys - (inc_cost_vsly / wtp_who_vsly),
      inhb_coi_hc_vsly = inc_dalys - (inc_cost_vsly / wtp_coi_hc),  ## we don't want this
      inhb_coi_vsly_vsly = inc_dalys - (inc_cost_vsly / wtp_coi_vsly)
    ) %>%
    dplyr::select(
      strategy, 
      scd_birth_cohort, 
      cohort_alive,
      infected_total,
      infected_total_alive,
      infected_total_dead,
      uninfected_total,
      uninfected_total_alive,
      uninfected_total_dead,
      ##
      vacc_total, 
      vacc_total_alive,
      vacc_total_dead, 
      vacc_infected,
      vacc_infected_alive,
      vacc_infected_dead, 
      vacc_uninfected,
      vacc_uninfected_alive,
      vacc_uninfected_dead,
      ##
      unvacc_total,
      unvacc_total_alive,
      unvacc_total_dead,
      unvacc_infected, 
      unvacc_infected_alive,
      unvacc_infected_dead,
      unvacc_uninfected,
      unvacc_uninfected_alive,
      unvacc_uninfected_dead, 
      ##
      cases, hosp, deaths, 
      cfr,
      ##
      cost_vacc,
      cost_direct, cost_direct_coi, 
      cost_indirect_hc, cost_indirect_coi_hc, 
      cost_indirect_vsly, cost_indirect_coi_vsly,
      cost_hc, cost_vsly,
      dalys,
      inc_cases, inc_hosp, inc_deaths, 
      inc_cost_hc, inc_cost_vsly, inc_dalys, 
      icer_hc, icer_vsly, 
      ## wtp
      daly_denom,
      cost_coi_hc_num,
      cost_coi_vsly_num,
      wtp_who_gdp, wtp_who_vsly, wtp_coi_hc, wtp_coi_vsly,       
      ## nmb
      nmb_who_gdp_hc, nmb_who_vsly_hc, nmb_coi_hc_hc, nmb_coi_vsly_hc,
      nmb_who_gdp_vsly, nmb_who_vsly_vsly, nmb_coi_hc_vsly, nmb_coi_vsly_vsly,
      ## nhb
      nhb_who_gdp_hc, nhb_who_vsly_hc, nhb_coi_hc_hc, nhb_coi_vsly_hc,
      nhb_who_gdp_vsly, nhb_who_vsly_vsly, nhb_coi_hc_vsly, nhb_coi_vsly_vsly,
      ## inmb
      inmb_who_gdp_hc, inmb_who_vsly_hc, inmb_coi_hc_hc, inmb_coi_vsly_hc, 
      inmb_who_gdp_vsly, inmb_who_vsly_vsly, inmb_coi_hc_vsly, inmb_coi_vsly_vsly,
      ## inhb
      inhb_who_gdp_hc, inhb_who_vsly_hc, inhb_coi_hc_hc, inhb_coi_vsly_hc,
      inhb_who_gdp_vsly, inhb_who_vsly_vsly, inhb_coi_hc_vsly, inhb_coi_vsly_vsly
    )
  
  ## output 
  return(results)
}

#################################################################################################
## END OF MODULE
#################################################################################################