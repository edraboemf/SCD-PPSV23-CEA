#################################################################################################
## TABLES
#################################################################################################
##******************************************************************************
## Table 1. Model Input Parameters
##******************************************************************************
if(isTRUE(create_table1)){

  tab1_params <- temp_ppsv23_params %>%
    filter(!is.na(Order)) %>%
    select(Variable, Value, Lower, Upper, Distribution, Year, Order) %>%
    mutate(
      Year  = ifelse(Year == "NA", NA, Year),
      Value = ifelse(Value == "NA", NA, Value),
      Lower = ifelse(Lower == "NA", NA, Lower),
      Upper = ifelse(Upper == "NA", NA, Upper),
      Value = as.numeric(Value),
      Lower = as.numeric(Lower),
      Upper = as.numeric(Upper),
      ##
      Value = ifelse(Value < 1, format(round(Value, 3), nsmall = 3, big.mark = ""),
                     ifelse(Value >= 1000, format(round(Value, 0), nsmall = 0, big.mark = " "),
                            format(round(Value, 2), nsmall = 2, big.mark = ""))),
      Lower = ifelse(Lower < 1, format(round(Lower, 3), nsmall = 3, big.mark = ""),
                     ifelse(Lower >= 1000, format(round(Lower, 0), nsmall = 0, big.mark = " "),
                            format(round(Lower, 2), nsmall = 2, big.mark = ""))),
      Upper = ifelse(Upper < 1, format(round(Upper, 3), nsmall = 3, big.mark = ""),
                     ifelse(Upper >= 1000, format(round(Upper, 0), nsmall = 0, big.mark = " "),
                            format(round(Upper, 2), nsmall = 2, big.mark = "")))
    ) %>%
    select(-Order)
  
  ## Create flextable with section grouping
  tab1_params_ft <- flextable(tab1_params) %>%
    #add_header_row(colwidths = c(1, 4, 1, 5), values = c("", "Total Effects", "", "Incremental Effects")) %>% 
    #theme_booktabs() %>%
    theme_vanilla() %>%
    autofit() %>%
    fontsize(size = 11, part = "all") %>%
    align(align = "center", part = "all") %>%
    bold(part = "header") %>%
    set_table_properties(layout = "autofit")
  
  ## Save into a word document
  sect_properties <- prop_section(
    page_size = page_size(
      orient = "portrait"
    ),
    type = "continuous",
    page_margins = page_mar()
  )
  
  ## save to word
  save_as_docx(
    `Table 1. Model Input Parameters.` = tab1_params_ft, 
    path = file.path(ppsv23_tables_dir, "input_params_table.docx"), 
    pr_section = sect_properties
  )  

}

##******************************************************************************
## Table 2. Results of a Cost Effectiveness Analysis of PPSV23 
## Vaccination Relative to No PPCV23 Vaccination in Addition to Routine PCV13 
## Vaccination In Children 2-4 Years Old With SCD in Burkina Faso.
##******************************************************************************
if(isTRUE(create_table2)){
  ## base case results 
  tab2_param_bc <- base_cea_results_3gdp %>%
    dplyr::select(
      strategy, 
      cases, hosp, deaths, 
      inc_cases, inc_hosp, inc_deaths
    ) %>%
    dplyr::mutate(
      `Strategy` = factor(strategy, levels = c(1, 2), labels = c("PCV13", "PPSV23")),
      `Infections` = format_lancet(x = cases, digits = 0),
      `Hospitalizations` = format_lancet(x = hosp, digits = 0),
      `Deaths` = format_lancet(x = deaths, digits = 0),
      `ΔInfections` = format_lancet(x = inc_cases, digits = 0),
      `ΔHospitalizations` = format_lancet(x = inc_hosp, digits = 0),
      `ΔDeaths` = format_lancet(x = inc_deaths, digits = 0)
    ) %>%
    dplyr::select(
      `Strategy`,
       `Infections`, `Hospitalizations`, `Deaths`,
       `ΔInfections`, `ΔHospitalizations`, `ΔDeaths`
    )
  
    ## PSA
    tab2_param_boot <- dplyr::bind_rows(
        Mean = psa_summary_results %>%
          dplyr::ungroup() %>%
          dplyr::transmute(
            wtp, strategy,
            ##
            cases = cases_estimate_mean,
            cases_lower = cases_lower_mean,
            cases_upper = cases_upper_mean,
            ##
            hosp = hosp_estimate_mean,
            hosp_lower = hosp_lower_mean,
            hosp_upper = hosp_upper_mean,
            ##
            deaths = deaths_estimate_mean,
            deaths_lower = deaths_lower_mean,
            deaths_upper = deaths_upper_mean,
            ##
            cost = cost_estimate_mean,
            cost_lower = cost_lower_mean,
            cost_upper = cost_upper_mean,
            ##
            dalys = dalys_estimate_mean,
            dalys_lower = dalys_lower_mean,
            dalys_upper = dalys_upper_mean, 
            ##
            nmb = nmb_estimate_mean,
            nmb_lower = nmb_lower_mean,
            nmb_upper = nmb_upper_mean,
            ##
            nhb = nhb_estimate_mean,
            nhb_lower = nhb_lower_mean,
            nhb_upper = nhb_upper_mean,
            ##
            inc_cases = inc_cases_estimate_mean,
            inc_cases_lower = inc_cases_lower_mean,
            inc_cases_upper = inc_cases_upper_mean,
            ##
            inc_hosp = inc_hosp_estimate_mean,
            inc_hosp_lower = inc_hosp_lower_mean,
            inc_hosp_upper = inc_hosp_upper_mean,
            ##
            inc_deaths = inc_deaths_estimate_mean,
            inc_deaths_lower = inc_deaths_lower_mean,
            inc_deaths_upper = inc_deaths_upper_mean,
            ##
            inc_cost = inc_cost_estimate_mean,
            inc_cost_lower = inc_cost_lower_mean,
            inc_cost_upper = inc_cost_upper_mean,
            ##
            inc_dalys = inc_dalys_estimate_mean,
            inc_dalys_lower = inc_dalys_lower_mean,
            inc_dalys_upper = inc_dalys_upper_mean,
            ##
            inmb = inmb_estimate_mean,
            inmb_lower = inmb_lower_mean,
            inmb_upper = inmb_upper_mean,
            ##
            inhb = inhb_estimate_mean,
            inhb_lower = inhb_lower_mean,
            inhb_upper = inhb_upper_mean,
            ##
            icer = icer_estimate_mean,
            icer_lower = icer_lower_mean,
            icer_upper = icer_upper_mean,
            ##
            ce = ce_estimate,
            ce_lower = ce_lower,
            ce_upper = ce_upper
          ),
        Median = psa_summary_results %>%
          dplyr::ungroup() %>%
          dplyr::transmute(
            wtp, strategy,
            ##
            cases = cases_estimate_median,
            cases_lower = cases_lower_median,
            cases_upper = cases_upper_median,
            ##
            hosp = hosp_estimate_median,
            hosp_lower = hosp_lower_median,
            hosp_upper = hosp_upper_median,
            ##
            deaths = deaths_estimate_median,
            deaths_lower = deaths_lower_median,
            deaths_upper = deaths_upper_median,
            ##
            cost = cost_estimate_median,
            cost_lower = cost_lower_median,
            cost_upper = cost_upper_median,
            ##
            dalys = dalys_estimate_median,
            dalys_lower = dalys_lower_median,
            dalys_upper = dalys_upper_median,
            ## 
            nmb = nmb_estimate_median,
            nmb_lower = nmb_lower_median,
            nmb_upper = nmb_upper_median,
            ## 
            nhb = nhb_estimate_median,
            nhb_lower = nhb_lower_median,
            nhb_upper = nhb_upper_median,
            ##
            inc_cases = inc_cases_estimate_median,
            inc_cases_lower = inc_cases_lower_median,
            inc_cases_upper = inc_cases_upper_median,
            ##
            inc_hosp = inc_hosp_estimate_median,
            inc_hosp_lower = inc_hosp_lower_median,
            inc_hosp_upper = inc_hosp_upper_median,
            ##
            inc_deaths = inc_deaths_estimate_median,
            inc_deaths_lower = inc_deaths_lower_median,
            inc_deaths_upper = inc_deaths_upper_median,
            ##
            inc_cost = inc_cost_estimate_median,
            inc_cost_lower = inc_cost_lower_median,
            inc_cost_upper = inc_cost_upper_median,
            ##
            inc_dalys = inc_dalys_estimate_median,
            inc_dalys_lower = inc_dalys_lower_median,
            inc_dalys_upper = inc_dalys_upper_median,
            ##
            inmb = inmb_estimate_median,
            inmb_lower = inmb_lower_median,
            inmb_upper = inmb_upper_median,
            ##
            inhb = inhb_estimate_median,
            inhb_lower = inhb_lower_median,
            inhb_upper = inhb_upper_median,
            ##
            icer = icer_estimate_median,
            icer_lower = icer_lower_median,
            icer_upper = icer_upper_median,
            ##
            ce = ce_estimate,
            ce_lower = ce_lower,
            ce_upper = ce_upper
          ),
          .id = "Statistic"
      ) %>%      
      dplyr::mutate(
        `Strategy` = factor(strategy, levels = c(1, 2), labels = c("PCV13", "PPSV23")),
        `Costs\n(US$)` = format_lancet_ci(mean = cost, lower = cost_lower, upper = cost_upper, digits = 0, linebreak = TRUE),
        `DALYs` = format_lancet_ci(mean = dalys, lower = dalys_lower, upper = dalys_upper, digits = 0, linebreak = TRUE),
        `ΔCosts\n(US$)` = format_lancet_ci(mean = inc_cost, lower = inc_cost_lower, upper = inc_cost_upper, digits = 0, linebreak = TRUE),
        `ΔDALYs` = format_lancet_ci(mean = inc_dalys, lower = inc_dalys_lower, upper = inc_dalys_upper, digits = 0, linebreak = TRUE),
        `ΔNMB\n(US$)` = format_lancet_ci(mean = inmb, lower = inmb_lower, upper = inmb_upper, digits = 0, linebreak = TRUE),
        `ΔNHB` = format_lancet_ci(mean = inhb, lower = inhb_lower, upper = inhb_upper, digits = 0, linebreak = TRUE),
        `ICER\n(US$/DALY\nAverted)` = format_lancet_ci(mean = icer, lower = icer_lower, upper = icer_upper, digits = 0, linebreak = TRUE),
        `CEP` = format_lancet_ci(mean = ce, lower = ce_lower, upper = ce_upper, digits = 2, linebreak = TRUE),
        `Infections` = format_lancet_ci(mean = cases, lower = cases_lower, upper = cases_upper, digits = 0, linebreak = TRUE),
        `Hospitalizations` = format_lancet_ci(mean = hosp, lower = hosp_lower, upper = hosp_upper, digits = 0, linebreak = TRUE),
        `Deaths` = format_lancet_ci(mean = deaths, lower = deaths_lower, upper = deaths_upper, digits = 0, linebreak = TRUE),
        `ΔInfections` = format_lancet_ci(mean = inc_cases, lower = inc_cases_lower, upper = inc_cases_upper, digits = 0, linebreak = TRUE),
        `ΔHospitalizations` = format_lancet_ci(mean = inc_hosp, lower = inc_hosp_lower, upper = inc_hosp_upper, digits = 0, linebreak = TRUE),
        `ΔDeaths` = format_lancet_ci(mean = inc_deaths, lower = inc_deaths_lower, upper = inc_deaths_upper, digits = 0, linebreak = TRUE)
      ) %>%
      dplyr::select(
        `Statistic`, `Strategy`, `Costs\n(US$)`, `DALYs`,
        `ΔCosts\n(US$)`, `ΔDALYs`, `ΔNMB\n(US$)`, 
        `ΔNHB`, `ICER\n(US$/DALY\nAverted)`, `CEP`,
        `Infections`, `Hospitalizations`, `Deaths`,
        `ΔInfections`, `ΔHospitalizations`, `ΔDeaths`
      )
    
    ## Create separator rows
    separator_bc <- tibble::tibble(
      Strategy = "Base case, Est.",
      `Costs\n(US$)` = NA, DALYs = NA,
      `ΔCosts\n(US$)` = NA, `ΔDALYs` = NA,
      `ΔNMB\n(US$)` = NA, `ΔNHB` = NA,
      `ICER\n(US$/DALY\nAverted)` = NA,
      `CEP` = NA,
      `Infections` = NA, 
      `Hospitalizations` = NA, 
      `Deaths` = NA,
      `ΔInfections` = NA, 
      `ΔHospitalizations` = NA, 
      `ΔDeaths` = NA
    )
    
    separator_psa <- tibble::tibble(
      Strategy = "Uncertainty analysis, Est. (95% CI)",
      `Costs\n(US$)` = NA, DALYs = NA,
      `ΔCosts\n(US$)` = NA, `ΔDALYs` = NA,
      `ΔNMB\n(US$)` = NA, `ΔNHB` = NA,
      `ICER\n(US$/DALY\nAverted)` = NA,
      `CEP` = NA,
      `Infections` = NA, 
      `Hospitalizations` = NA, 
      `Deaths` = NA,
      `ΔInfections` = NA, 
      `ΔHospitalizations` = NA, 
      `ΔDeaths` = NA
    )

    separator_mean <- tibble::tibble(
      Strategy = "Means",
      `Costs\n(US$)` = NA, DALYs = NA,
      `ΔCosts\n(US$)` = NA, `ΔDALYs` = NA,
      `ΔNMB\n(US$)` = NA, `ΔNHB` = NA,
      `ICER\n(US$/DALY\nAverted)` = NA,
      `CEP` = NA,
      `Infections` = NA, 
      `Hospitalizations` = NA, 
      `Deaths` = NA,
      `ΔInfections` = NA, 
      `ΔHospitalizations` = NA, 
      `ΔDeaths` = NA
    )

    separator_median <- tibble::tibble(
      Strategy = "Medians",
      `Costs\n(US$)` = NA, DALYs = NA,
      `ΔCosts\n(US$)` = NA, `ΔDALYs` = NA,
      `ΔNMB\n(US$)` = NA, `ΔNHB` = NA,
      `ICER\n(US$/DALY\nAverted)` = NA,
      `CEP` = NA,
      `Infections` = NA, 
      `Hospitalizations` = NA, 
      `Deaths` = NA,
      `ΔInfections` = NA, 
      `ΔHospitalizations` = NA, 
      `ΔDeaths` = NA
    )

    ## psa means
    tab2_param_boot_means <- tab2_param_boot %>%
      dplyr::filter(Statistic == "Mean") %>%
      dplyr::select(-Statistic)
  
    ## psa medians
    tab2_param_boot_medians <- tab2_param_boot %>%
      dplyr::filter(Statistic == "Median") %>%
      dplyr::select(-Statistic)

    ## Combine tables with separator rows
    tab2_param <- bind_rows(
      separator_bc, tab2_param_bc,
      separator_psa, 
      separator_mean, tab2_param_boot_means,  
      separator_median, tab2_param_boot_medians
    )
    
    tab2_params_ft <- flextable(tab2_param) %>%
    # Right-align numeric columns
    align(j = 2:ncol(tab2_param), align = "center", part = "all") %>%
    # Top-align for multiline CI cells
    valign(j = 2:ncol(tab2_param), valign = "top", part = "all") %>%
    # Add header labels (optional if you want nicer display)
    set_header_labels(
      `Strategy` = "Strategy",
      `Costs\n(US$)` = "Costs\n(US$)",
      `DALYs` = "DALYs",
      `ΔCosts\n(US$)` = "ΔCosts\n(US$)",
      `ΔDALYs` = "ΔDALYs",
      `ΔNMB\n(US$)` = "ΔNMB\n(US$)",
      `ΔNHB` = "ΔNHB",
      `ICER\n(US$/DALY\nAverted)` = "ICER\n(US$/DALY\nAverted)",
      `CEP` = "Pr(CE)",
      `Infections` = "Infections", 
      `Hospitalizations` = "Hospitalizations", 
      `Deaths` = "Deaths",
      `ΔInfections` = "ΔInfections", 
      `ΔHospitalizations` = "ΔHospitalizations", 
      `ΔDeaths` = "ΔDeaths"
    ) %>%
    #theme_booktabs() %>%
    autofit() %>%
    border_inner_h(part = "body", border = fp_border(color = "black", width = 0.5)) %>%
    bold(i = c(1, 4, 5, 8), j = 1, bold = TRUE, part = "body") %>%
    italic(i = c(5, 8), j = 1, italic = TRUE, part = "body") %>%
    bold(part = "header")

  ## Save into a word document
  tab2_doc <- read_docx()
  tab2_doc <- body_add_fpar(
    x = tab2_doc,
    value = fpar(
      ftext(
        "Table 2. Results of a Cost Effectiveness Analysis of PPSV23 Vaccination Relative to No PPCV23 Vaccination in Addition to Routine PCV13 Vaccination in Children 2–4 Years Old With SCD in Burkina Faso.",
        fp_text(bold = TRUE)
      )
    ),
    style = NULL
  )

  tab2_doc <- body_add_flextable(
    tab2_doc,
    value = tab2_params_ft,
    align = "center",
    pos = "after"
  )  

  ## save to word
  print(
    x = tab2_doc, 
    target = file.path(ppsv23_tables_dir, "results_table.docx")
  )

}


##******************************************************************************
## Table S1. Results of a Cost Effectiveness Analysis of PPSV23 
## Vaccination Relative to No PPCV23 Vaccination in Addition to Routine PCV13 
## Vaccination In Children 2-4 Years Old With SCD in Burkina Faso.
##******************************************************************************
if(isTRUE(create_tableS1)){
  ## base case results 
  ## coi, human capital
  tabS1_param_bc_coi_hc <- base_cea_results_3gdp %>%
    dplyr::select(
      strategy, cost_hc, dalys, inc_cost_hc, 
      inc_dalys, inmb_coi_hc_hc, inhb_coi_hc_hc, icer_hc
    ) %>%
    mutate(
      `Strategy` = factor(strategy, levels = c(1, 2), labels = c("PCV13", "PPSV23")),
      `Costs\n(US$)` = format_lancet(x = cost_hc, digits = 0),
      `DALYs` = format_lancet(x = dalys, digits = 0),
      `ΔCosts\n(US$)` = format_lancet(x = inc_cost_hc, digits = 0),
      `ΔDALYs` = format_lancet(x = inc_dalys, digits = 0),
      `ΔNMB\n(US$)` = format_lancet(x = inmb_coi_hc_hc, digits = 0),
      `ΔNHB` = format_lancet(x = inhb_coi_hc_hc, digits = 0),
      `ICER\n(US$/DALY\nAverted)` = format_lancet(x = icer_hc, digits = 0)
    ) %>%
    dplyr::select(
      `Strategy`, `Costs\n(US$)`, `DALYs`,
      `ΔCosts\n(US$)`, `ΔDALYs`, `ΔNMB\n(US$)`, 
      `ΔNHB`, `ICER\n(US$/DALY\nAverted)`
    )

  ## coi, vsly
  tabS1_param_bc_coi_vsly <- base_cea_results_3gdp %>%
    dplyr::select(
      strategy, cost_vsly, dalys, inc_cost_vsly, 
      inc_dalys, inmb_coi_vsly_vsly, inhb_coi_vsly_vsly, icer_vsly
    ) %>%
    mutate(
      `Strategy` = factor(strategy, levels = c(1, 2), labels = c("PCV13", "PPSV23")),
      `Costs\n(US$)` = format_lancet(x = cost_vsly, digits = 0),
      `DALYs` = format_lancet(x = dalys, digits = 0),
      `ΔCosts\n(US$)` = format_lancet(x = inc_cost_vsly, digits = 0),
      `ΔDALYs` = format_lancet(x = inc_dalys, digits = 0),
      `ΔNMB\n(US$)` = format_lancet(x = inmb_coi_vsly_vsly, digits = 0),
      `ΔNHB` = format_lancet(x = inhb_coi_vsly_vsly, digits = 0),
      `ICER\n(US$/DALY\nAverted)` = format_lancet(x = icer_vsly, digits = 0)
    ) %>%
    dplyr::select(
      `Strategy`, `Costs\n(US$)`, `DALYs`,
      `ΔCosts\n(US$)`, `ΔDALYs`, `ΔNMB\n(US$)`, 
      `ΔNHB`, `ICER\n(US$/DALY\nAverted)`
    )
      
    ## who, vsly
    tabS1_param_bc_who_vsly <- base_cea_results_3gdp %>%
      dplyr::select(
        strategy, cost_vsly, dalys, inc_cost_vsly, 
        inc_dalys, inmb_who_gdp_vsly, inhb_who_gdp_vsly, icer_vsly
      ) %>%
      mutate(
        `Strategy` = factor(strategy, levels = c(1, 2), labels = c("PCV13", "PPSV23")),
        `Costs\n(US$)` = format_lancet(x = cost_vsly, digits = 0),
        `DALYs` = format_lancet(x = dalys, digits = 0),
        `ΔCosts\n(US$)` = format_lancet(x = inc_cost_vsly, digits = 0),
        `ΔDALYs` = format_lancet(x = inc_dalys, digits = 0),
        `ΔNMB\n(US$)` = format_lancet(x = inmb_who_gdp_vsly, digits = 0),
        `ΔNHB` = format_lancet(x = inhb_who_gdp_vsly, digits = 0),
        `ICER\n(US$/DALY\nAverted)` = format_lancet(x = icer_vsly, digits = 0)
      ) %>%
      dplyr::select(
        `Strategy`, `Costs\n(US$)`, `DALYs`,
        `ΔCosts\n(US$)`, `ΔDALYs`, `ΔNMB\n(US$)`, 
        `ΔNHB`, `ICER\n(US$/DALY\nAverted)`
      )
    
    ## Create separator rows
    separator_1 <- tibble::tibble(
      Strategy = "COI, Human Capital Approach",
      `Costs\n(US$)` = NA, DALYs = NA,
      `ΔCosts\n(US$)` = NA, `ΔDALYs` = NA,
      `ΔNMB\n(US$)` = NA, `ΔNHB` = NA,
      `ICER\n(US$/DALY\nAverted)` = NA
    )
    
    separator_2 <- tibble::tibble(
      Strategy = "COI, VSLY Approach",
      `Costs\n(US$)` = NA, DALYs = NA,
      `ΔCosts\n(US$)` = NA, `ΔDALYs` = NA,
      `ΔNMB\n(US$)` = NA, `ΔNHB` = NA,
      `ICER\n(US$/DALY\nAverted)` = NA
    )

    separator_3 <- tibble::tibble(
      Strategy = "WHO, VSLY Approach",
      `Costs\n(US$)` = NA, DALYs = NA,
      `ΔCosts\n(US$)` = NA, `ΔDALYs` = NA,
      `ΔNMB\n(US$)` = NA, `ΔNHB` = NA,
      `ICER\n(US$/DALY\nAverted)` = NA
    )

    ## Combine tables with separator rows
    tabS1_param <- bind_rows(
      separator_1, tabS1_param_bc_coi_hc,
      separator_2, tabS1_param_bc_coi_vsly,
      separator_3, tabS1_param_bc_who_vsly
    )
    
    tabs1_params_ft <- flextable(tabS1_param) %>%
    # Right-align numeric columns
    align(j = 2:ncol(tabS1_param), align = "center", part = "all") %>%
    # Top-align for multiline CI cells
    valign(j = 2:ncol(tabS1_param), valign = "top", part = "all") %>%
    # Add header labels (optional if you want nicer display)
    set_header_labels(
      `Strategy` = "Strategy",
      `Costs\n(US$)` = "Costs\n(US$)",
      `DALYs` = "DALYs",
      `ΔCosts\n(US$)` = "ΔCosts\n(US$)",
      `ΔDALYs` = "ΔDALYs",
      `ΔNMB\n(US$)` = "ΔNMB\n(US$)",
      `ΔNHB` = "ΔNHB",
      `ICER\n(US$/DALY\nAverted)` = "ICER\n(US$/DALY\nAverted)"
    ) %>%
    #theme_booktabs() %>%
    autofit() %>%
    border_inner_h(part = "body", border = fp_border(color = "black", width = 0.5)) %>%
    #bold(i = c(1, 4, 7, 10), j = 1, bold = TRUE, part = "body") %>%
    bold(part = "header")

  ## Save into a word document
  sect_properties <- prop_section(
    page_size = page_size(
      orient = "portrait"
    ),
    type = "continuous",
    page_margins = page_mar()
  )

  ## save to word
  save_as_docx(
    `Table S1. Results of a Cost Effectiveness Analysis of PPSV23 Vaccination Relative to No PPCV23 Vaccination in Addition to Routine PCV13 Vaccination In Children 2-4 Years Old With SCD in Burkina Faso.` = tabs1_params_ft, 
    path = file.path(ppsv23_tables_dir, "tableS1.docx"), 
    pr_section = sect_properties
  )
}

#################################################################################################
## END OF MODULE
#################################################################################################
