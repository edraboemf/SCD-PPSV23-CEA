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
      strategy, cost_hc, dalys, inc_cost_hc, 
      inc_dalys, inmb_who_gdp_hc, inhb_who_gdp_hc, icer_hc
    ) %>%
    mutate(
      `Strategy` = factor(strategy, levels = c(1, 2), labels = c("PCV13", "PPSV23")),
      `Costs\n(US$)` = format_lancet(x = cost_hc, digits = 0),
      `DALYs` = format_lancet(x = dalys, digits = 0),
      `ΔCosts\n(US$)` = format_lancet(x = inc_cost_hc, digits = 0),
      `ΔDALYs` = format_lancet(x = inc_dalys, digits = 0),
      `ΔNMB\n(US$)` = format_lancet(x = inmb_who_gdp_hc, digits = 0),
      `ΔNHB` = format_lancet(x = inhb_who_gdp_hc, digits = 0),
      `ICER\n(US$/DALY\nAverted)` = format_lancet(x = icer_hc, digits = 0)
    ) %>%
    dplyr::select(
      `Strategy`, `Costs\n(US$)`, `DALYs`,
      `ΔCosts\n(US$)`, `ΔDALYs`, `ΔNMB\n(US$)`, 
      `ΔNHB`, `ICER\n(US$/DALY\nAverted)`
    )
      
    ## PSA
    tab2_param_boot <- psa_summary_results %>%
      mutate(
        `Strategy` = factor(strategy, levels = c(1, 2), labels = c("PCV13", "PPSV23")),
        `Costs\n(US$)` = format_lancet_ci(mean = cost, lower = cost_lower, upper = cost_upper, digits = 0, linebreak = TRUE),
        `DALYs` = format_lancet_ci(mean = dalys, lower = dalys_lower, upper = dalys_upper, digits = 0, linebreak = TRUE),
        `ΔCosts\n(US$)` = format_lancet_ci(mean = inc_cost, lower = inc_cost_lower, upper = inc_cost_upper, digits = 0, linebreak = TRUE),
        `ΔDALYs` = format_lancet_ci(mean = inc_dalys, lower = inc_dalys_lower, upper = inc_dalys_upper, digits = 0, linebreak = TRUE),
        `ΔNMB\n(US$)` = format_lancet_ci(mean = inmb, lower = inmb_lower, upper = inmb_upper, digits = 0, linebreak = TRUE),
        `ΔNHB` = format_lancet_ci(mean = inhb, lower = inhb_lower, upper = inhb_upper, digits = 0, linebreak = TRUE),
        `ICER\n(US$/DALY\nAverted)` = format_lancet_ci(mean = ce, lower = ce_lower, upper = ce_upper, digits = 2, linebreak = TRUE)
      ) %>%
      dplyr::select(
        `Strategy`, `Costs\n(US$)`, `DALYs`,
        `ΔCosts\n(US$)`, `ΔDALYs`, `ΔNMB\n(US$)`, 
        `ΔNHB`, `ICER\n(US$/DALY\nAverted)`
      )
    
    ## Create separator rows
    separator_bc <- tibble::tibble(
      Strategy = "Base case, Est.",
      `Costs\n(US$)` = NA, DALYs = NA,
      `ΔCosts\n(US$)` = NA, `ΔDALYs` = NA,
      `ΔNMB\n(US$)` = NA, `ΔNHB` = NA,
      `ICER\n(US$/DALY\nAverted)` = NA
    )
    
    separator_psa <- tibble::tibble(
      Strategy = "Uncertainty analysis, Est. (95% CI)",
      `Costs\n(US$)` = NA, DALYs = NA,
      `ΔCosts\n(US$)` = NA, `ΔDALYs` = NA,
      `ΔNMB\n(US$)` = NA, `ΔNHB` = NA,
      `ICER\n(US$/DALY\nAverted)` = NA
    )
    
    ## Combine tables with separator rows
    tab2_param <- bind_rows(
      separator_bc, tab2_param_bc,
      separator_psa, tab2_param_boot
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
      `ICER\n(US$/DALY\nAverted)` = "ICER\n(US$/DALY\nAverted)"
    ) %>%
    #theme_booktabs() %>%
    autofit() %>%
    border_inner_h(part = "body", border = fp_border(color = "black", width = 0.5)) %>%
    bold(i = c(1, 4), j = 1, bold = TRUE, part = "body") %>%
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
    `Table 2. Results of a Cost Effectiveness Analysis of PPSV23 Vaccination Relative to No PPCV23 Vaccination in Addition to Routine PCV13 Vaccination In Children 2-4 Years Old With SCD in Burkina Faso.` = tab2_params_ft, 
    path = file.path(ppsv23_tables_dir, "results_table.docx"), 
    pr_section = sect_properties
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
