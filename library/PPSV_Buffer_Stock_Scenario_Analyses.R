#################################################################################################
## BUFFER STOCK SCENARIO ANALYSIS
#################################################################################################

##******************************************************************************
## Base case
##******************************************************************************
buffer_cea_results <- do.call(
  rbind, 
  lapply(
    X = buffer_stock_values, 
    FUN = buffer_cea_func,
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
)

buffer_sensi_tab <- buffer_cea_results %>%
  dplyr::select(
    buffer_stock, strategy, cost_hc, dalys, 
    nmb_who_gdp_hc, nhb_who_gdp_hc, 
    inc_cost_hc, inc_dalys, icer_hc, 
    inmb_who_gdp_hc, inhb_who_gdp_hc
  ) %>%
  mutate(
    strategy = factor(strategy, levels = c(1,2), labels = c("PCV13", "PPSV23")),
    cost = sprintf("%s", comma2(cost_hc)),
    dalys = sprintf("%s", comma2(dalys)),
    nmb = sprintf("%s", comma2(nmb_who_gdp_hc)),
    nhb = sprintf("%s", comma2(nhb_who_gdp_hc)),
    div = " ",
    inc_cost = sprintf("%s", comma2(inc_cost_hc)),
    inc_dalys = sprintf("%s", comma2(inc_dalys)),
    icer = ifelse(is.finite(icer_hc), sprintf("%s", comma2(icer_hc)), "—"),
    inmb = sprintf("%s", comma2(inmb_who_gdp_hc)),
    inhb = sprintf("%s", comma2(inhb_who_gdp_hc))
  ) %>%
  select(
    buffer_stock, strategy, cost, dalys, 
    nmb, nhb, div, inc_cost, inc_dalys, icer, inmb, inhb
  )

## table
buffer_stock_table <- prepare_buffer_table_data(buffer_sensi_tab)

buffer_stock_final_table <- buffer_stock_table %>%
  mutate(`Buffer Stock` = as.numeric(`Buffer Stock`)) %>%  
  add_row(`Buffer Stock` = 0, Strategy = "0% Buffer Stock") %>%
  add_row(`Buffer Stock` = 0.05, Strategy = "5% Buffer Stock") %>%
  add_row(`Buffer Stock` = 0.10, Strategy = "10% Buffer Stock") %>%
  add_row(`Buffer Stock` = 0.25, Strategy = "25% Buffer Stock") %>%
  mutate(order = case_when(
    `Buffer Stock` == 0 & Strategy == "0% Buffer Stock"~ 1,
    `Buffer Stock` == 0 & Strategy == "PCV13" ~ 2,
    `Buffer Stock` == 0 & Strategy == "PPSV23" ~ 3,
    `Buffer Stock` == 0.05 & Strategy == "5% Buffer Stock"~ 4,
    `Buffer Stock` == 0.05 & Strategy == "PCV13" ~ 5,
    `Buffer Stock` == 0.05 & Strategy == "PPSV23" ~ 6,
    `Buffer Stock` == 0.10 & Strategy == "10% Buffer Stock" ~ 7,
    `Buffer Stock` == 0.10 & Strategy == "PCV13" ~ 8,
    `Buffer Stock` == 0.10 & Strategy == "PPSV23" ~ 9,
    `Buffer Stock` == 0.25 & Strategy == "25% Buffer Stock" ~ 10,
    `Buffer Stock` == 0.25 & Strategy == "PCV13" ~ 11,
    `Buffer Stock` == 0.25 & Strategy == "PPSV23" ~ 12,
    TRUE ~ 99 
  )) %>%
  arrange(order) %>%
  select(-c(order, `Buffer Stock`)) %>%
  as.data.table()

## Create flextable with section grouping
buffer_stock_final_table_ft <- flextable(buffer_stock_final_table) %>%
  add_header_row(
    colwidths = c(1, 4, 1, 5), 
    values = c("", "Total Effects", "", "Incremental Effects")
  ) %>% 
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
    orient = "landscape",
    width = 9.0, 
    height = 12.0
  ),
  type = "continuous",
  page_margins = page_mar()
)

## save
save_as_docx(
  `Table 3. Scenario Analysis --  Sensitivity of the ICER to the Buffer Stock Rate` = buffer_stock_final_table_ft, 
  path = file.path(ppsv23_tables_dir, "scenario_buffer_stock_table.docx"), 
  pr_section = sect_properties
)

#################################################################################################
## END OF MODULE
#################################################################################################
