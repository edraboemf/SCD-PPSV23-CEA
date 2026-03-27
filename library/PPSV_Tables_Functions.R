#################################################################################################
## TABLES FUNCTIONS
#################################################################################################

##******************************************************************************
## CEA Results Table
##******************************************************************************
## Function to reshape and prepare results
prepare_table_data <- function(df, panel_label) {
  df %>% 
    mutate(
      `Costs` = cost,
      `DALYs` = dalys,
      `NMB` = nmb,
      `NHB` = nhb,
      `div`= " ",
      `ΔCosts` = inc_cost,
      `ΔDALYs` = inc_dalys,
      `ΔNMB` = inmb,
      `ΔNHB` = inhb,
      ICER = icer,
      Panel = panel_label
    ) %>%
    select(
      Panel, Strategy = strategy,
      `Costs`, `DALYs`, `NMB`, `NHB`,
      `div`,
      `ΔCosts`, `ΔDALYs`, `ΔNMB`, `ΔNHB`, ICER
    )
}

## Base case
base_cea_results_new <- base_cea_results %>%
  mutate(
    strategy = factor(strategy, levels = c(1,2), labels = c("PCV13", "PPSV23")),
    cost = sprintf("%s", comma2(cost)),
    dalys = sprintf("%s", comma2(dalys)),
    nmb = sprintf("%s", comma2(nmb)),
    nhb = sprintf("%s", comma2(nhb)),
    div = " ",
    inc_cost = sprintf("%s", comma2(inc_cost)),
    inc_dalys = sprintf("%s", comma2(inc_dalys)),
    icer = ifelse(is.finite(icer), sprintf("%s", comma2(icer)), "—"),
    inmb = sprintf("%s", comma2(inmb)),
    inhb = sprintf("%s", comma2(inhb))
  ) %>%
  select(strategy, cost, dalys, nmb, nhb, div, inc_cost, inc_dalys, icer, inmb, inhb)

table_base <- prepare_table_data(base_cea_results_new, "Base case")

## Uncertainty analysis
psa_summary_results_new <- psa_summary_results %>%
  mutate(
    cost = sprintf("%s\n(%s, %s)", comma2(cost), comma2(cost_lower), comma2(cost_upper)),
    dalys = sprintf("%s\n(%s, %s)", comma2(dalys), comma2(dalys_lower), comma2(dalys_upper)),
    nmb = sprintf("%s\n(%s, %s)", comma2(nmb), comma2(nmb_lower), comma2(nmb_upper)),
    nhb = sprintf("%s (%s, %s)", comma2(nhb), comma2(nhb_lower), comma2(nhb_upper)),
    div = " ",
    inc_cost = sprintf("%s\n(%s, %s)", comma2(inc_cost), comma2(inc_cost_lower), comma2(inc_cost_upper)),
    inc_dalys = sprintf("%s\n(%s, %s)", comma2(inc_dalys), comma2(inc_dalys_lower), comma2(inc_dalys_upper)),
    icer = ifelse(is.finite(ce_mean), sprintf("%s\n(%s, %s)", comma2(ce), comma2(ce_lower), comma2(ce_upper)), "—"),
    inmb = sprintf("%s\n(%s, %s)", comma2(inmb), comma2(inmb_lower), comma2(inmb_upper)),
    inhb = sprintf("%s\n(%s, %s)", comma2(inhb), comma2(inhb_lower), comma2(inhb_upper))
  ) %>%
  select(strategy, cost, dalys, nmb, nhb, div, inc_cost, inc_dalys, icer, inmb, inhb)

table_sens <- prepare_table_data(psa_summary_results_new, "Uncertainty analysis")

## Combine into one final table
final_table <- bind_rows(table_base, table_sens) %>%
  add_row(Panel = "Base case", Strategy = "Base case") %>% 
  add_row(Panel = "Uncertainty analysis", Strategy = "Uncertainty analysis") %>%
  mutate(order = case_when(
    Panel == "Base case" & Strategy == "Base case" ~ 1,
    Panel == "Base case" & Strategy == "PCV13" ~ 2,
    Panel == "Base case" & Strategy == "PPSV23" ~ 3,
    Panel == "Uncertainty analysis" & Strategy == "Uncertainty analysis" ~ 4,
    Panel == "Uncertainty analysis" & Strategy == "PCV13" ~ 5,
    Panel == "Uncertainty analysis" & Strategy == "PPSV23" ~ 6
  )
  ) %>%
  arrange(order) %>%
  select(!c(order, Panel)) %>%    
  as.data.table()

# final_table

## Create flextable with section grouping
final_table_ft <- flextable(final_table) %>%
  add_header_row(colwidths = c(1, 4, 1, 5), values = c("", "Total Effects", "", "Incremental Effects")) %>% 
  #theme_booktabs() %>%
  theme_vanilla() %>%
  autofit() %>%
  fontsize(size = 11, part = "all") %>%
  align(align = "center", part = "all") %>%
  bold(part = "header") %>%
  set_table_properties(layout = "autofit")

# final_table_ft

## Save into a word document
sect_properties <- prop_section(
  page_size = page_size(
    orient = "landscape",
    width = 9.0, height = 12.0
  ),
  type = "continuous",
  page_margins = page_mar()
)

save_as_docx(
  `Table 2. Cost-Effectiveness Analysis Results` = final_table_ft, 
  path = paste0(ppsv23_model_dir, "outputs/CEA_results_table.docx"), 
  pr_section = sect_properties
)


#################################################################################################
## END OF MODULE
#################################################################################################