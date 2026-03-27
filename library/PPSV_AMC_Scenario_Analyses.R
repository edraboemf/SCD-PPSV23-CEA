#################################################################################################
## AMC PAYMENT SCENARIO ANALYSES
#################################################################################################

##******************************************************************************
## Base case
##******************************************************************************
## cea results
amc_pay_cea_results <- do.call(
  rbind, 
  lapply(
    X = scenario_c_amc_vaccine_ppsv23, 
    FUN = amc_pay_cea_func,
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
  )
)

#print(head(amc_pay_cea_results))
#print(colnames(amc_pay_cea_results))

## format
amc_pay_sensi_tab <- amc_pay_cea_results %>%
  dplyr::select(
    amc_pay, strategy, cost = cost_hc, dalys, nmb = nmb_who_gdp_hc, nhb = nhb_who_gdp_hc,
    inc_cost = inc_cost_hc, inc_dalys, icer = icer_hc, 
    inmb = inmb_who_gdp_hc, inhb = inhb_who_gdp_hc
  ) %>%
  dplyr::mutate(
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
  dplyr::select(
    amc_pay, strategy, cost, dalys, nmb, nhb, div, 
    inc_cost, inc_dalys, icer, inmb, inhb
  )

amc_pay_table <- prepare_amc_table_data(amc_pay_sensi_tab)

amc_pay_final_table <- amc_pay_table %>%
  mutate(`AMC` = as.numeric(`AMC`)) %>%  
  add_row(`AMC` = 10.46, Strategy = "US$10.46 Tail Price") %>%
  add_row(`AMC` = 9.58, Strategy = "US$9.58 Tail Price") %>%
  add_row(`AMC` = 7.70, Strategy = "US$7.70 Tail Price") %>%
  add_row(`AMC` = 3.30, Strategy = "US$3.30 Tail Price") %>%
  mutate(order = case_when(
    `AMC` == 10.46 & Strategy == "US$10.46 Tail Price" ~ 1,
    `AMC` == 10.46 & Strategy == "PCV13" ~ 2,
    `AMC` == 10.46 & Strategy == "PPSV23" ~ 3,
    `AMC` == 9.58 & Strategy == "US$9.58 Tail Price"~ 4,
    `AMC` == 9.58 & Strategy == "PCV13" ~ 5,
    `AMC` == 9.58 & Strategy == "PPSV23" ~ 6,
    `AMC` == 7.70 & Strategy == "US$7.70 Tail Price" ~ 7,
    `AMC` == 7.70 & Strategy == "PCV13" ~ 8,
    `AMC` == 7.70 & Strategy == "PPSV23" ~ 9,
    `AMC` == 3.30 & Strategy == "US$3.30 Tail Price" ~ 10,
    `AMC` == 3.30 & Strategy == "PCV13" ~ 11,
    `AMC` == 3.30 & Strategy == "PPSV23" ~ 12,
    TRUE ~ 99 
  )) %>%
  arrange(order) %>%
  dplyr::select(-c(order, `AMC`)) %>%
  as.data.table()

## Create flextable with section grouping
amc_pay_final_table_ft <- flextable(amc_pay_final_table) %>%
  add_header_row(colwidths = c(1, 4, 1, 5), values = c("", "Total Effects", "", "Incremental Effects")) %>% 
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
    width = 9.0, height = 12.0
  ),
  type = "continuous",
  page_margins = page_mar()
)

save_as_docx(
  `Table 3. Scenario Analysis --  Sensitivity of the ICER to the AMC Payment Rate` = amc_pay_final_table_ft, 
  path = file.path(ppsv23_tables_dir, "scenario_amc_pay_table.docx"), 
  pr_section = sect_properties
)

##******************************************************************************
## Filter for strategy 2 and split by subsidy
##******************************************************************************

amc_sensi_cea_results <- do.call(
  rbind,
  lapply(
    X = 1:nrow(amc_sensi_vals),
    FUN = amc_pay_cea_plot_func,
    sensi_params = amc_sensi_vals,
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
  )
)

head(amc_sensi_cea_results)

interp_data_all <- amc_sensi_cea_results %>%
  dplyr::filter(strategy == 2) %>%
  mutate(icer = icer_hc) %>%
  mutate(
    across(c(tail_price, copay, amc_dose_subsidy, icer), as.numeric)
  ) %>%
  #dplyr::select(tail_price, copay, amc_dose_subsidy, icer) %>%
  group_split(amc_dose_subsidy) %>%
  purrr::map_dfr(interp_one_subsidy)

# head(interp_data_all)
# tail(interp_data_all)
# max(interp_data_all$icer)

## Plot with smooth filled contours
p_amc_sensi <- interp_data_all %>%
  mutate(amc_dose_subsidy = factor(
    amc_dose_subsidy,
    levels = c(0.10, 0.20, 0.30),
    labels = c(
      "AMC subsidy for 10% of doses",
      "AMC subsidy for 20% of doses",
      "AMC subsidy for 30% of doses"
    )
  )) %>%
  ggplot(aes(x = tail_price, y = copay, z = icer)) +
  geom_contour_filled(color = NA, bins = 5) +
  scale_fill_lancet(name = "ICER, US$ per DALY averted:") +
  facet_wrap(~amc_dose_subsidy) +
  labs(
    x = paste0("Tail price, ", ref_year, " US$"),
    y = paste0("Copay, ", ref_year, " US$")
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.background = element_rect(fill = "transparent", colour = "transparent", size = 0.3),
    legend.key.size = unit(0.4, "cm"),  
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    plot.margin = unit(c(0.3, 0.3, 0.2, 0.2), "cm"),
    strip.text = element_text(face = "bold")
  ) +
  scale_x_continuous(
    labels = scales::label_number(accuracy = 0.01),
    limits = c(min(tail_price_range), max(tail_price_range)),
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.01),
    limits = c(min(copay_range), max(copay_range)),
    expand = c(0.01, 0.01)
  )

print(p_amc_sensi)
amc_sensi_fname <- "amc_sensi.png"
ggsave(
  filename = amc_sensi_fname, 
  plot = p_amc_sensi, 
  path = ppsv23_figures_dir,
  scale = 1, 
  width = 7, 
  height = 4, 
  units = c("in", "cm", "mm", "px")[1], 
  dpi = 300, 
  limitsize = TRUE, 
  bg = "White"
)

#################################################################################################
## END OF MODULE
#################################################################################################
