##******************************************************************************
##* Probabilistic Sensitivity Analysis (PSA)
##******************************************************************************

## Define parameter specifications for PSA
par_mean <- ppsv23_params_value
par_lower <- ppsv23_params_lower
par_upper <- ppsv23_params_upper
dist <- ppsv23_params_dist

params_dist <- fit_distributions_from_ci(
  par_mean, 
  par_lower, 
  par_upper, 
  dist
) %>%
as.data.frame()



# Helper function to format numbers
format_num <- function(x) {
  ifelse(
    is.na(x), 
    NA_character_, 
    trimws(  # trims leading and trailing spaces
      ifelse(
        abs(x) < 0.0001, 
        sprintf("%.4f", x),
        ifelse(
          abs(x) < 10, 
          sprintf("%.2f", x),
          format(round(x, 0), big.mark = " ", scientific = FALSE)
        )
      )
    )
  )
}

dist_table_df <- params_dist %>%
  mutate(
    dist_label = case_when(      
      distribution == "Normal" ~ paste0(
        "Normal(", format_num(mean), ", ", format_num(sqrt(variance)), ")"
      ),      
      distribution == "Lognormal" ~ paste0(
        "Lognormal(", format_num(meanlog), ", ", format_num(sdlog), ")"
      ),      
      distribution == "Gamma" ~ paste0(
        "Gamma(", format_num(shape), ", ", format_num(scale), ")"
      ),      
      distribution == "Beta" ~ paste0(
        "Beta(", format_num(alpha), ", ", format_num(beta), ")"
      ),      
      distribution == "PERT" ~ paste0(
        "PERT(", format_num(min), ", ", format_num(mode), ", ", format_num(max), ")"
      ),      
      distribution == "Binomial" ~ paste0(
        "Binomial(", format_num(mean), ", ", format_num(variance), ")"
      ),      
      distribution == "Poisson" ~ paste0(
        "Poisson(", format_num(mean), ")"
      ),      
      is.na(distribution) & variance == 0 ~ format_num(mean),  # deterministic
      
      TRUE ~ NA_character_
    )
  ) %>%
  select(parameter, dist_label) %>%
  rename(
    Parameter = parameter,
    Distribution = dist_label
  ) %>%
  left_join(
    temp_ppsv23_params %>% select(Parameter, Label),
    by = c("Parameter")
  ) %>%
  na.omit() %>%
  select(Label, Distribution) %>%
  rename(Parameter = Label)

head(dist_table_df)

# Create the flextable
ft <- flextable(dist_table_df) %>%
  autofit() %>%
  theme_vanilla() %>%
  set_header_labels(
    Parameter = "Parameter",
    Distribution = "Distribution"
  ) %>%
  bold(part = "header") 

# Preview in RStudio Viewer
ft

## save to word
save_as_docx(
  `Table S2. Calibrated Values of Parameters.` = ft, 
  path = file.path(ppsv23_tables_dir, "calib_params.docx")
)  




## Generate PSA input matrix
psa_params <- draw_param_func(
  fit_obj = params_dist, 
  n_sim = n_sim
)

## Run PSA using the CEA function
psa_results_3gdp <- rbindlist(
  lapply(
    X = 1:n_sim, 
    FUN = psa_func, 
    params = psa_params,
    ppsv23_params = ppsv23_params_value,
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

## Summarize PSA results
summary_results_3gdp <- psa_results_3gdp %>%
  dplyr::group_by(strategy) %>%
  dplyr::summarise(
    cost = mean(cost_hc, na.rm = TRUE),
    dalys = mean(dalys, na.rm = TRUE),
    nmb = mean(nmb_who_gdp_hc, na.rm = TRUE),
    nhb = mean(nhb_who_gdp_hc, na.rm = TRUE),
    inc_cost = mean(inc_cost_hc, na.rm = TRUE),
    inc_dalys = mean(inc_dalys, na.rm = TRUE),
    inmb = mean(inmb_who_gdp_hc, na.rm = TRUE),
    inhb = mean(inhb_who_gdp_hc, na.rm = TRUE)
  ) %>%
  mutate(icer = inc_cost / inc_dalys) %>%
  ungroup() %>%
  mutate(
    strategy = factor(
      strategy, 
      levels = c(1, 2), 
      labels = c("PCV13", "PPSV23")
    )
  ) %>%
  dplyr::select(
    strategy, 
    cost, 
    dalys, 
    nmb, 
    nhb, 
    inc_cost,
    inc_dalys,
    icer, 
    inmb, 
    inhb
  ) %>%
  as.data.table()

##******************************************
## Plot CEAC or cloud
##******************************************
plot_data <- psa_results_3gdp %>%
  dplyr::mutate(
    wtp_1gdp = as.numeric(wtp_who_gdp) / 3,
    wtp_3gdp = as.numeric(wtp_who_gdp)
  )

## base case values
base_inc_dalys <- base_cea_results_3gdp$inc_dalys[2]
base_inc_cost <- base_cea_results_3gdp$inc_cost_hc[2] / 1000

## subset the data: each row is a simulation iteration
df <- plot_data %>% as.data.frame() %>% filter(strategy %in% c("2")) 

## psa median values
psa_inc_dalys <- median(df$inc_dalys, na.rm = TRUE)
psa_inc_cost <- median(df$inc_cost_hc / 1000, na.rm = TRUE)

## Subset variables of interest
vars <- c("inc_dalys", "inc_cost_hc")

## Robust Mahalanobis distance
mcd <- covMcd(
  x = df[, vars], 
  alpha = 1.00,
  nsamp = n_sim
)
# mcd <- MASS::cov.trob(df[, vars])
md_robust <- mahalanobis(
  x = as.matrix(df[, vars]), 
  center = mcd$center, 
  cov = mcd$cov 
)

level <- 0.95
# nrow_df <- nrow(df[, vars])
# nvars <- 2
# threshold <- nvars * (nrow_df - 1) / (nrow_df - nvars) * qf(level, nvars, nrow_df - nvars)
threshold <- qchisq(level, df = length(vars))
df_inside <- df[md_robust <= threshold, ]

## plot
p_ceap_3gdp <- ggplot(df, aes(x = inc_dalys, y = inc_cost_hc / 1000)) +
  geom_point(color = "gray", alpha = 0.4, size = 2) +
  geom_point(data = df_inside, aes(inc_dalys, inc_cost_hc / 1000), alpha = 0.5, size = 2) +
  geom_point(aes(x = 0, y = 0), color = "black", alpha = 0.5, size = 2) +
  geom_point(aes(x = base_inc_dalys, y = base_inc_cost), alpha = 0.5, size = 2, color = "#BC3C29FF") +
  geom_segment(aes(x = base_inc_dalys, xend = base_inc_dalys, y = 0, yend = base_inc_cost), linetype = "dashed", color = "#BC3C29FF", alpha = 0.3) +
  geom_segment(aes(x = 0, xend = base_inc_dalys, y = base_inc_cost, yend = base_inc_cost), linetype = "dashed", color = "#BC3C29FF", alpha = 0.3) +
  geom_point(aes(x = psa_inc_dalys, y = psa_inc_cost), alpha = 0.5, size = 2, color = "#1111EC") +
  geom_segment(aes(x = psa_inc_dalys, xend = psa_inc_dalys, y = 0, yend = psa_inc_cost), linetype = "dashed", color = "#1111EC", alpha = 0.3) +
  geom_segment(aes(x = 0, xend = psa_inc_dalys, y = psa_inc_cost, yend = psa_inc_cost), linetype = "dashed", color = "#1111EC", alpha = 0.3) +
  stat_ellipse(
    data = df,
    aes(x = inc_dalys, y = inc_cost_hc / 1000),
    inherit.aes = FALSE,
    type = "t",
    level = 0.95
  ) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_vline(xintercept = 0, linetype = "solid") +
  geom_abline(aes(slope = wtp_1gdp / 1000, intercept = 0), linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_abline(aes(slope = wtp_3gdp / 1000, intercept = 0), linetype = "dashed", color = "black", linewidth = 0.5) +
  annotate(
    "text", x = 490, y = 400,
    label = paste0("WTP = US$", format(round(unique(plot_data$wtp_1gdp), 0), nsmall = 0, big.mark = ","), " per DALY averted (1 x GDP per capita)"), 
    vjust = -0.5, hjust = 0,
    color = "black", size = 3
  ) +
  annotate(
    "text", x = 180, y = 450,
    label = paste0("WTP = US$", format(round(unique(plot_data$wtp_3gdp), 0), nsmall = 0, big.mark = ","), " per DALY averted (3 x GDP per capita)"), 
    vjust = -0.5, hjust = 0,
    color = "black", size = 3
  ) +
  xlab("DALYs averted") +
  ylab(paste0("Incremental costs, ", ref_year, " US$'000s")) +
  theme_bw() +
  theme(legend.position = 'bottom', 
        legend.title = element_blank(),
        plot.margin = unit(c(0.3, 0.3, 0.2, 0.2),"cm")
  ) + 
  scale_x_continuous(
    limits = c(-500, 2500), 
    labels = scales::comma, 
    breaks = seq(-500, 2500, 500), 
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(
    limits = c(-500, 500), 
    labels = scales::comma, 
    breaks = seq(-500, 500, 250), 
    expand = c(0.01, 0.01)
  ) +
  scale_color_lancet() 

print(p_ceap_3gdp)

ceap_fname <- "psa_ceap.png"
ggsave(
  filename = ceap_fname, 
  plot = p_ceap_3gdp, 
  path = ppsv23_figures_dir,
  scale = 1, 
  width = 7, 
  height = 4, 
  units = c("in", "cm", "mm", "px")[1], 
  dpi = 300, 
  limitsize = TRUE, 
  bg = "White"
)

##******************************************
## Plot CEAC or cloud (costs and DALYS)
##******************************************
plot_data_new <- psa_results_3gdp %>%
  dplyr::mutate(
    wtp_1gdp = as.numeric(wtp_who_gdp) / 3,
    wtp_3gdp = as.numeric(wtp_who_gdp),
    strategy = factor(strategy, levels = c(1,2), labels = c("PCV13", "PPSV23"))
  )

base_dalys_new <- base_cea_results_3gdp$dalys[2]
base_cost_new <- base_cea_results_3gdp$cost_hc[2] / 1000

## subset the data: each row is a simulation iteration
df_new <- plot_data_new %>% as.data.frame()

## Compute mean and covariance
vars_new <- c("dalys", "cost_hc")

## compute robust center and cov for the whole dataset
df_new <- df_new %>%
  group_by(strategy) %>%
  group_modify(~{
    dat <- .x
    if(nrow(dat) < 6) {
      dat$md_robust <- NA
      dat$inside_robust <- NA
      return(dat)
    }
    mcd_g <- covMcd(
      x = dat[, vars_new], 
      alpha = 1.00,
      nsamp = n_sim
    )
    dat$md_robust <- mahalanobis(
      x = as.matrix(dat[, vars_new]),
      center = mcd_g$center,
      cov = mcd_g$cov
    )
    dat$inside_robust <- dat$md_robust <= qchisq(0.95, df = length(vars_new))
    dat
  }) %>% ungroup()

df_inside_new <- df_new %>%
  group_by(sim) %>%
  filter(any(inside_robust == TRUE)) %>%
  ungroup()

## plot
p_ceap_3gdp_new <- ggplot(plot_data_new, aes(x = dalys, y = cost_hc / 1000, color = strategy)) +
  geom_point(alpha = 0.25, size = 2) +
  stat_ellipse(
    data = df_inside_new, #subset(plot_data_new, dalys != 0 | cost_hc != 0),
    aes(group = strategy, color = strategy),
    level = 0.95
  ) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_vline(xintercept = 0, linetype = "solid") +
  xlab("DALYs (lower is better)") +
  ylab(paste0("Costs, ", ref_year, " US$'000s")) +
  theme_bw() +
  theme(
    legend.position = 'bottom', 
    legend.title = element_blank(),
    plot.margin = unit(c(0.3, 0.3, 0.2, 0.2),"cm")
  ) + 
  scale_x_reverse( 
    limits = c(8000, -1000),
    labels = scales::comma,
    breaks = seq(-1000, 8000, 1000),
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(
    limits = c(-1000, 8000),
    labels = scales::comma,
    breaks = seq(-1000, 8000, 1000),
    expand = c(0.01, 0.01)
  ) +
  scale_color_lancet()

print(p_ceap_3gdp_new)

ceap_fname_new <- "psa_ceap_2groups.png"
ggsave(
  filename = ceap_fname_new, 
  plot = p_ceap_3gdp_new, 
  path = ppsv23_figures_dir,
  scale = 1, 
  width = 7, 
  height = 4, 
  units = c("in", "cm", "mm", "px")[1], 
  dpi = 300, 
  limitsize = TRUE, 
  bg = "White"
)
       
##******************************************
## PSA summary results with bootstrap confidence intervals
##******************************************
## variables
boot_group_var <- "strategy"

## rename some variables in the dataframe
psa_results_3gdp_renamed <- psa_results_3gdp %>%
  dplyr::rename(
    cost = cost_hc, 
    wtp = wtp_who_gdp
  ) %>%
  dplyr::select(sim, strategy, cost, dalys, wtp) 

psa_summary_results <- psa_summary(
  data = psa_results_3gdp_renamed,
  boot_group_var = "strategy",
  ref_strategy = "1",
  wtp = unique(psa_results_3gdp$wtp_who_gdp)[1]
)

# View(psa_summary_results)

##******************************************
## Cost-effectiveness acceptability curve (CEAC)
##******************************************
wtp_3gdp <- unique(psa_results_3gdp$wtp_who_gdp)
wtp_1gdp <- wtp_3gdp / 3
wtp_range <- c(wtp_3gdp, wtp_1gdp, seq(0, 2 * 2500, by = 5))

## CEAC data
ceac_results <- lapply(
  X = wtp_range, 
  FUN = ceac_data_func, 
  data = psa_results_3gdp
) %>%
bind_rows()

## put in long format for plotting
ceac_long <- ceac_results %>%
  dplyr::select(
    wtp,
    PCV13 = pcv13_prob,
    PPSV23 = ppsv23_prob,
    PCV13_lower = pcv13_lower,
    PCV13_upper = pcv13_upper,
    PPSV23_lower = ppsv23_lower,
    PPSV23_upper = ppsv23_upper
  ) %>%
  pivot_longer(
    cols = c(PCV13, PPSV23), 
    names_to = "strategy", 
    values_to = "mean"
  ) %>%
  mutate(
    lower = case_when(
      strategy == "PCV13" ~ PCV13_lower,
      strategy == "PPSV23" ~ PPSV23_lower
    ),
    upper = case_when(
      strategy == "PCV13" ~ PCV13_upper,
      strategy == "PPSV23" ~ PPSV23_upper
    )
  ) %>%
  dplyr::select(wtp, strategy, mean, lower, upper)

## Plot CEAC
p_ceac <- ceac_long %>%
  ggplot(., aes(x = wtp, y = mean, group = strategy, color = strategy, fill = strategy)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  geom_line(linewidth = 0.5) +
  geom_vline(xintercept = wtp_3gdp, linetype = "dashed") +
  geom_vline(xintercept = wtp_1gdp, linetype = "dashed") +
  annotate(
    "text", 
    x = wtp_3gdp + 50, 
    y = 0.4,
    label = paste0(
      "WTP = US$", 
      format(round(wtp_3gdp, 0), nsmall = 0, big.mark = ","), 
      " per DALY averted\n(3 x GDP per capita)"
    ), 
    vjust = -0.5, 
    hjust = 0,
    color = "black", 
    size = 3
  ) +
  annotate(
    "text", 
    x = wtp_1gdp + 50, 
    y = 0.25,
    label = paste0(
      "WTP = US$", 
      format(round(wtp_1gdp, 0), nsmall = 0, big.mark = ","), 
      " per DALY averted\n(1 x GDP per capita)"
    ), 
    vjust = -0.5, 
    hjust = 0,
    color = "black", 
    size = 3
  ) +
  scale_color_lancet() +  
  scale_fill_lancet() +  
  labs(
    x = paste0("Willingness-to-pay threshold, ", ref_year, " US$"), 
    y = "Probability that intervention is cost-effective"
  ) +  
  theme_bw() +
  theme(
    legend.position = c(0.9, 0.80),
    legend.background = element_rect(fill = "transparent", colour = "black", size = 0.3),
    legend.title = element_blank(),
    plot.margin = unit(c(0.3, 0.3, 0.2, 0.2),"cm")
  ) +
  scale_x_continuous(
    labels = scales::label_number(big.mark = " "), 
    limits = c(0, max(wtp_range)), 
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.01), 
    limits = c(0, 1), 
    expand = c(0.01, 0.01)
  )

print(p_ceac)

ceac_fname <- "psa_ceac.png"
ggsave(
  filename = ceac_fname, 
  plot = p_ceac, 
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