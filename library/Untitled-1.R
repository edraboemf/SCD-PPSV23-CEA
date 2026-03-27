
# library(dplyr)
# library(MASS)
# library(ggplot2)
# library(scales)

##******************************************
## Plot CEAC or cloud with robust 95% percentile contour
##******************************************
plot_data <- psa_results_3gdp %>%
  dplyr::mutate(
    wtp_1gdp = as.numeric(wtp_who_gdp) / 3,
    wtp_3gdp = as.numeric(wtp_who_gdp)
  )

base_inc_dalys <- base_cea_results_3gdp$inc_dalys[2]
base_inc_cost <- base_cea_results_3gdp$inc_cost_hc[2] / 1000

## subset the data: each row is a simulation iteration
df <- plot_data %>% as.data.frame() %>% filter(!(inc_dalys == 0 & inc_cost_hc == 0)) 

## compute kernel density contour for 95% percentile
contour_data <- df %>%
  # If strategy exists, can group_by(strategy), else skip
  group_modify(~{
    dat <- .x
    if(nrow(dat) < 6) return(dat)

    # 2D KDE
    kde <- kde2d(dat$inc_dalys, dat$inc_cost_hc, n = 200)
    kde_df <- expand.grid(inc_dalys = kde$x, inc_cost_hc = kde$y)
    kde_df$density <- as.vector(kde$z)

    # remove duplicates
    kde_df <- distinct(kde_df)

    # filter to plot limits to avoid zero-contour warnings
    kde_df <- kde_df %>%
      filter(
        inc_dalys >= -500 & inc_dalys <= 2500,
        inc_cost_hc >= -500 & inc_cost_hc <= 1000
      )

    # threshold for top 95% of density
    threshold <- sort(kde_df$density, decreasing = TRUE)[round(0.95 * nrow(kde_df))]
    threshold <- min(max(kde_df$density), threshold) # ensure within density range

    kde_df %>% mutate(threshold = threshold)
  }) %>% 
  ungroup()



p_ceap_3gdp <- ggplot(df, aes(x = inc_dalys, y = inc_cost_hc / 1000)) +
  # PSA points
  geom_point(color = "gray", alpha = 0.3, size = 2) +
  
  # 2D density filled
  # geom_density_2d_filled(
  #   aes(fill = after_stat(level)),
  #   contour_var = "ndensity", 
  #   alpha = 0.8, 
  #   show.legend = TRUE
  # ) +
  # # Use discrete fill from white to red
  # scale_fill_manual(values = colorRampPalette(c("white", "blue"))(10)) +
  
  stat_density_2d(
    aes(fill = after_stat(level)),
    geom = "polygon",
    contour = TRUE,
    alpha = 0.8
  ) +
  scale_fill_gradient(
    low = "blue",
    high = "red",
    name = "Density"
  ) +
  # Base points
  annotate("point", x = 0, y = 0, color = "black", alpha = 0.3, size = 2) +
  annotate("point", x = base_inc_dalys, y = base_inc_cost, color = "#BC3C29FF", alpha = 0.3, size = 2) +

  # Dashed segments
  annotate("segment", x = base_inc_dalys, xend = base_inc_dalys, y = 0, yend = base_inc_cost,
           linetype = "dashed", color = "#BC3C29FF", alpha = 0.3) +
  annotate("segment", x = 0, xend = base_inc_dalys, y = base_inc_cost, yend = base_inc_cost,
           linetype = "dashed", color = "#BC3C29FF", alpha = 0.3) +
  # Axes
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_vline(xintercept = 0, linetype = "solid") +
  # WTP lines
  geom_abline(aes(slope = wtp_1gdp / 1000, intercept = 0), linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_abline(aes(slope = wtp_3gdp / 1000, intercept = 0), linetype = "dashed", color = "black", linewidth = 0.5) +

  # WTP annotations
  annotate("text", x = 490, y = 400,
           label = paste0("WTP = US$", format(round(unique(plot_data$wtp_1gdp), 0), big.mark = ","), " per DALY (1x GDP)"), 
           vjust = -0.5, hjust = 0, color = "black", size = 3) +
  annotate("text", x = 180, y = 450,
           label = paste0("WTP = US$", format(round(unique(plot_data$wtp_3gdp), 0), big.mark = ","), " per DALY (3x GDP)"), 
           vjust = -0.5, hjust = 0, color = "black", size = 3) +

  # Labels and theme
  xlab("DALYs averted") +
  ylab(paste0("Incremental costs, ", ref_year, " US$'000s")) +
  theme_bw() +
  theme(
    legend.position = 'right', 
    legend.title = element_blank(),
    plot.margin = unit(c(0.3, 0.3, 0.2, 0.2),"cm"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(limits = c(-500, 2500), labels = scales::comma, breaks = seq(-500, 2500, 500)) +
  scale_y_continuous(limits = c(-500, 1000), labels = scales::comma, breaks = seq(-500, 1000, 250)) +
  scale_color_lancet()

print(p_ceap_3gdp)