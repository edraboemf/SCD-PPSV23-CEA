#################################################################################################
## TORNADO PLOT
#################################################################################################

##******************************************
##* Data for the Tornado diagram
##******************************************
## original value of output
owsa_output <- owsa_output_3gdp
base.value <- unique(owsa_output$owsa_table$base)
wtp.three.time.gdp <- unique(owsa_output$base_result$wtp_who_gdp)
wtp.one.time.gdp <- wtp.three.time.gdp / 3

## calculate the difference between upper and lower bounds
owsa_df <- owsa_output$owsa_table %>%
  mutate(diff = abs(high - low)) %>%
  select(parameter, low, high, diff) %>%
  filter(parameter %in% names(which(ppsv23_params_include == 1))) %>%
  filter(diff >= 0.01 * max(diff))

## get order of parameters according to size of intervals
order.parameters <- owsa_df %>% 
  arrange(diff) %>%
  mutate(parameter = factor(x = parameter, levels = parameter)) %>%
  select(parameter) %>% 
  unlist() %>% 
  levels()

## width of columns in plot (value between 0 and 1)
width <- 0.75

## get data frame in shape for ggplot and geom_rect
plot_data <- owsa_df %>% 
  gather(key = 'type', value = 'output.value', low:high) %>%
  select(parameter, type, output.value, diff) %>%
  mutate(
    parameter = factor(parameter, levels = order.parameters),
    ymin = pmin(output.value, base.value),
    ymax = pmax(output.value, base.value),
    xmin = as.numeric(parameter) - width/2,
    xmax = as.numeric(parameter) + width/2,
    type = factor(
      type, 
      levels = c("low", "high"), 
      labels = c("Lower bound", "Upper bound")
    )
  )

# View(plot_data)

## create plot: (use scale_x_continuous to change labels in y axis to name of parameters)
p_owsa <- ggplot() + 
  geom_rect(data = plot_data, aes(ymax = ymax, ymin = ymin, xmax = xmax, xmin = xmin, fill = type)) +
  geom_hline(yintercept = base.value, ) +
  geom_hline(yintercept = wtp.one.time.gdp, linetype = "dashed") +
  geom_hline(yintercept = wtp.three.time.gdp, linetype = "dashed") +
  annotate(
    "text", 
    x = 0.1, 
    y = base.value, 
    label = paste("ICER = US$", paste0(formatC(base.value, format = "f", big.mark = ",", digits = 0), " per DALY averted\n")), 
    hjust = 1,
    size = 3.3
  ) +
  annotate(
    "text", 
    x = 3, 
    y = wtp.one.time.gdp, 
    label = paste(" WTP = US$", paste0(formatC(wtp.one.time.gdp, format = "f", big.mark = ",", digits = 0), "  \nper DALY averted", "  \n(1 x GDP per capita)")), 
    hjust = 1
  ) +
  annotate(
    "text", 
    x = 6, 
    y = wtp.three.time.gdp, 
    label = paste("WTP = US$", paste0(formatC(wtp.three.time.gdp, format = "f", big.mark = ",", digits = 0), " ", "\nper DALY averted ",  "\n(3 x GDP per capita) ")), 
    hjust = 0
  ) +
  labs(
    x = "Model input paameters", 
    y = paste0("ICER, ", ref_year, " US$ per DALYs averted")
  ) + 
  theme_bw() +
  theme(
    legend.position = c(0.9, 0.10),
    legend.title = element_blank(),
    #legend.background = element_rect(fill = "transparent", colour = "black", size = 0.3),
    legend.background = element_rect(fill = "white", colour = "black", size = 0.3),
    legend.key.size = unit(1, 'cm'), #change legend key size
    legend.key.height = unit(0.3, 'cm'), #change legend key height
    legend.key.width = unit(0.5, 'cm'), #change legend key width
    plot.margin = unit(c(0.3, 0.3, 0.2, 0.2),"cm"),
    panel.grid.minor = element_blank()
  ) + 
  scale_x_continuous(
    breaks = c(1:length(order.parameters)), 
    labels = ppsv23_params_label[order.parameters], 
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(
    labels = scales::comma, 
    limits = c(-1000, 5000), 
    breaks = seq(-1000, 5000, 500), 
    expand = c(0.01, 0.01)
  ) +
  scale_fill_lancet() + 
  coord_flip() 

print(p_owsa)
owsa_fname <- "owsa_icer.png"
ggsave(
  filename = owsa_fname, 
  plot = p_owsa, 
  path = ppsv23_figures_dir,
  scale = 1, 
  width = 11, 
  height = 5, 
  units = c("in", "cm", "mm", "px")[1], 
  dpi = 300, 
  limitsize = TRUE, 
  bg = "White"
)

#################################################################################################
## END OF MODULE
#################################################################################################