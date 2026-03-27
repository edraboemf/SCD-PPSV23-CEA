# Install packages if needed
# install.packages(c("sf", "ggplot2", "rnaturalearth", "rnaturalearthdata", "dplyr"))

# install.packages("rnaturalearth")
# library("sf")
# library("rnaturalearth")
# library(ggsci)
# library(cowplot) 

# Load Africa map
africa <- ne_countries(continent = "Africa", returnclass = "sf")

# Define meningitis belt countries (WHO definition)
meningitis_belt <- c(
  "Senegal","Gambia","Guinea-Bissau","Guinea","Sierra Leone","Liberia",
  "Côte d'Ivoire","Ghana","Togo","Benin","Burkina Faso","Mali","Niger",
  "Nigeria","Cameroon","Chad","Central African Republic","Sudan","South Sudan",
  "Ethiopia","Eritrea"
)

# Define SCD belt (approximate high-prevalence countries)
scd_belt <- c(
  "Nigeria","Democratic Republic of the Congo","Ghana","Burkina Faso",
  "Cameroon","Togo","Benin","Côte d'Ivoire","Mali","Niger","Uganda",
  "Tanzania","Kenya","Angola","Zambia","Sierra Leone","Liberia","Guinea"
)

# Add classification to dataset
africa <- africa %>%
  mutate(
    region_type = case_when(
      name %in% meningitis_belt & name %in% scd_belt ~ "Both SCD &\nMeningitis Belts",
      name %in% meningitis_belt ~ "Meningitis Belt",
      name %in% scd_belt ~ "SCD Belt",
      TRUE ~ "Other"
    )
  )

  # Create severity score
  africa <- africa %>%
  mutate(
    severity = case_when(
      region_type == "Other" ~ 0,
      region_type == "SCD Belt" ~ 1,
      region_type == "Meningitis Belt" ~ 2,
      region_type == "Both SCD &\nMeningitis Belts" ~ 3
    )
  )
  
  # Coordinates for Burkina Faso (approximate bounding box)
  bf_bbox <- st_as_sfc(st_bbox(africa %>% filter(name == "Burkina Faso")))
  
  # Filter Burkina Faso
  burkina <- africa %>% filter(name == "Burkina Faso")

  # Calculate centroid for annotation
  bf_centroid <- st_centroid(st_union(burkina$geometry))
  bf_coords <- st_coordinates(bf_centroid)

  # Add a column for plotting the main map box
  africa_box <- africa %>%
    mutate(is_bf = ifelse(name == "Burkina Faso", TRUE, FALSE))

  # Plot with red gradient
  main_map <- ggplot(data = africa) +
    geom_sf(aes(fill = severity), color = "black", size = 0.2) +
    geom_sf(data = bf_bbox, fill = NA, color = "black", size = 0.5) +  # Burkina Faso box
    scale_fill_gradient(
      low = "white", #"#fee5d9",   # light pink
      high = "#a50f15",  # deep red
      name = "Burden Level",
      breaks = c(0, 1, 2, 3),
      labels = c("Other", "SCD Belt", "Meningitis Belt", "Both SCD &\nMeningitis Belts")
    ) +
    labs(
      title = ""
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.title.align = 0.0,
      legend.title = element_text(size = 8, face = "bold", vjust = 1.0),
      legend.text = element_text(size = 8, vjust = 1.0),
      plot.title = element_text(size = 10, face = "bold", hjust = 0.0)
    )

  # Burkina Faso inset map with border
  bf_map <- ggplot(data = burkina) +
    geom_sf(aes(fill = severity), color = "black", size = 0.4) +
    scale_fill_gradient(low = "#a50f15", high = "#a50f15", guide = "none") +
    geom_text(
      aes(x = bf_coords[1], y = bf_coords[2], label = "Burkina\nFaso"),
      color = "white", size = 3, fontface = "bold"
    ) +
    theme_void() +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
  
  # Combine main map and inset in southwest corner (Gulf of Guinea)
  final_map <- ggdraw() +
    draw_plot(main_map) +
    draw_plot(bf_map, x = 0.15, y = 0.35, width = 0.15, height = 0.15)  # SW position
  
  print(final_map)

  map_fname <- "meningitis_scd_map.png"
  ggsave(
    filename = map_fname, 
    plot = final_map, 
    path = ppsv23_figures_dir,
    scale = 1, 
    width = 7, 
    height = 5, 
    units = c("in", "cm", "mm", "px")[1], 
    dpi = 300, 
    limitsize = TRUE, 
    bg = "White"
  )
  