register_google(key = "AIzaSyAwXtHde4Zz5sG30Kkmj24koUpd2fkSpxI")
# Define bounding box for Lake Okeechobee
lake_bbox <- c(left = -81.2, bottom = 26.5, right = -80.5, top = 27.5)

# Define bounding box for Lake Okeechobee
lake_bbox <- c(left = -81.2, bottom = 26.5, right = -80.5, top = 27.5)
lake_map <- get_googlemap(center = c(lon = -80.85, lat = 27.0), zoom = 10, maptype = "satellite")


create_bird_heatmap_map <- function(selected_year = NULL) {  
  df <- birds_filtered  
  
  # Filter by year if specified  
  if (!is.null(selected_year)) {  
    filtered_df <- df %>% filter(Year == selected_year)  
  } else {  
    filtered_df <- df  
  }  
  
  # Check if we have data  
  if (nrow(filtered_df) == 0) {  
    cat("No data available for the specified year:", selected_year, "\n")  
    return(NULL)  
  }  
  
  # Round coordinates to create grid cells  
  filtered_df <- filtered_df %>%  
    mutate(  
      Lat_rounded = round(Lat / 0.06) * 0.06,  # Higher precision  
      Long_rounded = round(Long / 0.06) * 0.06  
    )  
  
  # Aggregate bird counts by rounded coordinates  
  agg_df <- filtered_df %>%  
    group_by(Lat_rounded, Long_rounded) %>%  
    summarize(Avg_Birds = mean(All.Bird.Total, na.rm = TRUE),  
              Count = n(), .groups = "drop")  
  
  # Create label column for displaying text inside each grid cell  
  agg_df <- agg_df %>%
    mutate(Label = paste0("Avg: ", round(Avg_Birds, 1), "\nn = ", Count))
  
  # Create title based on year selection  
  plot_title <- ifelse(!is.null(selected_year),   
                       paste("Bird Count Heatmap (", selected_year, ")", sep = ""),  
                       "Bird Count Heatmap (All Years)")  
  
  # Plot using ggmap  
  p <- ggmap(lake_map) +  
    geom_tile(data = agg_df, aes(x = Long_rounded, y = Lat_rounded, fill = Avg_Birds),   
              alpha = 0.7) +  # Adjust transparency  
    scale_fill_viridis(option = "viridis", direction = -1, name = "Avg Bird Count") +  
    geom_text(data = agg_df, aes(x = Long_rounded, y = Lat_rounded, label = Label),  
              color = "white", size = 2, fontface = "bold", vjust = 0.5, hjust = 0.5) +  # Add text labels  
    labs(title = plot_title, x = "Longitude", y = "Latitude") +  
    theme_minimal() +  
    coord_fixed()  
  
  return(p)  
}  

# Generate the heatmap overlay on the map  
bird_heatmap_map <- create_bird_heatmap_map()  
print(bird_heatmap_map)  
