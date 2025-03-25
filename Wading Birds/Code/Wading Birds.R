### Wading Bird Data (March 21st 2025)
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggmap)
library(sf)
library(viridis)
library(ggspatial)
library(maps)
library(elevatr) 
library(terra)    

birds <- read.csv("Master_LakeO_Foraging_UF.csv")

birds[is.na(birds)] <- 0 ### Fill all NAs with 0's so now we have absence data.
birds <- birds %>% filter(Y != 0) ### Filter lat and long for any points that are 0s (Around 5 were)

birds <- birds %>% rename(Lat = Y, Long = X) ### Rename Y and X columns to Lat and Long
birds <- birds %>%
  separate(Date, into = c("Year", "Month", "Day"), sep = "-")

birds <- birds %>% filter(!is.na(Long)) ### Some Long are NAs
birds$Long <- as.numeric(birds$Long)

### Creat a visual to make sure points are in and around Lake O (Some are outside and need to be filtered out)
ggplot(birds, aes(x = Long, y = Lat)) +
  borders("state", region = "florida", colour = "gray80", fill = "gray95") +
  geom_point(color = "blue", size = 2) +
  theme_minimal() +
  labs(title = "Sample Sites in Florida",
       x = "Longitude",
       y = "Latitude")

# Define the bounding box for Lake Okeechobee to remove outlier points 
lake_bbox <- c(min_lon = -81.1, max_lon = -80.5, min_lat = 26.5, max_lat = 27.5)

# Filter the birds dataframe to keep only points within Lake Okeechobee (Around 42 removed)
birds_filtered <- birds %>%
  filter(Long >= lake_bbox["min_lon"], Long <= lake_bbox["max_lon"],
         Lat >= lake_bbox["min_lat"], Lat <= lake_bbox["max_lat"])

ggplot(birds_filtered, aes(x = Long, y = Lat)) +
  borders("state", region = "florida", colour = "gray80", fill = "gray95") +
  geom_point(color = "blue", size = 2) +
  theme_minimal() +
  labs(title = "Sample Sites in Florida",
       x = "Longitude",
       y = "Latitude")


# Function to create a bird count heatmap using birds_filtered dataframe (to ensure all points within Lake O)
create_bird_heatmap <- function(selected_year = NULL) {

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
      Lat_rounded = round(Lat, 1),
      Long_rounded = round(Long, 1)
    )
  # Aggregate bird counts by rounded coordinates
  agg_df <- filtered_df %>%
    group_by(Lat_rounded, Long_rounded) %>%
    summarize(Total_Birds = sum(All.Bird.Total), .groups = "drop")
  # Create title based on year selection
  if (!is.null(selected_year)) {
    plot_title <- paste("Bird Count Heatmap (", selected_year, ")", sep = "")
  } else {
    plot_title <- "Bird Count Heatmap (All Years)"
  }
  # Create the heatmap using ggplot2
  p <- ggplot(agg_df, aes(x = Long_rounded, y = Lat_rounded, fill = Total_Birds)) +
    geom_tile(color = "white", linewidth = 0.1) +
    scale_fill_viridis(
      option = "viridis",
      direction = -1,
      name = "Bird Count"
    ) +
    geom_text(aes(label = round(Total_Birds, 0)), color = "white", size = 3) +
    theme_minimal() +
    labs(
      title = plot_title,
      x = "Longitude",
      y = "Latitude"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      axis.title = element_text(size = 12),
      legend.title = element_text(size = 10)
    ) +
    coord_fixed()
  
  return(p)
}

# Create heatmap for all years
bird_heatmap_all <- create_bird_heatmap()
print(bird_heatmap_all)

# Creat a yearly heatmap
create_yearly_heatmaps <- function() {
  years <- unique(birds_filtered$Year)
  
  for (year in years) {
    cat("Creating heatmap for year:", year, "\n")
    p <- create_bird_heatmap(selected_year = year)
    if (!is.null(p)) {
      ggsave(paste0("bird_heatmap_", year, ".png"), plot = p, width = 10, height = 8)
    }
  }
  
  # Also create an all-years heatmap
  cat("Creating combined heatmap for all years\n")
  p_all <- create_bird_heatmap()
  if (!is.null(p_all)) {
    ggsave("bird_heatmap_all_years.png", plot = p_all, width = 10, height = 8)
  }
}

birds_heatmap_yearly <- create_yearly_heatmaps()
print(birds_heatmap_yearly)



