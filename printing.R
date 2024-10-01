# Load required libraries
library(dplyr)
library(sf)
library(raster)
library(rayshader)
library(elevatr)
library(stringr)
library(rgl)
library(magick)
library(stringi)
library(zip)
library(systemfonts)

# Set working directory (modify this to your actual working directory)
setwd('~/volcanos')

# Function to extract and register font
import_font <- function(zip_path, font_name) {
  # Create a temporary directory to extract the font
  temp_dir <- tempdir()
  
  # Unzip the font file
  zip::unzip(zip_path, exdir = temp_dir)
  
  # Find the font file (assuming it's a .ttf file)
  font_file <- list.files(temp_dir, pattern = "\\.ttf$", full.names = TRUE)[1]
  
  if (is.na(font_file)) {
    stop("No .ttf font file found in the zip archive")
  }
  
  # Register the font
  systemfonts::register_font(name = font_name, plain = font_file)
  
  cat("Font registered:", font_name, "\n")
}

# Import and register the custom font
import_font("Teko.zip", "Teko_font")

# Function to create safe filenames
create_safe_filename <- function(name) {
  safe_name <- stri_trans_general(name, "Latin-ASCII")
  safe_name <- gsub("[^a-zA-Z0-9._-]", "_", safe_name)
  safe_name <- tolower(safe_name)
  return(safe_name)
}

# Read the volcano data
volcanoes <- read.csv("processed_volcanoes.csv", encoding = "UTF-8", stringsAsFactors = FALSE)

# Remove rows with missing Latitude or Longitude
volcanoes_clean <- volcanoes %>%
  filter(!is.na(Latitude) & !is.na(Longitude))

# Convert to sf object
volcanoes_sf <- st_as_sf(volcanoes_clean, coords = c("Longitude", "Latitude"), crs = 4326)

# Function to get and process DEM data for a single volcano
get_volcano_dem <- function(volcano, buffer_size = 10000) {
  tryCatch({
    volcano_buffer <- st_buffer(volcano, dist = buffer_size)
    elev <- get_elev_raster(volcano_buffer, z = 11, src = "aws")
    elev_crop <- crop(elev, volcano_buffer)
    return(elev_crop)
  }, error = function(e) {
    message("Error getting DEM data: ", e$message)
    return(NULL)
  })
}

# Simplified function to create 3D visualization for a single volcano
visualize_volcano <- function(dem, output_file) {
  tryCatch({
    if (is.null(output_file) || nchar(output_file) == 0) {
      stop("Invalid output filename")
    }
    
    # Convert raster to matrix
    elmat <- raster_to_matrix(dem)
    
    # Create a simple shaded relief
    shaded_relief <- sphere_shade(elmat, texture = "desert")
    
    # Plot the 3D visualization
    plot_3d(shaded_relief, elmat,
            zscale = 10,
            fov = 0,
            theta = 135,
            zoom = 0.75,
            phi = 45,
            windowsize = c(500, 500),
            solid = FALSE,
            shadow = TRUE,
            soliddepth = -10,
            baseshape = "hex")
    
    # Save the plot
    rgl.snapshot(filename = output_file)
    
    # Close the rgl device
    close3d()
    
    message("Visualization saved: ", output_file)
  }, error = function(e) {
    message("Error creating visualization: ", e$message)
    print(traceback())
  })
}

# Create output directory if it doesn't exist
dir.create("figures", showWarnings = FALSE)

# Process and visualize each volcano
volcano_images <- list()
for (i in 1:nrow(volcanoes_sf)) {
  volcano <- volcanoes_sf[i,]
  volcano_name <- as.character(volcano$Nombre)
  
  # Remove asterisks from the volcano name
  volcano_name <- gsub("\\*", "", volcano_name)
  
  cat("Processing", volcano_name, "\n")
  
  # Get DEM data
  dem <- get_volcano_dem(volcano)
  
  if (!is.null(dem)) {
    # Create visualization
    file_name <- create_safe_filename(volcano_name)
    output_file <- file.path("figures", paste0("rayshader_mt", sprintf("%02d", i), "_", file_name, ".png"))
    visualize_volcano(dem, output_file)
    
    # Read the image and add text information
    if (file.exists(output_file)) {
      img <- try(image_read(output_file))
      if (!inherits(img, "try-error")) {
        
        # Option 1: Using nested if-else statements
        text_location <- if (nchar(volcano_name) > 30) {
          "+100+10"
        } else if (nchar(volcano_name) > 20) {
          "+120+10"
        } else {
          "+185+10"
        }
        
        img <- image_annotate(img, volcano_name, location = text_location, color = "black", size = 21, font = "Teko_font", weight=700)
        img <- image_annotate(img, paste(as.character(volcano$Ubicación),"-",as.character(volcano$Coordenadas)), location = "+180+40", color = "black", size = 15, font = "Teko_font")
        img <- image_annotate(img, paste("Altura:", as.character(volcano$Altura),"msnm"), location = "+180+60", color = "black", size = 15, font = "Teko_font")
        
        # Store the image in the list
        volcano_images[[i]] <- img
        
        # Overwrite the original image with the annotated one
        image_write(img, path = output_file)
      } else {
        message("Error reading image file: ", output_file)
      }
    } else {
      message("Output file not created: ", output_file)
    }
  } else {
    cat("Skipping visualization for", volcano_name, "due to error in DEM data retrieval\n")
  }
  
  cat("Completed", volcano_name, "\n\n")
}

# Debug information
cat("Number of images processed:", length(volcano_images), "\n")

# Combine images into a single image with 4 volcanoes per row
num_volcanoes <- length(volcano_images)
num_rows <- ceiling(num_volcanoes / 4)

# Create a white background with extra space for titles
title_height <- 200  # Adjust this value to increase or decrease space for titles
footer_height <- 50  # Adjust this value to increase or decrease space for footer
combined_image <- image_blank(2000, 500 * num_rows + title_height + footer_height, color = "white")


# Add titles
combined_image <- image_annotate(combined_image, "Volcanes en Argentina", 
                                 location = "+800+50", color = "black", 
                                 size = 60, font = "Teko_font", weight = 700)

combined_image <- image_annotate(combined_image, "Argentine Volcanoes", 
                                 location = "+900+120", color = "black", 
                                 size = 40, font = "Teko_font")


# Add footer
footer_y <- image_info(combined_image)$height - footer_height / 2  # Position for footer
combined_image <- image_annotate(combined_image, "thomas Artopoulos - https://github.com/thomasartopoulos/argentine-volcanoes", 
                                 location = paste0("+750+", footer_y)
                                 , color = "black", 
                                 size = 20, font = "Teko_font",weight = 700)

# Combine volcano images
for (i in 1:num_volcanoes) {
  if (!is.null(volcano_images[[i]])) {
    row <- ceiling(i / 4)
    col <- (i - 1) %% 4
    x_offset <- col * 500
    y_offset <- (row - 1) * 500 + title_height  # Add title_height to y_offset
    
    cat("Combining image", i, "at position", x_offset, y_offset, "\n")
    
    combined_image <- image_composite(combined_image, volcano_images[[i]], 
                                      offset = paste0("+", x_offset, "+", y_offset))
  } else {
    cat("Skipping image", i, "as it is NULL\n")
  }
}

# Save the combined image
image_write(combined_image, path = "figures/combined_volcanoes.png")

cat("Combined image saved. Check 'figures/combined_volcanoes.png'.\n")
cat("Processing complete. Check the 'figures' directory for individual and combined images.\n")