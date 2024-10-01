# Install required packages if not already installed
if (!requireNamespace("rvest", quietly = TRUE)) install.packages("rvest")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
if (!requireNamespace("measurements", quietly = TRUE)) install.packages("measurements")

# Load required libraries
library(rvest)
library(dplyr)
library(sf)
library(stringr)
library(measurements)

setwd('~/volcanos')

# Improved function to convert DMS to decimal degrees
dms_to_dd <- function(dms) {
  # Extract degrees, minutes, seconds, and direction
  # This pattern matches both single and double quotes for seconds
  parts <- str_match(dms, "(\\d+)°\\s*(\\d+)'\\s*(\\d+)[\"']+\\s*([NSWO])")
  
  if (is.na(parts[1])) {
    return(NA)
  }
  
  deg <- as.numeric(parts[2])
  min <- as.numeric(parts[3])
  sec <- as.numeric(parts[4])
  dir <- parts[5]
  
  # Convert to decimal degrees
  dd <- deg + min/60 + sec/3600
  
  # Adjust sign based on direction
  if (dir %in% c("S", "O", "W")) {
    dd <- -dd
  }
  
  return(dd)
}

# Function to split and convert coordinates
split_and_convert_coords <- function(coord_string) {
  coords <- str_split(coord_string, "(?<=([NSWO]))")[[1]]
  lat <- dms_to_dd(coords[1])
  lon <- dms_to_dd(coords[2])
  return(c(lat, lon))
}

# Define the URL
url <- "https://www.ign.gob.ar/NuestrasActividades/Geografia/DatosArgentina/VolcanesActivos"

# Read the webpage
webpage <- read_html(url)

# Extract the table by class
table <- webpage %>% 
  html_node(".table.table-striped.table-bordered.table-info-geo") %>% 
  html_table() %>%
  dplyr::as_tibble()  # Ensure the table is a tibble

# Process the coordinates
coords_processed <- lapply(table$Coordenadas, split_and_convert_coords)

# Add new columns for latitude and longitude
table$Latitude <- sapply(coords_processed, function(x) x[1])
table$Longitude <- sapply(coords_processed, function(x) x[2])

# Clean the Nombre column by removing asterisks
table$Nombre <- gsub("\\*", "", table$Nombre)

# Print the first few rows of the processed table
print(head(table))

write.csv(table, "processed_volcanoes.csv", row.names = FALSE)