# Standalone shapefile functions for R Shiny Dashboard
# This file contains the shapefile integration functions
# Source this file in app.R to ensure functions are available

# Load required library
if (!require(sf, quietly = TRUE)) {
  cat("Warning: sf package not available. Shapefile functions will not work.\n")
}

# Fungsi untuk load data dari shapefile Indonesia
load_shapefile_data <- function() {
  tryCatch({
    # Check if sf package is available
    if (!requireNamespace("sf", quietly = TRUE)) {
      cat("sf package not available. Cannot load shapefile.\n")
      return(NULL)
    }
    
    # Check if shapefile exists
    if (!file.exists("sovi_administrasi_kabupaten.shp")) {
      cat("Shapefile not found: sovi_administrasi_kabupaten.shp\n")
      return(NULL)
    }
    
    # Load shapefile menggunakan sf package
    shapefile_data <- sf::st_read("sovi_administrasi_kabupaten.shp", quiet = TRUE)
    
    # Convert ke data frame biasa dan ambil centroid untuk koordinat
    shapefile_df <- sf::st_drop_geometry(shapefile_data)
    centroids <- sf::st_centroid(shapefile_data$geometry)
    coords <- sf::st_coordinates(centroids)
    
    # Gabungkan data
    shapefile_df$Longitude <- coords[, 1]
    shapefile_df$Latitude <- coords[, 2]
    
    # Rename kolom untuk konsistensi dengan sistem yang ada
    names(shapefile_df)[names(shapefile_df) == "DISTRICTCO"] <- "DISTRICTCODE"
    
    # Pastikan semua kolom numerik SOVI dalam format yang benar
    numeric_cols <- c("CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE", 
                     "NOELECTRIC", "LOWEDU", "GROWTH", "POVERTY", "ILLITERATE", 
                     "NOTRAINING", "DPRONE", "RENTED", "NOSEWER", "TAPWATER", 
                     "POPULATION", "DISTRICTCODE")
    
    for (col in numeric_cols) {
      if (col %in% names(shapefile_df)) {
        shapefile_df[[col]] <- as.numeric(shapefile_df[[col]])
      }
    }
    
    cat("✅ Shapefile loaded successfully:", nrow(shapefile_df), "records\n")
    return(shapefile_df)
    
  }, error = function(e) {
    cat("❌ Error loading shapefile:", e$message, "\n")
    return(NULL)
  })
}

# Test function
test_shapefile_function <- function() {
  cat("Testing shapefile integration...\n")
  result <- load_shapefile_data()
  
  if (!is.null(result)) {
    cat("✅ Test passed!\n")
    cat("Data shape:", nrow(result), "rows x", ncol(result), "columns\n")
    
    # Check key columns
    required_cols <- c("DISTRICTCODE", "Latitude", "Longitude")
    missing_cols <- required_cols[!required_cols %in% names(result)]
    
    if (length(missing_cols) == 0) {
      cat("✅ All required columns present\n")
    } else {
      cat("⚠️  Missing columns:", paste(missing_cols, collapse = ", "), "\n")
    }
    
    return(TRUE)
  } else {
    cat("❌ Test failed - function returned NULL\n")
    return(FALSE)
  }
}

# Run test if this script is executed directly
if (sys.nframe() == 0) {
  test_shapefile_function()
}