# Test script untuk memverifikasi integrasi shapefile
library(sf)

# Test fungsi load_shapefile_data
load_shapefile_data <- function() {
  tryCatch({
    # Load shapefile menggunakan sf package
    shapefile_data <- st_read("sovi_administrasi_kabupaten.shp", quiet = TRUE)
    
    # Convert ke data frame biasa dan ambil centroid untuk koordinat
    shapefile_df <- st_drop_geometry(shapefile_data)
    centroids <- st_centroid(shapefile_data$geometry)
    coords <- st_coordinates(centroids)
    
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
    
    return(shapefile_df)
    
  }, error = function(e) {
    cat("Error loading shapefile:", e$message, "\n")
    return(NULL)
  })
}

# Test the function
cat("Testing shapefile loading...\n")
shapefile_data <- load_shapefile_data()

if (!is.null(shapefile_data)) {
  cat("✅ Shapefile loaded successfully!\n")
  cat("Number of records:", nrow(shapefile_data), "\n")
  cat("Columns:", paste(names(shapefile_data), collapse = ", "), "\n")
  cat("First few coordinates:\n")
  print(head(shapefile_data[, c("nmkab", "DISTRICTCODE", "Latitude", "Longitude")]))
  
  # Test some SOVI variables
  cat("\nSample SOVI data:\n")
  print(head(shapefile_data[, c("nmkab", "POPULATION", "CHILDREN", "POVERTY")]))
} else {
  cat("❌ Failed to load shapefile\n")
}