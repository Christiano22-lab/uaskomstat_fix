# Minimal test for load_shapefile_data function
library(sf)

# Load only the function definition from app.R
source_lines <- readLines("app.R")

# Find the start and end of load_shapefile_data function
start_line <- grep("load_shapefile_data <- function\\(\\)", source_lines)
if (length(start_line) > 0) {
  # Find the matching closing brace
  brace_count <- 0
  end_line <- start_line[1]
  
  for (i in start_line[1]:length(source_lines)) {
    line <- source_lines[i]
    brace_count <- brace_count + length(gregexpr("\\{", line)[[1]]) - length(gregexpr("\\}", line)[[1]])
    if (i > start_line[1] && brace_count == 0) {
      end_line <- i
      break
    }
  }
  
  # Extract and evaluate the function
  func_lines <- source_lines[start_line[1]:end_line]
  func_code <- paste(func_lines, collapse = "\n")
  eval(parse(text = func_code))
  
  cat("✅ Function loaded successfully\n")
  
  # Test the function
  tryCatch({
    result <- load_shapefile_data()
    if (!is.null(result)) {
      cat("✅ Function executed successfully\n")
      cat("Rows:", nrow(result), "\n")
      cat("Columns:", ncol(result), "\n")
      cat("Key columns present:\n")
      key_cols <- c("DISTRICTCODE", "Latitude", "Longitude")
      for (col in key_cols) {
        if (col %in% names(result)) {
          cat("  ✅", col, "\n")
        } else {
          cat("  ❌", col, "(missing)\n")
        }
      }
    } else {
      cat("⚠️  Function returned NULL\n")
    }
  }, error = function(e) {
    cat("❌ Error executing function:", e$message, "\n")
  })
} else {
  cat("❌ Could not find load_shapefile_data function in app.R\n")
}