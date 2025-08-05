# Sales & Supply Chain Analytics Dashboard - Package Installation Script
# This script installs all required packages for the dashboard

# Function to install packages if not already installed
install_if_missing <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE, quietly = TRUE)) {
      install.packages(package, dependencies = TRUE)
      cat("Installed:", package, "\n")
    } else {
      cat("Package already installed:", package, "\n")
    }
  }
}

# Core packages for the main application
core_packages <- c(
  "shiny",
  "bslib", 
  "shinydashboard",
  "dplyr",
  "ggplot2",
  "lubridate",
  "DT",
  "leaflet",
  "leaflet.extras",
  "tidyr",
  "cluster",
  "factoextra",
  "isotree",
  "scales",
  "tidyverse"
)

# Additional packages for clustering and data processing
clustering_packages <- c(
  "fastDummies",
  "data.table",
  "lattice",
  "tidymodels"
)

# Install all packages
cat("Installing core packages...\n")
install_if_missing(core_packages)

cat("\nInstalling clustering packages...\n")
install_if_missing(clustering_packages)

cat("\nAll packages installed successfully!\n")
cat("You can now run the dashboard with: shiny::runApp()\n")

# Optional: Load packages to verify installation
cat("\nVerifying package installation...\n")
for (package in c(core_packages, clustering_packages)) {
  tryCatch({
    library(package, character.only = TRUE)
    cat("✓", package, "loaded successfully\n")
  }, error = function(e) {
    cat("✗ Error loading", package, ":", e$message, "\n")
  })
}

cat("\nSetup complete! Ready to run the dashboard.\n") 