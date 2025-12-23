# =============================================================================
# MultiOmicsSurvey Dashboard - Application Entry Point
# =============================================================================

# =============================================================================
# Conditional Package Installation
# =============================================================================

# List of required packages
required_packages <- c(
  "shiny",
  "shinydashboard",

  "plotly",
  "dplyr",
  "tidyr",
  "colourpicker",
  "shinyWidgets",
  "shinycssloaders",
  "base64enc"
)

# Function to check and install missing packages
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages) > 0) {
    message("Installing missing packages: ", paste(new_packages, collapse = ", "))
    install.packages(new_packages, dependencies = TRUE)
  }
}

# Install missing packages
install_if_missing(required_packages)

# =============================================================================
# Load Application
# =============================================================================

# Load global functions, data, and variables
source("global.R")

# Load UI and server definitions
source("ui.R")
source("server.R")

# Run the application
shinyApp(ui = ui, server = server)
