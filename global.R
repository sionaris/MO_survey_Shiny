# =============================================================================
# Global Configuration and Data Loading
# =============================================================================

# Load required packages
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(tidyr)
library(colourpicker)
library(shinyWidgets)
library(shinycssloaders)
library(base64enc)

# Source configuration and utilities
source("R/config/constants.R")
source("R/config/plot_names.R")
source("R/utils/data_utils.R")
source("R/utils/plot_utils.R")
source("R/utils/color_utils.R")

# Source modules
source("R/modules/plots_module.R")
source("R/modules/exploration_module.R")
source("R/modules/benchmarks_module.R")

# =============================================================================
# Load Data
# =============================================================================

# Load master dataset
message("Loading master dataset...")
master_data <- load_master_data("data/master_dataset.csv")
message(paste("Loaded", nrow(master_data), "samples with", ncol(master_data), "columns"))

# Load color palettes
message("Loading color palettes...")
hex_colors <- readRDS("data/hex_col.rds")
cluster_colors <- readRDS("data/clust_col.rds")

# Build plot index (scan available plots on startup)
message("Building plot index...")
plot_index <- build_plot_index("data/plots")
message(paste("Found plots for", length(plot_index), "methods"))

# Define plots directory path
plots_dir <- "data/plots"

# =============================================================================
# Load Benchmark Data
# =============================================================================

message("Loading benchmark data...")
benchmark_data <- list(
  feature = readRDS("data/benchmark_data/feature_perturbation_data.rds"),
  sample = readRDS("data/benchmark_data/sample_perturbation_data.rds")
)
category_colors <- readRDS("data/benchmark_data/category_colors.rds")
method_categories <- readRDS("data/benchmark_data/method_categories.rds")
message("Benchmark data loaded successfully")
