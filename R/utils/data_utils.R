# =============================================================================
# Data Manipulation Utilities
# =============================================================================

#' Load and validate master dataset
#' 
#' @param path Path to CSV file
#' @return Validated data frame
#' @examples
#' master_data <- load_master_data("data/master_dataset.csv")
load_master_data <- function(path) {
  tryCatch({
    # Load with check.names=FALSE to preserve original column names (spaces and hyphens)
    data <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    
    # Validate required columns exist (using actual column names with spaces)
    required_clinical <- c("Sample.ID", "Vital status", "Ethnicity", "Race", 
                           "Lymph node status", "Histology", "Menopausal status",
                           "PR status", "ER status", "HER2 status", "Metastasis", "Stage")
    
    missing_cols <- setdiff(required_clinical, names(data))
    if (length(missing_cols) > 0) {
      warning(paste("Missing clinical columns:", paste(missing_cols, collapse = ", ")))
    }
    
    # Convert character columns to factors for better handling
    char_cols <- sapply(data, is.character)
    data[char_cols] <- lapply(data[char_cols], as.factor)
    
    return(data)
  }, error = function(e) {
    stop(paste("Error loading master dataset:", e$message))
  })
}

#' Build index of available plots by scanning directories
#' 
#' @param plots_dir Path to plots directory
#' @return Named list of available plots per method
#' @examples
#' plot_index <- build_plot_index("data/plots")
build_plot_index <- function(plots_dir) {
  plot_index <- list()
  
  # Scan single algorithm folders
  single_alg_dir <- file.path(plots_dir, "single_algorithm")
  if (dir.exists(single_alg_dir)) {
    alg_folders <- list.dirs(single_alg_dir, full.names = FALSE, recursive = FALSE)
    for (alg in alg_folders) {
      alg_path <- file.path(single_alg_dir, alg)
      png_files <- list.files(alg_path, pattern = "\\.png$", full.names = FALSE, ignore.case = TRUE)
      if (length(png_files) > 0) {
        plot_index[[alg]] <- list(
          category = "Single Algorithms",
          path = file.path("single_algorithm", alg),
          files = png_files
        )
      }
    }
  }
  
  # Scan baseline folders
  baseline_folders <- c("MOVICS_baseline" = "MOVICS baseline", 
                        "ER_baseline" = "ER baseline")
  for (folder in names(baseline_folders)) {
    folder_path <- file.path(plots_dir, folder)
    if (dir.exists(folder_path)) {
      png_files <- list.files(folder_path, pattern = "\\.png$", full.names = FALSE, ignore.case = TRUE)
      if (length(png_files) > 0) {
        plot_index[[baseline_folders[folder]]] <- list(
          category = "Baselines",
          path = folder,
          files = png_files
        )
      }
    }
  }
  
  # Scan consensus folders
  consensus_folders <- c("Consensus" = "Consensus (CC)", 
                         "Consensus_more_than_2" = "Consensus > 2 (MC)")
  for (folder in names(consensus_folders)) {
    folder_path <- file.path(plots_dir, folder)
    if (dir.exists(folder_path)) {
      png_files <- list.files(folder_path, pattern = "\\.png$", full.names = FALSE, ignore.case = TRUE)
      if (length(png_files) > 0) {
        plot_index[[consensus_folders[folder]]] <- list(
          category = "Consensus",
          path = folder,
          files = png_files
        )
      }
    }
  }
  
  return(plot_index)
}

#' Get available methods from plot index
#' 
#' @param plot_index The plot index list
#' @return Character vector of available method names
get_available_methods <- function(plot_index) {
  return(names(plot_index))
}

#' Get plots for a specific method
#' 
#' @param plot_index The plot index list
#' @param method Method name
#' @return Character vector of plot filenames
get_method_plots <- function(plot_index, method) {
  if (method %in% names(plot_index)) {
    return(plot_index[[method]]$files)
  }
  return(character(0))
}

#' Get full path to a plot file
#' 
#' @param plots_dir Base plots directory
#' @param plot_index The plot index list
#' @param method Method name
#' @param filename Plot filename
#' @return Full path to the plot file
get_plot_path <- function(plots_dir, plot_index, method, filename) {
  if (method %in% names(plot_index)) {
    return(file.path(plots_dir, plot_index[[method]]$path, filename))
  }
  return(NULL)
}

#' Prepare data for sunburst chart
#' 
#' @param data Data frame
#' @param levels Vector of column names for hierarchy (first should be clustering method)
#' @return List with ids, labels, parents, values for plotly sunburst
#' @examples
#' sunburst_data <- prepare_sunburst_data(master_data, c("CIMLR", "ER.status", "HER2.status"))
prepare_sunburst_data <- function(data, levels) {
  if (length(levels) < 1) {
    stop("At least one level is required for sunburst")
  }
  
  # Remove rows with NA in any of the hierarchy columns
  data_clean <- data[complete.cases(data[, levels, drop = FALSE]), ]
  
  ids <- character()
  labels <- character()
  parents <- character()
  values <- numeric()
  
  # Root level (total)
  root_id <- "Total"
  ids <- c(ids, root_id)
  labels <- c(labels, "Total")
  parents <- c(parents, "")
  values <- c(values, nrow(data_clean))
  
  # Build hierarchy level by level
  for (level_idx in seq_along(levels)) {
    level_col <- levels[level_idx]
    
    if (level_idx == 1) {
      # First level - children of root
      level_counts <- table(data_clean[[level_col]])
      for (level_val in names(level_counts)) {
        id <- paste(level_col, level_val, sep = " - ")
        ids <- c(ids, id)
        labels <- c(labels, as.character(level_val))
        parents <- c(parents, root_id)
        values <- c(values, as.numeric(level_counts[level_val]))
      }
    } else {
      # Subsequent levels - aggregate by all previous levels
      prev_levels <- levels[1:(level_idx - 1)]
      
      # Get unique combinations of previous levels
      if (length(prev_levels) == 1) {
        prev_combos <- unique(data_clean[[prev_levels]])
        prev_combos <- data.frame(V1 = prev_combos, stringsAsFactors = FALSE)
        names(prev_combos) <- prev_levels
      } else {
        prev_combos <- unique(data_clean[, prev_levels, drop = FALSE])
      }
      
      for (i in seq_len(nrow(prev_combos))) {
        # Build filter for this combination
        filter_expr <- rep(TRUE, nrow(data_clean))
        parent_parts <- character()
        
        for (prev_col in prev_levels) {
          filter_expr <- filter_expr & (data_clean[[prev_col]] == prev_combos[i, prev_col])
          parent_parts <- c(parent_parts, paste(prev_col, prev_combos[i, prev_col], sep = " - "))
        }
        
        parent_id <- paste(parent_parts, collapse = " | ")
        filtered_data <- data_clean[filter_expr, ]
        
        if (nrow(filtered_data) > 0) {
          level_counts <- table(filtered_data[[level_col]])
          
          for (level_val in names(level_counts)) {
            id <- paste(parent_id, paste(level_col, level_val, sep = " - "), sep = " | ")
            ids <- c(ids, id)
            labels <- c(labels, as.character(level_val))
            parents <- c(parents, parent_id)
            values <- c(values, as.numeric(level_counts[level_val]))
          }
        }
      }
    }
  }
  
  return(list(
    ids = ids,
    labels = labels,
    parents = parents,
    values = values
  ))
}

#' Get unique levels for a column
#' 
#' @param data Data frame
#' @param column Column name
#' @return Character vector of unique values (sorted)
get_unique_levels <- function(data, column) {

  if (column %in% names(data)) {
    levels <- unique(as.character(data[[column]]))
    levels <- levels[!is.na(levels)]
    return(sort(levels))
  }
  return(character(0))
}

#' Prepare data for bar chart
#' 
#' @param data Data frame
#' @param method_col Column name for clustering method
#' @param group_var Column name for grouping variable (optional)
#' @return Data frame suitable for plotting
prepare_bar_data <- function(data, method_col, group_var = NULL) {
  if (!method_col %in% names(data)) {
    stop(paste("Method column not found:", method_col))
  }
  
  # Remove NA values in method column
  data_clean <- data[!is.na(data[[method_col]]), ]
  
  if (is.null(group_var) || group_var == "") {
    # Simple count by cluster
    counts <- as.data.frame(table(data_clean[[method_col]]), stringsAsFactors = FALSE)
    names(counts) <- c("Cluster", "Count")
    return(counts)
  } else {
    # Count by cluster and grouping variable
    if (!group_var %in% names(data_clean)) {
      stop(paste("Grouping variable not found:", group_var))
    }
    
    data_clean <- data_clean[!is.na(data_clean[[group_var]]), ]
    counts <- as.data.frame(table(data_clean[[method_col]], data_clean[[group_var]]), 
                            stringsAsFactors = FALSE)
    names(counts) <- c("Cluster", "Group", "Count")
    return(counts)
  }
}
