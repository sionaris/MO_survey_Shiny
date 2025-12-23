# =============================================================================
# Color Handling Utilities
# =============================================================================

#' Get default colors for a clinical variable
#' 
#' @param variable Clinical variable name (column format, e.g., "ER.status")
#' @param hex_colors The hex_col.rds data (list)
#' @return Named vector of colors, or NULL if not found
#' @examples
#' colors <- get_clinical_colors("ER.status", hex_colors)
get_clinical_colors <- function(variable, hex_colors) {
  # Use CLINICAL_MAPPING to get the correct key in hex_colors
  if (variable %in% names(CLINICAL_MAPPING)) {
    key <- CLINICAL_MAPPING[variable]
    if (key %in% names(hex_colors)) {
      return(hex_colors[[key]])
    }
  }
  
  # Try direct lookup
  if (variable %in% names(hex_colors)) {
    return(hex_colors[[variable]])
  }
  
  return(NULL)
}

#' Get cluster colors mapped to levels
#' 
#' @param levels Vector of cluster level names
#' @param cluster_colors The clust_col.rds vector
#' @return Named vector of colors
#' @examples
#' colors <- get_cluster_colors(c("1", "2", "3"), cluster_colors)
get_cluster_colors <- function(levels, cluster_colors) {
  # Sort levels alphanumerically
  sorted_levels <- sort(levels)
  
  # Map sorted levels to colors in order (cycling if needed)
  n_colors <- length(cluster_colors)
  colors <- character(length(sorted_levels))
  
  for (i in seq_along(sorted_levels)) {
    color_idx <- ((i - 1) %% n_colors) + 1
    colors[i] <- cluster_colors[color_idx]
  }
  
  names(colors) <- sorted_levels
  return(colors)
}

#' Build a combined color map for sunburst
#' 
#' @param data Master dataset
#' @param levels Vector of column names in hierarchy
#' @param hex_colors The hex_col.rds data
#' @param cluster_colors The clust_col.rds vector
#' @param custom_colors Optional list of custom colors by level
#' @return Named vector of colors for all labels
build_sunburst_colors <- function(data, levels, hex_colors, cluster_colors, 
                                  custom_colors = NULL) {
  all_colors <- c("Total" = "#FFFFFF")  # Root color
  
  for (level_col in levels) {
    level_values <- unique(as.character(data[[level_col]]))
    level_values <- level_values[!is.na(level_values)]
    
    if (!is.null(custom_colors) && level_col %in% names(custom_colors)) {
      # Use custom colors
      for (val in level_values) {
        if (val %in% names(custom_colors[[level_col]])) {
          all_colors[val] <- custom_colors[[level_col]][val]
        }
      }
    } else if (level_col %in% names(CLINICAL_MAPPING)) {
      # It's a clinical variable - use hex_colors
      clinical_colors <- get_clinical_colors(level_col, hex_colors)
      if (!is.null(clinical_colors)) {
        for (val in level_values) {
          if (val %in% names(clinical_colors)) {
            all_colors[val] <- clinical_colors[val]
          }
        }
      }
    } else {
      # It's a clustering method - use cluster_colors
      method_colors <- get_cluster_colors(level_values, cluster_colors)
      all_colors <- c(all_colors, method_colors)
    }
  }
  
  return(all_colors)
}

#' Generate a color palette for N items
#' 
#' @param n Number of colors needed
#' @param palette Base palette to use ("default", "viridis", "blues")
#' @return Vector of hex colors
generate_palette <- function(n, palette = "default") {
  if (palette == "default") {
    # Use the cluster colors as base and extend if needed
    base_colors <- c("#2EC4B6", "#E71D36", "#FF9F1C", "#BDD5EA", "#FFA5AB",
                     "#011627", "#023E8A", "#9D4EDD", "#f09c6c", "#09f3b3")
    if (n <= length(base_colors)) {
      return(base_colors[1:n])
    } else {
      # Cycle through colors
      return(rep(base_colors, ceiling(n / length(base_colors)))[1:n])
    }
  } else if (palette == "viridis") {
    return(viridis::viridis(n))
  } else if (palette == "blues") {
    return(colorRampPalette(c("#deebf7", "#08519c"))(n))
  }
  
  return(rainbow(n))
}

#' Validate hex color
#' 
#' @param color String to validate
#' @return TRUE if valid hex color, FALSE otherwise
is_valid_hex <- function(color) {
  if (is.null(color) || is.na(color) || !is.character(color)) {
    return(FALSE)
  }
  grepl("^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{8})$", color)
}

#' Ensure color is valid, return default if not
#' 
#' @param color Color to check
#' @param default Default color to return if invalid
#' @return Valid hex color
safe_color <- function(color, default = "#666666") {
  if (is_valid_hex(color)) {
    return(color)
  }
  return(default)
}
