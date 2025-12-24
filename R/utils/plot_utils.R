# =============================================================================
# Plotting Utilities
# =============================================================================

#' Create interactive bar chart
#' 
#' @param data Data frame with Cluster, Group (optional), and Count columns
#' @param chart_type "Stacked" or "Grouped"
#' @param cluster_colors Named vector of colors for clusters
#' @param group_colors Named vector of colors for groups (optional)
#' @param title Chart title
#' @param x_label X-axis label
#' @param y_label Y-axis label
#' @return Plotly object
create_bar_chart <- function(data, chart_type = "Stacked", 
                             cluster_colors = NULL, group_colors = NULL,
                             title = "", x_label = "Cluster", y_label = "Count") {
  
  if ("Group" %in% names(data)) {
    # Bar chart with grouping variable
    barmode <- ifelse(chart_type == "Stacked", "stack", "group")
    
    # Create plot with groups as different traces
    groups <- unique(data$Group)
    
    p <- plot_ly()
    
    for (grp in groups) {
      grp_data <- data[data$Group == grp, ]
      
      # Determine color for this group
      grp_color <- if (!is.null(group_colors) && grp %in% names(group_colors)) {
        group_colors[[grp]]
      } else {
        NULL
      }
      
      # Use explicit values instead of formula to avoid scoping issues
      p <- add_trace(p, 
                     x = grp_data$Cluster,
                     y = grp_data$Count,
                     type = "bar",
                     name = grp,
                     marker = list(color = grp_color),
                     hovertemplate = paste0(
                       "<b>Cluster:</b> %{x}<br>",
                       "<b>", grp, ":</b> %{y}<br>",
                       "<extra></extra>"
                     ))
    }
    
    p <- layout(p,
                barmode = barmode,
                title = list(text = title, x = 0.5),
                xaxis = list(title = x_label, categoryorder = "category ascending"),
                yaxis = list(title = y_label),
                legend = list(orientation = "h", y = -0.2),
                hovermode = "closest")
    
  } else {
    # Simple bar chart (no grouping)
    # Use cluster colors if provided
    colors <- if (!is.null(cluster_colors)) {
      sapply(data$Cluster, function(c) {
        if (c %in% names(cluster_colors)) cluster_colors[[c]] else "#999999"
      })
    } else {
      NULL
    }
    
    p <- plot_ly(data,
                 x = ~Cluster,
                 y = ~Count,
                 type = "bar",
                 marker = list(color = colors),
                 hovertemplate = paste0(
                   "<b>Cluster:</b> %{x}<br>",
                   "<b>Count:</b> %{y}<br>",
                   "<extra></extra>"
                 ))
    
    p <- layout(p,
                title = list(text = title, x = 0.5),
                xaxis = list(title = x_label, categoryorder = "category ascending"),
                yaxis = list(title = y_label),
                hovermode = "closest")
  }
  
  return(p)
}

#' Create interactive sunburst chart
#' 
#' @param sunburst_data List with ids, labels, parents, values (from prepare_sunburst_data)
#' @param colors Named list of colors keyed by label
#' @param title Chart title
#' @return Plotly object
create_sunburst <- function(sunburst_data, colors = NULL, title = "") {
  
  # Build color vector matching the order of ids
  marker_colors <- if (!is.null(colors)) {
    sapply(sunburst_data$labels, function(lbl) {
      if (lbl %in% names(colors)) colors[[lbl]] else "#CCCCCC"
    })
  } else {
    NULL
  }
  
  p <- plot_ly(
    ids = sunburst_data$ids,
    labels = sunburst_data$labels,
    parents = sunburst_data$parents,
    values = sunburst_data$values,
    type = "sunburst",
    branchvalues = "total",
    marker = list(colors = marker_colors),
    hovertemplate = paste0(
      "<b>%{label}</b><br>",
      "Count: %{value}<br>",
      "Percent of parent: %{percentParent:.1%}<br>",
      "<extra></extra>"
    ),
    textinfo = "label+percent parent"
  )
  
  p <- layout(p,
              title = list(text = title, x = 0.5))
  
  return(p)
}

#' Create a method comparison bar chart
#' 
#' @param data Master dataset
#' @param methods Vector of method column names
#' @param group_var Grouping variable column name
#' @param chart_type "Stacked" or "Grouped"
#' @param group_colors Named vector of colors for groups
#' @return List of plotly objects, one per method
create_method_comparison_charts <- function(data, methods, group_var, 
                                            chart_type = "Stacked",
                                            group_colors = NULL) {
  charts <- list()
  
  for (method in methods) {
    if (method %in% names(data)) {
      bar_data <- prepare_bar_data(data, method, group_var)
      
      # Get display name for method
      display_name <- names(METHOD_MAPPING)[METHOD_MAPPING == method]
      if (length(display_name) == 0) display_name <- method
      
      # Get display name for grouping variable
      group_display <- if (group_var %in% names(CLINICAL_DISPLAY_NAMES)) {
        CLINICAL_DISPLAY_NAMES[group_var]
      } else {
        group_var
      }
      
      chart <- create_bar_chart(
        data = bar_data,
        chart_type = chart_type,
        group_colors = group_colors,
        title = paste(display_name, "Clustering by", group_display),
        x_label = "Cluster",
        y_label = "Count"
      )
      
      charts[[method]] <- chart
    }
  }
  
  return(charts)
}
