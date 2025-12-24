# =============================================================================
# Exploration Module - Interactive Data Exploration
# =============================================================================

#' Exploration Module UI
#' 
#' @param id Module namespace ID
#' @return UI elements for the exploration tab
explorationUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tabsetPanel(
      id = ns("exploration_tabs"),
      type = "tabs",
      
      # ===== Bar Charts Sub-tab =====
      tabPanel(
        title = "Bar Charts",
        value = "bar_charts",
        icon = icon("chart-bar"),
        br(),
        fluidRow(
          # Controls column
          column(
            width = 3,
            div(
              class = "control-panel",
              
              # Method selection (multiple)
              h4("Method Selection"),
              checkboxGroupInput(
                inputId = ns("bar_methods"),
                label = "Select Methods",
                choices = setNames(
                  as.character(METHOD_MAPPING[SINGLE_ALGORITHMS]),
                  SINGLE_ALGORITHMS
                ),
                selected = "CIMLR"
              ),
              
              hr(),
              
              # Additional methods (baselines)
              checkboxGroupInput(
                inputId = ns("bar_baselines"),
                label = "Baselines & Consensus",
                choices = setNames(
                  as.character(METHOD_MAPPING[BASELINE_METHODS]),
                  BASELINE_METHODS
                ),
                selected = NULL
              ),
              
              hr(),
              
              # Grouping variable
              selectInput(
                inputId = ns("bar_group_var"),
                label = "Grouping Variable",
                choices = setNames(CLINICAL_VARIABLES, CLINICAL_DISPLAY_NAMES[CLINICAL_VARIABLES]),
                selected = "ER status"
              ),
              
              # Chart type
              radioButtons(
                inputId = ns("bar_chart_type"),
                label = "Chart Type",
                choices = c("Stacked", "Grouped"),
                selected = "Stacked",
                inline = TRUE
              ),
              
              hr(),
              
              # Color customization panel
              div(
                class = "color-panel",
                h5("Color Customization"),
                actionButton(
                  inputId = ns("bar_use_default_colors"),
                  label = "Use Default Colors",
                  class = "btn-info btn-sm btn-block"
                ),
                br(), br(),
                uiOutput(ns("bar_color_pickers"))
              )
            )
          ),
          
          # Plot display column
          column(
            width = 9,
            div(
              class = "plot-card",
              # Navigation for multiple methods
              uiOutput(ns("bar_nav_controls")),
              
              # Plot display
              withSpinner(
                plotlyOutput(ns("bar_plot"), height = "500px"),
                type = 4,
                color = "#3c8dbc"
              ),
              
              # Method indicator
              div(
                style = "text-align: center; margin-top: 10px;",
                textOutput(ns("bar_method_indicator"))
              )
            )
          )
        )
      ),
      
      # ===== Sunbursts Sub-tab =====
      tabPanel(
        title = "Sunbursts",
        value = "sunbursts",
        icon = icon("circle-notch"),
        br(),
        fluidRow(
          # Controls column
          column(
            width = 3,
            div(
              class = "control-panel",
              
              # Method selection (single)
              selectInput(
                inputId = ns("sunburst_method"),
                label = "Select Clustering Method",
                choices = setNames(
                  as.character(METHOD_MAPPING),
                  names(METHOD_MAPPING)
                ),
                selected = "CIMLR"
              ),
              
              hr(),
              
              # Hierarchy levels
              h5("Hierarchy Levels"),
              p(class = "help-text", 
                "Select up to 3 clinical variables. The clustering method will be the root level."),
              
              selectizeInput(
                inputId = ns("sunburst_levels"),
                label = "Clinical Variables (in order)",
                choices = setNames(CLINICAL_VARIABLES, CLINICAL_DISPLAY_NAMES[CLINICAL_VARIABLES]),
                selected = DEFAULT_SUNBURST_LEVELS,
                multiple = TRUE,
                options = list(
                  maxItems = 3,
                  plugins = list('remove_button', 'drag_drop')
                )
              ),
              
              hr(),
              
              # Color customization panel
              div(
                class = "color-panel",
                h5("Color Customization"),
                actionButton(
                  inputId = ns("sunburst_use_default_colors"),
                  label = "Use Default Colors",
                  class = "btn-info btn-sm btn-block"
                ),
                br(), br(),
                
                # Cluster colors
                div(
                  class = "color-section",
                  h6("Cluster Colors"),
                  uiOutput(ns("sunburst_cluster_colors"))
                ),
                
                # Clinical variable colors (collapsible)
                uiOutput(ns("sunburst_clinical_colors"))
              )
            )
          ),
          
          # Plot display column
          column(
            width = 9,
            div(
              class = "plot-card",
              withSpinner(
                plotlyOutput(ns("sunburst_plot"), height = "600px"),
                type = 4,
                color = "#3c8dbc"
              )
            )
          )
        )
      )
    )
  )
}

#' Exploration Module Server
#' 
#' @param id Module namespace ID
#' @param master_data Data frame containing the master dataset
#' @param hex_colors List of clinical variable colors from hex_col.rds
#' @param cluster_colors Vector of cluster colors from clust_col.rds
explorationServer <- function(id, master_data, hex_colors, cluster_colors) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # =========================================================================
    # BAR CHARTS
    # =========================================================================
    
    # Combined selected methods
    selected_methods <- reactive({
      c(input$bar_methods, input$bar_baselines)
    })
    
    # Current method index for navigation
    current_method_idx <- reactiveVal(1)
    
    # Reset index when methods change
    observeEvent(selected_methods(), {
      current_method_idx(1)
    })
    
    # Get unique levels for the selected grouping variable
    group_levels <- reactive({
      req(input$bar_group_var)
      get_unique_levels(master_data, input$bar_group_var)
    })
    
    # Reactive values for custom colors
    bar_custom_colors <- reactiveValues(colors = list())
    
    # Initialize colors when grouping variable changes
    observeEvent(input$bar_group_var, {
      levels <- group_levels()
      default_colors <- get_clinical_colors(input$bar_group_var, hex_colors)
      
      new_colors <- list()
      for (lvl in levels) {
        if (!is.null(default_colors) && lvl %in% names(default_colors)) {
          new_colors[[lvl]] <- default_colors[lvl]
        } else {
          new_colors[[lvl]] <- "#666666"
        }
      }
      bar_custom_colors$colors <- new_colors
    })
    
    # Use default colors button
    observeEvent(input$bar_use_default_colors, {
      levels <- group_levels()
      default_colors <- get_clinical_colors(input$bar_group_var, hex_colors)
      
      new_colors <- list()
      for (lvl in levels) {
        if (!is.null(default_colors) && lvl %in% names(default_colors)) {
          new_colors[[lvl]] <- default_colors[lvl]
        } else {
          new_colors[[lvl]] <- "#666666"
        }
      }
      bar_custom_colors$colors <- new_colors
      
      # Update color pickers
      for (lvl in levels) {
        picker_id <- paste0("bar_color_", gsub("[^a-zA-Z0-9]", "_", lvl))
        updateColourInput(session, picker_id, value = new_colors[[lvl]])
      }
    })
    
    # Render color pickers for bar chart
    output$bar_color_pickers <- renderUI({
      levels <- group_levels()
      req(length(levels) > 0)
      
      color_inputs <- lapply(levels, function(lvl) {
        picker_id <- paste0("bar_color_", gsub("[^a-zA-Z0-9]", "_", lvl))
        current_color <- bar_custom_colors$colors[[lvl]]
        if (is.null(current_color)) current_color <- "#666666"
        
        colourInput(
          inputId = ns(picker_id),
          label = lvl,
          value = current_color,
          showColour = "both"
        )
      })
      
      tagList(color_inputs)
    })
    
    # Observe color picker changes
    observe({
      levels <- group_levels()
      req(length(levels) > 0)
      
      for (lvl in levels) {
        picker_id <- paste0("bar_color_", gsub("[^a-zA-Z0-9]", "_", lvl))
        local({
          local_lvl <- lvl
          local_picker_id <- picker_id
          
          observeEvent(input[[local_picker_id]], {
            bar_custom_colors$colors[[local_lvl]] <- input[[local_picker_id]]
          }, ignoreInit = TRUE)
        })
      }
    })
    
    # Navigation controls for multiple methods
    output$bar_nav_controls <- renderUI({
      methods <- selected_methods()
      
      if (length(methods) <= 1) {
        return(NULL)
      }
      
      div(
        class = "card-nav",
        actionButton(ns("bar_prev"), icon("chevron-left"), class = "btn-sm"),
        span(style = "margin: 0 15px;", 
             paste(current_method_idx(), "of", length(methods))),
        actionButton(ns("bar_next"), icon("chevron-right"), class = "btn-sm")
      )
    })
    
    # Navigation button handlers
    observeEvent(input$bar_prev, {
      methods <- selected_methods()
      current <- current_method_idx()
      if (current > 1) {
        current_method_idx(current - 1)
      } else {
        current_method_idx(length(methods))  # Wrap around
      }
    })
    
    observeEvent(input$bar_next, {
      methods <- selected_methods()
      current <- current_method_idx()
      if (current < length(methods)) {
        current_method_idx(current + 1)
      } else {
        current_method_idx(1)  # Wrap around
      }
    })
    
    # Method indicator
    output$bar_method_indicator <- renderText({
      methods <- selected_methods()
      if (length(methods) == 0) return("")
      
      idx <- min(current_method_idx(), length(methods))
      current_method <- methods[idx]
      
      # Get display name
      display_name <- names(METHOD_MAPPING)[METHOD_MAPPING == current_method]
      if (length(display_name) == 0) display_name <- current_method
      
      return(display_name)
    })
    
    # Render bar chart
    output$bar_plot <- renderPlotly({
      methods <- selected_methods()
      validate(
        need(length(methods) > 0, "Please select at least one method.")
      )
      
      idx <- min(current_method_idx(), length(methods))
      current_method <- methods[idx]
      
      validate(
        need(current_method %in% names(master_data), 
             paste("Method column not found:", current_method))
      )
      
      # Prepare data
      bar_data <- prepare_bar_data(master_data, current_method, input$bar_group_var)
      
      # Get colors
      colors <- unlist(bar_custom_colors$colors)
      
      # Get display names
      method_display <- names(METHOD_MAPPING)[METHOD_MAPPING == current_method]
      if (length(method_display) == 0) method_display <- current_method
      
      group_display <- CLINICAL_DISPLAY_NAMES[input$bar_group_var]
      
      # Create chart
      create_bar_chart(
        data = bar_data,
        chart_type = input$bar_chart_type,
        group_colors = colors,
        title = paste(method_display, "Clustering by", group_display),
        x_label = "Cluster",
        y_label = "Count"
      )
    })
    
    # =========================================================================
    # SUNBURSTS
    # =========================================================================
    
    # Get cluster levels for selected method
    cluster_levels <- reactive({
      req(input$sunburst_method)
      get_unique_levels(master_data, input$sunburst_method)
    })
    
    # Reactive values for sunburst custom colors
    sunburst_custom_colors <- reactiveValues(
      cluster_colors = list(),
      clinical_colors = list()
    )
    
    # Initialize cluster colors when method changes
    observeEvent(input$sunburst_method, {
      levels <- cluster_levels()
      default_colors <- get_cluster_colors(levels, cluster_colors)
      
      new_colors <- list()
      for (lvl in levels) {
        new_colors[[lvl]] <- default_colors[lvl]
      }
      sunburst_custom_colors$cluster_colors <- new_colors
    })
    
    # Initialize clinical colors when levels change
    observeEvent(input$sunburst_levels, {
      clinical_levels <- input$sunburst_levels
      new_clinical <- list()
      
      for (clinical_var in clinical_levels) {
        var_levels <- get_unique_levels(master_data, clinical_var)
        default_colors <- get_clinical_colors(clinical_var, hex_colors)
        
        var_colors <- list()
        for (lvl in var_levels) {
          if (!is.null(default_colors) && lvl %in% names(default_colors)) {
            var_colors[[lvl]] <- default_colors[lvl]
          } else {
            var_colors[[lvl]] <- "#666666"
          }
        }
        new_clinical[[clinical_var]] <- var_colors
      }
      sunburst_custom_colors$clinical_colors <- new_clinical
    })
    
    # Use default colors button for sunburst
    observeEvent(input$sunburst_use_default_colors, {
      # Reset cluster colors
      levels <- cluster_levels()
      default_colors <- get_cluster_colors(levels, cluster_colors)
      
      new_cluster_colors <- list()
      for (lvl in levels) {
        new_cluster_colors[[lvl]] <- default_colors[lvl]
      }
      sunburst_custom_colors$cluster_colors <- new_cluster_colors
      
      # Update cluster color pickers
      for (lvl in levels) {
        picker_id <- paste0("sunburst_cluster_", gsub("[^a-zA-Z0-9]", "_", lvl))
        updateColourInput(session, picker_id, value = new_cluster_colors[[lvl]])
      }
      
      # Reset clinical colors
      clinical_levels <- input$sunburst_levels
      new_clinical <- list()
      
      for (clinical_var in clinical_levels) {
        var_levels <- get_unique_levels(master_data, clinical_var)
        default_colors <- get_clinical_colors(clinical_var, hex_colors)
        
        var_colors <- list()
        for (lvl in var_levels) {
          if (!is.null(default_colors) && lvl %in% names(default_colors)) {
            var_colors[[lvl]] <- default_colors[lvl]
          } else {
            var_colors[[lvl]] <- "#666666"
          }
          
          # Update color picker
          picker_id <- paste0("sunburst_clinical_", 
                             gsub("[^a-zA-Z0-9]", "_", clinical_var), "_",
                             gsub("[^a-zA-Z0-9]", "_", lvl))
          updateColourInput(session, picker_id, value = var_colors[[lvl]])
        }
        new_clinical[[clinical_var]] <- var_colors
      }
      sunburst_custom_colors$clinical_colors <- new_clinical
    })
    
    # Render cluster color pickers
    output$sunburst_cluster_colors <- renderUI({
      levels <- cluster_levels()
      req(length(levels) > 0)
      
      color_inputs <- lapply(levels, function(lvl) {
        picker_id <- paste0("sunburst_cluster_", gsub("[^a-zA-Z0-9]", "_", lvl))
        current_color <- sunburst_custom_colors$cluster_colors[[lvl]]
        if (is.null(current_color)) current_color <- "#666666"
        
        colourInput(
          inputId = ns(picker_id),
          label = paste("Cluster", lvl),
          value = current_color,
          showColour = "both"
        )
      })
      
      tagList(color_inputs)
    })
    
    # Render clinical variable color pickers (collapsible sections)
    output$sunburst_clinical_colors <- renderUI({
      clinical_levels <- input$sunburst_levels
      req(length(clinical_levels) > 0)
      
      sections <- lapply(clinical_levels, function(clinical_var) {
        var_levels <- get_unique_levels(master_data, clinical_var)
        display_name <- CLINICAL_DISPLAY_NAMES[clinical_var]
        
        color_inputs <- lapply(var_levels, function(lvl) {
          picker_id <- paste0("sunburst_clinical_", 
                             gsub("[^a-zA-Z0-9]", "_", clinical_var), "_",
                             gsub("[^a-zA-Z0-9]", "_", lvl))
          
          current_colors <- sunburst_custom_colors$clinical_colors[[clinical_var]]
          current_color <- if (!is.null(current_colors) && lvl %in% names(current_colors)) {
            current_colors[[lvl]]
          } else {
            "#666666"
          }
          
          colourInput(
            inputId = ns(picker_id),
            label = lvl,
            value = current_color,
            showColour = "both"
          )
        })
        
        div(
          class = "color-section",
          h6(display_name),
          tagList(color_inputs)
        )
      })
      
      tagList(sections)
    })
    
    # Observe cluster color picker changes
    observe({
      levels <- cluster_levels()
      req(length(levels) > 0)
      
      for (lvl in levels) {
        picker_id <- paste0("sunburst_cluster_", gsub("[^a-zA-Z0-9]", "_", lvl))
        local({
          local_lvl <- lvl
          local_picker_id <- picker_id
          
          observeEvent(input[[local_picker_id]], {
            sunburst_custom_colors$cluster_colors[[local_lvl]] <- input[[local_picker_id]]
          }, ignoreInit = TRUE)
        })
      }
    })
    
    # Observe clinical color picker changes
    observe({
      clinical_levels <- input$sunburst_levels
      req(length(clinical_levels) > 0)
      
      for (clinical_var in clinical_levels) {
        var_levels <- get_unique_levels(master_data, clinical_var)
        
        for (lvl in var_levels) {
          picker_id <- paste0("sunburst_clinical_", 
                             gsub("[^a-zA-Z0-9]", "_", clinical_var), "_",
                             gsub("[^a-zA-Z0-9]", "_", lvl))
          local({
            local_var <- clinical_var
            local_lvl <- lvl
            local_picker_id <- picker_id
            
            observeEvent(input[[local_picker_id]], {
              if (is.null(sunburst_custom_colors$clinical_colors[[local_var]])) {
                sunburst_custom_colors$clinical_colors[[local_var]] <- list()
              }
              sunburst_custom_colors$clinical_colors[[local_var]][[local_lvl]] <- input[[local_picker_id]]
            }, ignoreInit = TRUE)
          })
        }
      }
    })
    
    # Render sunburst chart
    output$sunburst_plot <- renderPlotly({
      req(input$sunburst_method)
      
      # Build hierarchy levels (method first, then clinical variables)
      hierarchy <- c(input$sunburst_method, input$sunburst_levels)
      
      validate(
        need(length(hierarchy) >= 2, "Please select at least one clinical variable.")
      )
      
      # Prepare sunburst data
      sunburst_data <- prepare_sunburst_data(master_data, hierarchy)
      
      # Build combined colors
      all_colors <- c()
      
      # Add cluster colors
      cluster_cols <- unlist(sunburst_custom_colors$cluster_colors)
      if (length(cluster_cols) > 0) {
        all_colors <- c(all_colors, cluster_cols)
      }
      
      # Add clinical colors
      for (clinical_var in input$sunburst_levels) {
        clinical_cols <- sunburst_custom_colors$clinical_colors[[clinical_var]]
        if (!is.null(clinical_cols)) {
          all_colors <- c(all_colors, unlist(clinical_cols))
        }
      }
      
      # Add total color
      all_colors <- c("Total" = "#FFFFFF", all_colors)
      
      # Get method display name
      method_display <- names(METHOD_MAPPING)[METHOD_MAPPING == input$sunburst_method]
      if (length(method_display) == 0) method_display <- input$sunburst_method
      
      # Create sunburst
      create_sunburst(
        sunburst_data = sunburst_data,
        colors = all_colors,
        title = paste(method_display, "Clustering Hierarchy")
      )
    })
  })
}
