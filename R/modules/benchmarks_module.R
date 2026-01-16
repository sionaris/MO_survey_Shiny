# =============================================================================
# Benchmarks Module - Algorithm Performance Benchmarks
# =============================================================================

#' Benchmarks Module UI
#' 
#' @param id Module namespace ID
#' @return UI elements for the benchmarks tab
benchmarksUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tabsetPanel(
      id = ns("benchmark_tabs"),
      type = "tabs",
      
      # =====================================================================
      # Static Plots Tab
      # =====================================================================
      tabPanel(
        title = "Pre-generated Plots",
        icon = icon("image"),
        br(),
        fluidRow(
          column(
            width = 3,
            div(
              class = "plot-controls",
              selectInput(
                inputId = ns("static_plot_select"),
                label = "Select Benchmark Plot",
                choices = list(
                  "Feature Perturbations" = c(
                    "Feature perturbations: ARI with ground truth" = "benchmarks_agreement_with_ground_truth_clustering_under_feature_perturbations.png",
                    "Feature perturbations: memory" = "benchmarks_peak_memory_under_feature_perturbations.png",
                    "Feature perturbations: runtimes" = "benchmarks_runtime_under_feature_perturbations.png"
                  ),
                  "Sample Perturbations" = c(
                    "Sample perturbations: ARI with ground truth" = "benchmarks_agreement_with_ground_truth_clustering_under_sample_perturbations.png",
                    "Sample perturbations: memory" = "benchmarks_peak_memory_under_sample_perturbations.png",
                    "Sample perturbations: runtimes" = "benchmarks_runtime_under_sample_perturbations.png"
                  ),
                  "Stability" = c(
                    "90% subset resampling stability" = "stability_90pct_violin.png"
                  )
                ),
                selected = "benchmarks_agreement_with_ground_truth_clustering_under_feature_perturbations.png"
              ),
              
              # Plot description
              div(
                class = "plot-info-panel",
                h5("Plot Description"),
                uiOutput(ns("static_plot_description"))
              ),
              
              # Download button
              downloadButton(
                outputId = ns("download_static_plot"),
                label = "Download Plot",
                class = "btn-primary btn-block"
              )
            )
          ),
          
          column(
            width = 9,
            div(
              class = "plot-card",
              div(
                class = "img-container",
                withSpinner(
                  uiOutput(ns("static_plot_display")),
                  type = 4,
                  color = "#3c8dbc"
                )
              )
            )
          )
        )
      ),
      
      # =====================================================================
      # Interactive Plots Tab
      # =====================================================================
      tabPanel(
        title = "Interactive Plots",
        icon = icon("chart-line"),
        br(),
        fluidRow(
          # Left sidebar with controls
          column(
            width = 3,
            div(
              class = "plot-controls",
              
              # Perturbation type selection
              selectInput(
                inputId = ns("perturbation_type"),
                label = "Perturbation Type",
                choices = c(
                  "Feature Perturbations" = "feature",
                  "Sample Perturbations" = "sample"
                ),
                selected = "feature"
              ),
              
              # Metric selection
              selectInput(
                inputId = ns("metric_select"),
                label = "Metric",
                choices = c(
                  "ARI with Ground Truth" = "ARI_to_Ground_Truth",
                  "Peak Memory (MiB)" = "Peak_MiB",
                  "Runtime (seconds)" = "Time_s"
                ),
                selected = "ARI_to_Ground_Truth"
              ),
              
              hr(),
              
              # Method selection
              h5("Select Methods"),
              pickerInput(
                inputId = ns("method_select"),
                label = NULL,
                choices = NULL,  # Populated by server
                selected = NULL,
                multiple = TRUE,
                options = pickerOptions(
                  actionsBox = TRUE,
                  liveSearch = TRUE,
                  selectedTextFormat = "count > 3",
                  countSelectedText = "{0} methods selected"
                )
              ),
              
              hr(),
              
              # Color customization section
              h5("Color Settings"),
              checkboxInput(
                inputId = ns("use_custom_colors"),
                label = "Use custom colors",
                value = FALSE
              ),
              
              conditionalPanel(
                condition = sprintf("input['%s'] == true", ns("use_custom_colors")),
                uiOutput(ns("color_pickers"))
              )
            )
          ),
          
          # Main plot area
          column(
            width = 9,
            div(
              class = "plot-card",
              withSpinner(
                plotlyOutput(ns("interactive_plot"), height = "600px"),
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

#' Benchmarks Module Server
#' 
#' @param id Module namespace ID
#' @param benchmark_data List containing feature and sample perturbation data
#' @param category_colors Named vector of category colors
#' @param method_categories Named vector mapping methods to categories
benchmarksServer <- function(id, benchmark_data, category_colors, method_categories) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Path to benchmark plots
    benchmark_plots_dir <- "data/plots/Benchmarks"
    
    # =========================================================================
    # Static Plots Section
    # =========================================================================
    
    # Plot descriptions
    plot_descriptions <- list(
      "benchmarks_agreement_with_ground_truth_clustering_under_feature_perturbations.png" = 
        "Shows how well each algorithm maintains its clustering quality (measured by Adjusted Rand Index) as the number of features decreases from 100% to 10% of the original features.",
      "benchmarks_peak_memory_under_feature_perturbations.png" = 
        "Shows peak memory usage (in MiB) for each algorithm as the number of features varies. Lower memory usage indicates better scalability.",
      "benchmarks_runtime_under_feature_perturbations.png" = 
        "Shows runtime (in seconds) for each algorithm as the number of features varies. Lower runtime indicates better computational efficiency.",
      "benchmarks_agreement_with_ground_truth_clustering_under_sample_perturbations.png" = 
        "Shows how well each algorithm maintains its clustering quality (measured by Adjusted Rand Index) as the number of samples decreases from 100% to 10% of the original samples.",
      "benchmarks_peak_memory_under_sample_perturbations.png" = 
        "Shows peak memory usage (in MiB) for each algorithm as the number of samples varies. Lower memory usage indicates better scalability.",
      "benchmarks_runtime_under_sample_perturbations.png" = 
        "Shows runtime (in seconds) for each algorithm as the number of samples varies. Lower runtime indicates better computational efficiency.",
      "stability_90pct_violin.png" = 
        "Violin plot showing the distribution of pairwise Adjusted Rand Index values between replicate clusterings at 90% sample subsetting. Higher ARI indicates more stable/reproducible clustering across random subsamples. Algorithms are ordered by median stability within their category."
    )
    
    # Display static plot description
    output$static_plot_description <- renderUI({
      req(input$static_plot_select)
      desc <- plot_descriptions[[input$static_plot_select]]
      if (is.null(desc)) desc <- "No description available."
      p(desc, style = "font-size: 12px; color: #666;")
    })
    
    # Display static plot
    output$static_plot_display <- renderUI({
      req(input$static_plot_select)
      plot_path <- file.path(benchmark_plots_dir, input$static_plot_select)
      
      validate(
        need(file.exists(plot_path), "Plot file not found.")
      )
      
      img_data <- base64enc::base64encode(plot_path)
      img_src <- paste0("data:image/png;base64,", img_data)
      
      div(
        class = "plot-viewer",
        style = "max-height: 70vh; overflow-y: auto;",
        tags$img(
          src = img_src,
          style = "max-width: 100%; height: auto;",
          alt = "Benchmark plot"
        )
      )
    })
    
    # Download static plot
    output$download_static_plot <- downloadHandler(
      filename = function() {
        input$static_plot_select
      },
      content = function(file) {
        plot_path <- file.path(benchmark_plots_dir, input$static_plot_select)
        file.copy(plot_path, file)
      },
      contentType = "image/png"
    )
    
    # =========================================================================
    # Interactive Plots Section
    # =========================================================================
    
    # Current data based on perturbation type
    current_data <- reactive({
      req(input$perturbation_type)
      if (input$perturbation_type == "feature") {
        benchmark_data$feature
      } else {
        benchmark_data$sample
      }
    })
    
    # Available methods
    available_methods <- reactive({
      data <- current_data()
      req(data)
      sort(unique(data$Algorithm))
    })
    
    # Update method picker when perturbation type changes
    observe({
      methods <- available_methods()
      req(methods)
      
      # Group methods by category
      method_list <- list()
      for (cat in unique(method_categories[methods])) {
        cat_methods <- methods[method_categories[methods] == cat]
        if (length(cat_methods) > 0) {
          method_list[[cat]] <- cat_methods
        }
      }
      
      updatePickerInput(
        session,
        "method_select",
        choices = method_list,
        selected = methods[1:min(5, length(methods))]  # Select first 5 by default
      )
    })
    
    # Reactive: selected methods' categories
    selected_categories <- reactive({
      req(input$method_select)
      unique(method_categories[input$method_select])
    })
    
    # Generate color pickers for selected categories
    output$color_pickers <- renderUI({
      cats <- selected_categories()
      req(cats)
      
      color_inputs <- lapply(cats, function(cat) {
        default_color <- category_colors[cat]
        if (is.na(default_color)) default_color <- "#666666"
        
        colourInput(
          inputId = ns(paste0("color_", gsub("[^a-zA-Z0-9]", "_", cat))),
          label = cat,
          value = default_color,
          showColour = "both"
        )
      })
      
      tagList(color_inputs)
    })
    
    # Get colors for plotting
    get_plot_colors <- reactive({
      cats <- selected_categories()
      req(cats)
      
      if (input$use_custom_colors) {
        # Get custom colors from inputs
        colors <- sapply(cats, function(cat) {
          input_id <- paste0("color_", gsub("[^a-zA-Z0-9]", "_", cat))
          color <- input[[input_id]]
          if (is.null(color)) category_colors[cat] else color
        })
      } else {
        # Use default category colors
        colors <- category_colors[cats]
      }
      
      setNames(colors, cats)
    })
    
    # Y-axis labels
    y_labels <- c(
      "ARI_to_Ground_Truth" = "Adjusted Rand Index",
      "Peak_MiB" = "Peak Memory (MiB)",
      "Time_s" = "Runtime (seconds)"
    )
    
    # Render interactive plot
    output$interactive_plot <- renderPlotly({
      data <- current_data()
      req(data, input$method_select, input$metric_select)
      
      # Filter to selected methods
      plot_data <- data[data$Algorithm %in% input$method_select, ]
      
      validate(
        need(nrow(plot_data) > 0, "No data available for selected methods.")
      )
      
      # Get colors
      colors <- get_plot_colors()
      
      # Add category to data if not present
      if (!"Category" %in% names(plot_data)) {
        plot_data$Category <- method_categories[plot_data$Algorithm]
      }
      
      # Get y-axis label
      y_label <- y_labels[input$metric_select]
      if (is.na(y_label)) y_label <- input$metric_select
      
      # X-axis label
      x_label <- if (input$perturbation_type == "feature") {
        "Feature Subset (%)"
      } else {
        "Sample Subset (%)"
      }
      
      # Create plotly figure
      p <- plot_ly()
      
      for (method in input$method_select) {
        method_data <- plot_data[plot_data$Algorithm == method, ]
        cat <- method_categories[method]
        color <- colors[cat]
        
        p <- add_trace(
          p,
          data = method_data,
          x = ~Subset_Percent,
          y = ~get(input$metric_select),
          type = "scatter",
          mode = "lines+markers",
          name = method,
          line = list(color = color, width = 2),
          marker = list(color = color, size = 8),
          legendgroup = cat,
          hovertemplate = paste(
            "<b>%{text}</b><br>",
            x_label, ": %{x}%<br>",
            y_label, ": %{y:.3f}<br>",
            "<extra></extra>"
          ),
          text = method
        )
      }
      
      # Layout
      p <- layout(
        p,
        title = list(
          text = paste(
            if (input$perturbation_type == "feature") "Feature" else "Sample",
            "Perturbation:",
            y_label
          ),
          font = list(size = 16)
        ),
        xaxis = list(
          title = x_label,
          range = c(5, 105),
          tickvals = seq(10, 100, 10)
        ),
        yaxis = list(
          title = y_label
        ),
        legend = list(
          title = list(text = "Algorithm"),
          orientation = "v",
          x = 1.02,
          y = 1
        ),
        hovermode = "closest",
        margin = list(r = 150)
      )
      
      p
    })
    
  })
}
