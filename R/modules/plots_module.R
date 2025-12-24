# =============================================================================
# Plots Module - Browse Result Plots
# =============================================================================

#' Plots Module UI
#' 
#' @param id Module namespace ID
#' @return UI elements for the plots tab
plotsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      # Left sidebar with controls
      column(
        width = 3,
        div(
          class = "plot-controls",
          # Method selection dropdown
          selectInput(
            inputId = ns("method_select"),
            label = "Select Method",
            choices = NULL,  # Will be populated by server
            selected = NULL
          ),
          
          # Plot selection dropdown
          selectInput(
            inputId = ns("plot_select"),
            label = "Select Plot",
            choices = NULL,  # Will be populated based on method
            selected = NULL
          ),
          
          # Plot information panel
          div(
            class = "plot-info-panel",
            h5("Plot Information"),
            verbatimTextOutput(ns("plot_info"))
          ),
          
          # Download button
          downloadButton(
            outputId = ns("download_plot"),
            label = "Download Plot",
            class = "btn-primary btn-block"
          )
        )
      ),
      
      # Main plot display area
      column(
        width = 9,
        div(
          class = "plot-card",
          div(
            class = "img-container",
            withSpinner(
              uiOutput(ns("plot_display")),
              type = 4,
              color = "#3c8dbc"
            )
          )
        )
      )
    )
  )
}

#' Plots Module Server
#' 
#' @param id Module namespace ID
#' @param plot_index Reactive or list containing available plots index
#' @param plots_dir Path to the plots directory
plotsServer <- function(id, plot_index, plots_dir) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initialize method dropdown with available methods
    observe({
      methods <- get_available_methods(plot_index)
      
      # Organize methods by category
      method_choices <- list()
      
      # Single algorithms
      single_algs <- intersect(SINGLE_ALGORITHMS, methods)
      if (length(single_algs) > 0) {
        method_choices[["Single Algorithms"]] <- single_algs
      }
      
      # Baselines
      baselines <- intersect(c("MOVICS baseline", "ER baseline"), methods)
      if (length(baselines) > 0) {
        method_choices[["Baselines"]] <- baselines
      }
      
      # Consensus
      consensus <- intersect(c("Consensus (CC)", "Consensus > 2 (MC)"), methods)
      if (length(consensus) > 0) {
        method_choices[["Consensus"]] <- consensus
      }
      
      updateSelectInput(
        session,
        "method_select",
        choices = method_choices,
        selected = if (length(methods) > 0) methods[1] else NULL
      )
    })
    
    # Update plot dropdown when method changes
    observeEvent(input$method_select, {
      req(input$method_select)
      
      # Get raw plot filenames
      raw_plots <- get_method_plots(plot_index, input$method_select)
      
      # Filter and rename for display
      plot_choices <- filter_and_rename_plots(raw_plots)
      
      updateSelectInput(
        session,
        "plot_select",
        choices = plot_choices,
        selected = if (length(plot_choices) > 0) plot_choices[1] else NULL
      )
    })
    
    # Current plot path reactive
    current_plot_path <- reactive({
      req(input$method_select, input$plot_select)
      get_plot_path(plots_dir, plot_index, input$method_select, input$plot_select)
    })
    
    # Display the selected plot
    output$plot_display <- renderUI({
      plot_path <- current_plot_path()
      
      validate(
        need(!is.null(plot_path), "Please select a plot to display."),
        need(file.exists(plot_path), "Plot file not found.")
      )
      
      # Read and encode image as base64 for display
      img_data <- base64enc::base64encode(plot_path)
      img_src <- paste0("data:image/png;base64,", img_data)
      
      # Container with scrollable area and max height for laptop screens
      div(
        class = "plot-viewer",
        tags$img(
          src = img_src,
          class = "plot-image",
          alt = input$plot_select
        )
      )
    })
    
    # Plot information display
    output$plot_info <- renderText({
      req(input$method_select, input$plot_select)
      
      # Get display name for the plot
      display_name <- get_plot_display_name(input$plot_select)
      
      info <- paste0(
        "Category: ", plot_index[[input$method_select]]$category, "\n",
        "Method: ", input$method_select, "\n",
        "Plot: ", display_name, "\n",
        "File: ", input$plot_select
      )
      
      return(info)
    })
    
    # Download handler
    output$download_plot <- downloadHandler(
      filename = function() {
        req(input$plot_select)
        input$plot_select
      },
      content = function(file) {
        plot_path <- current_plot_path()
        req(plot_path)
        file.copy(plot_path, file)
      },
      contentType = "image/png"
    )
  })
}
