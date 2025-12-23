# =============================================================================
# MultiOmicsSurvey Dashboard - Main UI Definition
# =============================================================================

ui <- dashboardPage(
  skin = "blue",
  
  # =========================================================================
  # Header
  # =========================================================================
  dashboardHeader(
    title = span(
      icon("dna"),
      "MultiOmicsSurvey"
    ),
    titleWidth = 250
  ),
  
  # =========================================================================
  # Sidebar
  # =========================================================================
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "sidebar_menu",
      
      menuItem(
        text = "Plots",
        tabName = "plots_tab",
        icon = icon("images"),
        selected = TRUE
      ),
      
      menuItem(
        text = "Exploration",
        tabName = "exploration_tab",
        icon = icon("chart-pie")
      ),
      
      # Information section at bottom
      hr(),
      div(
        style = "padding: 10px; font-size: 12px; color: #888;",
        p(icon("info-circle"), " About"),
        p("Interactive dashboard for exploring multi-omics breast cancer clustering results."),
        p(paste("Samples:", nrow(master_data))),
        p(paste("Methods:", length(SINGLE_ALGORITHMS)))
      )
    )
  ),
  
  # =========================================================================
  # Body
  # =========================================================================
  dashboardBody(
    # Include custom CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css")
    ),
    
    tabItems(
      # =====================================================================
      # Plots Tab
      # =====================================================================
      tabItem(
        tabName = "plots_tab",
        h2("Result Plots Browser"),
        p("Browse pre-generated plots from the MultiOmicsSurvey analysis."),
        hr(),
        plotsUI("plots")
      ),
      
      # =====================================================================
      # Exploration Tab
      # =====================================================================
      tabItem(
        tabName = "exploration_tab",
        h2("Interactive Data Exploration"),
        p("Create custom visualizations from the master dataset."),
        hr(),
        explorationUI("exploration")
      )
    )
  )
)
