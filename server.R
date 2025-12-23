# =============================================================================
# MultiOmicsSurvey Dashboard - Main Server Logic
# =============================================================================

server <- function(input, output, session) {
  
  # =========================================================================
  # Initialize Modules
  # =========================================================================
  
  # Plots module - for browsing result plots
  plotsServer(
    id = "plots",
    plot_index = plot_index,
    plots_dir = plots_dir
  )
  
  # Exploration module - for interactive data exploration
  explorationServer(
    id = "exploration",
    master_data = master_data,
    hex_colors = hex_colors,
    cluster_colors = cluster_colors
  )
  
  # =========================================================================
  # Session cleanup
  # =========================================================================
  session$onSessionEnded(function() {
    # Cleanup code if needed
  })
}
