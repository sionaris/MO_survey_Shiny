# MultiOmicsSurvey Dashboard

Interactive R Shiny dashboard for exploring multi-omics breast cancer clustering results from the MultiOmicsSurvey project.

## Overview

This dashboard provides two main functionalities:

1. **Plots Browser**: Browse pre-generated result plots from 18 clustering algorithms plus baselines and consensus methods
2. **Interactive Exploration**: Create custom bar charts and sunburst visualizations from the master dataset

## Installation

### Prerequisites

- R >= 4.0.0
- RStudio (recommended)

### Required Packages

Install required packages by running:

```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "plotly",
  "dplyr",
  "tidyr",
  "colourpicker",
  "shinyWidgets",
  "shinycssloaders",
  "base64enc"
))
```

### Setup

1. Clone or download this repository
2. Ensure `shiny_app_data.zip` has been extracted to create the `data/` folder
3. Open `app.R` in RStudio
4. Click "Run App" or execute:

```r
shiny::runApp()
```

## Features

### Plots Tab

- **Method Selection**: Choose from:
  - 18 Single Algorithms (ab-SNF, ANF, CIMLR, COCA, iClusterBayes, KLIC, LRAcluster, MDICC, MFA, MOFA, MONET, MSNE, NEMO, RWR-F, RWR-NF, SNF, Spectrum, wMKL)
  - Baselines (MOVICS baseline, ER baseline)
  - Consensus methods (Consensus CC, Consensus > 2 MC)
- **Plot Browser**: View all available plots for selected method
- **Download**: Export any plot as PNG

### Exploration Tab

#### Bar Charts
- Select one or more clustering methods
- Group by clinical variables (ER status, HER2 status, Stage, etc.)
- Choose between stacked or grouped bar charts
- Customize colors with color pickers
- Navigate between methods with prev/next buttons

#### Sunbursts
- Select a clustering method as root
- Build hierarchy with up to 3 additional clinical variables
- Interactive sunburst with zoom and hover details
- Full color customization for clusters and clinical variable levels

## Project Structure

```
MultiOmicsSurvey_Dashboard/
├── app.R                        # App launcher (entry point)
├── global.R                     # Global config, data loading
├── ui.R                         # Main UI definition
├── server.R                     # Main server logic
├── R/
│   ├── config/
│   │   └── constants.R          # App-wide constants
│   ├── modules/
│   │   ├── plots_module.R       # Plots tab module
│   │   └── exploration_module.R # Exploration tab module
│   └── utils/
│       ├── data_utils.R         # Data manipulation helpers
│       ├── plot_utils.R         # Plotting helpers
│       └── color_utils.R        # Color handling utilities
├── www/
│   └── css/
│       └── custom.css           # Custom styling
├── data/
│   ├── master_dataset.csv       # Main dataset
│   ├── hex_col.rds              # Clinical variable colors
│   ├── clust_col.rds            # Cluster colors
│   └── plots/                   # Plot images by method
├── README.md
└── .gitignore
```

## Data Description

### Master Dataset
Contains patient samples with:
- **Clinical variables**: Vital status, Ethnicity, Race, Lymph node status, Histology, Menopausal status, PR/ER/HER2 status, Metastasis, Stage
- **Clustering results**: Assignments from 18 algorithms plus baselines

### Color Palettes
- `hex_col.rds`: Default colors for clinical variable levels
- `clust_col.rds`: Default colors for cluster assignments

## Usage Guide

### Browsing Plots
1. Navigate to the "Plots" tab
2. Select a method from the dropdown
3. Choose a specific plot from the available options
4. Use the download button to save the plot

### Creating Bar Charts
1. Navigate to "Exploration" → "Bar Charts"
2. Select one or more clustering methods
3. Choose a grouping variable
4. Toggle between stacked/grouped display
5. Customize colors as needed
6. Use navigation buttons to switch between methods

### Creating Sunbursts
1. Navigate to "Exploration" → "Sunbursts"
2. Select a clustering method (root level)
3. Add clinical variables to build hierarchy
4. Customize colors for each level
5. Click segments to zoom in

## Technical Notes

- The app loads all data on startup for optimal performance
- Plot images are displayed using base64 encoding
- Color customizations are session-specific (not persisted)
- The dashboard uses responsive design for different screen sizes

## License

This project is part of the MultiOmicsSurvey research project.

## Contact

For questions or issues, please open a GitHub issue or contact the maintainers.
