# MultiOmicsSurvey Dashboard

Interactive R Shiny dashboard for exploring multi-omics breast cancer clustering results from the MultiOmicsSurvey project.

## Features

| Tab | Description |
|-----|-------------|
| **Plots** | Browse pre-generated result plots from 18 clustering algorithms, baselines, and consensus methods |
| **Exploration** | Create custom bar charts and sunburst visualizations |
| **Benchmarks** | Explore algorithm performance benchmarks and stability analysis |

## Quick Start

```r
# Install dependencies
install.packages(c("shiny", "shinydashboard", "plotly", "dplyr", "tidyr",
                   "colourpicker", "shinyWidgets", "shinycssloaders", "base64enc"))

# Run the app
shiny::runApp()
```

## Tabs Overview

### 📊 Plots
- Browse plots from 18 algorithms (ab-SNF, ANF, CIMLR, COCA, etc.)
- View baselines and consensus results
- Download any plot as PNG

### 🔍 Exploration
- **Bar Charts**: Compare clustering distributions across clinical variables
- **Sunbursts**: Hierarchical visualization with customizable colors

### ⚡ Benchmarks
**Pre-generated Plots:**
- Feature/sample perturbation analysis (ARI, memory, runtime)
- 90% subset resampling stability (violin plot)

**Interactive Plots:**
- Select perturbation type and metric
- Choose specific algorithms to compare
- Customize colors by category

## Data

| File | Contents |
|------|----------|
| `master_dataset.csv` | 625 patient samples with clinical variables and clustering assignments |
| `hex_col.rds` / `clust_col.rds` | Color palettes |
| `data/plots/` | Pre-generated PNG plots |
| `data/benchmark_data/` | Benchmark performance data |

## Project Structure

```
├── app.R / global.R / ui.R / server.R   # Core app files
├── R/
│   ├── config/                          # Constants and plot name mappings
│   ├── modules/                         # UI modules (plots, exploration, benchmarks)
│   └── utils/                           # Helper functions
├── www/css/                             # Custom styling
└── data/                                # Datasets and plots
```

## License

Part of the [MultiOmicsSurvey](https://github.com/sionaris/MultiOmicsSurvey) research project.
