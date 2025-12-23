# =============================================================================
# Application Constants and Mappings
# =============================================================================

#' Method display names to CSV column names mapping
#' Keys are display names (used in UI), values are column names in master_dataset.csv
METHOD_MAPPING <- c(
  "ab-SNF" = "ab.SNF",
  "ANF" = "ANF",
  "CIMLR" = "CIMLR",
  "COCA" = "COCA",
  "iClusterBayes" = "iClusterBayes",
  "KLIC" = "KLIC",
  "LRAcluster" = "LRAcluster",
  "MDICC" = "MDICC",
  "MFA" = "MFA",
  "MOFA" = "MOFA",
  "MONET" = "MONET",
  "MSNE" = "MSNE",
  "NEMO" = "NEMO",
  "RWR-F" = "RWR.F",
  "RWR-NF" = "RWR.NF",
  "SNF" = "SNF",
  "Spectrum" = "Spectrum",
  "wMKL" = "wMKL",
  "ER" = "ER",
  "MOVICS" = "MOVICS",
  "CC" = "CC",
  "MC" = "MC"
)

#' Clinical variable column names to hex_col.rds keys mapping
#' Keys are column names in master_dataset.csv, values are keys in hex_col.rds
CLINICAL_MAPPING <- c(
  "ER.status" = "ER status",
  "HER2.status" = "HER2 status",
  "Stage" = "Stage",
  "PR.status" = "PR status",
  "Vital.status" = "Vital status",
  "Ethnicity" = "Ethnicity",
  "Race" = "Race",
  "Lymph.node.status" = "Lymph node status",
  "Histology" = "Histology",
  "Menopausal.status" = "Menopausal status",
  "Metastasis" = "Metastasis"
)

#' Display names for clinical variables (for UI labels)
CLINICAL_DISPLAY_NAMES <- c(
  "ER.status" = "ER Status",
  "HER2.status" = "HER2 Status",
  "Stage" = "Stage",
  "PR.status" = "PR Status",
  "Vital.status" = "Vital Status",

  "Ethnicity" = "Ethnicity",
  "Race" = "Race",
  "Lymph.node.status" = "Lymph Node Status",
  "Histology" = "Histology",
  "Menopausal.status" = "Menopausal Status",
  "Metastasis" = "Metastasis"
)

#' Single algorithm methods (for checkbox selections)
SINGLE_ALGORITHMS <- c(
  "ab-SNF", "ANF", "CIMLR", "COCA", "iClusterBayes",
  "KLIC", "LRAcluster", "MDICC", "MFA", "MOFA",
  "MONET", "MSNE", "NEMO", "RWR-F", "RWR-NF",
  "SNF", "Spectrum", "wMKL"
)

#' Baseline and consensus methods
BASELINE_METHODS <- c("MOVICS", "ER", "CC", "MC")

#' All available methods
ALL_METHODS <- c(SINGLE_ALGORITHMS, BASELINE_METHODS)

#' Plot categories for the Plots tab
#' Maps display names to folder paths
PLOT_CATEGORIES <- list(
  "Single Algorithms" = list(
    methods = SINGLE_ALGORITHMS,
    path_prefix = "single_algorithm"
  ),
  "Baselines" = list(
    methods = c("MOVICS baseline", "ER baseline"),
    folders = c("MOVICS_baseline", "ER_baseline")
  ),
  "Consensus" = list(
    methods = c("Consensus (CC)", "Consensus > 2 (MC)"),
    folders = c("Consensus", "Consensus_more_than_2")
  )
)

#' Mapping from plot dropdown selection to folder path
PLOT_FOLDER_MAPPING <- c(
  # Single algorithms use their display names as folder names
  "ab-SNF" = "single_algorithm/ab-SNF",
  "ANF" = "single_algorithm/ANF",
  "CIMLR" = "single_algorithm/CIMLR",
  "COCA" = "single_algorithm/COCA",
  "iClusterBayes" = "single_algorithm/iClusterBayes",
  "KLIC" = "single_algorithm/KLIC",
  "LRAcluster" = "single_algorithm/LRAcluster",
  "MDICC" = "single_algorithm/MDICC",
  "MFA" = "single_algorithm/MFA",
  "MOFA" = "single_algorithm/MOFA",
  "MONET" = "single_algorithm/MONET",
  "MSNE" = "single_algorithm/MSNE",
  "NEMO" = "single_algorithm/NEMO",
  "RWR-F" = "single_algorithm/RWR-F",
  "RWR-NF" = "single_algorithm/RWR-NF",
  "SNF" = "single_algorithm/SNF",
  "Spectrum" = "single_algorithm/Spectrum",
  "wMKL" = "single_algorithm/wMKL",
  # Baselines
  "MOVICS baseline" = "MOVICS_baseline",
  "ER baseline" = "ER_baseline",
  # Consensus
  "Consensus (CC)" = "Consensus",
  "Consensus > 2 (MC)" = "Consensus_more_than_2"
)

#' Clinical variables available for grouping/sunburst
CLINICAL_VARIABLES <- c(
  "ER.status", "HER2.status", "Stage", "PR.status",
  "Vital.status", "Ethnicity", "Race",
  "Lymph.node.status", "Histology",
  "Menopausal.status", "Metastasis"
)

#' Default sunburst hierarchy
DEFAULT_SUNBURST_LEVELS <- c("ER.status", "HER2.status", "Stage")

#' Maximum number of sunburst hierarchy levels
MAX_SUNBURST_LEVELS <- 4
