# =============================================================================
# Plot Name Mappings and Display Names
# =============================================================================

#' Patterns to IGNORE (these plots will not be shown to users)
#' Uses regex patterns
IGNORED_PLOT_PATTERNS <- c(
 "^sig_(?!Multiplot).*_barchart",  # sig_* bar charts except sig_Multiplot
 "^ntp_expr_down_heatmap_transNEO",
 "^ntp_expr_up_heatmap_transNEO",
 "^Summary of ordinal clinical variables",
 "^Summary_of_clinical_variables",
 "^transNEO Summary of ordinal clinical variables",
 "^transNEO_Summary_of_clinical_variables"
)

#' Check if a plot filename should be ignored
#' @param filename The plot filename (without path)
#' @return TRUE if should be ignored, FALSE otherwise
should_ignore_plot <- function(filename) {
 # Remove .png extension for matching
 name <- sub("\\.png$", "", filename, ignore.case = TRUE)
 
 for (pattern in IGNORED_PLOT_PATTERNS) {
   if (grepl(pattern, name, perl = TRUE)) {
     return(TRUE)
   }
 }
 return(FALSE)
}

#' Get display name for a plot file
#' @param filename The plot filename (without path)
#' @return Human-readable display name
get_plot_display_name <- function(filename) {
 # Remove .png extension and -1 suffix
 name <- sub("\\.png$", "", filename, ignore.case = TRUE)
 name <- sub("-1$", "", name)
 
 # === EXACT MATCHES (order matters - check these first) ===
 
 # Classification agreement
 if (grepl("^Classification_agreement", name)) {
   return("Subtype alluvial plot")
 }
 
 # Silhouette
 if (grepl("^Silhouette", name)) {
   return("Silhouette plot")
 }
 
 # Kappa NTP vs PAM transNEO
 if (grepl("^kappa_NTP_vs_PAM_transNEO", name)) {
   return("Kappa agreement NTP/PAM on transNEO")
 }
 
 # Consensus index
 if (grepl("^Consensus_index", name)) {
   return("M3C consensus index plot")
 }
 
 # Entropy
 if (grepl("^Entropy", name)) {
   return("M3C entropy plot")
 }
 
 # RCSI
 if (grepl("^RCSI", name)) {
   return("M3C Relative Cluster Stability Index plot")
 }
 
 # Stat_sig / Stat_Sig
 if (grepl("^Stat_[Ss]ig", name)) {
   return("M3C Statistical Significance plot")
 }
 
 # Variance explained plots
 if (grepl("^all_factors_cumul_var_expl_plot", name)) {
   return("Variance explained plot (cumulative)")
 }
 if (grepl("^all_factors_var_expl_plot", name)) {
   return("Variance explained plot (per factor)")
 }
 
 # Gene sets GSVA
 if (grepl("^gene_sets_of_interest_heatmap_gsva", name)) {
   return("Immune sets GSVA")
 }
 
 # === PATTERN MATCHES ===
 
 # COSMIC FGA barplot
 if (grepl("^COSMIC_criteria_FGA_barplot_", name)) {
   return("COSMIC Fraction of Genome Altered")
 }
 
 # FGA barplot (raw)
 if (grepl("^FGA_barplot_", name)) {
   return("Fraction of Genome Altered (raw)")
 }
 
 # Default Comprehensive heatmap
 if (grepl("^default_.*_Comprehensive_heatmap", name) || grepl("^default_Comprehensive_heatmap", name)) {
   return("Multi-omic heatmap")
 }
 
 # Downregulated biomarkers
 if (grepl("^downregulated_biomarkers_heatmap_using_downregulated_genes", name)) {
   return("Down-regulated genes heatmap")
 }
 if (grepl("^downregulated_miRNA_biomarkers_heatmap_using_downregulated_genes", name)) {
   return("Down-regulated miRNAs heatmap")
 }
 if (grepl("^downregulated_pathway_heatmap_using_downregulated_pathways", name)) {
   return("Down-regulated pathways heatmap")
 }
 
 # Upregulated biomarkers
 if (grepl("^upregulated_biomarkers_heatmap_using_upregulated_genes", name)) {
   return("Up-regulated genes heatmap")
 }
 if (grepl("^upregulated_miRNA_biomarkers_heatmap_using_upregulated_genes", name)) {
   return("Up-regulated miRNAs heatmap")
 }
 if (grepl("^upregulated_pathway_heatmap_using_upregulated_pathways", name)) {
   return("Up-regulated pathways heatmap")
 }
 
 # Representative pathways
 if (grepl("^representative_downregulated_pathway_heatmap", name)) {
   return("Representative down-regulated pathways")
 }
 if (grepl("^representative_upregulated_pathway_heatmap", name)) {
   return("Representative up-regulated pathways")
 }
 
 # Hypermethylated/Hypomethylated
 if (grepl("^hypermethylated_biomarkers_heatmap", name)) {
   return("Hypermethylated biomarkers heatmap")
 }
 if (grepl("^hypomethylated_biomarkers_heatmap", name)) {
   return("Hypomethylated biomarkers heatmap")
 }
 
 # Kappa agreements
 if (grepl("^kappa_.*_vs_NTP_TCGA", name)) {
   return("Kappa NTP agreement on TCGA")
 }
 if (grepl("^kappa_.*_vs_PAM_TCGA", name)) {
   return("Kappa PAM agreement on TCGA")
 }
 
 # Oncoprint
 if (grepl("_TCGA_RNAseq-CNV-Methylation-miRNA-SNPs_eval_on_transNEO_oncoprint", name)) {
   return("Oncoprint")
 }
 
 # Multiplot barcharts
 if (grepl("^sig_Multiplot_.*_barcharts", name)) {
   return("Bar charts collage: significant")
 }
 if (grepl("^Multiplot_.*_barcharts", name)) {
   return("Bar charts collage: all")
 }
 
 # Embeddings distance matrix
 if (grepl("_embeddings_distance_matrix_heatmap", name)) {
   return("Embeddings' distance heatmap")
 }
 
 # Factors and clinical variables
 if (grepl("_factors_and_clinical_variables", name)) {
   return("Factors: violin plots with clinical variables")
 }
 if (grepl("_factors_and_ER_GGally", name)) {
   return("Factors: distribution of ER status")
 }
 if (grepl("_factors_and_HER2_GGally", name)) {
   return("Factors: distribution of HER2 status")
 }
 if (grepl("_factors_and_Stage_GGally", name)) {
   return("Factors: distribution of Stage")
 }
 
 # Top weights
 if (grepl("_top_weights", name)) {
   return("Feature weights per factor/modality")
 }
 
 # === KERNEL PCA PLOTS ===
 if (grepl("_kernelPCA", name)) {
   # Extract what it's for
   if (grepl("_CNV_kernelPCA", name)) return("Kernel PCA: Copy Number Variants")
   if (grepl("_Methylation_kernelPCA", name)) return("Kernel PCA: Methylation")
   if (grepl("_RNAseq_kernelPCA", name)) return("Kernel PCA: RNA-seq")
   if (grepl("_miRNA_kernelPCA", name)) return("Kernel PCA: miRNA")
   if (grepl("_SNPs_kernelPCA", name)) return("Kernel PCA: SNPs")
   if (grepl("_Fusion_kernelPCA", name)) return("Kernel PCA: Fused matrix")
   if (grepl("Final Affinity Matrix_kernelPCA", name)) return("Kernel PCA: Final affinity matrix")
   if (grepl("Final similarity matrix S_kernelPCA", name)) return("Kernel PCA: Final similarity matrix")
   if (grepl("Average Adjacency_kernelPCA", name)) return("Kernel PCA: Average adjacency")
   # Generic fallback
   return(paste("Kernel PCA:", sub(".*_(.*)_kernelPCA.*", "\\1", name)))
 }
 
 # === PCA PLOTS ===
 if (grepl("_CNV_PCA", name)) return("PCA: Copy Number Variants")
 if (grepl("_Methylation_PCA", name)) return("PCA: Methylation")
 if (grepl("_RNAseq_PCA", name)) return("PCA: RNA-seq")
 if (grepl("_miRNA_PCA", name)) return("PCA: miRNA")
 
 # === MDS PLOTS ===
 if (grepl("_SNPs_MDS", name)) return("MDS: Single Nucleotide Polymorphisms")
 
 # === BAR CHARTS ===
 if (grepl("_ER status_barchart", name)) return("Bar chart: ER status")
 if (grepl("_Ethnicity_barchart", name)) return("Bar chart: Ethnicity")
 if (grepl("_HER2 status_barchart", name)) return("Bar chart: HER2 status")
 if (grepl("_Histology_barchart", name)) return("Bar chart: Histology")
 if (grepl("_Lymph node status_barchart", name)) return("Bar chart: Lymph node status")
 if (grepl("_Menopausal status_barchart", name)) return("Bar chart: Menopausal status")
 if (grepl("_Metastasis_barchart", name)) return("Bar chart: Metastasis")
 if (grepl("_PR status_barchart", name)) return("Bar chart: PR status")
 if (grepl("_Race_barchart", name)) return("Bar chart: Race")
 if (grepl("_Stage_barchart", name)) return("Bar chart: Stage")
 if (grepl("_Vital status_barchart", name)) return("Bar chart: Vital status")
 
 # === DEFAULT: Return cleaned up filename ===
 # Remove method prefix and clean up
 return(name)
}

#' Filter and rename plot files for display
#' @param filenames Vector of plot filenames
#' @return Named vector: display names as names, original filenames as values
filter_and_rename_plots <- function(filenames) {
 # Filter out ignored plots
 keep <- !sapply(filenames, should_ignore_plot)
 filtered <- filenames[keep]
 
 # Get display names
 display_names <- sapply(filtered, get_plot_display_name)
 
 # Create named vector (display name -> filename)
 result <- setNames(filtered, display_names)
 
 # Sort by display name
 result <- result[order(names(result))]
 
 return(result)
}
