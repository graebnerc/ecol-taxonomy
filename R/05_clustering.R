# 05 - Clustering robustness layer.
# Reads data/tidy/taxonomy_indicators.csv and runs the hierarchical (Ward)
# clustering typology. This is the consolidated successor to the legacy
# clustering script (now in _archive/R/clustering_legacy.R).
#
# The vulnerability x potential map (04_typology.R) is the headline; this
# clustering is kept only as an illustrative robustness layer. K is fixed at 4
# for continuity with the earlier growth-model comparison, NOT because the data
# support four groups: 07_robustness.R finds silhouette peaks weakly at k=3 and
# the gap statistic gives k=1 (little discrete structure), which is itself the
# argument for the continuous map over hard clustering.

here::i_am("R/05_clustering.R")
library(here)
library(data.table)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(countrycode)
library(cluster)
library(factoextra)
library(ggalluvial)
library(ggplot2)
source(here("R/config.R"))
source(here("R/country_classification.R"))
source(here("R/functions/clustering_helpers.R"))

indicators <- as_tibble(fread(here("data/tidy/taxonomy_indicators.csv")))

# Cluster on the same six block variables used for the 2-D typology (defined in
# config.R), so the data-driven clusters are directly comparable to the
# vulnerability x potential quadrants (robustness layer for 04_typology.R).
ANALYSIS_VARS <- c(VULN_VARS, POT_VARS)
K <- 4  # illustrative only; unsupported by silhouette/gap (see 07_robustness.R)

scaled <- scale_indicators(indicators, vars = ANALYSIS_VARS)
dist_mat <- dist(scaled, method = "euclidean")

# Choice of linkage (higher agnes coefficient = stronger structure -> ward).
linkage_tbl <- compare_linkage(dist_mat)
print(linkage_tbl)

hc_ward <- agnes(dist_mat, method = "ward")
message(sprintf("Cophenetic correlation: %.3f", cor(dist_mat, cophenetic(hc_ward))))

# Dendrogram ------------------------------------------------------------------
dend <- plot_dendrogram(hc_ward, k = K, title = "EU-27 ecological taxonomy")
ggsave(file.path(PLOT_DIR, "dendrogram.pdf"), dend, width = 8, height = 8)

# Clusters vs growth-model groups --------------------------------------------
clusters <- cutree(as.hclust(hc_ward), k = K)
alluvial <- plot_cluster_alluvial(
  clusters,
  classify_fun = \(iso3) get_country_classification(iso3, "jee")
)
ggsave(file.path(PLOT_DIR, "clusters_vs_development.pdf"), alluvial, width = 8, height = 6)

# Cluster membership table ----------------------------------------------------
membership <- tibble(
  country = names(clusters),
  ecological_model = as.integer(clusters)
) |>
  arrange(ecological_model, country)
print(membership, n = Inf)
fwrite(membership, here("data/tidy/cluster_membership.csv"))

message("05_clustering.R done: wrote dendrogram.pdf, clusters_vs_development.pdf, cluster_membership.csv")
