# Package dependencies for this project.
#
# Reproducibility (Phase 0): we recommend pinning versions with renv. renv is
# not initialised automatically because renv::init() downloads/installs the full
# dependency tree and is best run interactively. To pin the environment:
#
#   install.packages("renv")
#   renv::init()          # snapshots the packages below into renv.lock
#
# Until then, this script installs any missing packages so the pipeline runs.

required <- c(
  # pipeline
  "here", "data.table", "dplyr", "tidyr", "purrr", "tibble", "countrycode",
  "cluster", "factoextra", "ggalluvial", "ggplot2", "knitr", "scales", "ggpubr",
  # green list + complexity (Phase 1)
  "pdftools", "readxl", "stringr", "Matrix",
  # data acquisition (get_data.R)
  "WDI", "eurostat"
)

missing <- setdiff(required, rownames(installed.packages()))
if (length(missing) > 0) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing)
} else {
  message("All required packages are installed.")
}
