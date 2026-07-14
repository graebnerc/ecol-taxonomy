# Package dependencies for this project.
#
# Reproducibility (Phase 0): exact versions are pinned in `renv.lock` (R 4.4.3,
# CRAN; 129 packages = the direct dependencies below plus their recursive tree).
# To restore that exact environment into a project-local library:
#
#   install.packages("renv")
#   renv::restore()       # installs the versions recorded in renv.lock
#
# The lockfile is a plain manifest: we deliberately did NOT run renv::init(), so
# there is no renv/activate.R or .Rprofile hook and the project still runs against
# your system library. To refresh the lockfile after changing dependencies, re-run
# the renv::snapshot(packages = required, lockfile = "renv.lock") call used to
# create it. Meanwhile, this script installs any missing packages so it runs.

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
