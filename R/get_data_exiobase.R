# Build the EXIOBASE footprint layer directly from the official IOT archives.
#
# REPLACES the external Python script that produced data/tidy/TXNY_GWP_Trade.csv
# (flagged in info/AuditReport_2026-07-14.md as an undocumented off-repo
# dependency). Everything the taxonomy needs is now computed in-repo from a
# named, checksummed source.
#
# SOURCE
#   EXIOBASE 3.10.2, product-by-product IOT archives, Zenodo record 20051562
#   (https://zenodo.org/records/20051562, published 2026-05-13, covers 1995-2024).
#   Downloaded and md5-verified by data/raw/exiobase/fetch.sh (gitignored, 2.9 GB).
#
# WHAT IT COMPUTES, per year
#   totals    : per region -- GWP_pba, GWP_Imports, GWP_Exports,
#               ValueAdded_pba, Employment_pba   (the columns of the old file)
#   bilateral : the full 49 x 49 origin x destination matrix of GHG embodied in
#               final demand -- the thing the old extract could not provide, and
#               without which "the core offshores to the European East" is not
#               testable (see info/PaperTodos.md, section Offshoring).
#
# METHOD (standard environmentally-extended Leontief attribution)
#   A       = Z / x                        technical coefficients
#   X       = (I - A)^-1 %*% Y_agg         output driven by each region's final demand
#   s       = f / x                        direct GHG intensity per unit output
#   E[i,j]  = sum over sectors of region i of  s * X[,j]
#   plus household direct emissions F_Y, which occur in region i and serve
#   region i's own final demand, so they are added to the diagonal E[i,i].
#
#   Identities (asserted below, not assumed):
#     GWP_pba[i]     = sum_j E[i,j]          row sum   = all emissions occurring in i
#     GWP_cba[i]     = sum_j E[j,i]          column sum = all emissions serving i
#     GWP_Exports[i] = sum_{j!=i} E[i,j]
#     GWP_Imports[i] = sum_{j!=i} E[j,i]
#
# GWP CHARACTERISATION
#   EXIOBASE 3.10.2 ships raw air-emission stressors in kg of gas (HFC and PFC
#   already in kg CO2-eq) and, unlike 3.8.x, no pre-characterised `impacts`
#   folder -- so the GWP100 basket must be declared explicitly. GWP_FACTORS below
#   is AR4 / IPCC 2007 GWP100, the basket EXIOBASE 3.8 used for its own "GWP100"
#   impact, so the series stays comparable in construction with the old extract.
#   Biogenic CO2 is EXCLUDED (climate-neutral by convention); biogenic CH4 and
#   N2O are INCLUDED. Set VARIANTS = TRUE to print alternative baskets and their
#   world totals.
#
# Writes data/tidy/exiobase_totals.csv and data/tidy/exiobase_bilateral.csv.
# Restartable: years already present in the output are skipped unless FORCE.

here::i_am("R/get_data_exiobase.R")
library(here)
suppressMessages({
  library(data.table); library(dplyr)
})
setDTthreads(0)

ZIP_DIR   <- here("data/raw/exiobase")
OUT_TOT   <- here("data/tidy/exiobase_totals.csv")
OUT_BIL   <- here("data/tidy/exiobase_bilateral.csv")
YEARS     <- 2013:2024
FORCE     <- FALSE
VARIANTS  <- FALSE   # TRUE: also report alternative GWP baskets (diagnostic)

# AR4 / IPCC 2007 GWP100. Matched by prefix on the EXIOBASE stressor name.
GWP_FACTORS <- list(
  # fossil / process CO2 -- biogenic streams deliberately excluded
  "CO2 - combustion - air"                              = 1,
  "CO2 - non combustion - Cement production - air"      = 1,
  "CO2 - non combustion - Lime production - air"        = 1,
  "CO2 - agriculture - peat decay - air"                = 1,
  "CO2 - waste - fossil - air"                          = 1,
  # methane, all sources, biogenic included
  "CH4 - combustion - air"                              = 25,
  "CH4_bio - combustion - air"                          = 25,
  "CH4 - agriculture - air"                             = 25,
  "CH4 - waste - air"                                   = 25,
  "CH4 - non combustion - Extraction/production of (natural) gas - air" = 25,
  "CH4 - non combustion - Extraction/production of crude oil - air"     = 25,
  "CH4 - non combustion - Mining of antracite - air"                    = 25,
  "CH4 - non combustion - Mining of bituminous coal - air"              = 25,
  "CH4 - non combustion - Mining of coking coal - air"                  = 25,
  "CH4 - non combustion - Mining of lignite (brown coal) - air"         = 25,
  "CH4 - non combustion - Mining of sub-bituminous coal - air"          = 25,
  "CH4 - non combustion - Oil refinery - air"                           = 25,
  # nitrous oxide, biogenic included
  "N2O - combustion - air"                              = 298,
  "N2O_bio - combustion - air"                          = 298,
  "N2O - agriculture - air"                             = 298,
  # fluorinated: SF6 in kg of gas, HFC/PFC already CO2-eq
  "SF6 - air"                                           = 22800,
  "HFC - air"                                           = 1,
  "PFC - air"                                           = 1
)

VA_ROWS <- c(   # gross value added at basic prices; product taxes excluded
  "Other net taxes on production",
  "Compensation of employees; wages, salaries, & employers' social contributions: Low-skilled",
  "Compensation of employees; wages, salaries, & employers' social contributions: Medium-skilled",
  "Compensation of employees; wages, salaries, & employers' social contributions: High-skilled",
  "Operating surplus: Consumption of fixed capital",
  "Operating surplus: Rents on land",
  "Operating surplus: Royalties on resources",
  "Operating surplus: Remaining net operating surplus")

# --- helpers -----------------------------------------------------------------

# pymrio writes 3 header lines (region, sector, index-name row). The core
# tables (Z, Y) carry 2 index columns; the extension tables (F, F_Y) carry 1.
# Passing the wrong count silently misaligns the matrix, so it is explicit.
read_labelled <- function(path, n_index_col = 2L) {
  hdr <- fread(path, nrows = 2L, header = FALSE, sep = "\t",
               colClasses = "character", showProgress = FALSE)
  lab1 <- as.character(hdr[1, -seq_len(n_index_col), with = FALSE])
  lab2 <- as.character(hdr[2, -seq_len(n_index_col), with = FALSE])
  dat <- fread(path, skip = 3L, header = FALSE, sep = "\t",
               drop = seq_len(n_index_col), showProgress = FALSE)
  rows <- fread(path, skip = 3L, header = FALSE, sep = "\t",
                select = seq_len(n_index_col), colClasses = "character",
                showProgress = FALSE)
  list(m = as.matrix(dat), col1 = lab1, col2 = lab2, rows = rows)
}

# EXIOBASE 3.10.2 carries a small number of NA cells in the emission matrices
# (in 2015: exactly one column, NA across nearly every stressor). Those cells
# must be zeroed EXPLICITLY -- left as NA they silently poison a whole region's
# total through the Leontief step. `zero_na` reports what it zeroed so the
# behaviour is visible, and extract_year asserts that no zeroed cell carries
# non-zero output.
zero_na <- function(M, label) {
  na <- is.na(M)
  if (any(na)) {
    cols <- which(colSums(na) > 0)
    cat(sprintf("    %s: zeroed %d NA cell(s) in %d column(s)\n",
                label, sum(na), length(cols)))
    M[na] <- 0
    attr(M, "na_cols") <- cols
  }
  M
}

characterise <- function(F_mat, stressors, factors) {
  idx <- match(names(factors), stressors)
  if (anyNA(idx))
    stop("GWP stressor(s) not found in this EXIOBASE release: ",
         paste(names(factors)[is.na(idx)], collapse = " | "))
  as.numeric(as.numeric(unlist(factors)) %*% F_mat[idx, , drop = FALSE])
}

extract_year <- function(year) {
  zip <- file.path(ZIP_DIR, sprintf("IOT_%d_pxp.zip", year))
  stopifnot("archive missing" = file.exists(zip))
  tmp <- file.path(tempdir(), sprintf("exio_%d", year))
  dir.create(tmp, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  members <- c("Z.txt", "Y.txt", "x.txt",
               "air_emissions/F.txt", "air_emissions/F_Y.txt",
               "air_emissions/unit.txt",
               "factor_inputs/F.txt", "factor_inputs/unit.txt",
               "employment/F.txt", "employment/unit.txt")
  utils::unzip(zip, files = members, exdir = tmp)

  t0 <- Sys.time()
  x <- fread(file.path(tmp, "x.txt"), skip = 1L, header = FALSE, sep = "\t",
             drop = 1:2, showProgress = FALSE)[[1]]
  n <- length(x)

  Zl <- read_labelled(file.path(tmp, "Z.txt"))
  Z <- Zl$m
  stopifnot("Z is not square" = nrow(Z) == n && ncol(Z) == n)
  sec_region <- Zl$col1                       # region label of each of the n sectors
  regions <- unique(sec_region)
  R <- length(regions)

  # A = Z / x (column-wise), then (I - A) in place to keep memory down.
  xs <- ifelse(x > 0, x, 1)
  A <- sweep(Z, 2L, xs, "/"); rm(Z); gc(FALSE)
  A <- -A; diag(A) <- diag(A) + 1            # A now holds (I - A)

  # Final demand aggregated to one column per destination region.
  Yl <- read_labelled(file.path(tmp, "Y.txt"))
  Y_dest <- factor(Yl$col1, levels = regions)
  Ya <- matrix(0, n, R, dimnames = list(NULL, regions))
  for (r in seq_len(R)) {
    cols <- which(Y_dest == regions[r])
    Ya[, r] <- rowSums(Yl$m[, cols, drop = FALSE])
  }
  rm(Yl); gc(FALSE)

  # Output in every sector driven by each destination region's final demand.
  X <- solve(A, Ya); rm(A); gc(FALSE)
  t_solve <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")))

  # Direct GHG intensity per unit output.
  Fl <- read_labelled(file.path(tmp, "air_emissions/F.txt"), 1L)
  stressors <- Fl$rows[[1]]
  Fl$m <- zero_na(Fl$m, "air_emissions/F")
  na_cols <- attr(Fl$m, "na_cols")
  if (!is.null(na_cols))
    stopifnot("an NA emission cell sits on a sector with non-zero output" =
                all(x[na_cols] == 0))
  f <- characterise(Fl$m, stressors, GWP_FACTORS); rm(Fl); gc(FALSE)
  s <- f / xs
  s[x <= 0] <- 0

  # E[i, j] = emissions in region i serving final demand of region j.
  SE <- s * X                                   # n x R, emissions by origin sector
  origin <- factor(sec_region, levels = regions)
  E <- rowsum(SE, group = origin, reorder = FALSE)
  E <- E[regions, regions, drop = FALSE]
  rm(SE, X); gc(FALSE)

  # Household direct emissions: occur in region i, serve region i -> diagonal.
  FYl <- read_labelled(file.path(tmp, "air_emissions/F_Y.txt"), 1L)
  FYl$m <- zero_na(FYl$m, "air_emissions/F_Y")
  fy <- characterise(FYl$m, FYl$rows[[1]], GWP_FACTORS)
  hh <- tapply(fy, factor(FYl$col1, levels = regions), sum)
  hh[is.na(hh)] <- 0
  diag(E) <- diag(E) + as.numeric(hh)

  # Value added and employment, by region of production.
  VAl <- read_labelled(file.path(tmp, "factor_inputs/F.txt"), 1L)
  vi <- match(VA_ROWS, VAl$rows[[1]])
  stopifnot("value-added row(s) not found" = !anyNA(vi))
  va <- rowsum(as.matrix(colSums(VAl$m[vi, , drop = FALSE])),
               group = origin, reorder = FALSE)

  EMl <- read_labelled(file.path(tmp, "employment/F.txt"), 1L)
  ei <- grep("^Employment people:", EMl$rows[[1]])
  stopifnot("employment rows not found" = length(ei) > 0)
  emp <- rowsum(as.matrix(colSums(EMl$m[ei, , drop = FALSE])),
                group = origin, reorder = FALSE)

  pba <- rowSums(E); cba <- colSums(E)
  totals <- data.table(
    year = year, region = regions,
    GWP_pba = pba,
    GWP_Imports = cba - diag(E),
    GWP_Exports = pba - diag(E),
    ValueAdded_pba = as.numeric(va[regions, ]),
    Employment_pba = as.numeric(emp[regions, ]))

  # Identity checks -- these must hold by construction; fail loudly if not.
  stopifnot(
    "CBA != PBA - exports + imports" =
      max(abs((pba - totals$GWP_Exports + totals$GWP_Imports) - cba)) < 1e-3 * max(pba),
    "world imports != world exports" =
      abs(sum(totals$GWP_Imports) - sum(totals$GWP_Exports)) < 1e-6 * sum(pba),
    "negative emissions" = all(E >= -1e-6 * max(abs(E))))

  bil <- as.data.table(as.table(E))
  setnames(bil, c("origin", "destination", "GWP"))
  bil[, year := year]

  cat(sprintf("  %d: world PBA %.2f Gt | trade %.2f Gt | VA %.1f tn EUR | solve %ds\n",
              year, sum(pba) / 1e12, sum(totals$GWP_Exports) / 1e12,
              sum(totals$ValueAdded_pba) / 1e6, t_solve))
  list(totals = totals, bilateral = bil[, .(year, origin, destination, GWP)])
}

# --- run ---------------------------------------------------------------------

done <- integer(0)
if (!FORCE && file.exists(OUT_TOT)) done <- unique(fread(OUT_TOT)$year)
todo <- setdiff(YEARS, done)
todo <- todo[file.exists(file.path(ZIP_DIR, sprintf("IOT_%d_pxp.zip", todo)))]

if (!length(todo)) {
  message("Nothing to do: all requested years already extracted.")
} else {
  cat(sprintf("Extracting %d year(s): %s\n", length(todo),
              paste(todo, collapse = ", ")))
  res <- lapply(todo, extract_year)
  tot <- rbindlist(lapply(res, `[[`, "totals"))
  bil <- rbindlist(lapply(res, `[[`, "bilateral"))
  if (!FORCE && file.exists(OUT_TOT)) {
    tot <- rbind(fread(OUT_TOT), tot); bil <- rbind(fread(OUT_BIL), bil)
  }
  setorder(tot, year, region); setorder(bil, year, origin, destination)
  fwrite(tot, OUT_TOT); fwrite(bil, OUT_BIL)
  message(sprintf("Wrote exiobase_totals.csv (%d rows) and exiobase_bilateral.csv (%d rows).",
                  nrow(tot), nrow(bil)))
}
