# Economic- and green-complexity functions, following Mealy & Teytelboym (2022)
# (info/GreenComplexity.pdf, sections 3.4-3.7) and Hausmann et al. (2014).
#
# All measures are computed on the GLOBAL country set; EU-27 rows are extracted
# afterwards by the caller. Matrices are country x product (rows x cols).

#' Build the binary Balassa (RCA >= 1) matrix from a country-product export table.
#' (Mealy & Teytelboym write RCA > 1; the >= vs > distinction is measure-zero in
#' continuous export data. The code uses >= 1 at the M <- (rca >= 1) step below.)
#'
#' @param exp_dt data.table/data.frame with columns iso3, hs6, export (pooled
#'   over the reference window).
#' @param min_country_export Drop countries whose total export is below this
#'   (removes micro-reporters; default ~ USD 1bn/yr over a 5-year window).
#' @return list(M = binary matrix, rca = RCA matrix), rows = countries, cols = products.
build_rca_matrix <- function(exp_dt, min_country_export = 5e9) {
  # Drop Atlas "unspecified" trade codes (~4.8% of world exports): 999999 and the
  # text sentinel XXXXXX are not real products and must not enter the RCA/PCI
  # estimation (audit finding N2). Effect on ECI/GCI/GCP is small (cor ~ 0.998),
  # but a few countries near the median flip quadrant, so it is applied on principle.
  exp_dt <- exp_dt[!(as.character(exp_dt$hs6) %in% c("999999", "XXXXXX")), ]

  # Country x product export matrix via a sparse pivot (fast; a dense tapply on
  # ~600k pairs is both slow and memory-heavy).
  fi <- factor(exp_dt$iso3)
  fp <- factor(exp_dt$hs6)
  X <- as.matrix(Matrix::sparseMatrix(
    i = as.integer(fi), j = as.integer(fp), x = exp_dt$export,
    dims = c(nlevels(fi), nlevels(fp)),
    dimnames = list(levels(fi), levels(fp))
  ))

  # Coverage filters: drop tiny reporters and products with no world trade,
  # iterating until stable (a drop can empty another row/column).
  repeat {
    keep_c <- rowSums(X) >= min_country_export
    keep_p <- colSums(X) > 0
    if (all(keep_c) && all(keep_p)) break
    X <- X[keep_c, keep_p, drop = FALSE]
  }

  # RCA (Balassa): (x_cp / sum_p x_cp) / (sum_c x_cp / sum_cp x_cp)
  country_tot <- rowSums(X)
  product_tot <- colSums(X)
  total <- sum(X)
  rca <- (X / country_tot) / (rep(product_tot, each = nrow(X)) / total)

  M <- (rca >= 1) * 1
  # Drop products no one is competitive in, and countries competitive in nothing
  # (they carry no complexity information and break the 1/u, 1/d normalisations).
  repeat {
    keep_p <- colSums(M) > 0
    keep_c <- rowSums(M) > 0
    if (all(keep_c) && all(keep_p)) break
    M <- M[keep_c, keep_p, drop = FALSE]
    rca <- rca[keep_c, keep_p, drop = FALSE]
  }
  list(M = M, rca = rca)
}

#' Economic Complexity Index (ECI) and Product Complexity Index (PCI).
#'
#' Uses the eigenvalue method (Hausmann et al. 2014). PCI is recovered from the
#' country-side eigenvector via PCI ~ diag(1/u) M' k, which shares the non-zero
#' eigenvalues of the product-side matrix - avoiding a P x P eigendecomposition.
#' Both are standardised (mean 0, sd 1); signs are fixed so ECI rises with diversity.
complexity_indices <- function(M) {
  d <- rowSums(M)                       # diversity
  u <- colSums(M)                       # ubiquity

  # Country similarity matrix ~M_c = diag(1/d) M diag(1/u) M'
  Mc <- (M / d) %*% (t(M) / u)          # C x C
  eg <- eigen(Mc)
  ord <- order(Re(eg$values), decreasing = TRUE)
  kc <- Re(eg$vectors[, ord[2]])        # 2nd eigenvector = country complexity

  # Map to product complexity: PCI_raw = (1/u) * M' kc
  kp <- as.numeric((t(M) %*% kc) / u)

  # Sign convention: ECI should correlate positively with diversity.
  if (stats::cor(kc, d) < 0) { kc <- -kc; kp <- -kp }

  z <- function(x) (x - mean(x)) / stats::sd(x)
  list(
    ECI = setNames(z(kc), rownames(M)),
    PCI = setNames(z(kp), colnames(M))
  )
}

#' Green Complexity Index (GCI) and Green Complexity Potential (GCP) per country.
#'
#' Note: in this data GCI is ~ a count of green products a country makes with
#' RCA >= 1 (cor(GCI, green diversity) ~ 0.998; the PCI weighting adds little),
#' as Mealy & Teytelboym also observe (their fn. 9). Read GCI as green diversity
#' more than "technologically sophisticated green capability".
#'
#' @param M binary RCA matrix (country x product).
#' @param PCI standardised product complexity (named by product code).
#' @param green_codes character vector of green HS6 codes.
green_indicators <- function(M, PCI, green_codes) {
  green_idx <- which(colnames(M) %in% green_codes)
  if (length(green_idx) == 0) stop("No green products present in the RCA matrix.")

  # PCI normalised to [0, 1] (Mealy eq. 8/9)
  PCI_norm <- (PCI - min(PCI)) / (max(PCI) - min(PCI))
  Mg  <- M[, green_idx, drop = FALSE]           # C x G
  pg  <- PCI_norm[green_idx]                    # G

  # GCI = sum of normalised PCI over green products a country is competitive in
  GCI <- as.numeric(Mg %*% pg)

  # Proximity of every product to each green product: phi_ig = C_ig / max(u_i, u_g)
  u  <- colSums(M)
  Cg <- crossprod(M, Mg)                        # P x G co-occurrence counts
  phi_g <- Cg / outer(u, u[green_idx], pmax)    # P x G

  # Density of a country's capabilities around each green product (Mealy eq. 7)
  omega <- (M %*% phi_g) / rep(colSums(phi_g), each = nrow(M))  # C x G

  # GCP = mean density-weighted normalised PCI over green products NOT yet held
  not_held <- 1 - Mg
  GCP <- rowSums(not_held * omega * rep(pg, each = nrow(M))) / rowSums(not_held)

  z <- function(x) (x - mean(x)) / stats::sd(x)
  data.frame(
    iso3      = rownames(M),
    GCI       = z(GCI),
    GCP       = z(GCP),
    diversity = rowSums(M),
    row.names = NULL
  )
}
