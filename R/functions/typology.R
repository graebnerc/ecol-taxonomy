# Helpers for the vulnerability x potential typology, shared by 04_typology.R
# and 07_robustness.R so every specification uses identical machinery.

#' Scale a numeric matrix column-wise: "z" (mean/sd) or "robust" (median/MAD).
scale_mat <- function(X, method = c("z", "robust")) {
  method <- match.arg(method)
  X <- as.matrix(X)
  if (method == "z") return(scale(X))
  med <- apply(X, 2, median)
  md  <- apply(X, 2, mad); md[md == 0] <- 1
  sweep(sweep(X, 2, med, "-"), 2, md, "/")
}

#' One block score (PC1 or the simple standardised mean), oriented so it rises
#' with `anchor`. Returns the standardised score plus loadings / variance share.
#' @param method "pca" or "mean"; @param scale "z" or "robust".
block_score <- function(df, vars, anchor, method = c("pca", "mean"),
                        scale = c("z", "robust")) {
  method <- match.arg(method); scale <- match.arg(scale)
  X <- scale_mat(df[, vars], scale)
  if (method == "pca") {
    pc <- prcomp(X, center = FALSE, scale. = FALSE)
    score <- pc$x[, 1]; load <- pc$rotation[, 1]
    ve <- summary(pc)$importance[2, 1]
  } else {
    score <- rowMeans(X); load <- setNames(rep(1 / ncol(X), ncol(X)), colnames(X)); ve <- NA
  }
  if (cor(score, df[[anchor]]) < 0) { score <- -score; load <- -load }
  list(score = as.numeric(scale(score)), loadings = load, var_explained = ve)
}

#' Two-part block axis (Fig. 1 structure). Combines a twin sub-index (PC1 of two
#' correlated indicators of one construct, oriented to rise with `twin_anchor`)
#' and one standalone indicator (`solo_var`, higher = more of the axis) at weight
#' `w_twin` : (1 - w_twin). The default 0.5 (equal) is deliberate: the two parts
#' are co-equal conceptual dimensions, so neither the correlated twins nor the
#' standalone should dominate by count; `w_twin` is exposed only so 07_robustness
#' can test that choice. Returns the standardised axis plus its standardised parts.
axis_score <- function(df, twin_vars, twin_anchor, solo_var,
                       method = c("pca", "mean"), scale = c("z", "robust"),
                       w_twin = 0.5) {
  method <- match.arg(method); scale <- match.arg(scale)
  tw   <- block_score(df, twin_vars, twin_anchor, method, scale)
  twin <- as.numeric(base::scale(tw$score))              # standardised twin sub-index
  solo <- as.numeric(base::scale(df[[solo_var]]))        # standardised standalone (higher = more)
  axis <- as.numeric(base::scale(w_twin * twin + (1 - w_twin) * solo))
  list(score = axis, twin = twin, solo = solo,
       twin_loadings = tw$loadings, twin_var_explained = tw$var_explained)
}

#' Assign the four quadrants from median splits of the two scores.
#' `labels = "short"` for compact robustness tables, "long" for the paper.
assign_quadrant <- function(vuln, pot, labels = c("long", "short")) {
  labels <- match.arg(labels)
  mx <- median(vuln); my <- median(pot)
  lab <- if (labels == "long")
    c(win = "Winners (low vuln / high pot)", exp = "Exposed but capable",
      low = "Low-stakes / low capability", risk = "At risk (high vuln / low pot)")
  else c(win = "Winners", exp = "Exposed", low = "Low-stakes", risk = "At risk")
  out <- ifelse(vuln <  mx & pot >= my, lab["win"],
         ifelse(vuln >= mx & pot >= my, lab["exp"],
         ifelse(vuln <  mx & pot <  my, lab["low"], lab["risk"])))
  unname(out)
}
