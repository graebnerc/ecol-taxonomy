# Reusable helpers for the clustering / typology analysis. Extracted from the
# legacy clustering script so the numbered pipeline stays readable.

#' Scale an indicator table and return the standardised numeric matrix.
#' Country column is moved to rownames.
scale_indicators <- function(indicator_tbl, vars = NULL) {
  df <- as.data.frame(indicator_tbl)
  rownames(df) <- df$country
  df$country <- NULL
  if (!is.null(vars)) df <- df[, vars, drop = FALSE]
  scale(df)
}

#' Compare agglomerative linkage methods by their agnes clustering coefficient.
#' Returns a tibble (Algorithm, Coefficient); higher = stronger structure.
compare_linkage <- function(dist_mat,
                            methods = c("average", "single", "complete", "ward")) {
  names(methods) <- methods
  coefs <- purrr::map_dbl(methods, \(m) cluster::agnes(dist_mat, method = m)$ac)
  tibble::tibble(Algorithm = names(coefs), Coefficient = unname(coefs))
}

#' Horizontal dendrogram coloured by k clusters.
plot_dendrogram <- function(agnes_obj, k, title = NULL) {
  factoextra::fviz_dend(
    agnes_obj,
    main = title, xlab = "Countries", ylab = "",
    k = k, cex = 0.75,
    rect = TRUE, rect_fill = TRUE,
    color_labels_by_k = TRUE, horiz = TRUE
  )
}

#' Alluvial plot linking the data-driven clusters to an external classification
#' (e.g. the Graebner et al. development-model groups).
#'
#' @param clusters Named integer vector from cutree() (names = country).
#' @param classify_fun Function mapping ISO3 -> group label.
plot_cluster_alluvial <- function(clusters, classify_fun, title = "Development and ecological models") {
  tb <- tibble::tibble(
    country = names(clusters),
    `Ecological model` = as.character(clusters),
    `Development model` = classify_fun(
      countrycode::countrycode(names(clusters), "country.name", "iso3c")
    )
  ) |>
    tidyr::pivot_longer(cols = -country, names_to = "group", values_to = "code")

  ggplot2::ggplot(
    tb,
    ggplot2::aes(x = group, stratum = code, alluvium = country,
                 fill = code, label = code)
  ) +
    ggalluvial::geom_flow(stat = "alluvium", lode.guidance = "frontback",
                          color = "darkgray", curve_type = "linear") +
    ggalluvial::geom_stratum() +
    ggplot2::geom_text(stat = "stratum", size = 3) +
    ggplot2::ggtitle(title) +
    ggplot2::theme_void() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(),
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5)
    )
}
