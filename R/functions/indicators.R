# Functions to turn the raw panel (full_taxonomy_data.csv) into a per-country
# table of taxonomy indicators. All per-capita / share / intensity transforms
# live here so that normalisation is explicit and in one place.

#' Build the per-country indicator table.
#'
#' @param base_data Panel read from data/tidy/full_taxonomy_data.csv (ISO3 x year).
#' @param extra_data Panel read from data/tidy/new_data.csv (adds GDP_ppp etc.);
#'   one row per country-year (see R/get_data_extra.R); pass NULL to skip
#'   GDP-based indicators.
#' @param first_year,last_year Reference window (inclusive).
#' @return Tibble, one row per country (English country name), with `*_normed`
#'   indicator columns averaged over the window.
build_indicator_table <- function(base_data, extra_data,
                                   first_year, last_year) {
  dat <- base_data |>
    dplyr::filter(year <= last_year, year >= first_year)

  if (!is.null(extra_data)) {
    # Guard against the B1 defect: extra_data must be one row per country-year,
    # else the left_join silently fans out the panel (see R/get_data_extra.R).
    stopifnot(!anyDuplicated(extra_data[, c("iso3c", "year")]))
    dat <- dplyr::left_join(
      dat, extra_data, by = c("country" = "iso3c", "year")
    )
  }
  has_gdp <- "GDP_ppp" %in% names(dat)

  dat <- dat |>
    dplyr::mutate(population = population * 1000) |>
    dplyr::mutate(
      # --- Vulnerability block (intensity/mix; income-independent) ---------
      CarbonIntensity_normed   = GWP_pba / ValueAdded_pba,                 # GHG per unit value added
      EnergyIntensity_normed   = FinalEnergyConsumption / ValueAdded_pba,  # energy per unit value added
      ShareFossils_normed      = ShareFossils_PrimEnProd,                  # fossil share of primary production
      # --- Other energy/emissions indicators (context, robustness) --------
      GWP_trade_normed         = (GWP_Imports - GWP_Exports) / population, # net embodied-GWP imports p.c.
      GWP_normed               = GWP_pba / population,                     # production-based GHG p.c.
      EnergyConsumption_normed = FinalEnergyConsumption / population,      # final energy demand p.c.
      # --- Potential block ------------------------------------------------
      GreenPatents_normed      = GreenPatents_n / (population / 1000000),  # green patents per million
      ValueAdded_normed        = ValueAdded_pba / population,              # value added p.c.
      # --- Context / other ------------------------------------------------
      EnergyProduction_normed  = PrimaryEnergyProduction / population,     # primary energy production p.c.
      EnergyExports_normed     = EnergyNetTrade / population,              # net energy exports p.c.
      ShareRenewables_normed   = ShareRenewables_PrimEnProd               # renewable share of primary production
    )

  if (has_gdp) {
    dat <- dplyr::mutate(dat, GDP_normed = GDP_ppp / population)          # GDP (PPP) p.c.
  }

  dat |>
    dplyr::select(country, dplyr::contains("_normed")) |>
    dplyr::summarise(
      dplyr::across(dplyr::everything(), \(x) mean(x, na.rm = TRUE)),
      .by = "country"
    ) |>
    dplyr::mutate(country = countrycode::countrycode(country, "iso3c", "country.name"))
}
