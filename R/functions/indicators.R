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
#' @details The green-patent variable is selected by `PATENT_MEASURE` in
#'   R/config.R ("applications" or "grants"); both are always built as
#'   `GreenPatentsApps_normed` / `GreenPatentsGrants_normed` so 07 can test the
#'   other one without rebuilding the panel.
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
      # Decomposition of CarbonIntensity: GHG/VA = (GHG/energy) x (energy/VA).
      # CarbonPerEnergy is the carbon content of energy (fuel mix / decarbon-
      # isation), near-orthogonal to EnergyIntensity and far less income-loaded;
      # used in the decomposed-vulnerability robustness spec (see info/PaperTodos.md).
      CarbonPerEnergy_normed   = GWP_pba / FinalEnergyConsumption,         # GHG per unit final energy
      # Consumption-based (footprint) counterparts of the two carbon variables.
      # GWP_cba = production + net embodied imports, i.e. the emissions serving
      # domestic final demand. The headline vulnerability axis stays PRODUCTION-
      # based on purpose: it measures the domestic ADJUSTMENT BURDEN (the plants,
      # workers and energy system a country must itself retool), not consumption
      # RESPONSIBILITY. The CBA pair supports (a) the accounting-choice robustness
      # spec in 07 and (b) the burden-vs-responsibility layer in
      # R/appendix_burden_responsibility.R. See info/PaperTodos.md item 4.
      GWP_cba                  = GWP_pba + GWP_Imports - GWP_Exports,      # (intermediate)
      CarbonIntensityCBA_normed = GWP_cba / ValueAdded_pba,                # footprint GHG per unit value added
      GWP_cba_normed           = GWP_cba / population,                     # footprint GHG p.c.
      # Fossil share of PRODUCTION kept only as a context/robustness variable:
      # it measures fossil *extraction*, not *dependence*, so it is 0 for every
      # non-producer (Malta, Luxembourg, the Baltics - and even oil-shale Estonia
      # is mis-coded 0). The headline ShareFossils_normed below uses the demand-
      # side gross-available-energy share instead (see info/PaperTodos.md item 3).
      ShareFossilsProd_normed  = ShareFossils_PrimEnProd,                  # fossil share of primary production (context only)
      # --- Other energy/emissions indicators (context, robustness) --------
      GWP_trade_normed         = (GWP_Imports - GWP_Exports) / population, # net embodied-GWP imports p.c.
      GWP_normed               = GWP_pba / population,                     # production-based GHG p.c.
      EnergyConsumption_normed = FinalEnergyConsumption / population,      # final energy demand p.c.
      # --- Potential block ------------------------------------------------
      # Both patent measures are always built; the headline picks one below.
      GreenPatentsGrants_normed = GreenPatents_n / (population / 1000000),      # EPO grants per million (grant-lag truncated from 2019)
      GreenPatentsApps_normed   = GreenPatentsApps_n / (population / 1000000),  # EPO applications per million
      ValueAdded_normed        = ValueAdded_pba / population,              # value added p.c.
      # --- Context / other ------------------------------------------------
      EnergyProduction_normed  = PrimaryEnergyProduction / population,     # primary energy production p.c.
      EnergyExports_normed     = EnergyNetTrade / population,              # net energy exports p.c.
      ShareRenewables_normed   = ShareRenewables_PrimEnProd               # renewable share of primary production
    )

  if (has_gdp) {
    dat <- dplyr::mutate(dat, GDP_normed = GDP_ppp / population)          # GDP (PPP) p.c.
  }

  # Headline green-innovation variable. `measure` comes from R/config.R
  # (PATENT_MEASURE); "grants" reproduces the pre-2026-09 headline.
  measure <- if (exists("PATENT_MEASURE")) PATENT_MEASURE else "grants"
  dat <- dplyr::mutate(dat, GreenPatents_normed = if (measure == "applications")
                         GreenPatentsApps_normed else GreenPatentsGrants_normed)

  # Headline fossil-share vulnerability: demand-side (fossil share of gross
  # available energy, Eurostat, in %). Measures how fossil-based the energy a
  # country actually *consumes* is - the transition-relevant carbon lock-in -
  # and has full EU-27 coverage. Falls back to the production-based share if the
  # extra panel is absent (extra_data = NULL).
  if ("ShareFossils_GrossAvEn" %in% names(dat)) {
    dat <- dplyr::mutate(dat, ShareFossils_normed = ShareFossils_GrossAvEn / 100)
  } else {
    dat <- dplyr::mutate(dat, ShareFossils_normed = ShareFossilsProd_normed)
  }

  dat |>
    dplyr::select(country, dplyr::contains("_normed")) |>
    dplyr::summarise(
      dplyr::across(dplyr::everything(), \(x) mean(x, na.rm = TRUE)),
      .by = "country"
    ) |>
    dplyr::mutate(country = countrycode::countrycode(country, "iso3c", "country.name"))
}
