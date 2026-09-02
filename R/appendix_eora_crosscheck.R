# Appendix - does the MRIO TABLE choice matter? EXIOBASE vs EORA26.
#
# THE QUESTION. Both vulnerability intensities are ratios of EXIOBASE quantities
# (GHG / value added, final energy / value added), so a referee can ask whether
# the map is an artifact of that one database. Half the answer already exists:
# moving from EXIOBASE 3.8.x to 3.10.2 -- an 18% revision of the emission
# accounts -- moved 0/27 countries. That tests the RELEASE, not the TABLE.
#
# This tests the table, by rebuilding the same window on a completely independent
# MRIO: EORA26 (v199.82, basic prices), a different compiler, different source
# data, different sector classification (26 vs 200 products) and different
# country coverage (190 vs 49 regions).
#
# WINDOW. EORA26 in this repo covers 2008-2017, so the comparison runs on
# 2014-2017 -- which is exactly why appendix_window_options.R carries a 2014-2017
# row. Holding the window fixed isolates the TABLE choice from the WINDOW choice;
# comparing EORA-2014-2017 against EXIOBASE-2017-2021 would confound the two.
#
# METHOD. Production-based accounting needs no Leontief inverse: per-country GHG
# is the direct satellite block summed over that country's sectors (plus the
# final-demand satellite QY, i.e. household direct emissions), and value added is
# the primary-input block summed the same way. Both intensities are then rebuilt
# on EORA value added, so the swap is complete rather than half-EXIOBASE.
#
# GAS BASKET: CO2 ONLY, on BOTH sides. This is forced, not preferred.
# EORA26's GHG satellite block in this release is partly unusable:
#   * every fluorinated gas carries ~51,000 Gg (SF6 50931, NF3 51500, HFC23
#     51332, C2F6 51159, ...) -- near-identical across gases AND across years,
#     i.e. placeholder fill, not data. Real global SF6 is ~10 Gg. Characterised
#     at AR4 GWPs these alone produce ~8,000 Gt CO2e, 160x the world total.
#   * N2O totals ~55,600 Gg against a real ~10,000, i.e. inflated ~5x.
#   * CO2 is credible: 33.3 Gt against a real ~36.
# So the comparison runs on CO2 only, and EXIOBASE is recomputed on CO2 only for
# the same years rather than reusing its full-basket totals -- otherwise the two
# sides would differ by basket as well as by table, and the test would be
# meaningless. CO2 is ~75% of the GHG basket, so this remains a substantive test.
#
# That EORA cannot supply a usable multi-gas footprint is itself worth recording:
# it is not a drop-in alternative to EXIOBASE for GHG accounting.
#
# Needs data/raw/eora_v199.82/Eora26_YYYY_bp.zip (gitignored).
# Writes data/tidy/eora_crosscheck.csv and plots/eora_crosscheck.{png,pdf}.

here::i_am("R/appendix_eora_crosscheck.R")
library(here)
suppressMessages({
  library(data.table); library(dplyr); library(ggplot2)
  library(countrycode); library(knitr); library(ggrepel)
})
source(here("R/config.R"))
source(here("R/country_classification.R"))
source(here("R/functions/indicators.R"))
source(here("R/functions/typology.R"))

EORA_DIR <- here("data/raw/eora_v199.82")
Y1 <- 2014L; Y2 <- 2017L          # EORA coverage in this repo ends 2017
CACHE <- here("data/raw/eora_country_totals.rds")

# CO2 only (see the header). EORA reports it in Gg (= kt); CO2b is biogenic and
# excluded, matching the EXIOBASE convention in R/get_data_exiobase.R.
GWP <- c(CO2 = 1)
EXIO_CO2_ROWS <- c("CO2 - combustion - air",
                   "CO2 - non combustion - Cement production - air",
                   "CO2 - non combustion - Lime production - air",
                   "CO2 - agriculture - peat decay - air",
                   "CO2 - waste - fossil - air")

read_eora_year <- function(year) {
  zip <- file.path(EORA_DIR, sprintf("Eora26_%d_bp.zip", year))
  stopifnot("EORA archive missing" = file.exists(zip))
  tmp <- file.path(tempdir(), sprintf("eora_%d", year))
  dir.create(tmp, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  base <- sprintf("Eora26_%d_bp", year)
  # The archives are packaged INCONSISTENTLY: 2016 nests everything under
  # Eora26_2016_bp/, the other years put the files at the archive root. Detect it
  # from the manifest rather than assuming, and derive the prefix from wherever
  # labels_T.txt actually is.
  members <- utils::unzip(zip, list = TRUE)$Name
  members <- members[!grepl("^__MACOSX/|/\\._", members)]
  anchor <- members[basename(members) == "labels_T.txt"]
  stopifnot("labels_T.txt not found in the archive" = length(anchor) == 1L)
  prefix <- dirname(anchor); prefix <- if (prefix == ".") "" else paste0(prefix, "/")
  want <- paste0(prefix, c("labels_T.txt", "labels_Q.txt", "labels_VA.txt",
                           "labels_FD.txt",
                           sprintf("%s_Q.txt", base), sprintf("%s_QY.txt", base),
                           sprintf("%s_VA.txt", base)))
  absent <- setdiff(want, members)
  if (length(absent)) stop("EORA ", year, " archive is missing: ",
                           paste(basename(absent), collapse = ", "))
  utils::unzip(zip, files = want, exdir = tmp)
  d <- file.path(tmp, if (prefix == "") "." else sub("/$", "", prefix))

  lt <- fread(file.path(d, "labels_T.txt"), header = FALSE, sep = "\t",
              colClasses = "character")
  iso <- lt$V1                                    # one entry per T column
  lq <- fread(file.path(d, "labels_Q.txt"), header = FALSE, sep = "\t",
              colClasses = "character")

  # Match "I-GHG-<GAS> emissions (Gg)" and keep only the gases in the basket.
  gas <- sub("^I-GHG-(.+) emissions \\(Gg\\)$", "\\1", lq$V1)
  keep <- which(grepl("^I-GHG-", lq$V1) & gas %in% names(GWP))
  fac  <- GWP[gas[keep]]

  Q <- as.matrix(fread(file.path(d, sprintf("%s_Q.txt", base)), header = FALSE,
                       sep = "\t", showProgress = FALSE))
  stopifnot("Q columns do not match T labels" = ncol(Q) == length(iso))
  ghg_sector <- as.numeric(fac %*% Q[keep, , drop = FALSE])   # Gg CO2e per sector
  rm(Q); gc(FALSE)

  # Household / final-demand direct emissions: QY columns are country x FD
  # category, so aggregate them back to the country they belong to.
  QY <- as.matrix(fread(file.path(d, sprintf("%s_QY.txt", base)), header = FALSE,
                        sep = "\t", showProgress = FALSE))
  lfd <- fread(file.path(d, "labels_FD.txt"), header = FALSE, sep = "\t",
               colClasses = "character")
  ghg_fd <- if (ncol(QY) == nrow(lfd)) {
    v <- as.numeric(fac %*% QY[keep, , drop = FALSE])
    tapply(v, factor(lfd$V1, levels = unique(iso)), sum)
  } else NULL
  rm(QY); gc(FALSE)

  VA <- as.matrix(fread(file.path(d, sprintf("%s_VA.txt", base)), header = FALSE,
                        sep = "\t", showProgress = FALSE))
  stopifnot("VA columns do not match T labels" = ncol(VA) == length(iso))
  va_sector <- colSums(VA)                       # all primary inputs, 1000 USD

  f <- factor(iso, levels = unique(iso))
  out <- data.table(iso3 = levels(f),
                    ghg_Gg = as.numeric(tapply(ghg_sector, f, sum)),
                    va     = as.numeric(tapply(va_sector, f, sum)))
  if (!is.null(ghg_fd)) {
    m <- match(out$iso3, names(ghg_fd))
    out[, ghg_Gg := ghg_Gg + ifelse(is.na(ghg_fd[m]), 0, ghg_fd[m])]
  }
  out[, year := year]
  cat(sprintf("  %d: world GHG %.1f Gt | VA %.1f tn (EORA units)\n",
              year, sum(out$ghg_Gg) / 1e6, sum(out$va) / 1e9))
  out[]
}

if (file.exists(CACHE)) {
  eora <- readRDS(CACHE); message("Loaded cached EORA country totals.")
} else {
  cat("Reading EORA26 ", Y1, "-", Y2, " ...\n", sep = "")
  eora <- rbindlist(lapply(Y1:Y2, read_eora_year))
  saveRDS(eora, CACHE)
}

# --- EXIOBASE on the SAME gas basket and the SAME years ----------------------
# Production-based totals need only the satellite blocks, so this is cheap: no
# Z read, no Leontief inverse.
EXIO_CACHE <- here("data/raw/exiobase_co2_totals.rds")
read_exio_co2 <- function(year) {
  zip <- here("data/raw/exiobase", sprintf("IOT_%d_pxp.zip", year))
  stopifnot("EXIOBASE archive missing" = file.exists(zip))
  tmp <- file.path(tempdir(), sprintf("exco2_%d", year))
  dir.create(tmp, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  utils::unzip(zip, files = c("air_emissions/F.txt", "air_emissions/F_Y.txt",
                              "factor_inputs/F.txt"), exdir = tmp)
  rl <- function(pth) {
    hdr <- fread(pth, nrows = 2L, header = FALSE, sep = "\t",
                 colClasses = "character", showProgress = FALSE)
    list(m = as.matrix(fread(pth, skip = 3L, header = FALSE, sep = "\t",
                             drop = 1L, showProgress = FALSE)),
         reg = as.character(hdr[1, -1]),
         rows = fread(pth, skip = 3L, header = FALSE, sep = "\t", select = 1L,
                      colClasses = "character", showProgress = FALSE)[[1]])
  }
  F  <- rl(file.path(tmp, "air_emissions/F.txt"))
  FY <- rl(file.path(tmp, "air_emissions/F_Y.txt"))
  VA <- rl(file.path(tmp, "factor_inputs/F.txt"))
  F$m[is.na(F$m)] <- 0; FY$m[is.na(FY$m)] <- 0; VA$m[is.na(VA$m)] <- 0
  i <- match(EXIO_CO2_ROWS, F$rows); stopifnot("CO2 rows not found" = !anyNA(i))
  regs <- unique(F$reg)
  co2 <- tapply(colSums(F$m[i, , drop = FALSE]), factor(F$reg, levels = regs), sum) +
         tapply(colSums(FY$m[match(EXIO_CO2_ROWS, FY$rows), , drop = FALSE]),
                factor(FY$reg, levels = regs), sum)
  vi <- grep("^(Other net taxes|Compensation of employees|Operating surplus)", VA$rows)
  va <- tapply(colSums(VA$m[vi, , drop = FALSE]), factor(VA$reg, levels = regs), sum)
  data.table(iso2 = regs, co2_kg = as.numeric(co2), va_MEUR = as.numeric(va),
             year = year)
}
if (file.exists(EXIO_CACHE)) {
  exio_co2 <- readRDS(EXIO_CACHE)
} else {
  cat("Reading EXIOBASE CO2 totals ", Y1, "-", Y2, " ...\n", sep = "")
  exio_co2 <- rbindlist(lapply(Y1:Y2, read_exio_co2))
  saveRDS(exio_co2, EXIO_CACHE)
}
exio_co2[, iso3 := countrycode(iso2, "iso2c", "iso3c", warn = FALSE)]

eu_iso3 <- countrycode(base_countries, "country.name", "iso3c")
missing_eu <- setdiff(eu_iso3, unique(eora$iso3))
if (length(missing_eu)) stop("EU states absent from EORA: ",
                             paste(missing_eu, collapse = ", "))

eo <- eora[iso3 %in% eu_iso3, .(ghg = mean(ghg_Gg), va = mean(va)), by = iso3]
ex <- exio_co2[iso3 %in% eu_iso3, .(co2 = mean(co2_kg), va_x = mean(va_MEUR)), by = iso3]

# --- Rebuild the typology on EORA emissions AND EORA value added -------------
base_data  <- as_tibble(fread(here("data/tidy/full_taxonomy_data.csv")))
extra_data <- as_tibble(fread(here("data/tidy/new_data.csv")))
ind <- build_indicator_table(base_data, extra_data, first_year = Y1, last_year = Y2)
ind$iso3 <- countrycode(ind$country, "country.name", "iso3c")

# Complexity for the same window, from the wide Atlas cache.
source(here("R/functions/complexity.R"))
eby <- as.data.table(readRDS(here("data/raw/exports_by_year_1224.rds")))
green <- fread(here("data/tidy/green_products_hs6.csv"),
               colClasses = list(character = "hs6"))
rca <- build_rca_matrix(eby[year %between% c(Y1, Y2), .(export = sum(export)),
                            by = .(iso3, hs6)])
ci  <- complexity_indices(rca$M)
gi  <- green_indicators(rca$M, ci$PCI, green$hs6)
m <- match(ind$iso3, gi$iso3); ind$GCI <- gi$GCI[m]; ind$GCP <- gi$GCP[m]
stopifnot("EU country lost from complexity" = !anyNA(ind$GCI))

# Final energy for the same window, to rebuild energy intensity on EORA VA.
fe <- as.data.table(base_data)[year %between% c(Y1, Y2),
        .(fe = mean(FinalEnergyConsumption, na.rm = TRUE)), by = .(iso3 = country)]

ind <- as.data.table(ind)
ind <- merge(ind, eo, by = "iso3"); ind <- merge(ind, ex, by = "iso3")
ind <- merge(ind, fe, by = "iso3")
# Both sides: CO2 per unit of that table's OWN value added, and final energy per
# the same value added, so each variant is internally consistent.
ind[, `:=`(CarbonIntensityEORA = ghg / va,        EnergyIntensityEORA = fe / va,
           CarbonIntensityEXIO = co2 / va_x,      EnergyIntensityEXIO = fe / va_x)]

# axis_score() indexes df[, vars], which a data.table interprets differently --
# pass a plain data.frame.
score <- function(d, ivars) {
  d <- as.data.frame(d)
  v <- axis_score(d, ivars, ivars[1], FOSSIL_VAR)$score
  p <- axis_score(d, COMPLEXITY_VARS, "GCI", INNOV_VAR)$score
  list(v = v, p = p, q = assign_quadrant(v, p, "short"))
}
exio   <- score(ind, c("CarbonIntensityEXIO", "EnergyIntensityEXIO"))
eora_s <- score(ind, c("CarbonIntensityEORA", "EnergyIntensityEORA"))

cmp <- data.table(country = ind$country,
                  group = as.character(get_country_classification(ind$iso3, "jee")),
                  exio_int = ind$CarbonIntensityEXIO,
                  eora_int = ind$CarbonIntensityEORA,
                  q_exio = exio$q, q_eora = eora_s$q)

cat(sprintf("\n## EXIOBASE vs EORA26 -- same window (%d-%d), CO2 only on both sides\n\n", Y1, Y2))
cat(sprintf("  CO2 intensity, cross-country Pearson  = %.3f\n",
            cor(cmp$exio_int, cmp$eora_int)))
cat(sprintf("  CO2 intensity, cross-country Spearman = %.3f\n",
            cor(cmp$exio_int, cmp$eora_int, method = "spearman")))
cat(sprintf("  vulnerability axis, Spearman             = %.3f\n",
            cor(exio$v, eora_s$v, method = "spearman")))
cat(sprintf("  QUADRANT CHANGES                         = %d / 27\n",
            sum(cmp$q_exio != cmp$q_eora)))
if (any(cmp$q_exio != cmp$q_eora)) {
  cat("\n  moved:\n")
  print(kable(cmp[q_exio != q_eora, .(country, group, EXIOBASE = q_exio, EORA = q_eora)],
              format = "pipe"))
}
fwrite(cmp, here("data/tidy/eora_crosscheck.csv"))

p <- ggplot(cmp, aes(exio_int, eora_int, colour = group)) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey75", linewidth = 0.5) +
  geom_point(size = 2.2) +
  ggrepel::geom_text_repel(aes(label = country), size = 2.6, max.overlaps = 16,
                           segment.colour = "grey75", segment.size = 0.2) +
  scale_colour_manual(NULL, values = c(Core = "#0B3C5D", Finance = "#2E7CA8",
                                       Periphery = "#E65032", Workbench = "#F09372")) +
  labs(x = "Carbon intensity, EXIOBASE 3.10.2 (g CO2e per EUR value added)",
       y = "CO2 intensity, EORA26 (Gg CO2 per 1000 USD value added)",
       title = "Does the MRIO table choice matter?",
       subtitle = sprintf(paste("Same window (%d-%d), CO2 only on both sides, two independent MRIOs.",
                                "\nUnits differ; what matters is whether countries keep their places."),
                          Y1, Y2)) +
  theme_minimal(base_size = 9) +
  theme(legend.position = "top", legend.justification = "left",
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(colour = "grey35", size = 8, lineheight = 1.15))

ggsave(here("plots/eora_crosscheck.png"), p, width = 7.0, height = 5.4, dpi = 300)
ggsave(here("plots/eora_crosscheck.pdf"), p, width = 7.0, height = 5.4)

message("\nappendix_eora_crosscheck.R done.")
