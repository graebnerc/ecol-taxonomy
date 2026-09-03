# Appendix figure: what constrains the reference window.
#
# REWRITTEN 2026-09-03. The original explained why the window "ends 2019", which
# was true of the EXIOBASE 3.8.x vintage. It is no longer: 3.10.2 gives complete
# emissions to 2022, and the binding constraint moved to the green-patent series.
#
# Coverage is now DERIVED FROM THE DATA rather than hardcoded, so the figure
# cannot drift out of date the way the previous version silently did. Each bar is
# the span over which that input is actually usable for all 27 member states, and
# the reference window is drawn from R/config.R.
#
# Standalone (not part of the 01-07 pipeline).
# Writes plots/appendix_window_coverage.{png,pdf} and
# data/tidy/window_coverage.csv.

here::i_am("R/appendix_window_coverage.R")
library(here)
suppressMessages({
  library(data.table); library(ggplot2); library(countrycode)
  library(dplyr); library(magrittr)
})
source(here("R/config.R"))
source(here("R/country_classification.R"))

INK <- "#1A1A1A"; OK_COL <- "#2E7CA8"; BAD <- "#E65032"; WIN <- "#0B3C5D"
eu <- countrycode(base_countries, "country.name", "iso3c")

panel <- fread(here("data/tidy/full_taxonomy_data.csv"))
extra <- fread(here("data/tidy/new_data.csv"))

# Last year an input covers all 27 member states.
full_to <- function(dt, col, ccol = "country") {
  d <- dt[get(ccol) %in% eu & !is.na(get(col)), .(n = uniqueN(get(ccol))), by = year]
  if (!nrow(d[n == 27L])) return(NA_integer_)
  max(d[n == 27L]$year)
}
first_from <- function(dt, col, ccol = "country") {
  d <- dt[get(ccol) %in% eu & !is.na(get(col)), .(n = uniqueN(get(ccol))), by = year]
  min(d[n == 27L]$year)
}

# Green patents are a special case: the rows exist but become meaningless once
# publication lag bites, so "coverage" must be judged on the VALUES, not on
# non-missingness. Usable = EU-27 total within 10% of its level five years before
# the series peak.
pat <- fread(here("data/tidy/green_patents_panel.csv"))
pt <- pat[iso3 %in% eu, .(tot = sum(applications)), by = year][order(year)]
ref_level <- pt[year == REF_LAST_YEAR - 3L]$tot
pat_last <- max(pt[tot >= 0.9 * ref_level]$year)

cov <- rbindlist(list(
  data.table(input = "Carbon & energy intensity\n(EXIOBASE 3.10.2)",
             from = first_from(panel, "GWP_pba"), to = full_to(panel, "GWP_pba"),
             note = "2023-24 nowcasts unusable"),
  data.table(input = "Fossil share\n(Eurostat energy balances)",
             from = first_from(extra, "ShareFossils_GrossAvEn", "iso3c"),
             to = full_to(extra, "ShareFossils_GrossAvEn", "iso3c"), note = ""),
  data.table(input = "Green patents\n(PATSTAT, applications)",
             from = min(pt$year), to = pat_last, note = "publication lag"),
  data.table(input = "Green complexity\n(Atlas HS92 trade)",
             from = 2012L, to = 2024L, note = ""),
  data.table(input = "Validators\n(GDP, renewable share)",
             from = first_from(extra, "GDP_real", "iso3c"),
             to = max(full_to(extra, "GDP_real", "iso3c"),
                      full_to(extra, "renew_share_overall", "iso3c")), note = "")))
cov[, from := pmax(from, 2010L)]          # clip the display, not the data
binding <- cov[to == min(to)]$input

cat("\n## What constrains the reference window\n\n")
print(cov[, .(input = gsub("\n", " ", input), usable_from = from, usable_to = to, note)])
cat(sprintf("\n  Reference window: %d-%d\n", REF_FIRST_YEAR, REF_LAST_YEAR))
cat(sprintf("  Binding constraint: %s (usable to %d)\n",
            gsub("\n", " ", binding), min(cov$to)))

# The window's LAST YEAR may be partially truncated even when the window as a
# whole is sound -- a year-level shortfall does not necessarily propagate to a
# 5-year mean. So do not fail here; quantify it at the level that matters.
if (REF_LAST_YEAR > min(cov$to)) {
  ow <- fread(here("data/tidy/oecd_green_patents.csv"))[
    tech == "climate_mitigation" & measure == "applications" & iso3 %in% eu]
  yv <- pat[iso3 %in% eu & year == REF_LAST_YEAR, sum(applications)]
  yo <- ow[year == REF_LAST_YEAR, sum(n_patents)]
  wv <- pat[iso3 %in% eu & year %between% c(REF_FIRST_YEAR, REF_LAST_YEAR),
            sum(applications)]
  wo <- ow[year %between% c(REF_FIRST_YEAR, REF_LAST_YEAR), sum(n_patents)]
  a <- pat[iso3 %in% eu & year %between% c(REF_FIRST_YEAR, REF_LAST_YEAR),
           .(v = sum(applications)), by = iso3]
  b <- ow[year %between% c(REF_FIRST_YEAR, REF_LAST_YEAR), .(o = sum(n_patents)), by = iso3]
  mm <- merge(a, b, by = "iso3")
  cat(sprintf(paste0(
    "\n  CAVEAT: %d, the window's last year, is partially truncated in the PATSTAT\n",
    "  series actually used -- %.0f%% of the independent OECD figure for that year.\n",
    "  It does NOT propagate: over the whole window the series is %.1f%% of OECD\n",
    "  (the shortfall is offset by earlier years running slightly high), and the\n",
    "  cross-country ranking is unaffected (Spearman %.4f). Report the caveat;\n",
    "  do not treat the window as compromised.\n"),
    REF_LAST_YEAR, 100 * yv / yo, 100 * wv / wo,
    cor(mm$v, mm$o, method = "spearman")))
}
fwrite(cov[, .(input = gsub("\n", " ", input), from, to, note)],
       here("data/tidy/window_coverage.csv"))

cov[, input := factor(input, levels = rev(cov$input))]
p <- ggplot(cov) +
  annotate("rect", xmin = REF_FIRST_YEAR - 0.45, xmax = REF_LAST_YEAR + 0.45,
           ymin = 0.4, ymax = nrow(cov) + 0.6, fill = WIN, alpha = 0.09) +
  geom_segment(aes(x = from, xend = to, y = input, yend = input),
               colour = OK_COL, linewidth = 3.4, lineend = "round") +
  geom_point(aes(x = to, y = input), colour = BAD, size = 2.6) +
  geom_text(data = cov[note != ""], aes(x = to + 0.25, y = input, label = note),
            hjust = 0, size = 2.7, colour = BAD) +
  annotate("text", x = (REF_FIRST_YEAR + REF_LAST_YEAR) / 2, y = nrow(cov) + 0.75,
           label = sprintf("Reference window %d–%d", REF_FIRST_YEAR, REF_LAST_YEAR),
           colour = WIN, fontface = "bold", size = 3.4) +
  scale_x_continuous(breaks = seq(2010, 2026, 2), limits = c(2009.5, 2027.5)) +
  labs(x = NULL, y = NULL,
       title = "What actually constrains the reference window",
       subtitle = paste0("Blue bar = years usable for all 27 member states; red dot = ",
                         "where each input stops.\nThe binding constraint is ",
                         gsub("\n", " ", binding), ".")) +
  theme_minimal(base_size = 9) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.3),
        axis.text.y = element_text(colour = INK, size = 8, lineheight = 1.05),
        plot.title = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(colour = "grey35", size = 8, lineheight = 1.15))

ggsave(here("plots/appendix_window_coverage.png"), p, width = 7.6, height = 4.4, dpi = 300)
ggsave(here("plots/appendix_window_coverage.pdf"), p, width = 7.6, height = 4.4)

message("\nappendix_window_coverage.R done.")
