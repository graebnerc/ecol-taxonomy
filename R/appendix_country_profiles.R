# Appendix - country drill-downs after the map (PaperTodos item 2).
#
# The presentation feedback was: after showing the map, discuss the countries
# that surprise people (Malta above all), and show descriptives -- in particular
# whether energy demand is driven by BUSINESS or by HOUSEHOLDS.
#
# This assembles, for every country and highlighting the flagged ones:
#   * quadrant and both axis scores
#   * the sectoral split of final energy (R/get_data_energy_sectors.R)
#   * production vs consumption footprint, i.e. how much it offshores
#   * fossil dependency
#
# The countries worth a paragraph are those whose position is either surprising
# or fragile: Malta and Luxembourg (extreme offshorers), Ireland (borderline, and
# the only country that changed quadrant when the window moved), Slovakia (the
# other one), Slovenia (the single Workbench country among the Winners, and
# borderline), and the Exposed-but-capable trio.
#
# Writes data/tidy/country_profiles.csv and plots/country_profiles.{png,pdf}.

here::i_am("R/appendix_country_profiles.R")
library(here)
suppressMessages({
  library(data.table); library(dplyr); library(tidyr)
  library(ggplot2); library(countrycode); library(knitr)
})
source(here("R/config.R"))
source(here("R/country_classification.R"))

HH  <- "#2E7CA8"   # households
BUS <- "#E65032"   # business
TRA <- "#9C9C9C"   # transport
INK <- "#1A1A1A"

sec <- fread(here("data/tidy/energy_by_sector.csv"))
scr <- fread(here("data/tidy/taxonomy_scores.csv"))
ind <- fread(here("data/tidy/taxonomy_indicators.csv"))
bur <- fread(here("data/tidy/burden_responsibility.csv"))

Q_LEVELS <- c("Winners", "Exposed", "Low-stakes", "At risk")
short_q <- function(x) sub(" \\(.*$", "", sub("Exposed but capable", "Exposed",
             sub("Low-stakes / low capability", "Low-stakes", x)))

prof <- scr[, .(country, group, quadrant = short_q(quadrant),
                vulnerability, potential, fossil, innovation, boundary)] |>
  merge(sec[, .(country, households_pct, business_pct, transport_pct,
                industry_pct, services_pct)], by = "country") |>
  merge(bur[, .(country, burden_pc, resp_pc, offshoring)], by = "country") |>
  merge(ind[, .(country, fossil_share = round(ShareFossils_normed * 100, 1))],
        by = "country")
prof[, quadrant := factor(quadrant, levels = Q_LEVELS)]
setorder(prof, quadrant, -offshoring)

fwrite(prof, here("data/tidy/country_profiles.csv"))

# --- The countries that need a paragraph -------------------------------------

flagged <- c("Malta", "Luxembourg", "Ireland", "Slovakia", "Slovenia",
             "Netherlands", "Czechia", "Poland")

cat("\n## Drill-down countries\n\n")
print(kable(prof[country %in% flagged,
                 .(country, group, quadrant,
                   vuln = round(vulnerability, 2), pot = round(potential, 2),
                   fossil_pct = fossil_share,
                   hh_pct = households_pct, bus_pct = business_pct,
                   offshore_ratio = round(offshoring, 2))][order(quadrant, country)],
            format = "pipe"))

cat("\n\n## The offshoring vignette (Malta, Luxembourg)\n\n")
mv <- prof[country %in% c("Malta", "Luxembourg")]
for (i in seq_len(nrow(mv))) {
  r <- mv[i]
  cat(sprintf(paste0(
    "  %-11s %s | vulnerability %+.2f, potential %+.2f\n",
    "              produces %.1f t CO2e p.c. but CONSUMES %.1f (ratio %.2fx)\n",
    "              fossil share of energy %.0f%%; energy demand %.0f%% households / %.0f%% business\n"),
    r$country, r$quadrant, r$vulnerability, r$potential,
    r$burden_pc, r$resp_pc, r$offshoring, r$fossil_share,
    r$households_pct, r$business_pct))
}

cat("\n## Household vs business energy demand, by quadrant\n\n")
print(kable(prof[, .(n = .N,
                     households_pct = round(mean(households_pct), 1),
                     business_pct   = round(mean(business_pct), 1),
                     transport_pct  = round(mean(transport_pct), 1)),
                 by = quadrant], format = "pipe"))

cat("\n  Does the split explain the map? Correlations with the axes:\n")
cat(sprintf("    cor(household share, vulnerability) = %+.2f\n",
            cor(prof$households_pct, prof$vulnerability)))
cat(sprintf("    cor(business share,  vulnerability) = %+.2f\n",
            cor(prof$business_pct, prof$vulnerability)))
cat(sprintf("    cor(industry share,  vulnerability) = %+.2f\n",
            cor(prof$industry_pct, prof$vulnerability)))

# --- Figure -------------------------------------------------------------------

long <- melt(prof[, .(country, quadrant,
                      Households = households_pct, Business = business_pct,
                      Transport = transport_pct)],
             id.vars = c("country", "quadrant"),
             variable.name = "sector", value.name = "pct")
long[, country := factor(country, levels = prof[order(quadrant, households_pct)]$country)]
long[, sector := factor(sector, levels = c("Households", "Business", "Transport"))]

p <- ggplot(long, aes(pct, country, fill = sector)) +
  geom_col(width = 0.72, colour = "white", linewidth = 0.4) +
  facet_grid(quadrant ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_fill_manual(NULL, values = c(Households = HH, Business = BUS, Transport = TRA)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.01)),
                     labels = function(x) paste0(x, "%")) +
  labs(x = NULL, y = NULL,
       title = "Who uses the energy? Households vs business, by quadrant",
       subtitle = paste0("Share of final energy consumption, ", REF_FIRST_YEAR, "-",
                         REF_LAST_YEAR, " mean (Eurostat nrg_bal_c).")) +
  theme_minimal(base_size = 9) +
  theme(legend.position = "top", legend.justification = "left",
        panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.3),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0, hjust = 0, face = "bold",
                                         size = 8, colour = INK),
        axis.text.y = element_text(size = 7.4, colour = INK),
        plot.title = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(colour = "grey35", size = 8))

ggsave(here("plots/country_profiles.png"), p, width = 7.4, height = 7.4, dpi = 300)
ggsave(here("plots/country_profiles.pdf"), p, width = 7.4, height = 7.4)

message("\nappendix_country_profiles.R done.")
