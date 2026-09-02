# Appendix - WHERE do the embodied emissions come from? The bilateral test.
#
# THE CLAIM UNDER TEST
#   "The core's clean production profile rests on offshoring to the European
#   East, and that transfer is the mechanism of green-transition polarization."
#
# Until now this was untestable: data/tidy/TXNY_GWP_Trade.csv carried only
# country TOTALS of embodied imports and exports, with no origin dimension, so
# the core's footprint could not be split into intra-EU and extra-EU parts.
# R/get_data_exiobase.R now produces the full 49 x 49 origin x destination
# matrix, which settles it.
#
# WHAT THE TOTALS ALREADY IMPLIED (an arithmetic bound, computed before the
# bilateral data existed): over 2014-2018 the Workbench East exported ~0.275 Gt
# of embodied GHG to the ENTIRE world while the Core imported ~0.752 Gt from the
# entire world, so even if every Workbench tonne went to the Core it could
# account for at most ~37% of the Core's embodied imports. The Workbench is also
# roughly BALANCED in embodied trade (imports 0.296 vs exports 0.275 Gt), which
# already sits badly with a story about the East producing for the West.
#
# This script replaces that bound with the actual number.
#
# Writes data/tidy/offshoring_origins.csv and
# plots/offshoring_origins.{png,pdf}.

here::i_am("R/appendix_offshoring_origins.R")
library(here)
suppressMessages({
  library(data.table); library(dplyr); library(tidyr)
  library(ggplot2); library(countrycode); library(knitr)
})
source(here("R/config.R"))
source(here("R/country_classification.R"))

BIL <- here("data/tidy/exiobase_bilateral.csv")
stopifnot("run R/get_data_exiobase.R first" = file.exists(BIL))

eu_iso3 <- countrycode(base_countries, "country.name", "iso3c")

bil <- fread(BIL)[year %between% c(REF_FIRST_YEAR, REF_LAST_YEAR)]
bil[, `:=`(o3 = countrycode(origin, "iso2c", "iso3c", warn = FALSE),
           d3 = countrycode(destination, "iso2c", "iso3c", warn = FALSE))]

grp <- function(iso3, code) {
  out <- rep(NA_character_, length(iso3))
  eu <- !is.na(iso3) & iso3 %in% eu_iso3
  out[eu] <- as.character(get_country_classification(iso3[eu], "jee"))
  out[!eu] <- ifelse(code[!eu] == "CN", "China",
              ifelse(code[!eu] %in% c("WA","WE","WF","WL","WM"), "Rest of world (aggregates)",
                     "Other non-EU"))
  out
}
bil[, `:=`(o_grp = grp(o3, origin), d_grp = grp(d3, destination))]
bil[, eu_origin := !is.na(o3) & o3 %in% eu_iso3]

# Annual means over the reference window; imports exclude the diagonal.
ny <- uniqueN(bil$year)
imp <- bil[origin != destination & d3 %in% eu_iso3,
           .(GWP = sum(GWP) / ny), by = .(d_grp, o_grp)]

# --- 1. Where does each EU development model's embodied import come from? ---------

tab <- dcast(imp, d_grp ~ o_grp, value.var = "GWP", fill = 0)
mat <- as.matrix(tab[, -1]); rownames(mat) <- tab$d_grp
shares <- round(100 * mat / rowSums(mat), 1)

cat("\n## Embodied GHG imports of each EU development model, by ORIGIN (% of its imports)\n")
cat(sprintf("   EU-27 destinations, %d-%d annual mean.\n\n",
            REF_FIRST_YEAR, REF_LAST_YEAR))
print(kable(as.data.frame(shares), format = "pipe"))

cat("\n\n## Same, in Gt CO2e per year\n\n")
print(kable(round(as.data.frame(mat) / 1e12, 3), format = "pipe"))

# --- 2. The claim, stated as a number ----------------------------------------

core_imports <- sum(imp[d_grp == "Core"]$GWP)
core_from_wb <- sum(imp[d_grp == "Core" & o_grp == "Workbench"]$GWP)
core_from_eu <- sum(imp[d_grp == "Core" & o_grp %in%
                          c("Core","Finance","Periphery","Workbench")]$GWP)
eu_imports   <- sum(imp$GWP)
eu_from_eu   <- sum(imp[o_grp %in% c("Core","Finance","Periphery","Workbench")]$GWP)

cat("\n\n## The mechanism claim, tested\n\n")
cat(sprintf("  Core embodied imports, total                    : %.3f Gt/yr\n", core_imports/1e12))
cat(sprintf("  ... of which from the Workbench East            : %.3f Gt/yr = %.1f%%\n",
            core_from_wb/1e12, 100*core_from_wb/core_imports))
cat(sprintf("  ... of which from anywhere in the EU-27         : %.3f Gt/yr = %.1f%%\n",
            core_from_eu/1e12, 100*core_from_eu/core_imports))
cat(sprintf("  ... of which from outside the EU-27             : %.1f%%\n",
            100*(1 - core_from_eu/core_imports)))
cat(sprintf("\n  EU-27 embodied imports, total                   : %.3f Gt/yr\n", eu_imports/1e12))
cat(sprintf("  ... intra-EU share                              : %.1f%%\n",
            100*eu_from_eu/eu_imports))
cat(sprintf("  ... extra-EU share                              : %.1f%%\n",
            100*(1 - eu_from_eu/eu_imports)))

# --- 3. Net bilateral position between EU blocs ------------------------------

blocs <- c("Core", "Finance", "Periphery", "Workbench")
intra <- bil[o_grp %in% blocs & d_grp %in% blocs & origin != destination,
             .(GWP = sum(GWP) / ny), by = .(o_grp, d_grp)]
net <- merge(intra, intra, by.x = c("o_grp","d_grp"), by.y = c("d_grp","o_grp"),
             suffixes = c("_out","_in"))
net[, net_Mt := round((GWP_out - GWP_in) / 1e9, 1)]
cat("\n\n## Net INTRA-EU embodied flows between blocs (Mt CO2e/yr; + = row is a net exporter to column)\n\n")
print(kable(dcast(net[o_grp != d_grp], o_grp ~ d_grp, value.var = "net_Mt"),
            format = "pipe"))

fwrite(imp[, .(destination_group = d_grp, origin_group = o_grp,
               GWP_Gt_per_year = round(GWP / 1e12, 4))],
       here("data/tidy/offshoring_origins.csv"))

# --- 4. The development-model gradient -- the finding that SURVIVES ---------------
#
# The origin COMPOSITION barely differs across blocs (intra-EU share 20-32%
# everywhere), so "different development models source from different places" is NOT
# a finding. What differs systematically is the relationship between what a bloc
# PRODUCES and what it CONSUMES, and the ordering matches the Graebner et al.
# development-model hierarchy exactly. This does not need bilateral data at all --
# it falls out of the totals -- which makes it the robust version of the
# offshoring story.

tot <- fread(here("data/tidy/exiobase_totals.csv"))[
  year %between% c(REF_FIRST_YEAR, REF_LAST_YEAR)]
tot[, iso3 := countrycode(region, "iso2c", "iso3c", warn = FALSE)]
tot <- tot[iso3 %in% eu_iso3]
tot[, grp := as.character(get_country_classification(iso3, "jee"))]
nyr <- uniqueN(tot$year)

gradient <- tot[, .(pba = sum(GWP_pba) / nyr,
                    cba = sum(GWP_pba - GWP_Exports + GWP_Imports) / nyr,
                    va  = sum(ValueAdded_pba) / nyr), by = grp]
gradient[, `:=`(gap_pct = round(100 * (cba / pba - 1)),
                int_pba = round(pba / va / 1000))]

# Net intra-EU position, scaled by the bloc's own production emissions: how much
# of what a bloc emits is actually serving other EU members' final demand.
intra_eu <- bil[!is.na(o_grp) & !is.na(d_grp) &
                  o_grp %in% blocs & d_grp %in% blocs & origin != destination,
                .(v = sum(GWP) / ny), by = .(o_grp, d_grp)]
netpos <- merge(intra_eu[, .(out = sum(v)), by = o_grp],
                intra_eu[, .(inn = sum(v)), by = d_grp],
                by.x = "o_grp", by.y = "d_grp")
netpos <- merge(netpos, gradient[, .(o_grp = grp, pba)], by = "o_grp")
netpos[, `:=`(net_Mt = round((out - inn) / 1e9, 1),
              net_pct_own_pba = round(100 * (out - inn) / pba, 1))]

grad <- merge(gradient[, .(development_model = grp,
                           burden_t_pc = NA_real_, gap_pct, int_pba_g_per_eur = int_pba)],
              netpos[, .(development_model = o_grp, net_export_to_EU_Mt = net_Mt,
                         net_pct_of_own_pba = net_pct_own_pba)],
              by = "development_model")
grad[, burden_t_pc := NULL]
setorder(grad, -gap_pct)

# The two weightings DISAGREE and both are reported, because the difference is
# itself informative and quoting only one would misrepresent the result:
#   * bloc AGGREGATE (sum of Gt over members) -- the defensible unit for "how
#     does this development model behave"; gives a binary East/West split.
#   * unweighted COUNTRY MEAN -- "how does a typical member behave"; gives a
#     four-way gradient, but it is driven by small rich states (LU, MT) whose
#     ratios dominate an unweighted mean of a four-country bloc.
cmean <- tot[, .(pba = sum(GWP_pba), cba = sum(GWP_pba - GWP_Exports + GWP_Imports),
                 va = sum(ValueAdded_pba)), by = .(iso3, grp)][
  , .(gap_pct_country_mean = round(100 * mean(cba / pba - 1)),
      int_country_mean = round(mean(pba / va / 1000))), by = grp]
grad <- merge(grad, cmean, by.x = "development_model", by.y = "grp")
setorder(grad, -gap_pct)

cat("\n\n## Production vs consumption, by development model -- BOTH weightings\n\n")
print(kable(grad, format = "pipe"))
cat("\n  gap_pct / int_pba_g_per_eur : bloc aggregates (Gt and value added summed)\n",
    "  *_country_mean              : unweighted mean over member countries\n\n",
    "  On AGGREGATES the result is BINARY, not a gradient: the three western blocs\n",
    "  all consume ~22-27%% more than they produce, while the Workbench consumes\n",
    "  essentially exactly what it produces (0%%). The four-way ordering only\n",
    "  appears under unweighted country means, where Luxembourg and Malta drive\n",
    "  the Finance bloc. Quote the aggregate version; mention the other.\n", sep = "")

# Is it just income? Partial correlation of the country-level gap on the
# development-model dummies, net of log GDP p.c.
ind <- as_tibble(fread(here("data/tidy/taxonomy_indicators.csv")))
cg <- tot[, .(pba = sum(GWP_pba), cba = sum(GWP_pba - GWP_Exports + GWP_Imports)),
          by = .(iso3, grp)][, gap := cba / pba - 1]
cg <- merge(cg, as.data.table(ind)[, .(iso3 = countrycode(country, "country.name", "iso3c"),
                                       lg = log(GDP_normed))], by = "iso3")
m_inc  <- summary(lm(gap ~ lg, data = cg))$r.squared
m_grp  <- summary(lm(gap ~ factor(grp), data = cg))$r.squared
m_both <- summary(lm(gap ~ lg + factor(grp), data = cg))$r.squared
cat(sprintf("\n  R2(gap ~ log GDP p.c.)        = %.2f\n", m_inc))
cat(sprintf("  R2(gap ~ development model)       = %.2f\n", m_grp))
cat(sprintf("  R2(gap ~ both)               = %.2f  -> development model adds %.2f over income alone\n",
            m_both, m_both - m_inc))

fwrite(grad, here("data/tidy/development_model_gradient.csv"))

# --- 4. Figure ---------------------------------------------------------------

ord <- c("Core", "Finance", "Periphery", "Workbench")
pdat <- as.data.table(imp)[d_grp %in% ord]
pdat[, share := 100 * GWP / sum(GWP), by = d_grp]
pdat[, o_grp := factor(o_grp, levels = c(
  "Workbench", "Core", "Finance", "Periphery",
  "China", "Other non-EU", "Rest of world (aggregates)"))]
pdat[, d_grp := factor(d_grp, levels = rev(ord))]

# Sequential-within-group: EU origins in blues, non-EU in warm tones, so the
# intra-EU vs extra-EU split is readable before any single category is.
cols <- c("Workbench" = "#0B3C5D", "Core" = "#2E7CA8", "Finance" = "#6FA8C7",
          "Periphery" = "#A9C9DC", "China" = "#E65032", "Other non-EU" = "#F09372",
          "Rest of world (aggregates)" = "#F7C6B4")

p <- ggplot(pdat, aes(share, d_grp, fill = o_grp)) +
  geom_col(width = 0.68, colour = "white", linewidth = 0.5) +
  scale_fill_manual(NULL, values = cols) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.01)),
                     labels = function(x) paste0(x, "%")) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(x = NULL, y = NULL,
       title = "Where each bloc's embodied emissions actually come from",
       subtitle = paste0("Share of embodied GHG imports by origin, EU-27 development models, ",
                         REF_FIRST_YEAR, "-", REF_LAST_YEAR,
                         " mean.\nThe intra-EU transfer is the minority of every bloc's footprint.")) +
  theme_minimal(base_size = 9) +
  theme(legend.position = "top", legend.justification = "left",
        legend.key.size = unit(0.4, "cm"),
        panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.3),
        axis.text.y = element_text(face = "bold", colour = "#1A1A1A"),
        plot.title = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(colour = "grey35", size = 8, lineheight = 1.15))

ggsave(here("plots/offshoring_origins.png"), p, width = 7.2, height = 4.6, dpi = 300)
ggsave(here("plots/offshoring_origins.pdf"), p, width = 7.2, height = 4.6)

message("\nappendix_offshoring_origins.R done.")
