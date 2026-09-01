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

# --- 1. Where does each EU growth model's embodied import come from? ---------

tab <- dcast(imp, d_grp ~ o_grp, value.var = "GWP", fill = 0)
mat <- as.matrix(tab[, -1]); rownames(mat) <- tab$d_grp
shares <- round(100 * mat / rowSums(mat), 1)

cat("\n## Embodied GHG imports of each EU growth model, by ORIGIN (% of its imports)\n")
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
       subtitle = paste0("Share of embodied GHG imports by origin, EU-27 growth models, ",
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
