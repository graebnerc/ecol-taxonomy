# Appendix - the burden vs. responsibility layer (offshoring as a finding).
#
# The headline vulnerability axis is PRODUCTION-based on purpose: it measures the
# domestic adjustment burden - the plants, workers and energy system a country
# must itself retool. Consumption-based accounting measures something different:
# whose final demand the emissions serve, i.e. responsibility. This script
# quantifies the gap between the two and shows that it is systematic along the
# core-periphery axis.
#
# The point is that the two facts are compatible, because one is an INTENSITY
# (per unit of value added) and one is a LEVEL (per capita):
#   - the vulnerability RANKING is near-invariant to the accounting choice
#     (the "carbon: consumption-based (CBA)" spec in 07_robustness.R), so the
#     map is not an artifact of production-based accounting; and yet
#   - the per-capita FOOTPRINT gap is large and runs the other way: the
#     low-burden countries carry the high consumption responsibility.
#
# Net embodied imports p.c. is deliberately NOT promoted to an axis variable: it
# is a per-capita level and correlates ~+0.7 with log GDP p.c., so it would
# reimport exactly the income confound the per-value-added intensities remove
# (the same argument that kept fossil consumption p.c. out). It is an
# interpretive layer over the map, not a fifth dimension.
#
# CAVEAT (limits the claim): data/tidy/TXNY_GWP_Trade.csv carries only country
# TOTALS of embodied imports and exports - there is no origin dimension. So this
# establishes an asymmetry between two accountings; it does NOT establish that
# the core's embodied imports come from the European East rather than from
# outside the EU. That would need bilateral EXIOBASE flows.
#
# Writes data/tidy/burden_responsibility.csv and
# plots/burden_responsibility.{png,pdf}.

here::i_am("R/appendix_burden_responsibility.R")
library(here)
suppressMessages({
  library(data.table); library(dplyr); library(tidyr)
  library(ggplot2); library(countrycode); library(knitr)
})
source(here("R/config.R"))
source(here("R/country_classification.R"))

PBA_COL <- "#2E7CA8"   # production = burden
CBA_COL <- "#E65032"   # consumption = responsibility  (validated pair)
INK     <- "#1A1A1A"

ind    <- as_tibble(fread(here("data/tidy/taxonomy_indicators.csv")))
scores <- as_tibble(fread(here("data/tidy/taxonomy_scores.csv")))

Q_LEVELS <- c("Winners", "Exposed", "Low-stakes", "At risk")
short_q <- function(x) sub(" \\(.*$", "", sub("Exposed but capable", "Exposed",
             sub("Low-stakes / low capability", "Low-stakes", x)))

dat <- ind |>
  left_join(scores |> transmute(country, group, quadrant = short_q(quadrant),
                                vulnerability, potential), by = "country") |>
  mutate(
    quadrant   = factor(quadrant, levels = Q_LEVELS),
    burden_pc  = GWP_normed / 1000,             # production-based GHG, t CO2e p.c.
    resp_pc    = GWP_cba_normed / 1000,         # consumption-based GHG, t CO2e p.c.
    net_imp_pc = GWP_trade_normed / 1000,       # net embodied imports, t CO2e p.c.
    offshoring = resp_pc / burden_pc,           # >1 = consumes more than it produces
    int_pba    = CarbonIntensity_normed / 1000, # g CO2e per EUR value added
    int_cba    = CarbonIntensityCBA_normed / 1000)

fwrite(dat |> transmute(country, group, quadrant,
                        burden_pc = round(burden_pc, 2),
                        resp_pc = round(resp_pc, 2),
                        net_imp_pc = round(net_imp_pc, 2),
                        offshoring = round(offshoring, 2),
                        int_pba = round(int_pba), int_cba = round(int_cba)) |>
         arrange(quadrant, desc(offshoring)),
       here("data/tidy/burden_responsibility.csv"))

# --- The asymmetry, by quadrant and by development model --------------------------

summarise_by <- function(df, key) {
  df |> group_by(across(all_of(key))) |>
    summarise(n = n(),
              burden_t_pc = round(mean(burden_pc), 1),
              resp_t_pc   = round(mean(resp_pc), 1),
              gap_pct     = round(100 * (mean(resp_pc) / mean(burden_pc) - 1)),
              int_pba     = round(mean(int_pba)),
              int_cba     = round(mean(int_cba)),
              int_rise_pct = round(100 * (mean(int_cba) / mean(int_pba) - 1)),
              .groups = "drop")
}

by_quad  <- summarise_by(dat, "quadrant")
by_group <- summarise_by(dat, "group")

cat("\n## Burden vs responsibility, by quadrant\n\n")
print(kable(by_quad, format = "pipe"))
cat("\n\n## Burden vs responsibility, by development model\n\n")
print(kable(by_group, format = "pipe"))

cat("\n\n## The asymmetry in one line\n\n")
lg <- log(dat$GDP_normed)
cat(sprintf("  cor(net embodied imports p.c., log GDP p.c.) = %+.2f",
            cor(dat$net_imp_pc, lg)))
cat("   <- why it stays OUT of the axis (a per-capita level, tracks income)\n")
cat(sprintf("  cor(production intensity, consumption intensity) = %.3f (Spearman %.3f)\n",
            cor(dat$int_pba, dat$int_cba),
            cor(dat$int_pba, dat$int_cba, method = "spearman")))
cat("   <- why the vulnerability RANKING barely moves (see 07 spec 'carbon: consumption-based (CBA)')\n")
w <- dat$quadrant == "Winners"; r <- dat$quadrant == "At risk"
cat(sprintf("  Winners vs At risk, intensity gap: %.1fx production-based -> %.1fx consumption-based\n",
            mean(dat$int_pba[r]) / mean(dat$int_pba[w]),
            mean(dat$int_cba[r]) / mean(dat$int_cba[w])))
cat(sprintf("  Winners vs At risk, per capita:    %.1f vs %.1f t production-based -> %.1f vs %.1f t consumption-based\n",
            mean(dat$burden_pc[w]), mean(dat$burden_pc[r]),
            mean(dat$resp_pc[w]),   mean(dat$resp_pc[r])))

cat("\n\n## Largest offshorers (consumption / production footprint)\n\n")
print(kable(dat |> arrange(desc(offshoring)) |>
              transmute(country, group, quadrant,
                        burden_t_pc = round(burden_pc, 1),
                        resp_t_pc = round(resp_pc, 1),
                        ratio = round(offshoring, 2)) |> head(6),
            format = "pipe"))

# --- Figure: paired production -> consumption footprint, by quadrant ---------

plt <- dat |>
  mutate(country = reorder(country, resp_pc)) |>
  select(country, quadrant, burden_pc, resp_pc)

p <- ggplot(plt, aes(y = country)) +
  geom_segment(aes(x = burden_pc, xend = resp_pc, yend = country),
               colour = "grey65", linewidth = 0.5) +
  geom_point(aes(x = burden_pc, colour = "Production (burden)"), size = 2.3) +
  geom_point(aes(x = resp_pc, colour = "Consumption (responsibility)"), size = 2.3) +
  facet_grid(quadrant ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_colour_manual(NULL,
    values = c("Production (burden)" = PBA_COL,
               "Consumption (responsibility)" = CBA_COL),
    breaks = c("Production (burden)", "Consumption (responsibility)")) +
  scale_x_continuous(expand = expansion(mult = 0.05)) +
  labs(x = "GHG emissions, t CO2e per capita (2014-2018 mean)", y = NULL,
       title = "Burden and responsibility diverge, and the gap follows the map",
       subtitle = paste("Production-based emissions are what a country must retool;",
                        "consumption-based are what its demand causes.\nWinners carry the",
                        "smaller burden and the larger responsibility.")) +
  theme_minimal(base_size = 9) +
  theme(legend.position = "top",
        legend.justification = "left",
        legend.margin = margin(b = -4),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.3),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0, hjust = 0, face = "bold",
                                         size = 8, colour = INK),
        axis.text.y = element_text(size = 7.6, colour = INK),
        plot.title = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(colour = "grey35", size = 8, lineheight = 1.15),
        panel.spacing.y = unit(0.5, "lines"))

ggsave(here("plots/burden_responsibility.png"), p, width = 7.4, height = 7.2, dpi = 300)
ggsave(here("plots/burden_responsibility.pdf"), p, width = 7.4, height = 7.2)

message("\nappendix_burden_responsibility.R done: wrote data/tidy/burden_responsibility.csv ",
        "and plots/burden_responsibility.{png,pdf}")
