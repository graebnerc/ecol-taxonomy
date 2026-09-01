# Appendix - country classification table + descriptive profile of each quadrant.
#
# Two deliverables, both built from the frozen headline output so they cannot
# drift from 04_typology.R:
#   (1) the full EU-27 classification (growth model, both axes, the four part
#       scores, quadrant, borderline flag) -> data/tidy/quadrant_classification.csv
#   (2) what a "typical" member of each quadrant looks like on the underlying
#       indicators, in interpretable units  -> data/tidy/quadrant_profiles.csv
#       + the small-multiples panel          -> plots/quadrant_profiles.{png,pdf}
#
# Quadrant identity is carried by the y-axis position, not by colour, so the
# panel needs no categorical palette: dots are countries, the diamond is the
# quadrant mean.

here::i_am("R/appendix_quadrant_profiles.R")
library(here)
suppressMessages({
  library(data.table); library(dplyr); library(tidyr)
  library(ggplot2); library(knitr)
})
source(here("R/config.R"))

DOT  <- "#2E7CA8"   # country observations (deepened EUF steel)
MEAN <- "#E65032"   # quadrant mean (EUF orange); validated pair, all checks pass
INK  <- "#1A1A1A"

scores <- as_tibble(fread(here("data/tidy/taxonomy_scores.csv")))
ind    <- as_tibble(fread(here("data/tidy/taxonomy_indicators.csv")))

# Short quadrant labels, ordered along the story: best-placed -> worst-placed.
Q_LEVELS <- c("Winners", "Exposed", "Low-stakes", "At risk")
short_q <- function(x) sub(" \\(.*$", "", sub("Exposed but capable", "Exposed",
             sub("Low-stakes / low capability", "Low-stakes", x)))

dat <- scores |>
  mutate(quadrant = factor(short_q(quadrant), levels = Q_LEVELS)) |>
  left_join(ind, by = "country")

# --- (1) classification table -----------------------------------------------

classification <- dat |>
  transmute(
    country, growth_model = group, quadrant,
    vulnerability = round(vulnerability, 2), potential = round(potential, 2),
    intensity = round(intensity, 2), fossil = round(fossil, 2),
    complexity = round(complexity, 2), innovation = round(innovation, 2),
    borderline = boundary) |>
  arrange(quadrant, desc(potential))

fwrite(classification, here("data/tidy/quadrant_classification.csv"))

# --- (2) quadrant descriptives ----------------------------------------------

# Underlying indicators in interpretable units. GWP is in kg CO2e, value added
# and GDP in M EUR/USD, final energy in GWh (see R/functions/indicators.R), so:
#   kg / M EUR  = g per EUR ; GWh / M EUR = kWh per EUR.
prof_vars <- c(
  "Carbon intensity\n(g CO2e per EUR value added)"    = "carbon_int",
  "Energy intensity\n(kWh per EUR value added)"       = "energy_int",
  "Fossil share of gross\navailable energy (%)"       = "fossil_share",
  "Green complexity index\n(GCI, global z)"           = "gci",
  "Green complexity potential\n(GCP, global z)"       = "gcp",
  "Green patents\n(per million inhabitants)"          = "patents_pc",
  "GDP per capita\n(PPP, 1000 USD)"                   = "gdp_pc",
  "Net embodied GHG imports\n(t CO2e per capita)"     = "net_emb_imports")

prof <- dat |>
  transmute(
    country, quadrant, growth_model = group,
    carbon_int      = CarbonIntensity_normed / 1000,
    energy_int      = EnergyIntensity_normed,
    fossil_share    = ShareFossils_normed * 100,
    gci             = GCI,
    gcp             = GCP,
    patents_pc      = GreenPatents_normed,
    gdp_pc          = GDP_normed / 1000,
    net_emb_imports = GWP_trade_normed / 1000)

profiles <- prof |>
  group_by(quadrant) |>
  summarise(n = n(),
            across(all_of(unname(prof_vars)), \(x) round(mean(x), 2)),
            .groups = "drop")

# Axis scores per quadrant, and growth-model composition, alongside the levels.
axis_means <- dat |>
  group_by(quadrant) |>
  summarise(across(c(vulnerability, potential, intensity, fossil,
                     complexity, innovation), \(x) round(mean(x), 2)),
            growth_models = paste(sort(unique(as.character(group))), collapse = ", "),
            .groups = "drop")

profiles <- left_join(profiles, axis_means, by = "quadrant")
fwrite(profiles, here("data/tidy/quadrant_profiles.csv"))

# --- (3) descriptive panel ---------------------------------------------------

long <- prof |>
  pivot_longer(all_of(unname(prof_vars)), names_to = "var", values_to = "value") |>
  mutate(var = factor(var, levels = unname(prof_vars),
                      labels = names(prof_vars)),
         quadrant = factor(quadrant, levels = rev(Q_LEVELS)))

means <- long |>
  group_by(var, quadrant) |>
  summarise(value = mean(value), .groups = "drop")

set.seed(42)
p <- ggplot(long, aes(value, quadrant)) +
  geom_point(position = position_jitter(height = 0.16, width = 0),
             colour = DOT, size = 1.9, alpha = 0.75) +
  geom_point(data = means, shape = 23, size = 3.1, stroke = 0.9,
             fill = MEAN, colour = "white") +
  geom_text(data = means, aes(label = round(value, 1)), vjust = -1.25,
            size = 2.5, colour = INK) +
  facet_wrap(~var, scales = "free_x", ncol = 2) +
  scale_y_discrete(expand = expansion(add = c(0.55, 0.8))) +
  scale_x_continuous(expand = expansion(mult = 0.08)) +
  labs(x = NULL, y = NULL,
       title = "What a typical member of each quadrant looks like",
       subtitle = paste("EU-27, 2014-2018 reference window. Dots = countries;",
                        "diamond = quadrant mean.")) +
  theme_minimal(base_size = 9) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.3),
        strip.text = element_text(size = 7.6, lineheight = 1.05, hjust = 0),
        axis.text.y = element_text(colour = INK, size = 8),
        plot.title = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(colour = "grey35", size = 8),
        panel.spacing.x = unit(1.1, "lines"),
        panel.spacing.y = unit(0.9, "lines"))

ggsave(here("plots/quadrant_profiles.png"), p, width = 8.4, height = 9.6, dpi = 300)
ggsave(here("plots/quadrant_profiles.pdf"), p, width = 8.4, height = 9.6)

# --- (4) console tables ------------------------------------------------------

cat("\n## Country classification (EU-27)\n\n")
print(kable(classification, format = "pipe"))

cat("\n\n## Quadrant profiles - mean of each indicator\n\n")
print(kable(profiles |> select(quadrant, n, growth_models,
                               all_of(unname(prof_vars))), format = "pipe"))

cat("\n\n## Quadrant profiles - mean axis and part scores (z)\n\n")
print(kable(axis_means, format = "pipe"))

cat("\n\n## Quadrant x growth model\n\n")
print(kable(as.data.frame.matrix(table(Quadrant = dat$quadrant, dat$group)),
            format = "pipe"))
cat("\n")
