# 04 - Vulnerability x Potential typology (Phase 3 + 4).
#
# Mirrors Fig. 1 of the proposal: two vulnerability dimensions and two potential
# dimensions. Each block is reduced to one PC1 score via PCA; countries are then
# placed on a 2-D vulnerability x potential map with four quadrants. Includes the
# Phase-3 go/no-go check: are the two scores distinct from each other and not
# merely proxies for GDP per capita?

here::i_am("R/04_typology.R")
library(here)
library(data.table)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(countrycode)
source(here("R/config.R"))
source(here("R/country_classification.R"))
source(here("R/functions/typology.R"))

ind <- as_tibble(fread(here("data/tidy/taxonomy_indicators.csv")))
ind$group <- get_country_classification(
  countrycode::countrycode(ind$country, "country.name", "iso3c"), "jee")

# Blocks are defined in config.R (VULN_VARS, POT_VARS) and map to Fig. 1.
vuln <- block_score(ind, VULN_VARS, anchor = "ShareFossils_normed")
pot  <- block_score(ind, POT_VARS,  anchor = "GCI")

cat("\n--- Vulnerability block: PC1 loadings (var explained = ",
    sprintf("%.0f%%)", 100 * vuln$var_explained), "\n", sep = "")
print(round(vuln$loadings, 3))
cat("\n--- Potential block: PC1 loadings (var explained = ",
    sprintf("%.0f%%)", 100 * pot$var_explained), "\n", sep = "")
print(round(pot$loadings, 3))

scores <- ind |>
  transmute(country, group,
            vulnerability = vuln$score,
            potential     = pot$score)

# --- Phase 3 GO/NO-GO check ---------------------------------------------------
loggdp <- log(ind$GDP_normed)
r2 <- function(y) summary(lm(y ~ loggdp))$r.squared
cat("\n================ GO/NO-GO CHECK ================\n")
cat(sprintf("cor(vulnerability, potential)      = %+.2f  (want: not near +/-1)\n",
            cor(scores$vulnerability, scores$potential)))
cat(sprintf("R^2  vulnerability ~ log GDP p.c.  = %.2f\n", r2(scores$vulnerability)))
cat(sprintf("R^2  potential     ~ log GDP p.c.  = %.2f\n", r2(scores$potential)))
cat("Interpretation: if both R^2 are high AND |cor| ~ 1, the taxonomy is just\n",
    "income. Distinct, income-independent scores => the reframe adds signal.\n", sep = "")

# --- 2-D typology map ---------------------------------------------------------
mx <- median(scores$vulnerability); my <- median(scores$potential)
scores <- scores |>
  mutate(quadrant = case_when(
    vulnerability <  mx & potential >= my ~ "Winners (low vuln / high pot)",
    vulnerability >= mx & potential >= my ~ "Exposed but capable",
    vulnerability <  mx & potential <  my ~ "Low-stakes / low capability",
    TRUE                                  ~ "At risk (high vuln / low pot)"
  ))
fwrite(scores, here("data/tidy/taxonomy_scores.csv"))
cat("\n--- Quadrant membership ---\n")
scores |> arrange(quadrant, country) |>
  group_by(quadrant) |> summarise(countries = paste(country, collapse = ", "), .groups = "drop") |>
  as.data.frame() |> print(right = FALSE)

p_map <- ggplot(scores, aes(vulnerability, potential, colour = group)) +
  geom_hline(yintercept = my, linetype = 2, colour = "grey60") +
  geom_vline(xintercept = mx, linetype = 2, colour = "grey60") +
  geom_point(size = 2) +
  ggrepel::geom_text_repel(aes(label = country), size = 3, max.overlaps = 20) +
  labs(title = "EU-27 green-transition typology",
       x = "Vulnerability  (transition burden)  →",
       y = "Potential  (green capability)  →",
       colour = "Growth model") +
  theme_minimal()
ggsave(here("plots/typology_map.pdf"), p_map, width = 9, height = 7)
message("04_typology.R done: wrote taxonomy_scores.csv and plots/typology_map.pdf")
