# Appendix - decision support for the decomposed-vulnerability spec.
#
# Side-by-side of the current HEADLINE typology (VULN_VARS, demand-side fossil
# share) against the DECOMPOSED vulnerability spec {GHG/energy, energy/VA,
# fossil_gross}. Prints the full reclassification table and renders both maps in
# one figure so the 18/27 quadrant changes can be judged for face validity.
# See info/PaperTodos.md items 1/3 and the "Fossil-share measure" section.

here::i_am("R/appendix_decomposed_map.R")
library(here)
suppressMessages({
  library(data.table); library(dplyr); library(tidyr); library(ggplot2)
  library(ggrepel); library(countrycode)
})
source(here("R/config.R"))
source(here("R/country_classification.R"))
source(here("R/functions/typology.R"))

ind <- as_tibble(fread(here("data/tidy/taxonomy_indicators.csv")))
ind$group <- get_country_classification(
  countrycode(ind$country, "country.name", "iso3c"), "jee")

DECOMP_VARS <- c("CarbonPerEnergy_normed", "EnergyIntensity_normed", "ShareFossils_normed")

# DEFECT FIXED 2026-09-03. This script predated the four-dimension restructure and
# still scored BOTH specs with block_score() -- a flat single PCA over all three
# block variables. That is no longer the headline: 04_typology.R uses axis_score()
# (twin sub-index + standalone at equal weight). The comparison was therefore
# against the wrong baseline, and reported a "headline" map of 4/10/9/4 against
# the real 11/3/2/11, with Austria and Finland shown as Exposed rather than
# Winners. Any reclassification count taken from it was wrong.
#
# The headline now uses axis_score. The decomposed spec keeps a flat PCA, because
# its whole point is that the three vulnerability variables are treated as one
# undifferentiated block -- that IS the specification being tested.
headline_score <- function() {
  v <- axis_score(ind, INTENSITY_VARS, "CarbonIntensity_normed", FOSSIL_VAR)
  p <- axis_score(ind, COMPLEXITY_VARS, "GCI", INNOV_VAR)
  tibble(country = ind$country, group = ind$group,
         vulnerability = v$score, potential = p$score,
         quadrant = assign_quadrant(v$score, p$score, "short"))
}
decomposed_score <- function(vars) {
  v <- block_score(ind, vars, "ShareFossils_normed")
  p <- axis_score(ind, COMPLEXITY_VARS, "GCI", INNOV_VAR)   # potential unchanged
  tibble(country = ind$country, group = ind$group,
         vulnerability = v$score, potential = p$score,
         quadrant = assign_quadrant(v$score, p$score, "short"))
}
head_s <- headline_score()
dec_s  <- decomposed_score(DECOMP_VARS)

# Guard: the headline computed here must match 04_typology.R exactly.
.hl <- fread(here("data/tidy/taxonomy_scores.csv"))
.hl$q <- sub(" \\(.*$", "", sub("Exposed but capable", "Exposed",
          sub("Low-stakes / low capability", "Low-stakes", .hl$quadrant)))
.chk <- merge(head_s[, c("country", "quadrant")], .hl[, c("country", "q")], by = "country")
stopifnot("this script's headline does not match 04_typology.R" =
            all(.chk$quadrant == .chk$q))

# --- Reclassification overview -----------------------------------------------
cmp <- head_s |>
  transmute(country, group, headline = quadrant) |>
  left_join(dec_s |> transmute(country, decomposed = quadrant), by = "country") |>
  mutate(moved = ifelse(headline == decomposed, "", "  <-- moved")) |>
  arrange(moved == "", group, country)   # movers first

cat(sprintf("\n===== Reclassification: headline vs decomposed (%d/%d move) =====\n",
            sum(cmp$moved != ""), nrow(cmp)))
print(as.data.frame(cmp), row.names = FALSE, right = FALSE)

cat("\n--- Quadrant totals ---\n")
print(cbind(headline   = table(head_s$quadrant),
            decomposed = table(dec_s$quadrant)))
fwrite(cmp, here("data/tidy/appendix_decomposed_reclass.csv"))

# --- Side-by-side maps --------------------------------------------------------
lab_spec <- c(headline   = "Headline  (demand-side fossil share)",
              decomposed = "Decomposed  (GHG/energy + energy/VA + fossil)")
plot_df <- bind_rows(
  mutate(head_s, spec = lab_spec["headline"]),
  mutate(dec_s,  spec = lab_spec["decomposed"])
) |>
  mutate(spec = factor(spec, levels = lab_spec),
         moved = country %in% cmp$country[cmp$moved != ""])

med <- plot_df |> group_by(spec) |>
  summarise(mx = median(vulnerability), my = median(potential), .groups = "drop")

p <- ggplot(plot_df, aes(vulnerability, potential)) +
  geom_hline(data = med, aes(yintercept = my), linetype = 2, colour = "grey70") +
  geom_vline(data = med, aes(xintercept = mx), linetype = 2, colour = "grey70") +
  geom_point(aes(colour = group, shape = moved), size = 2.4) +
  ggrepel::geom_text_repel(
    aes(label = country, fontface = ifelse(moved, "bold", "plain"),
        colour = group), size = 2.7, max.overlaps = 30, show.legend = FALSE) +
  facet_wrap(~spec) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 17),
                     labels = c("same quadrant", "re-classified"), name = NULL) +
  labs(title = "EU-27 green-transition typology: headline vs decomposed vulnerability",
       subtitle = sprintf("%d of 27 countries change quadrant (triangles = movers)",
                          sum(cmp$moved != "")),
       x = "Vulnerability  (transition burden)  ->",
       y = "Potential  (green capability)  ->",
       colour = "Development model") +
  theme_minimal() + theme(legend.position = "bottom")

ggsave(here("plots/appendix_decomposed_map.pdf"), p, width = 13, height = 7)
ggsave(here("plots/appendix_decomposed_map.png"), p, width = 13, height = 7, dpi = 150)
message("appendix_decomposed_map.R done: wrote plots/appendix_decomposed_map.{pdf,png} ",
        "and data/tidy/appendix_decomposed_reclass.csv")
