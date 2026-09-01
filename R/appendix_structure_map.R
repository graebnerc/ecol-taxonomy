# Appendix - old flat blocks vs the four-dimension (two-part) structure.
#
# Compares the PREVIOUS headline (single PCA over all three variables per block,
# which over-weights the correlated twins and near-ignores the standalone) with
# the NEW structure (twin sub-index + standalone, equal weight; config.R). Prints
# the reclassification table and renders both maps side by side.
# See info/PaperTodos.md - "Symmetric four-dimension structure".

here::i_am("R/appendix_structure_map.R")
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

# OLD: flat single-PCA per block (all three variables together)
flat <- tibble(country = ind$country, group = ind$group,
               vulnerability = block_score(ind, VULN_VARS, "ShareFossils_normed")$score,
               potential     = block_score(ind, POT_VARS,  "GCI")$score) |>
  mutate(quadrant = assign_quadrant(vulnerability, potential, "short"))

# NEW: four-dimension structure (twin sub-index + standalone, equal weight)
vuln <- axis_score(ind, INTENSITY_VARS,  "CarbonIntensity_normed", FOSSIL_VAR)
pot  <- axis_score(ind, COMPLEXITY_VARS, "GCI",                    INNOV_VAR)
new  <- tibble(country = ind$country, group = ind$group,
               vulnerability = vuln$score, potential = pot$score) |>
  mutate(quadrant = assign_quadrant(vulnerability, potential, "short"))

# --- Reclassification overview -----------------------------------------------
cmp <- flat |>
  transmute(country, group, flat = quadrant) |>
  left_join(new |> transmute(country, structured = quadrant), by = "country") |>
  mutate(moved = ifelse(flat == structured, "", "  <-- moved")) |>
  arrange(moved == "", group, country)

cat(sprintf("\n===== Reclassification: flat PCA vs four-dimension structure (%d/%d move) =====\n",
            sum(cmp$moved != ""), nrow(cmp)))
print(as.data.frame(cmp), row.names = FALSE, right = FALSE)
cat("\n--- Quadrant totals ---\n")
print(cbind(flat = table(flat$quadrant), structured = table(new$quadrant)))
fwrite(cmp, here("data/tidy/appendix_structure_reclass.csv"))

# --- Side-by-side maps --------------------------------------------------------
lab <- c(flat = "Flat  (single PCA per block)",
         new  = "Structured  (intensity+fossil / complexity+patents)")
plot_df <- bind_rows(mutate(flat, spec = lab["flat"]),
                     mutate(new,  spec = lab["new"])) |>
  mutate(spec = factor(spec, levels = lab),
         moved = country %in% cmp$country[cmp$moved != ""])
med <- plot_df |> group_by(spec) |>
  summarise(mx = median(vulnerability), my = median(potential), .groups = "drop")

p <- ggplot(plot_df, aes(vulnerability, potential)) +
  geom_hline(data = med, aes(yintercept = my), linetype = 2, colour = "grey70") +
  geom_vline(data = med, aes(xintercept = mx), linetype = 2, colour = "grey70") +
  geom_point(aes(colour = group, shape = moved), size = 2.4) +
  ggrepel::geom_text_repel(
    aes(label = country, fontface = ifelse(moved, "bold", "plain"), colour = group),
    size = 2.7, max.overlaps = 30, show.legend = FALSE) +
  facet_wrap(~spec) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 17),
                     labels = c("same quadrant", "re-classified"), name = NULL) +
  labs(title = "EU-27 typology: flat blocks vs four-dimension structure",
       subtitle = sprintf("%d of 27 countries change quadrant (triangles = movers)",
                          sum(cmp$moved != "")),
       x = "Vulnerability  (transition burden)  ->",
       y = "Potential  (green capability)  ->",
       colour = "Growth model") +
  theme_minimal() + theme(legend.position = "bottom")

ggsave(here("plots/appendix_structure_map.pdf"), p, width = 13, height = 7)
ggsave(here("plots/appendix_structure_map.png"), p, width = 13, height = 7, dpi = 150)
message("appendix_structure_map.R done: wrote plots/appendix_structure_map.{pdf,png} ",
        "and data/tidy/appendix_structure_reclass.csv")
