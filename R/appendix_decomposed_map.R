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

score_of <- function(vars) {
  v <- block_score(ind, vars, "ShareFossils_normed")
  p <- block_score(ind, POT_VARS, "GCI")
  tibble(country = ind$country, group = ind$group,
         vulnerability = v$score, potential = p$score,
         quadrant = assign_quadrant(v$score, p$score, "short"))
}
head_s <- score_of(VULN_VARS)     # current headline (demand-side fossil share)
dec_s  <- score_of(DECOMP_VARS)   # decomposed vulnerability

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
       colour = "Growth model") +
  theme_minimal() + theme(legend.position = "bottom")

ggsave(here("plots/appendix_decomposed_map.pdf"), p, width = 13, height = 7)
ggsave(here("plots/appendix_decomposed_map.png"), p, width = 13, height = 7, dpi = 150)
message("appendix_decomposed_map.R done: wrote plots/appendix_decomposed_map.{pdf,png} ",
        "and data/tidy/appendix_decomposed_reclass.csv")
