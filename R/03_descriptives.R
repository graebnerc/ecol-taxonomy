# 03 - Descriptive analysis (Phase 2).
# Reads data/tidy/taxonomy_indicators.csv and produces:
#   * a correlation matrix of all indicators (figure + csv)
#   * how much each indicator is driven by income (R^2 of indicator ~ log GDP p.c.)
#   * ranked bar charts per indicator, coloured by development-model group
# This is a WP1 deliverable in its own right and motivates the block structure.

here::i_am("R/03_descriptives.R")
library(here)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(countrycode)
suppressMessages(library(ggpubr))
source(here("R/config.R"))
source(here("R/country_classification.R"))

ind <- as_tibble(fread(here("data/tidy/taxonomy_indicators.csv")))
ind$group <- get_country_classification(
  countrycode::countrycode(ind$country, "country.name", "iso3c"), "jee")

vars <- setdiff(names(ind), c("country", "group"))

# --- Correlation matrix -------------------------------------------------------
cormat <- cor(ind[, vars], use = "pairwise.complete.obs")
fwrite(as.data.frame(cormat), here("data/tidy/indicator_correlations.csv"), row.names = TRUE)

cor_long <- as.data.frame(as.table(cormat))
p_cor <- ggplot(cor_long, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", Freq)), size = 2.4) +
  scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#2166ac",
                       midpoint = 0, limits = c(-1, 1), name = "r") +
  labs(title = "Indicator correlations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank())
ggsave(here("plots/descriptives_correlations.pdf"), p_cor, width = 9, height = 8)

# --- Income-drivenness: R^2 of each indicator on log GDP per capita -----------
loggdp <- log(ind$GDP_normed)
income_r2 <- tibble(
  indicator = setdiff(vars, "GDP_normed"),
  r2_vs_logGDPpc = map_dbl(setdiff(vars, "GDP_normed"),
                           \(v) summary(lm(ind[[v]] ~ loggdp))$r.squared)
) |> arrange(desc(r2_vs_logGDPpc))
fwrite(income_r2, here("data/tidy/indicator_income_r2.csv"))
cat("\n--- How much each indicator is explained by log GDP per capita ---\n")
print(as.data.frame(income_r2), row.names = FALSE, digits = 3)

# --- Ranked bar charts --------------------------------------------------------
make_bar <- function(v) {
  ggplot(ind, aes(x = reorder(country, .data[[v]]), y = .data[[v]], fill = group)) +
    geom_col() + coord_flip() +
    labs(title = v, x = NULL, y = NULL) +
    theme_minimal(base_size = 7) +
    theme(legend.position = "none", plot.title = element_text(size = 8))
}
bars <- map(vars, make_bar)
ggsave(here("plots/descriptives_rankings.pdf"),
       ggarrange(plotlist = bars, ncol = 3, nrow = 5, common.legend = TRUE, legend = "bottom"),
       width = 12, height = 16)

message("03_descriptives.R done: wrote correlations (fig+csv), income R^2, rankings fig.")
