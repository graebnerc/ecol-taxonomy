# EXPLORATORY (not part of the audited 01-07 pipeline) -- for co-author discussion.
# Forward / out-of-sample validation: freeze the 2014-2018 typology, then ask
# whether a country's baseline position predicts what it did 2018-2023 on outcomes
# that never entered the scores (renewable share, real GDP). Turns the "window ends
# 2018" limit into a test: is the baseline map a predictor of subsequent dynamics?
# Writes plots/forward_validation.{png,pdf} and data/tidy/forward_validation.csv.

suppressMessages({
  library(here); library(data.table); library(dplyr)
  library(ggplot2); library(ggrepel); library(countrycode)
})
here::i_am("R/appendix_forward_validation.R")

# EUF palette
kobalt <- "#00395B"; steel <- "#69AACD"; green <- "#5FB46E"
orange <- "#E65032"; gray  <- "#6F6F6F"
grp_cols <- c(Core = orange, Finance = green, Periphery = steel, Workbench = "#B98BD9")

BASE <- 2018L; LAST <- 2023L   # baseline window ends 2018; latest solid actual = 2023

sc  <- fread(here("data/tidy/taxonomy_scores.csv"))          # frozen baseline scores
ind <- fread(here("data/tidy/taxonomy_indicators.csv"))      # for income control (GDP_normed)
nd  <- fread(here("data/tidy/new_data.csv"))                 # post-2018 outcomes

sc[, iso3 := countrycode(country, "country.name", "iso3c")]

wide <- function(y) nd[year == y, .(iso3 = iso3c, renew = renew_share_overall, gdp = GDP_real)]
b <- wide(BASE); l <- wide(LAST)
out <- merge(b, l, by = "iso3", suffixes = c("_b", "_l"))
out[, `:=`(d_renew    = renew_l - renew_b,               # pp change in renewable share
           gdp_growth = 100 * (gdp_l / gdp_b - 1))]      # % real GDP growth 2018->2023

df <- sc |>
  left_join(ind[, .(country, GDP_normed)], by = "country") |>
  left_join(out[, .(iso3, d_renew, gdp_growth)], by = "iso3") |>
  mutate(loggdp = log(GDP_normed))

# partial correlation of x, y controlling for z ------------------------------
pcor <- function(x, y, z) cor(resid(lm(x ~ z)), resid(lm(y ~ z)))

stat <- function(pred, outc, lab) {
  x <- df[[pred]]; y <- df[[outc]]
  data.table(predictor = pred, outcome = outc, label = lab,
             raw_cor = round(cor(x, y), 2),
             partial_cor_net_gdp = round(pcor(x, y, df$loggdp), 2))
}
res <- rbindlist(list(
  stat("potential",     "d_renew",    "potential -> renewable progress"),
  stat("vulnerability", "d_renew",    "vulnerability -> renewable progress"),
  stat("potential",     "gdp_growth", "potential -> GDP growth"),
  stat("vulnerability", "gdp_growth", "vulnerability -> GDP growth")
))
fwrite(res, here("data/tidy/forward_validation.csv"))
cat("Forward validation (baseline 2014-2018 -> outcomes 2018-2023):\n")
print(res)

# headline figure: baseline potential -> two out-of-sample outcomes -----------
lv  <- c("Δ renewable share 2018–2023 (pp)", "Real GDP growth 2018–2023 (%)")
long <- rbind(
  data.table(iso3 = df$iso3, group = df$group, potential = df$potential,
             outcome = lv[1], val = df$d_renew),
  data.table(iso3 = df$iso3, group = df$group, potential = df$potential,
             outcome = lv[2], val = df$gdp_growth)
)
long[, outcome := factor(outcome, levels = lv)]

rr <- res[predictor == "potential"]
ann <- data.table(
  outcome = factor(lv, levels = lv),
  lab = c(sprintf("r = %.2f   (partial %.2f)  → no signal",
                  rr[outcome == "d_renew"]$raw_cor, rr[outcome == "d_renew"]$partial_cor_net_gdp),
          sprintf("r = %.2f   (partial %.2f)  → convergence",
                  rr[outcome == "gdp_growth"]$raw_cor, rr[outcome == "gdp_growth"]$partial_cor_net_gdp))
)

p <- ggplot(long, aes(potential, val)) +
  geom_smooth(method = "lm", se = TRUE, colour = kobalt, fill = steel, alpha = 0.15, linewidth = 0.7) +
  geom_point(aes(colour = group), size = 2.4) +
  ggrepel::geom_text_repel(aes(label = iso3, colour = group), size = 2.7, max.overlaps = 18, show.legend = FALSE) +
  geom_text(data = ann, aes(x = -Inf, y = Inf, label = lab), hjust = -0.05, vjust = 1.4,
            size = 3.6, colour = kobalt, inherit.aes = FALSE) +
  facet_wrap(~outcome, scales = "free_y") +
  scale_colour_manual(values = grp_cols, name = "Growth model") +
  labs(
    title = "Does the frozen 2014–18 map forecast 2018–23 developments?",
    subtitle = "Baseline potential score (x) vs outcomes that never entered the scores",
    x = "Baseline potential score (frozen 2014–2018)  →",
    y = NULL,
    caption = "Out-of-sample forward validation. Exploratory — not part of the audited 01–07 pipeline."
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(colour = kobalt, face = "bold"),
        plot.subtitle = element_text(colour = gray, size = 11),
        plot.caption = element_text(colour = gray, size = 9),
        strip.text = element_text(colour = kobalt, face = "bold"))

ggsave(here("plots", "forward_validation.png"), p, width = 11, height = 5.4, dpi = 150, bg = "white")
ggsave(here("plots", "forward_validation.pdf"), p, width = 11, height = 5.4, bg = "white")
message("wrote plots/forward_validation.{png,pdf} and data/tidy/forward_validation.csv")
