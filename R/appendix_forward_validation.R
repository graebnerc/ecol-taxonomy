# Forward / out-of-sample validation: freeze the typology at the reference window,
# then ask whether a country's position predicts what it did AFTERWARDS on
# outcomes that never entered the scores (renewable share, real GDP).
#
# This is the one genuinely predictive check in the repo. Everything else in 06
# validates contemporaneously -- it asks whether the scores line up with outcomes
# measured over the same years, which cannot distinguish a good index from a
# well-fitted one. Here the outcome window starts where the scoring window ends.
#
# UPDATED 2026-09-03 to follow the reference window instead of the hardcoded
# 2014-2018/2018-2023 it was written with. The baseline is REF_LAST_YEAR and the
# outcome horizon runs to the last year both validators are available.
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

source(here("R/config.R"))

# The outcome window opens where the scoring window closes, and runs as far as
# BOTH validators reach. Derived, not hardcoded, so it follows REF_LAST_YEAR.
nd0 <- fread(here("data/tidy/new_data.csv"))
BASE <- REF_LAST_YEAR

# Each validator gets its OWN horizon. The two series end in different years
# (renewable share runs later than real GDP), and capping both at the shorter one
# would discard half the available follow-up for no reason. The horizon is
# reported with every coefficient so a short one cannot be read as a long one.
horizon <- function(col) max(nd0[!is.na(get(col)) & year > BASE]$year, -Inf)
LAST_RENEW <- horizon("renew_share_overall")
LAST_GDP   <- horizon("GDP_real")
LAST <- max(LAST_RENEW, LAST_GDP)
if (LAST_RENEW - BASE < 2L && LAST_GDP - BASE < 2L)
  stop("no validator has at least 2 years of follow-up after ", BASE)
cat(sprintf("Forward test from baseline %d: renewable share to %d (%d yr), real GDP to %d (%d yr)\n",
            BASE, LAST_RENEW, LAST_RENEW - BASE, LAST_GDP, LAST_GDP - BASE))

sc  <- fread(here("data/tidy/taxonomy_scores.csv"))          # frozen baseline scores
ind <- fread(here("data/tidy/taxonomy_indicators.csv"))      # for income control (GDP_normed)
nd  <- fread(here("data/tidy/new_data.csv"))                 # post-2018 outcomes

sc[, iso3 := countrycode(country, "country.name", "iso3c")]

pick <- function(y, col) nd[year == y, .(iso3 = iso3c, v = get(col))]
rb <- pick(BASE, "renew_share_overall"); rl <- pick(LAST_RENEW, "renew_share_overall")
gb <- pick(BASE, "GDP_real");            gl <- pick(LAST_GDP,   "GDP_real")
out <- merge(merge(rb, rl, by = "iso3", suffixes = c("_rb", "_rl")),
             merge(gb, gl, by = "iso3", suffixes = c("_gb", "_gl")), by = "iso3")
out[, `:=`(d_renew    = v_rl - v_rb,                  # pp change in renewable share
           gdp_growth = 100 * (v_gl / v_gb - 1))]     # % real GDP growth

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
cat(sprintf("Forward validation (scores %d-%d; outcomes from %d)\n",
            REF_FIRST_YEAR, REF_LAST_YEAR, BASE))
print(res)

# headline figure: baseline potential -> two out-of-sample outcomes -----------
lv  <- c(sprintf("\u0394 renewable share %d\u2013%d (pp, %d yr)", BASE, LAST_RENEW, LAST_RENEW - BASE),
         sprintf("Real GDP growth %d\u2013%d (%%, %d yr)", BASE, LAST_GDP, LAST_GDP - BASE))
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
  scale_colour_manual(values = grp_cols, name = "Development model") +
  labs(
    title = sprintf("Does the %d\u2013%d map forecast what happened next?",
                    REF_FIRST_YEAR, REF_LAST_YEAR),
    subtitle = "Baseline potential score (x) vs outcomes that never entered the scores",
    x = sprintf("Baseline potential score (%d\u2013%d)  \u2192",
                REF_FIRST_YEAR, REF_LAST_YEAR),
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
