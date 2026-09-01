# 06 - Validation & comparison with other classifications (Phase 5).
#
# (A) External validity: do the vulnerability/potential scores predict outcomes
#     they SHOULD, controlling for GDP per capita? Validators are variables NOT
#     used to build the scores: the overall renewable share (Eurostat SHARES REN,
#     column renew_share_overall) and real GDP growth over the window (both
#     offline, from new_data.csv; built by R/get_data_extra.R).
#     CIRCULARITY NOTE: the headline vulnerability (04, four-dimension structure)
#     gives fossil dependency ~half the weight, and the demand-side fossil share is
#     ~ 1 - renewable share (r = -0.72 at variable level). So renew_share is now
#     PARTLY CIRCULAR for vulnerability (cor(vulnerability, renew_share) = -0.46):
#     read it as descriptive, not as external validation. It stays a LEGITIMATE
#     validator for POTENTIAL (complexity + patents contain no fossil term;
#     cor = +0.25). For vulnerability, lean on gdp_growth and EPS, or a renewable-
#     DEPLOYMENT trajectory (see info/PaperTodos.md - symmetric structure / item 1).
# (B) Comparison with the Graebner et al. (2020) growth-model groups (JEE) and
#     the geographic grouping: group-mean scores, contingency of quadrants, an
#     alluvial map, and Cramer's V. Also cross-tabs against the data-driven
#     clusters from 05_clustering.R.

here::i_am("R/06_validation.R")
library(here)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggalluvial)
library(countrycode)
suppressMessages(library(ggpubr))
source(here("R/config.R"))
source(here("R/country_classification.R"))

scores <- as_tibble(fread(here("data/tidy/taxonomy_scores.csv")))
ind    <- as_tibble(fread(here("data/tidy/taxonomy_indicators.csv")))
scores$iso3 <- countrycode(scores$country, "country.name", "iso3c")
scores$geo  <- get_country_classification(scores$iso3, "geo_struc")

# ---- External validators (window means / growth), offline from new_data.csv --
nd <- as_tibble(fread(here("data/tidy/new_data.csv")))
val <- nd |>
  filter(year >= REF_FIRST_YEAR, year <= REF_LAST_YEAR) |>
  group_by(iso3 = iso3c) |>
  summarise(
    renew_share = mean(renew_share_overall, na.rm = TRUE),  # Eurostat SHARES REN (overall)
    gdp_growth  = (GDP_real[year == REF_LAST_YEAR] / GDP_real[year == REF_FIRST_YEAR])^
                    (1 / (REF_LAST_YEAR - REF_FIRST_YEAR)) - 1,  # explicit endpoints, not row order
    loggdp_pc   = log(mean(GDP_ppp, na.rm = TRUE)),  # per-country GDP level (control)
    .groups = "drop"
  )

# OECD Environmental Policy Stringency (aggregate EPS, 0-6). Covers 20/27 EU
# states (OECD members). The 7 missing (BG, HR, CY, LV, LT, RO, MT) are 6 of the
# 9 "At risk" countries, so the EPS partial correlation is estimated under severe
# range restriction on the taxonomy's low-potential tail (finding M1) - read it
# as indicative and do not let it speak to the At-risk group.
eps <- fread(here("info/OECD-EPS-Index.csv"))[
  CLIM_POL == "EPS" & TIME_PERIOD >= REF_FIRST_YEAR & TIME_PERIOD <= REF_LAST_YEAR &
    !is.na(OBS_VALUE), .(eps = mean(OBS_VALUE)), by = .(iso3 = REF_AREA)]
dat <- scores |> left_join(val, by = "iso3") |> left_join(as_tibble(eps), by = "iso3")
cat(sprintf("EPS coverage: %d of %d EU-27 countries.\n", sum(!is.na(dat$eps)), nrow(dat)))

# ---- (A) Correlations, raw and partial (controlling for log GDP p.c.) --------
partial <- function(x, y, z) {
  ok <- complete.cases(x, y, z)          # common subset (some validators have NAs)
  x <- x[ok]; y <- y[ok]; z <- z[ok]
  rxy <- cor(x, y); rxz <- cor(x, z); ryz <- cor(y, z)
  (rxy - rxz * ryz) / sqrt((1 - rxz^2) * (1 - ryz^2))
}
cat("\n=== (A) External validity: correlations with scores ===\n")
val_tbl <- expand_grid(score = c("vulnerability", "potential"),
                       outcome = c("renew_share", "gdp_growth", "eps")) |>
  rowwise() |>
  mutate(
    raw_cor     = cor(dat[[score]], dat[[outcome]], use = "complete.obs"),
    partial_cor = partial(dat[[score]], dat[[outcome]], dat$loggdp_pc)
  ) |>
  ungroup()
print(as.data.frame(val_tbl), row.names = FALSE, digits = 2)
cat("Expect: potential +/higher renewables, growth & policy stringency (EPS);\n",
    "vulnerability -/lower. (partial_cor controls for log GDP p.c.)\n", sep = "")

# ---- (B) Comparison with development-model & geographic groups ---------------
cat("\n=== (B) Mean scores by growth-model group (Graebner et al. 2020) ===\n")
grp_tbl <- dat |>
  group_by(`Growth model` = group) |>
  summarise(n = n(),
            vulnerability = mean(vulnerability),
            potential     = mean(potential), .groups = "drop") |>
  arrange(desc(potential))
print(as.data.frame(grp_tbl), row.names = FALSE, digits = 2)

# H2/H3 test: are catch-up (Workbench) economies systematically lower-potential /
# higher-vulnerability than Core? (regression on group, Core = reference)
dat$group <- relevel(factor(dat$group), ref = "Core")
cat("\n--- potential ~ growth-model group (ref = Core) ---\n")
print(summary(lm(potential ~ group, dat))$coefficients[, c(1, 4)], digits = 2)
cat("\n--- vulnerability ~ growth-model group (ref = Core) ---\n")
print(summary(lm(vulnerability ~ group, dat))$coefficients[, c(1, 4)], digits = 2)

# Contingency of quadrant x growth model + Cramer's V. With n = 27 and a 4x4
# table every expected count is < 5, so the chi-square p-value is invalid
# (finding M2). We keep V as a DESCRIPTIVE index of association and take the
# p-value from a Monte-Carlo Fisher test instead. V is also reported with
# Bergsma's (2013) bias correction, which the uncorrected V inflates in sparse
# tables.
cram_v <- function(tab, bias_correct = FALSE) {
  n   <- sum(tab); r <- nrow(tab); k <- ncol(tab)
  chi <- suppressWarnings(chisq.test(tab)$statistic)
  phi2 <- as.numeric(chi) / n
  if (!bias_correct) return(sqrt(phi2 / (min(r, k) - 1)))
  phi2c <- max(0, phi2 - (r - 1) * (k - 1) / (n - 1))          # Bergsma 2013
  rc <- r - (r - 1)^2 / (n - 1); kc <- k - (k - 1)^2 / (n - 1)
  sqrt(phi2c / (min(rc, kc) - 1))
}
report_assoc <- function(tab, what, B = 1e5) {
  p  <- fisher.test(tab, simulate.p.value = TRUE, B = B)$p.value
  ps <- if (p < 1 / B) sprintf("< %.0e", 1 / B) else sprintf("%.4f", p)
  cat(sprintf("Cramer's V (%s) = %.2f (bias-corrected %.2f); Monte-Carlo Fisher p = %s\n",
              what, cram_v(tab), cram_v(tab, TRUE), ps))
}
tab <- table(quadrant = dat$quadrant, group = as.character(dat$group))
cat("\n=== Quadrant x growth model ===\n"); print(tab)
report_assoc(tab, "quadrant, growth model")

# Cross-tab against the data-driven clusters (05_clustering.R), if present
cl_path <- here("data/tidy/cluster_membership.csv")
if (file.exists(cl_path)) {
  cl <- fread(cl_path)
  dat <- left_join(dat, cl, by = "country")
  ct <- table(quadrant = dat$quadrant, cluster = dat$ecological_model)
  cat("\n=== Quadrant x data-driven cluster (05) ===\n"); print(ct)
  report_assoc(ct, "quadrant, cluster")
}

# ---- Figures -----------------------------------------------------------------
score_long <- dat |>
  select(country, group, geo, vulnerability, potential) |>
  pivot_longer(c(vulnerability, potential), names_to = "score")
p_grp <- ggplot(score_long, aes(reorder(group, value), value, fill = group)) +
  geom_hline(yintercept = 0, colour = "grey70") +
  geom_boxplot(alpha = .5, outlier.shape = NA) + geom_jitter(width = .12, size = 1.3) +
  facet_wrap(~score) + coord_flip() +
  labs(title = "Typology scores by growth-model group",
       x = NULL, y = "standardised score") +
  theme_minimal() + theme(legend.position = "none")
ggsave(here("plots/validation_scores_by_group.pdf"), p_grp, width = 10, height = 5)
ggsave(here("plots/validation_scores_by_group.png"), p_grp, width = 10, height = 5, dpi = 150)

alluv <- dat |>
  transmute(country, `Growth model` = as.character(group), Quadrant = quadrant) |>
  pivot_longer(-country, names_to = "axis", values_to = "code")
p_al <- ggplot(alluv,
               aes(x = axis, stratum = code, alluvium = country, fill = code, label = code)) +
  ggalluvial::geom_flow(stat = "alluvium", lode.guidance = "frontback", color = "darkgray") +
  ggalluvial::geom_stratum() + ggplot2::geom_text(stat = "stratum", size = 2.6) +
  labs(title = "Growth model -> transition quadrant") +
  theme_void() + theme(legend.position = "none", plot.title = element_text(hjust = .5))
suppressMessages(ggsave(here("plots/validation_alluvial.pdf"), p_al, width = 8, height = 6))
suppressMessages(ggsave(here("plots/validation_alluvial.png"), p_al, width = 8, height = 6, dpi = 150))

fwrite(val_tbl, here("data/tidy/validation_external.csv"))
fwrite(grp_tbl, here("data/tidy/validation_group_means.csv"))
message("06_validation.R done: external validity + development-model comparison.")
