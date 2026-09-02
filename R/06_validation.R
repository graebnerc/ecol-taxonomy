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
# higher-vulnerability than Core?
#
# INFERENCE. Reported as PERMUTATION p-values, not OLS ones. With n = 27 and
# groups as small as Finance (n = 4), the t-distribution behind
# summary(lm(...))$coefficients is not credible -- and reporting it here while the
# contingency table below deliberately uses a Monte-Carlo Fisher test *because* n
# is small would be internally inconsistent. The permutation test makes no
# distributional assumption: it shuffles the group labels and asks how often a
# difference from Core at least this large arises by chance. OLS estimates are
# still shown (they are just group-mean differences), and the OLS p-value is
# printed alongside so the two can be compared.
dat$group <- relevel(factor(dat$group), ref = "Core")

perm_vs_ref <- function(y, g, ref = "Core", B = 20000L, seed = 42L) {
  set.seed(seed)
  g <- as.character(g)
  others <- setdiff(unique(g), ref)
  obs <- vapply(others, function(o) mean(y[g == o]) - mean(y[g == ref]), numeric(1))
  cnt <- setNames(integer(length(others)), others)
  for (b in seq_len(B)) {
    gp <- sample(g)
    d  <- vapply(others, function(o) mean(y[gp == o]) - mean(y[gp == ref]), numeric(1))
    cnt <- cnt + (abs(d) >= abs(obs))
  }
  data.frame(group = others,
             diff_vs_Core = round(unname(obs), 2),
             p_perm = round((unname(cnt) + 1) / (B + 1), 4))
}

report_group <- function(var, label) {
  y <- dat[[var]]
  co <- summary(lm(y ~ dat$group))$coefficients
  ols <- data.frame(group = sub("^dat\\$group", "", rownames(co))[-1],
                    p_ols = round(co[-1, 4], 4))
  pr <- perm_vs_ref(y, dat$group)
  out <- merge(pr, ols, by = "group", all.x = TRUE)
  cat(sprintf("\n--- %s by growth-model group (differences vs Core) ---\n", label))
  print(out[order(out$diff_vs_Core), ], row.names = FALSE)
  invisible(out)
}
cat("\n(p_perm = permutation test, 20000 shuffles of the group labels;",
    "p_ols shown for comparison only.)\n")
perm_pot  <- report_group("potential", "Potential")
perm_vuln <- report_group("vulnerability", "Vulnerability")

# IS THE FOUR-GROUP STRUCTURE REAL, or is it just Core vs everyone else?
# Tested directly rather than inferred from overlapping coefficients: does a
# 4-group model explain more than a Core/non-Core binary? The permutation version
# shuffles labels WITHIN the non-Core set only, so it tests exactly the question
# "do the three non-Core groups differ from each other" while holding the Core
# contrast fixed.
cat("\n--- Does the 4-group split beat a Core-vs-rest binary? ---\n")
for (v in c("vulnerability", "potential")) {
  y <- dat[[v]]; g <- as.character(dat$group)
  m2 <- lm(y ~ I(g == "Core")); m4 <- lm(y ~ factor(g))
  f_obs <- anova(m2, m4)$F[2]
  nc <- which(g != "Core"); set.seed(7)
  cnt <- sum(replicate(2000L, {
    gp <- g; gp[nc] <- sample(gp[nc])
    f <- anova(lm(y ~ I(gp == "Core")), lm(y ~ factor(gp)))$F[2]
    !is.na(f) && f >= f_obs }))
  cat(sprintf("  %-14s R2 %.2f -> %.2f | F = %.2f, permutation p = %.3f\n",
              v, summary(m2)$r.squared, summary(m4)$r.squared,
              f_obs, (cnt + 1) / 2001))
}
cat("  (high p = the four-way split adds nothing over Core-vs-rest)\n")

# POWER. "The non-Core groups are indistinguishable" must not be read as "they are
# alike": with these group sizes it may simply be undetectable. Report the
# smallest difference the design could catch, so absence of evidence is not
# mistaken for evidence of absence.
cat("\n--- What could this design actually detect? ---\n")
sdw <- summary(lm(dat$vulnerability ~ dat$group))$sigma
ns  <- table(dat$group)
for (pr in list(c("Finance","Periphery"), c("Finance","Workbench"),
                c("Periphery","Workbench"))) {
  nh <- 2 / (1/ns[[pr[1]]] + 1/ns[[pr[2]]])
  mdd <- power.t.test(n = nh, sd = sdw, sig.level = 0.05, power = 0.80)$delta
  cat(sprintf("  %-10s vs %-10s (n=%d,%d): 80%% power only for differences >= %.2f z\n",
              pr[1], pr[2], ns[[pr[1]]], ns[[pr[2]]], mdd))
}
cat(sprintf("  Observed non-Core spread: vulnerability %.2f z, potential %.2f z\n",
            diff(range(tapply(dat$vulnerability, dat$group, mean)[c("Finance","Periphery","Workbench")])),
            diff(range(tapply(dat$potential, dat$group, mean)[c("Finance","Periphery","Workbench")]))))
cat("  => any non-Core difference is below what n = 27 can resolve.\n")

# The comparison that actually carries the polarization claim, tested directly:
# Workbench vs Core, one difference rather than three. NB this PAIRWISE test uses
# only the two groups involved; the vs-Core p-values above shuffle all 27 labels
# and so borrow strength from groups outside the comparison. The pairwise version
# is the conservative one and is what should be quoted for a two-group claim.
wb_test <- function(var) {
  y <- dat[[var]]; g <- as.character(dat$group)
  k <- g %in% c("Core", "Workbench")
  p <- perm_vs_ref(y[k], g[k], ref = "Core", B = 20000L)
  sprintf("  %-14s Workbench - Core = %+.2f, permutation p = %.4f",
          var, p$diff_vs_Core[p$group == "Workbench"],
          p$p_perm[p$group == "Workbench"])
}
cat("\n--- The headline comparison, tested on its own ---\n")
cat(wb_test("potential"), "\n"); cat(wb_test("vulnerability"), "\n")

fwrite(rbind(cbind(score = "potential", perm_pot),
             cbind(score = "vulnerability", perm_vuln)),
       here("data/tidy/validation_group_tests.csv"))

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
