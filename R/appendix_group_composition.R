# Appendix - what the aggregate axes hide.
#
# THE POINT. Each axis is a twin sub-index plus a standalone, combined at equal
# weight, so two groups can land on the SAME axis score for OPPOSITE reasons. The
# group-mean tests in 06_validation.R compare axis levels and therefore cannot see
# this: they answer "is this group more vulnerable?" but not "vulnerable in the
# same way?".
#
# That matters for the paper's argument. The finding that a four-group model does
# not beat a Core-vs-rest binary is about LEVELS. If the composition of
# vulnerability or potential differs across groups that share a level, the growth
# models do face different transition situations after all -- just not ones a
# difference-in-means on the aggregate can detect.
#
# WHAT THIS DOES
#   1. group means and WITHIN-group dispersion on both axes and all four parts;
#   2. finds pairs that MATCH on an axis (|difference| < 0.35 z) while their parts
#      DIVERGE (|difference| > 0.5 z) -- offsetting composition;
#   3. permutation-tests those component differences, so a masked difference is a
#      tested claim rather than an eyeballed one;
#   4. plots the component profile of each group.
#
# Writes data/tidy/group_composition.csv and
# plots/group_composition.{png,pdf}.

here::i_am("R/appendix_group_composition.R")
library(here)
suppressMessages({
  library(data.table); library(dplyr); library(ggplot2)
  library(knitr); library(magrittr)
})
source(here("R/config.R"))

set.seed(11); B <- 20000L
GROUPS <- c("Core", "Finance", "Periphery", "Workbench")
AXES   <- c(vulnerability = "intensity|fossil", potential = "complexity|innovation")
PARTS  <- list(vulnerability = c("intensity", "fossil"),
               potential      = c("complexity", "innovation"))
V <- c("vulnerability", "intensity", "fossil", "potential", "complexity", "innovation")

s <- fread(here("data/tidy/taxonomy_scores.csv"))
s[, group := factor(group, levels = GROUPS)]

cat("## Group means (z)\n\n")
means <- s[, c(list(n = .N), lapply(.SD, function(x) round(mean(x), 2))),
           by = group, .SDcols = V][order(match(group, GROUPS))]
print(kable(means, format = "pipe"))

cat("\n\n## Within-group dispersion (sd)\n\n")
sds <- s[, c(list(n = .N), lapply(.SD, function(x) round(sd(x), 2))),
         by = group, .SDcols = V][order(match(group, GROUPS))]
print(kable(sds, format = "pipe"))
cat("\n  A group with a small sd on a part is a POINT MASS on that dimension, not a\n",
    "  distribution around a mean -- worth saying explicitly where it occurs.\n", sep = "")

# --- Masked differences -------------------------------------------------------
perm_diff <- function(y, g, a, b) {
  k <- g %in% c(a, b); yy <- y[k]; gg <- g[k]
  obs <- mean(yy[gg == b]) - mean(yy[gg == a])
  cnt <- sum(replicate(B, {
    gp <- sample(gg); abs(mean(yy[gp == b]) - mean(yy[gp == a])) >= abs(obs) }))
  c(diff = obs, p = (cnt + 1) / (B + 1))
}

g <- as.character(s$group)
res <- rbindlist(lapply(combn(GROUPS, 2, simplify = FALSE), function(pp) {
  rbindlist(lapply(names(PARTS), function(ax) {
    ad <- perm_diff(s[[ax]], g, pp[1], pp[2])
    pa <- PARTS[[ax]]
    p1 <- perm_diff(s[[pa[1]]], g, pp[1], pp[2])
    p2 <- perm_diff(s[[pa[2]]], g, pp[1], pp[2])
    data.table(pair = paste(pp[1], "vs", pp[2]), axis = ax,
               d_axis = round(ad[["diff"]], 2), p_axis = round(ad[["p"]], 4),
               part1 = pa[1], d_part1 = round(p1[["diff"]], 2), p_part1 = round(p1[["p"]], 4),
               part2 = pa[2], d_part2 = round(p2[["diff"]], 2), p_part2 = round(p2[["p"]], 4),
               masked = abs(ad[["diff"]]) < 0.35 &
                        (abs(p1[["diff"]]) > 0.5 | abs(p2[["diff"]]) > 0.5))
  }))
}))

cat("\n\n## Every pair, axis difference vs its component differences\n\n")
print(kable(res[, .(pair, axis, d_axis, p_axis, part1, d_part1, p_part1,
                    part2, d_part2, p_part2, masked)], format = "pipe"))

cat("\n\n## MASKED differences: same axis level, different composition\n\n")
mk <- res[masked == TRUE]
if (!nrow(mk)) {
  cat("  none found.\n")
} else {
  for (i in seq_len(nrow(mk))) {
    r <- mk[i]
    cat(sprintf("  %-24s %s differs by only %+.2f (p = %.2f)\n",
                r$pair, r$axis, r$d_axis, r$p_axis))
    cat(sprintf("  %-24s   but %s %+.2f (p = %.4f) and %s %+.2f (p = %.4f)\n\n",
                "", r$part1, r$d_part1, r$p_part1, r$part2, r$d_part2, r$p_part2))
  }
  cat("  A masked pair with SIGNIFICANT component differences is a real finding:\n",
      "  the groups face the same amount of burden or capability, composed\n",
      "  differently -- which a difference-in-means on the axis cannot detect.\n", sep = "")
}

fwrite(res, here("data/tidy/group_composition.csv"))

# --- The same question of the QUADRANTS ---------------------------------------
# If the paper makes quadrants rather than development models its unit, the same test
# applies: is each quadrant a coherent TYPE, or do countries arrive there by
# different routes? Answering this decides whether the four cells can be written
# as types at all -- a framing question, not a robustness one.
short_q <- function(x) sub(" \\(.*$", "", sub("Exposed but capable", "Exposed",
             sub("Low-stakes / low capability", "Low-stakes", x)))
s[, q := factor(short_q(quadrant),
                levels = c("Winners", "Exposed", "Low-stakes", "At risk"))]
P <- c("intensity", "fossil", "complexity", "innovation")

cat("\n\n## Are QUADRANTS more coherent than development models?\n\n")
qsd <- s[, c(list(n = .N), lapply(.SD, function(x) round(sd(x), 2))),
         by = q, .SDcols = P][order(q)]
print(kable(qsd, format = "pipe"))
mq <- mean(unlist(s[, lapply(.SD, sd), by = q, .SDcols = P][, -1]))
mg <- mean(unlist(s[, lapply(.SD, sd), by = group, .SDcols = P][, -1]))
cat(sprintf("\n  mean within-unit sd: quadrants %.2f | development models %.2f\n", mq, mg))
cat("  (similar = the quadrants are no more internally coherent than the growth\n",
    "   models, so neither classification should be written as a set of types)\n", sep = "")

cat("\n## Do countries reach the same quadrant by OPPOSITE routes?\n\n")
rt <- rbindlist(lapply(levels(s$q), function(qq) {
  d <- s[q == qq]; if (nrow(d) < 3) return(NULL)
  data.table(quadrant = qq, n = nrow(d),
             r_intensity_fossil = round(cor(d$intensity, d$fossil), 2),
             r_complexity_innov = round(cor(d$complexity, d$innovation), 2))
}))
print(kable(rt, format = "pipe"))
cat("\n  A NEGATIVE correlation inside a quadrant means members trade one component\n",
    "  off against the other: they share a score, not a situation.\n", sep = "")

for (qq in c("At risk", "Winners")) {
  d <- s[q == qq][order(-intensity)]
  a <- d[1]; b <- d[.N]
  cat(sprintf("\n  %s -- widest contrast: %s (intensity %+.2f, fossil %+.2f)\n",
              qq, a$country, a$intensity, a$fossil))
  cat(sprintf("  %s    vs %s (intensity %+.2f, fossil %+.2f)\n",
              strrep(" ", nchar(qq)), b$country, b$intensity, b$fossil))
}
fwrite(rt, here("data/tidy/quadrant_coherence.csv"))

# --- Figure -------------------------------------------------------------------
long <- melt(s[, c("country", "group", "intensity", "fossil",
                   "complexity", "innovation"), with = FALSE],
             id.vars = c("country", "group"), variable.name = "component",
             value.name = "score")
long[, component := factor(component,
      levels = c("intensity", "fossil", "complexity", "innovation"),
      labels = c("Emission intensity\n(vulnerability twin)",
                 "Fossil dependency\n(vulnerability standalone)",
                 "Green complexity\n(potential twin)",
                 "Green innovation\n(potential standalone)"))]
gm <- long[, .(score = mean(score)), by = .(group, component)]

p <- ggplot(long, aes(score, group)) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey80") +
  geom_point(position = position_jitter(height = 0.14, width = 0),
             colour = "#2E7CA8", size = 1.9, alpha = 0.75) +
  geom_point(data = gm, shape = 23, size = 3.1, stroke = 0.9,
             fill = "#E65032", colour = "white") +
  facet_wrap(~component, ncol = 2) +
  scale_y_discrete(limits = rev(GROUPS)) +
  labs(x = "z-score", y = NULL,
       title = "The axes hide compositional differences between development models",
       subtitle = paste("Dots = countries, diamond = group mean. Two groups can share",
                        "an axis score\nwhile differing on the parts that make it up.")) +
  theme_minimal(base_size = 9) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.3),
        strip.text = element_text(size = 8, lineheight = 1.05, hjust = 0),
        axis.text.y = element_text(face = "bold", colour = "#1A1A1A"),
        plot.title = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(colour = "grey35", size = 8, lineheight = 1.15),
        panel.spacing.x = unit(1.1, "lines"))

ggsave(here("plots/group_composition.png"), p, width = 8.0, height = 5.6, dpi = 300)
ggsave(here("plots/group_composition.pdf"), p, width = 8.0, height = 5.6)

message("\nappendix_group_composition.R done.")
