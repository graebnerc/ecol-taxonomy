# EXPLORATORY (not part of the audited 01-07 pipeline) -- for co-author discussion.
# Green-capability trajectory: recompute green complexity on a RECENT window
# (2019-2023) with the identical method (R/functions/complexity.R) and compare to
# the frozen baseline (2014-2018, data/tidy/green_complexity_eu.csv). Tests whether
# the catch-up East closed the green-capability gap. GCI/GCP are z-scored to the
# global set each window, so a change = movement RELATIVE to the world.
# Writes data/tidy/green_complexity_recent.csv, data/tidy/capability_trajectory.csv,
# plots/capability_trajectory.{png,pdf}, and caches data/raw/pooled_exports_1923.rds.

here::i_am("R/appendix_capability_trajectory.R")
library(here)
suppressMessages({
  library(data.table); library(dplyr); library(countrycode)
  library(Matrix); library(ggplot2); library(ggrepel)
})
source(here("R/config.R"))
source(here("R/country_classification.R"))
source(here("R/functions/complexity.R"))

RECENT_FIRST <- 2019L; RECENT_LAST <- 2023L
atlas_path <- here("data/raw/atlas_hs92_6d.csv")
cache_1923 <- here("data/raw/pooled_exports_1923.rds")
green_path <- here("data/tidy/green_products_hs6.csv")

# --- pool the recent window from the Atlas (cache so the 968MB read happens once) --
if (file.exists(cache_1923)) {
  message("Loading cached recent-window exports ...")
  exp_dt <- readRDS(cache_1923)
} else {
  stopifnot("Atlas data missing" = file.exists(atlas_path))
  message("Reading Atlas (heavy) to pool ", RECENT_FIRST, "-", RECENT_LAST, " ...")
  atlas <- fread(atlas_path,
                 select = c("country_iso3_code", "product_hs92_code", "year", "export_value"),
                 colClasses = list(character = "product_hs92_code", double = "export_value"))
  setnames(atlas, c("iso3", "hs6", "year", "export"))
  atlas[, hs6 := formatC(hs6, width = 6, flag = "0")]
  exp_dt <- atlas[year >= RECENT_FIRST & year <= RECENT_LAST & export > 0,
                  .(export = sum(export)), by = .(iso3, hs6)]
  saveRDS(exp_dt, cache_1923)
  rm(atlas); gc()
}
message(sprintf("Pooled %d-%d: %d countries x %d products.",
                RECENT_FIRST, RECENT_LAST, uniqueN(exp_dt$iso3), uniqueN(exp_dt$hs6)))

green_codes <- fread(green_path, colClasses = list(character = "hs6"))$hs6

# --- identical complexity machinery ------------------------------------------
rca <- build_rca_matrix(exp_dt)
ci  <- complexity_indices(rca$M)
gci <- green_indicators(rca$M, ci$PCI, green_codes)
gci$ECI <- ci$ECI[gci$iso3]

eu_iso3 <- countrycode(base_countries, "country.name", "iso3c")
recent <- gci |>
  filter(iso3 %in% eu_iso3) |>
  mutate(country = countrycode(iso3, "iso3c", "country.name")) |>
  select(iso3, country, ECI, GCI, GCP, diversity) |>
  arrange(desc(GCI))
fwrite(recent, here("data/tidy/green_complexity_recent.csv"))

# --- compare to frozen baseline ----------------------------------------------
base <- fread(here("data/tidy/green_complexity_eu.csv"))
traj <- merge(base[, .(iso3, GCI_base = GCI, GCP_base = GCP)],
              as.data.table(recent)[, .(iso3, GCI_rec = GCI, GCP_rec = GCP)], by = "iso3")
traj[, `:=`(dGCI = GCI_rec - GCI_base, dGCP = GCP_rec - GCP_base,
            group = get_country_classification(iso3, "jee"))]
fwrite(traj, here("data/tidy/capability_trajectory.csv"))

cat(sprintf("\ncor(baseline GCI, ΔGCI) = %.2f  (negative => catch-up / convergence)\n",
            cor(traj$GCI_base, traj$dGCI)))
cat("\nΔGCI (2019-23 minus 2014-18), relative to world, by growth model:\n")
print(traj[, .(mean_dGCI = round(mean(dGCI), 2), n = .N), by = group])

# --- figure: baseline capability vs change (catch-up diagnostic) -------------
kobalt <- "#00395B"; steel <- "#69AACD"; green <- "#5FB46E"; gray <- "#6F6F6F"
grp_cols <- c(Core = "#E65032", Finance = green, Periphery = steel, Workbench = "#B98BD9")
rr <- cor(traj$GCI_base, traj$dGCI)

p <- ggplot(traj, aes(GCI_base, dGCI)) +
  geom_hline(yintercept = 0, colour = gray, linetype = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = kobalt, fill = steel, alpha = 0.15, linewidth = 0.7) +
  geom_point(aes(colour = group), size = 2.6) +
  ggrepel::geom_text_repel(aes(label = iso3, colour = group), size = 2.9, max.overlaps = 20, show.legend = FALSE) +
  scale_colour_manual(values = grp_cols, name = "Growth model") +
  labs(title = "Did green capability converge, 2014–18 → 2019–23?",
       subtitle = sprintf("Baseline GCI vs its change (relative to the world)  |  r = %.2f  →  %s",
                          rr, ifelse(rr < -0.2, "laggards catching up", "no convergence")),
       x = "Baseline Green Complexity Index (2014–2018)  →",
       y = "Δ GCI, 2019–23 vs 2014–18 (world-relative)",
       caption = "Same Atlas method both windows. Exploratory — not part of the audited 01–07 pipeline.") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(colour = kobalt, face = "bold"),
        plot.subtitle = element_text(colour = gray, size = 11),
        plot.caption = element_text(colour = gray, size = 9))
ggsave(here("plots", "capability_trajectory.png"), p, width = 10, height = 6, dpi = 150, bg = "white")
ggsave(here("plots", "capability_trajectory.pdf"), p, width = 10, height = 6, bg = "white")
message("wrote plots/capability_trajectory.{png,pdf} + data/tidy/{green_complexity_recent,capability_trajectory}.csv")
