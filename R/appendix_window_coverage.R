# Appendix figure: data coverage vs. reference windows.
# Makes visible why the taxonomy window ends 2019 and what a shift to a
# 2018-2022 window would give up (EXIOBASE cap + green-patent truncation).
# Standalone (not part of the 01-07 pipeline); writes plots/appendix_window_coverage.{png,pdf}.

suppressMessages({
  library(here)
  library(ggplot2)
})
here::i_am("R/appendix_window_coverage.R")

# EUF brand palette (info/custom.scss / presentation/custom.scss) ------------
kobalt <- "#00395B"; steel <- "#69AACD"; green <- "#5FB46E"
orange <- "#E65032"; gray  <- "#6F6F6F"

# Coverage of each block input (end years verified from data/tidy sources) ----
# Rows are ordered top -> bottom.
labels <- c(
  "Carbon & energy intensity\n(EXIOBASE value added + GHG)",
  "Fossil share\n(Eurostat energy balances)",
  "Green patents p.c.\n(PATSTAT)",
  "Green complexity GCI / GCP\n(Atlas HS92 trade)",
  "Validators: GDP, renewables\n(WDI / Eurostat)"
)
y <- length(labels):1                       # top row highest y
names(y) <- labels

xstart <- 2012                              # clip left edge for readability

reliable <- data.frame(
  lab = labels,
  y   = y,
  x0  = xstart,
  x1  = c(2019, 2023, 2019, 2024, 2025)     # last reliable year
)
# Truncated / unreliable tail (green patents: publication lag)
trunc <- data.frame(lab = labels[3], y = y[3], x0 = 2019, x1 = 2022)

# Reference windows ----------------------------------------------------------
base_win <- data.frame(xmin = 2013.5, xmax = 2018.5)   # 2014-2018 (current)
prop_win <- data.frame(xmin = 2017.5, xmax = 2022.5)   # 2018-2022 (proposed)

bar_h <- 0.3
ytop  <- max(y) + 0.5

p <- ggplot() +
  # windows -----------------------------------------------------------------
  geom_rect(data = base_win,
            aes(xmin = xmin, xmax = xmax, ymin = 0.4, ymax = ytop),
            fill = green, alpha = 0.12) +
  geom_rect(data = prop_win,
            aes(xmin = xmin, xmax = xmax, ymin = 0.4, ymax = ytop),
            fill = NA, colour = orange, linewidth = 0.9, linetype = "22") +
  # coverage bars -----------------------------------------------------------
  geom_segment(data = reliable,
               aes(x = x0, xend = x1, y = y, yend = y),
               colour = steel, linewidth = 7, lineend = "round") +
  geom_segment(data = trunc,
               aes(x = x0, xend = x1, y = y, yend = y),
               colour = orange, linewidth = 7, lineend = "butt", alpha = 0.55) +
  # EXIOBASE hard stop ------------------------------------------------------
  geom_vline(xintercept = 2019, colour = orange, linetype = "solid", linewidth = 0.5) +
  # window bracket labels ---------------------------------------------------
  annotate("text", x = 2016, y = ytop + 0.28, label = "Baseline 2014–2018",
           colour = green, fontface = "bold", size = 4) +
  annotate("text", x = 2020.2, y = ytop + 0.62, label = "Proposed 2018–2022",
           colour = orange, fontface = "bold", size = 4) +
  # call-outs of the implication -------------------------------------------
  annotate("text", x = 2021, y = y[1] + 0.42,
           label = "no data 2020–2022", colour = orange, size = 3.2, hjust = 0) +
  annotate("text", x = 2020.5, y = y[3] - 0.42,
           label = "truncated (patent lag)", colour = orange, size = 3.2, hjust = 0.5) +
  annotate("text", x = 2019, y = 0.15, label = "EXIOBASE ends",
           colour = orange, size = 3.2, hjust = 0.5) +
  scale_y_continuous(breaks = y, labels = labels, limits = c(0, ytop + 0.9)) +
  scale_x_continuous(breaks = seq(2012, 2025, 2), limits = c(xstart, 2025.5)) +
  labs(
    title = "Data coverage vs. reference windows",
    subtitle = "Shifting to 2018–2022 pushes past the EXIOBASE cap and into the truncated patent years",
    x = NULL, y = NULL,
    caption = "Blue = reliable coverage; orange = truncated/unavailable. Bars clipped at 2012."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title    = element_text(colour = kobalt, face = "bold"),
    plot.subtitle = element_text(colour = gray),
    plot.caption  = element_text(colour = gray, size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.y = element_text(colour = kobalt, lineheight = 0.9),
    axis.text.x = element_text(colour = kobalt)
  )

ggsave(here::here("plots", "appendix_window_coverage.png"), p,
       width = 11, height = 6, dpi = 150, bg = "white")
ggsave(here::here("plots", "appendix_window_coverage.pdf"), p,
       width = 11, height = 6, bg = "white")
message("wrote plots/appendix_window_coverage.{png,pdf}")
