# Build the green-product list in HS1992 6-digit, transparently, from public sources.
#
# Source 1: OECD Combined List of Environmental Goods (CLEG), Table A.1 of
#   info/OECD-Report_List.pdf. 248 six-digit HS 2007 codes, each tagged with an
#   environmental "medium" and membership of the Friends (WTO 2009), PEGS, APEC,
#   Core CLEG and Core CLEG+ sub-lists.
# Source 2: HS 2007 -> HS 1992 concordance, sheet "Conversion Tables" of
#   info/"HS 2007-to-HS1992 .xls".
#
# Output:
#   data/tidy/green_products_cleg_hs2007.csv  - raw extracted CLEG (HS2007, provenance)
#   data/tidy/green_products_hs6.csv          - final green set in HS1992 (hs6, is_green,
#                                               is_renewable, ...), consumed by 02_complexity.R
#
# This reconstructs the OECD list transparently. It is close to, but not identical
# with, Mealy & Teytelboym's (2022) 293-code green list (they union WTO Core + OECD
# + APEC). Replace with the authors' authoritative list if/when they share it.

here::i_am("R/build_green_list.R")
library(here)
suppressMessages({
  library(pdftools)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(data.table)
})

pdf_path  <- here("info/OECD-Report_List.pdf")
conv_path <- here("info/HS 2007-to-HS1992 .xls")

# --- 1. Extract the CLEG from the PDF -----------------------------------------
# Table spans pages 2-7. We parse word-level coordinates and assign each "X"
# mark to a sub-list column by its x-position (column centres from the header).
TABLE_PAGES <- 2:7
COL_CENTRES <- c(friends = 252, pegs = 312, apec = 370,
                 core_cleg = 427, core_cleg_plus = 485)

assign_column <- function(x) names(COL_CENTRES)[which.min(abs(x - COL_CENTRES))]

pages <- pdf_data(pdf_path)

parse_page <- function(d) {
  d <- d[order(d$y, d$x), ]
  d$row <- cumsum(c(TRUE, diff(d$y) > 3))          # group words into visual rows
  out <- lapply(split(d, d$row), function(g) {
    g <- g[order(g$x), ]
    code <- g$text[1]
    if (!grepl("^[0-9]{6}$", code)) return(NULL)   # skip non-data rows (headers etc.)
    medium <- g$text[2]
    marks <- g$text %in% "X"
    cols  <- vapply(g$x[marks], assign_column, character(1))
    tibble(
      hs2007 = code, medium = medium,
      friends        = "friends"        %in% cols,
      pegs           = "pegs"           %in% cols,
      apec           = "apec"           %in% cols,
      core_cleg      = "core_cleg"      %in% cols,
      core_cleg_plus = "core_cleg_plus" %in% cols
    )
  })
  bind_rows(out)
}

# Template sanity check: Friends header should sit near x = 244 on each page.
for (p in TABLE_PAGES) {
  fx <- pages[[p]]$x[pages[[p]]$text == "Friends"]
  if (length(fx) && abs(fx[1] - 244) > 12) {
    warning(sprintf("Page %d header layout differs (Friends x=%s); check columns.", p, fx[1]))
  }
}

cleg <- bind_rows(lapply(pages[TABLE_PAGES], parse_page)) |>
  distinct(hs2007, .keep_all = TRUE)

stopifnot("Expected 248 CLEG codes" = nrow(cleg) == 248)
message(sprintf("Extracted %d CLEG codes (HS2007).", nrow(cleg)))
message("Medium categories: ", paste(sort(unique(cleg$medium)), collapse = ", "))

fwrite(cleg, here("data/tidy/green_products_cleg_hs2007.csv"))

# --- 2. HS 2007 -> HS 1992 concordance ----------------------------------------
conv <- read_excel(conv_path, sheet = "Conversion Tables",
                   col_types = "text") |>
  rename(hs2007 = From, hs1992 = To) |>
  filter(hs2007 != "HS 2007") |>                  # drop the in-sheet header row
  mutate(across(everything(), \(x) str_pad(str_trim(x), 6, pad = "0")))

# --- 3. Map the green codes to HS1992 -----------------------------------------
mapped <- cleg |>
  left_join(conv, by = "hs2007", relationship = "many-to-many")

unmatched <- mapped |> filter(is.na(hs1992)) |> pull(hs2007)
if (length(unmatched)) {
  message(sprintf("WARNING: %d CLEG codes had no HS1992 match: %s",
                  length(unmatched), paste(unmatched, collapse = ", ")))
}

# Collapse to the unique HS1992 green set, carrying provenance.
# is_renewable: REP = "Renewable Energy Plant" medium in the CLEG. Provisional
# proxy for the renewable subset until Mealy & Teytelboym's 57-code list is available.
green_hs92 <- mapped |>
  filter(!is.na(hs1992)) |>
  group_by(hs6 = hs1992) |>
  summarise(
    is_green      = 1L,
    is_renewable  = as.integer(any(medium == "REP")),
    media         = paste(sort(unique(medium)), collapse = ";"),
    n_src_hs2007  = n_distinct(hs2007),
    src_hs2007    = paste(sort(unique(hs2007)), collapse = ";"),
    .groups = "drop"
  ) |>
  arrange(hs6)

fwrite(green_hs92, here("data/tidy/green_products_hs6.csv"))

message(sprintf(
  "Wrote green_products_hs6.csv: %d unique HS1992 green products (%d flagged renewable).",
  nrow(green_hs92), sum(green_hs92$is_renewable)
))
