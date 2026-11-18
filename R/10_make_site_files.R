# 10_make_site_files.R
# Writes cards.css (from card_functions.R) and one small .qmd page per stock.

source(here::here("R", "functions.R"))
source(here::here("R", "plot_functions.R"))
source(here::here("R", "card_functions.R"))

# ---- 1. Card CSS, from the single source in card_functions.R ----
writeLines(card_css, here("cards.css"))

# ---- 2. One page per stock ----
stocks <- read_csv(here("data", "processed", "stocks.csv"), show_col_types = FALSE)
dir.create(here("stocks"), showWarnings = FALSE)

for (i in seq_len(nrow(stocks))) {
  id <- stocks$stock_id[i]
  writeLines(c(
    "---",
    paste0('title: "', stocks$stock_name[i], '"'),
    "params:",
    paste0('  stock_id: "', id, '"'),
    "---",
    "",
    "{{< include ../_stock_page.qmd >}}"
  ), here("stocks", paste0(id, ".qmd")))
}

# ---- 3. Check the PDFs exist for the download buttons ----
pdfs <- list.files(here("reports", "output"), pattern = "\\.pdf$")
cat("Stock pages written:", nrow(stocks), "\n")
cat("PDF reports found:  ", length(pdfs), "\n")
if (length(pdfs) < nrow(stocks)) warning("Some PDFs are missing. Re-run R/09_render_reports.R.")