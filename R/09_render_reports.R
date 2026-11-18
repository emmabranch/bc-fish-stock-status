# 09_render_reports.R
# Renders the rapid assessment PDF for every stock into reports/output/.

source(here::here("R", "functions.R"))

if (quarto::quarto_version() < "1.4") {
  stop("Typst PDFs need Quarto 1.4 or later. Update Quarto from quarto.org.")
}

stocks  <- read_csv(here("data", "processed", "stocks.csv"), show_col_types = FALSE)
qmd     <- here("reports", "rapid_assessment.qmd")
out_dir <- here("reports", "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

for (id in stocks$stock_id) {
  message("Rendering ", id, " ...")
  out_name <- paste0("rapid_assessment_", id, ".pdf")
  quarto::quarto_render(
    input          = qmd,
    output_format  = "typst",
    execute_params = list(stock_id = id),
    output_file    = out_name,
    quiet          = TRUE
  )
  file.rename(here("reports", out_name), file.path(out_dir, out_name))
}

message("\nDone. Reports in reports/output/:")
print(list.files(out_dir))