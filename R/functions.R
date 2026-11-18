# functions.R
# Shared helper functions. Load with: source(here::here("R", "functions.R"))

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(janitor)
})

# Detect file encoding from the first 5000 lines (no parallel reader)
detect_enc <- function(path) {
  lines <- readLines(path, n = 5000, warn = FALSE, encoding = "bytes")
  if (all(validUTF8(lines))) "UTF-8" else "latin1"
}

# Read a DFO Sustainability Survey CSV.
# The file has a title row above the real header, so we find the header
# as the first row that is at least 80% filled in.
read_ssf <- function(path) {
  enc <- detect_enc(path)
  raw <- read.csv(path, header = FALSE,
                  fileEncoding = if (enc == "UTF-8") "UTF-8-BOM" else "latin1",
                  check.names = FALSE, stringsAsFactors = FALSE,
                  colClasses = "character", fill = TRUE)
  n_check <- min(10, nrow(raw))
  filled <- apply(raw[1:n_check, ], 1, function(r) sum(nzchar(trimws(r))))
  header_row <- which(filled >= 0.8 * ncol(raw))[1]
  out <- raw[-(1:header_row), , drop = FALSE]
  names(out) <- trimws(unlist(raw[header_row, ]))
  suppressMessages(
    out |>
      as_tibble(.name_repair = "unique") |>
      clean_names() |>
      filter(if_any(everything(), ~ nzchar(trimws(.x))))
  )
}

# Read DFO commercial landings and convert text numbers ("22,450,009") to numeric.
# Footnote rows at the bottom have no year, so they are dropped.
read_landings <- function(path) {
  enc <- detect_enc(path)
  data.table::fread(path, encoding = if (enc == "UTF-8") "UTF-8" else "Latin-1",
                    nThread = 1, showProgress = FALSE,
                    colClasses = "character") |>
    as_tibble() |>
    clean_names() |>
    mutate(
      year         = suppressWarnings(as.integer(year)),
      quantity_kgs = suppressWarnings(parse_number(quantity_kgs)),
      value_cad    = suppressWarnings(parse_number(value_cad))
    ) |>
    filter(!is.na(year))
}

# Load the RAM Legacy assessment-only database into an environment
load_ram <- function() {
  rdata <- list.files(here("data", "raw", "ramldb"), pattern = "asmt.*\\.RData$",
                      recursive = TRUE, full.names = TRUE)
  if (length(rdata) == 0) stop("RAM .RData file not found. Run R/01_download.R first.")
  env <- new.env()
  load(rdata[1], envir = env)
  env
}