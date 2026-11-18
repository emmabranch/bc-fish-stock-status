# 02_inspect.R
# First look at each source. Read-only: nothing is saved.
# Run ONE SECTION AT A TIME (select the lines, then Cmd + Enter).

# ======================= SECTION 0: setup =======================
if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")
library(tidyverse)
library(here)
library(janitor)

# Detect encoding from the first 5000 lines without a parallel reader
detect_enc <- function(path) {
  lines <- readLines(path, n = 5000, warn = FALSE, encoding = "bytes")
  if (all(validUTF8(lines))) "UTF-8" else "latin1"
}

# Small files: base R reader (slow but very stable)
read_small_csv <- function(path) {
  enc <- detect_enc(path)
  message(basename(path), " encoding: ", enc)
  read.csv(path,
           fileEncoding = if (enc == "UTF-8") "UTF-8-BOM" else "latin1",
           check.names = FALSE, stringsAsFactors = FALSE) |>
    as_tibble()
}

# Large files: data.table::fread (single-threaded to be safe)
read_big_csv <- function(path) {
  enc <- detect_enc(path)
  message(basename(path), " encoding: ", enc)
  data.table::fread(path,
                    encoding = if (enc == "UTF-8") "UTF-8" else "Latin-1",
                    nThread = 1, showProgress = FALSE) |>
    as_tibble()
}

# File sizes, in case something is truncated or unexpectedly large
raw_files <- list.files(here("data", "raw"), recursive = TRUE, full.names = TRUE)
print(tibble(file = str_remove(raw_files, paste0(here("data", "raw"), "/")),
             size_mb = round(file.size(raw_files) / 1e6, 1)) |>
        filter(size_mb > 0.05 | str_detect(file, "ssf|landings")), n = 50)
# ===================== END SECTION 0 ============================


# ======================= SECTION 1: SSF 2024 =====================
cat("\n================ SSF 2024 ================\n")

# Read with no header so we can see the top rows as they really are
ssf_raw <- read.csv(here("data", "raw", "ssf_2024.csv"),
                    header = FALSE, fileEncoding = "UTF-8-BOM",
                    check.names = FALSE, stringsAsFactors = FALSE,
                    colClasses = "character", fill = TRUE)
cat("Raw dimensions:", nrow(ssf_raw), "rows x", ncol(ssf_raw), "columns\n\n")

# How many non-empty cells are in each of the first 8 rows?
filled <- apply(ssf_raw[1:8, ], 1, function(r) sum(nzchar(trimws(r))))
cat("Non-empty cells in rows 1 to 8:", filled, "\n\n")

# Show the first 8 cells of each of the first 6 rows (truncated to 40 characters)
for (i in 1:6) {
  cells <- str_trunc(trimws(unlist(ssf_raw[i, 1:8])), 40)
  cat("Row", i, ":", paste(cells, collapse = " | "), "\n")
}

# Use the first row that is mostly filled in as the header
header_row <- which(filled >= 0.8 * ncol(ssf_raw))[1]
cat("\nUsing row", header_row, "as the header\n\n")

ssf <- ssf_raw[-(1:header_row), ]
names(ssf) <- trimws(unlist(ssf_raw[header_row, ]))
ssf <- ssf |>
  as_tibble(.name_repair = "unique") |>
  clean_names() |>
  filter(if_any(everything(), ~ nzchar(trimws(.x))))  # drop fully blank rows

cat("Rows:", nrow(ssf), " Columns:", ncol(ssf), "\n\nColumn names (numbered):\n")
cat(paste0(seq_along(names(ssf)), ": ", str_trunc(names(ssf), 70)), sep = "\n")

region_col <- names(ssf)[str_detect(names(ssf), "region")][1]
stock_col  <- names(ssf)[str_detect(names(ssf), "stock")][1]
status_col <- names(ssf)[str_detect(names(ssf), "status|zone")][1]
cat("\nGuessed columns -> region:", region_col,
    "| stock:", stock_col, "| status:", status_col, "\n\n")

if (!is.na(region_col)) {
  print(count(ssf, .data[[region_col]]))
  pac <- ssf |>
    filter(str_detect(.data[[region_col]], regex("pacific", ignore_case = TRUE)))
  cat("\nPacific stocks:", nrow(pac), "\n")
  if (!is.na(stock_col) && !is.na(status_col)) {
    pac |>
      select(all_of(c(stock_col, status_col))) |>
      print(n = Inf, width = 200)
  }
}
# ===================== END SECTION 1 ============================


# ======================= SECTION 2: landings =====================
cat("\n================ LANDINGS ================\n")
land <- read_big_csv(here("data", "raw", "landings_1990_2024.csv")) |> clean_names()
cat("Rows:", nrow(land), "\n\nColumn names:\n")
print(names(land))
cat("\nFirst 3 rows:\n")
print(head(land, 3), width = 200)
# ===================== END SECTION 2 ============================


# ======================= SECTION 3: RAM Legacy ===================
cat("\n================ RAM LEGACY ================\n")
rdata <- list.files(here("data", "raw", "ramldb"), pattern = "\\.RData$",
                    recursive = TRUE, full.names = TRUE)
cat("RData files:\n")
print(tibble(file = basename(rdata), size_mb = round(file.size(rdata) / 1e6, 1)))

pick <- rdata[str_detect(basename(rdata), "asmt")]
if (length(pick) == 0) pick <- rdata
ram <- new.env()
load(pick[1], envir = ram)
cat("\nLoaded:", basename(pick[1]), "\nObjects inside:\n")
print(ls(ram))

if (exists("stock", envir = ram)) {
  bc_stocks <- ram$stock |>
    as_tibble() |>
    filter(
      str_detect(region, regex("Canada West Coast", ignore_case = TRUE)) |
        str_detect(stocklong, regex("British Columbia|Hecate|Haida|Strait of Georgia|West Coast of Vancouver",
                                    ignore_case = TRUE)) |
        str_detect(commonname, regex("Pacific halibut", ignore_case = TRUE))
    ) |>
    select(stockid, commonname, region, stocklong) |>
    arrange(commonname)
  
  cat("\nBC-related RAM stocks:", nrow(bc_stocks), "\n")
  cat("\nRegions represented:\n")
  print(count(bc_stocks, region, sort = TRUE), n = Inf)
  
  cat("\nNumber of stocks per species (top 40):\n")
  print(count(bc_stocks, commonname, sort = TRUE), n = 40)
  
  targets <- "sablefish|halibut|Pacific cod|herring|prawn|shrimp|Dungeness|crab"
  cat("\nTarget species, full detail:\n")
  bc_stocks |>
    filter(str_detect(commonname, regex(targets, ignore_case = TRUE))) |>
    mutate(stocklong = str_trunc(stocklong, 70)) |>
    print(n = Inf, width = 200)
}
# ===================== END SECTION 3 ============================