# 01_download.R
# Downloads raw data into data/raw/. Safe to re-run: files already present are skipped.

library(here)
options(timeout = 1200)  # allow up to 20 minutes per file (the RAM zip is large)

raw_dir <- here("data", "raw")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

download_if_missing <- function(urls, dest) {
  if (file.exists(dest) && file.size(dest) > 0) {
    message("Already have: ", basename(dest))
    return(invisible(TRUE))
  }
  for (u in urls) {
    message("Downloading: ", basename(dest))
    ok <- tryCatch({
      download.file(u, dest, mode = "wb", quiet = TRUE)
      TRUE
    }, error = function(e) {
      message("  failed from ", u, ": ", conditionMessage(e))
      FALSE
    }, warning = function(w) {
      message("  failed from ", u, ": ", conditionMessage(w))
      FALSE
    })
    if (ok && file.exists(dest) && file.size(dest) > 0) return(invisible(TRUE))
    if (file.exists(dest)) file.remove(dest)
  }
  message("  COULD NOT DOWNLOAD ", basename(dest))
  invisible(FALSE)
}

# ---- 1. DFO Sustainability Survey for Fisheries (2018 to 2024) ----
ssf_bases <- c(
  "https://api-proxy.edh-cde.dfo-mpo.gc.ca/catalogue/records/49f25051-3946-426e-a589-b7063f75bbd5/attachments/",
  "https://api-proxy.edh.azure.cloud.dfo-mpo.gc.ca/catalogue/records/49f25051-3946-426e-a589-b7063f75bbd5/attachments/"
)
ssf_files <- c(
  "2024" = "2024%20Sustainability%20Survey%20for%20Fisheries.csv",
  "2023" = "2023%20Sustainability%20Survey%20For%20Fisheries.csv",
  "2022" = "2022%20Sustainability%20Survey%20for%20Fisheries%20ENG.xlsx",
  "2021" = "eng-sustainability-survey-for-fisheries-2021.csv",
  "2020" = "2020-sustainability-survey-for-fisheries-eng-v.-july-2022.csv",
  "2019" = "revised-2019_survey_data_en.csv",
  "2018" = "2018_survey_data_en.csv"
)
for (yr in names(ssf_files)) {
  ext  <- tools::file_ext(ssf_files[[yr]])
  dest <- file.path(raw_dir, paste0("ssf_", yr, ".", ext))
  download_if_missing(paste0(ssf_bases, ssf_files[[yr]]), dest)
}

# ---- 2. DFO commercial seafisheries landings (1990 to 2024) ----
land_base <- "https://api-proxy.edh-cde.dfo-mpo.gc.ca/catalogue/records/6e16f703-9c57-46c9-9194-6573398c17e6/attachments/"
download_if_missing(
  paste0(land_base, "1990-2024%20Commercial%20Seafisheries%20Landings%20EN.csv"),
  file.path(raw_dir, "landings_1990_2024.csv")
)
download_if_missing(
  paste0(land_base, "Data%20Dictionary.csv"),
  file.path(raw_dir, "landings_data_dictionary.csv")
)

# ---- 3. RAM Legacy Stock Assessment Database v4.66 ----
ram_zip <- file.path(raw_dir, "RAMLDB_v4.66.zip")
download_if_missing(
  "https://zenodo.org/records/14043031/files/RAMLDB%20v4.66.zip?download=1",
  ram_zip
)
ram_dir <- file.path(raw_dir, "ramldb")
if (file.exists(ram_zip) && !dir.exists(ram_dir)) {
  message("Unzipping RAM Legacy...")
  unzip(ram_zip, exdir = ram_dir)
}

# ---- Summary ----
files <- list.files(raw_dir, full.names = TRUE)
files <- files[!dir.exists(files)]
print(data.frame(
  file = basename(files),
  size_mb = round(file.size(files) / 1e6, 1)
))
message("RAM .RData files found: ",
        length(list.files(ram_dir, pattern = "\\.RData$", recursive = TRUE)))