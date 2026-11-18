# 00_setup.R
# One-off project setup: folders, packages, .gitignore, and README stub.

# ---- 1. Folders ----
dirs <- c(
  "R",               # analysis scripts
  "data/raw",        # downloaded files, never edited by hand (not committed)
  "data/processed",  # tidy outputs created by scripts (committed)
  "figures",         # saved plots
  "reports"          # parameterised Quarto report
)
for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# Move this script into R/ 
if (file.exists("00_setup.R")) file.rename("00_setup.R", "R/00_setup.R")

# ---- 2. Packages ----
pkgs <- c(
  "tidyverse", "readxl", "janitor", "here", "glue", "scales",
  "ggrepel", "plotly", "DT", "gganimate", "gifski", "quarto"
)
missing <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing) > 0) install.packages(missing)
invisible(lapply(pkgs, library, character.only = TRUE))

# ---- 3. .gitignore ----
# The RAM Legacy zip is about 112 MB, over GitHub's 100 MB file limit,
# so raw data stays local. Scripts re-download it.
gitignore_lines <- c(
  ".Rproj.user", ".Rhistory", ".RData", ".Ruserdata", ".DS_Store",
  "data/raw/", "*_cache/", "/.quarto/"
)
existing <- if (file.exists(".gitignore")) readLines(".gitignore") else character()
writeLines(unique(c(existing, gitignore_lines)), ".gitignore")

# ---- 4. README stub ----
if (!file.exists("README.md")) {
  writeLines(c(
    "# BC fish stock status: an exploratory rapid assessment toolkit",
    "",
    "A personal, exploratory project that compiles open data on the status of",
    "selected British Columbia fish and shellfish stocks, and produces",
    "standardised plots and short summary reports in R and Quarto.",
    "",
    "**Disclaimer.** This is not an official assessment. It does not replicate",
    "the methodology of any seafood recommendation program and makes no",
    "recommendations about what to buy or eat. Stock status information comes",
    "from Fisheries and Oceans Canada (DFO) and the RAM Legacy Stock Assessment",
    "Database, and may lag behind the most recent assessments.",
    "",
    "Work in progress."
  ), "README.md")
}

message("Setup complete. R version: ", R.version.string)
message("Folders: ", paste(dirs, collapse = ", "))