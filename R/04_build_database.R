# 04_build_database.R
# Builds the tidy stock status database in data/processed/.

source(here::here("R", "functions.R"))

ram  <- load_ram()
ssf  <- read_ssf(here("data", "raw", "ssf_2024.csv"))
land <- read_landings(here("data", "raw", "landings_1990_2024.csv"))

out_dir <- here("data", "processed")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- 1. Hand-curated stock list ----
tier_labels <- c(
  A = "Biomass and fishing pressure relative to MSY",
  B = "Biomass relative to MSY only",
  C = "Biomass and catch trends, no MSY ratios",
  D = "DFO status zone only, no assessment time series compiled"
)

stock_lookup <- tribble(
  ~stock_id,      ~species,          ~stock_name,                                               ~ssf_stock,                                           ~ram_stockid,  ~data_tier,
  "sablefish",    "Sablefish",       "Sablefish, BC coast",                                      "Sablefish",                                          "SABLEFPCAN",  "A",
  "halibut",      "Pacific halibut", "Pacific halibut, coastwide (Canada and US)",               "Pacific Halibut",                                    "PHALNPAC",    "A",
  "pcod_5abcd",   "Pacific cod",     "Pacific cod, Queen Charlotte Sound and Hecate Strait",     NA,                                                   "PCOD5ABCD",   "B",
  "herring_sog",  "Pacific herring", "Pacific herring, Strait of Georgia",                       "Pacific Herring - Strait of Georgia",                "HERRSOG",     "C",
  "herring_wcvi", "Pacific herring", "Pacific herring, West Coast of Vancouver Island",          "Pacific Herring - West Coast of Vancouver Island",   "HERRWCVANI",  "C",
  "herring_cc",   "Pacific herring", "Pacific herring, Central Coast",                           "Pacific Herring - Central Coast",                    "HERRCC",      "C",
  "herring_prd",  "Pacific herring", "Pacific herring, Prince Rupert District",                  "Pacific Herring - Prince Rupert District",           "HERRPRD",     "C",
  "herring_hg",   "Pacific herring", "Pacific herring, Haida Gwaii",                             "Pacific Herring - Haida Gwaii",                      "HERRHG",      "C",
  "spot_prawn",   "Spot prawn",      "Spot prawn, BC coast",                                     "Spot Prawn",                                         NA,            "D",
  "dungeness",    "Dungeness crab",  "Dungeness crab, BC coast",                                 "Dungeness Crab",                                     NA,            "D"
)

# ---- 2. SSF 2024 fields for each stock ----
ssf_fields <- ssf |>
  filter(c_region == "Pacific") |>
  select(
    ssf_stock            = a_stock,
    dfo_status_zone      = starts_with("x2_1a_"),
    status_method        = starts_with("x2_1c_"),
    usr_description      = starts_with("x1_1c_"),
    lrp_description      = starts_with("x1_2d_"),
    co_managed           = starts_with("q_is_the_stock_co_managed"),
    co_manager           = starts_with("identify_the_other_jurisdiction"),
    ifmp                 = starts_with("x8_1b_"),
    science_report_links = starts_with("g_provide_the_link")
  ) |>
  mutate(
    across(everything(), ~ na_if(str_squish(.x), "")),
    dfo_status_zone = str_remove(dfo_status_zone, " Zone$")
  )

# ---- 3. RAM assessment metadata ----
ram_meta <- ram$metadata |>
  as_tibble() |>
  select(ram_stockid = stockid, ram_assessid = assessid,
         ram_assess_years = assessyear,
         management_authority = managementauthority) |>
  mutate(ram_last_year = as.integer(str_extract(ram_assess_years, "\\d{4}$")))

# ---- 4. stocks table ----
stocks <- stock_lookup |>
  left_join(ssf_fields, by = "ssf_stock") |>
  left_join(ram_meta, by = "ram_stockid") |>
  mutate(
    data_tier_label = unname(tier_labels[data_tier]),
    ssf_survey_year = if_else(is.na(dfo_status_zone), NA_integer_, 2024L)
  ) |>
  relocate(data_tier_label, .after = data_tier)

unmatched <- stocks |> filter(!is.na(ssf_stock), is.na(dfo_status_zone))
if (nrow(unmatched) > 0) {
  warning("SSF stocks not matched: ", paste(unmatched$ssf_stock, collapse = "; "))
}

# ---- 5. timeseries table (RAM Legacy) ----
metric_map <- c(
  b_bmsy = "BdivBmsypref", u_umsy = "UdivUmsypref",
  ssb = "SSB", tb = "TB", total_catch = "TC", landings = "TL",
  exploitation_rate = "ER", f = "F"
)
ram_ids <- stocks$ram_stockid[!is.na(stocks$ram_stockid)]

ts_long <- ram$timeseries_values_views |>
  as_tibble() |>
  filter(stockid %in% ram_ids) |>
  select(ram_stockid = stockid, year, any_of(metric_map)) |>
  pivot_longer(-c(ram_stockid, year), names_to = "metric", values_to = "value") |>
  filter(!is.na(value)) |>
  mutate(year = as.integer(year), value = as.numeric(value))

ts_units <- ram$timeseries_units_views |>
  as_tibble() |>
  filter(stockid %in% ram_ids) |>
  select(ram_stockid = stockid, any_of(metric_map)) |>
  mutate(across(-ram_stockid, as.character)) |>
  pivot_longer(-ram_stockid, names_to = "metric", values_to = "unit") |>
  distinct(ram_stockid, metric, .keep_all = TRUE)

timeseries <- ts_long |>
  left_join(ts_units, by = c("ram_stockid", "metric")) |>
  left_join(select(stocks, stock_id, ram_stockid), by = "ram_stockid") |>
  mutate(
    unit   = if_else(metric %in% c("b_bmsy", "u_umsy"), "ratio", unit),
    source = "RAM Legacy v4.66, assessment-only"
  ) |>
  select(stock_id, ram_stockid, year, metric, value, unit, source) |>
  arrange(stock_id, metric, year)

# ---- 6. landings table (DFO, British Columbia) ----
landings_patterns <- tribble(
  ~species,          ~pattern,
  "Sablefish",       "sablefish|black ?cod",
  "Pacific halibut", "halibut",
  "Pacific cod",     "^cod|pacific cod|grey cod|cod, pacific",
  "Pacific herring", "herring",
  "Spot prawn",      "prawn",
  "Dungeness crab",  "dungeness"
)

bc_land    <- land |> filter(province == "British Columbia")
bc_species <- sort(unique(bc_land$species))

species_match <- landings_patterns |>
  mutate(dfo_species = map(pattern, ~ bc_species[str_detect(bc_species, regex(.x, ignore_case = TRUE))])) |>
  unnest(dfo_species, keep_empty = TRUE) |>
  select(-pattern)

landings <- species_match |>
  filter(!is.na(dfo_species)) |>
  inner_join(bc_land, by = c("dfo_species" = "species"), relationship = "many-to-many") |>
  group_by(species, dfo_species, year) |>
  summarise(
    landings_t = if (all(is.na(quantity_kgs))) NA_real_ else sum(quantity_kgs, na.rm = TRUE) / 1000,
    value_cad  = if (all(is.na(value_cad)))    NA_real_ else sum(value_cad,    na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(source = "DFO Commercial Seafisheries Landings, 1990 to 2024")

# ---- 7. data dictionary ----
data_dictionary <- tribble(
  ~table, ~field, ~type, ~description, ~source,
  "stocks", "stock_id", "text", "Short project identifier for the stock", "Project",
  "stocks", "species", "text", "Common name of the species", "Project",
  "stocks", "stock_name", "text", "Plain-language name of the stock and area", "Project",
  "stocks", "ssf_stock", "text", "Stock name as written in the DFO Sustainability Survey for Fisheries", "DFO SSF 2024",
  "stocks", "ram_stockid", "text", "Stock identifier in the RAM Legacy database", "RAM Legacy v4.66",
  "stocks", "data_tier", "text", "Evidence tier used in this project (A to D)", "Project",
  "stocks", "data_tier_label", "text", "Plain-language description of the data tier", "Project",
  "stocks", "dfo_status_zone", "text", "DFO Precautionary Approach zone: Healthy, Cautious, Critical, or Uncertain", "DFO SSF 2024",
  "stocks", "status_method", "text", "How DFO determined the status zone", "DFO SSF 2024",
  "stocks", "usr_description", "text", "DFO description of the Upper Stock Reference point", "DFO SSF 2024",
  "stocks", "lrp_description", "text", "DFO description of the Limit Reference Point", "DFO SSF 2024",
  "stocks", "co_managed", "text", "Whether the stock is co-managed with another jurisdiction", "DFO SSF 2024",
  "stocks", "co_manager", "text", "Name of the co-managing jurisdiction, if any", "DFO SSF 2024",
  "stocks", "ifmp", "text", "Integrated Fisheries Management Plan in effect at the time of the survey", "DFO SSF 2024",
  "stocks", "science_report_links", "text", "Links to the scientific reports DFO cited", "DFO SSF 2024",
  "stocks", "ram_assessid", "text", "RAM Legacy assessment identifier", "RAM Legacy v4.66",
  "stocks", "ram_assess_years", "text", "Year range covered by the assessment in RAM Legacy", "RAM Legacy v4.66",
  "stocks", "management_authority", "text", "Management authority recorded in RAM Legacy", "RAM Legacy v4.66",
  "stocks", "ram_last_year", "integer", "Last year covered by the RAM Legacy assessment", "Derived",
  "stocks", "ssf_survey_year", "integer", "Survey year for the DFO status zone. Surveys generally report on the previous year's data", "Derived",
  "timeseries", "stock_id", "text", "Project stock identifier (joins to stocks)", "Project",
  "timeseries", "ram_stockid", "text", "RAM Legacy stock identifier", "RAM Legacy v4.66",
  "timeseries", "year", "integer", "Year", "RAM Legacy v4.66",
  "timeseries", "metric", "text", "b_bmsy: biomass / biomass at MSY; u_umsy: fishing pressure / fishing pressure at MSY (F or exploitation rate); ssb: spawning stock biomass; tb: total biomass; total_catch: total catch; landings: total landings; exploitation_rate: fraction of the stock caught; f: instantaneous fishing mortality", "RAM Legacy v4.66",
  "timeseries", "value", "numeric", "Value of the metric", "RAM Legacy v4.66",
  "timeseries", "unit", "text", "Unit of the metric as recorded in RAM Legacy", "RAM Legacy v4.66",
  "timeseries", "source", "text", "Data source and version", "Project",
  "landings", "species", "text", "Project species name (joins to stocks$species)", "Project",
  "landings", "dfo_species", "text", "Species label used in the DFO landings file", "DFO landings",
  "landings", "year", "integer", "Calendar year", "DFO landings",
  "landings", "landings_t", "numeric", "Commercial landings in BC, in tonnes (live weight). Species level, not stock level", "DFO landings",
  "landings", "value_cad", "numeric", "Landed value in Canadian dollars, not adjusted for inflation", "DFO landings",
  "landings", "source", "text", "Data source", "Project"
)

# ---- 8. Save ----
write_csv(stocks,          file.path(out_dir, "stocks.csv"), na = "")
write_csv(timeseries,      file.path(out_dir, "timeseries.csv"), na = "")
write_csv(landings,        file.path(out_dir, "landings.csv"), na = "")
write_csv(data_dictionary, file.path(out_dir, "data_dictionary.csv"), na = "")
saveRDS(list(stocks = stocks, timeseries = timeseries,
             landings = landings, data_dictionary = data_dictionary),
        file.path(out_dir, "bc_stock_status_db.rds"))

# ---- 9. Checks ----
cat("\n==== stocks ====\n")
stocks |>
  select(stock_id, data_tier, dfo_status_zone, ram_last_year, management_authority) |>
  print(n = Inf, width = 200)

cat("\n==== timeseries: year range by stock and metric ====\n")
timeseries |>
  group_by(stock_id, metric) |>
  summarise(years = paste0(min(year), "-", max(year)), unit = first(unit), .groups = "drop") |>
  print(n = Inf, width = 200)

cat("\n==== landings: species matches ====\n")
print(species_match, n = Inf)

cat("\n==== All BC species labels in the landings file (to check the matches) ====\n")
cat(paste(bc_species, collapse = " | "), "\n")

cat("\n==== RAM reference points available (bioparams) ====\n")
ram$bioparams_values_views |>
  as_tibble() |>
  filter(stockid %in% ram_ids) |>
  select(-any_of("stocklong")) |>
  mutate(across(-stockid, as.character)) |>
  pivot_longer(-stockid, names_to = "param", values_to = "value") |>
  filter(!is.na(value)) |>
  group_by(stockid) |>
  summarise(params = paste(param, collapse = ", ")) |>
  print(n = Inf, width = 200)

cat("\nFiles written to data/processed/:\n")
print(list.files(out_dir))