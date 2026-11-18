# 03_explore_stocks.R
# Checks what data each candidate stock has in SSF, RAM Legacy, and landings.

source(here::here("R", "functions.R"))

# ---- 1. SSF 2024: target Pacific stocks ----
cat("\n================ SSF 2024: TARGET STOCKS ================\n")
ssf <- read_ssf(here("data", "raw", "ssf_2024.csv"))

ssf_targets <- ssf |>
  filter(c_region == "Pacific",
         str_detect(a_stock, regex("Sablefish|Halibut|Spot Prawn|Dungeness|Herring|Pacific cod",
                                   ignore_case = TRUE))) |>
  select(
    stock          = a_stock,
    status_zone    = starts_with("x2_1a_"),
    uncertain_why  = starts_with("x2_1b_"),
    how_determined = starts_with("x2_1c_"),
    usr            = starts_with("x1_1c_"),
    lrp            = starts_with("x1_2d_"),
    co_managed     = starts_with("q_is_the_stock_co_managed"),
    co_manager     = starts_with("identify_the_other_jurisdiction"),
    ifmp           = starts_with("x8_1b_"),
    report_link    = starts_with("g_provide_the_link")
  )

# Long format so each field is readable on its own line
ssf_targets |>
  pivot_longer(-stock, names_to = "field", values_to = "value") |>
  mutate(value = str_trunc(str_squish(value), 100)) |>
  filter(nzchar(value)) |>
  group_by(stock) |>
  group_walk(~ {
    cat("\n--", .y$stock, "--\n")
    cat(paste0("  ", .x$field, ": ", .x$value), sep = "\n")
  })

# ---- 2. RAM Legacy: what each candidate stock has ----
cat("\n\n================ RAM LEGACY: DATA AVAILABILITY ================\n")
ram <- load_ram()

candidates <- c("SABLEFPCAN", "PHALNPAC",
                "PCOD5ABCD", "PCOD5AB", "PCODHS", "PCODWCVANI",
                "HERRSOG", "HERRHG", "HERRCC", "HERRPRD", "HERRWCVANI")

cat("\nColumns in the metadata table:\n")
print(names(ram$metadata))

cat("\nAssessment info:\n")
ram$metadata |>
  as_tibble() |>
  filter(stockid %in% candidates) |>
  select(any_of(c("stockid", "assessid", "assessyear", "assessorid", "assessmethod"))) |>
  print(n = Inf, width = 200)

key_cols <- c("BdivBmsypref", "UdivUmsypref", "BdivBmgtpref", "UdivUmgtpref",
              "SSB", "TB", "TC", "TL", "ER", "F")

ts <- ram$timeseries_values_views |>
  as_tibble() |>
  filter(stockid %in% candidates) |>
  select(stockid, year, any_of(key_cols))

cat("\nYears with data, by stock and metric (blank = no data):\n")
ts |>
  pivot_longer(-c(stockid, year), names_to = "metric", values_to = "value") |>
  filter(!is.na(value)) |>
  group_by(stockid, metric) |>
  summarise(years = paste0(min(year), "-", max(year)), .groups = "drop") |>
  pivot_wider(names_from = metric, values_from = years) |>
  select(stockid, any_of(key_cols)) |>
  print(n = Inf, width = Inf)

# ---- 3. Landings: Pacific species list ----
cat("\n\n================ LANDINGS: PACIFIC ================\n")
land <- read_landings(here("data", "raw", "landings_1990_2024.csv"))

cat("\nValues in the 'province' column:\n")
print(count(land, province), n = Inf)

cat("\nPacific species, 2024 landings (tonnes):\n")
land |>
  filter(str_detect(province, regex("pacific|british columbia", ignore_case = TRUE)),
         year == max(year)) |>
  group_by(species_group, species) |>
  summarise(tonnes = round(sum(quantity_kgs, na.rm = TRUE) / 1000), .groups = "drop") |>
  arrange(desc(tonnes)) |>
  print(n = Inf)