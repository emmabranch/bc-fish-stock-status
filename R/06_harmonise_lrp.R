# 06_harmonise_lrp.R
# Adds a harmonised indicator to the database: biomass relative to each
# stock's limit reference point (b_lrp). Below 1 = below the LRP.

source(here::here("R", "functions.R"))

ram    <- load_ram()
db_dir <- here("data", "processed")

stocks     <- read_csv(file.path(db_dir, "stocks.csv"), show_col_types = FALSE)
timeseries <- read_csv(file.path(db_dir, "timeseries.csv"), show_col_types = FALSE) |>
  filter(metric != "b_lrp")                      # makes the script safe to re-run
landings   <- read_csv(file.path(db_dir, "landings.csv"), show_col_types = FALSE)
dd         <- read_csv(file.path(db_dir, "data_dictionary.csv"), show_col_types = FALSE)

# ---- 1. Reference points stored in RAM ----
ram_ids <- stocks$ram_stockid[!is.na(stocks$ram_stockid)]

bp <- ram$bioparams_values_views |>
  as_tibble() |>
  filter(stockid %in% ram_ids) |>
  select(ram_stockid = stockid, any_of(c("SSB0", "SSBlim", "TBlim", "TB0", "SSBmsy", "TBmsy"))) |>
  mutate(across(-ram_stockid, ~ suppressWarnings(as.numeric(.x))))

cat("\nReference point values stored in RAM (tonnes):\n")
print(bp, width = Inf)

# ---- 2. How the LRP is defined for each stock ----
herring_basis <- "0.3 x unfished spawning biomass (SSB0 from the 2018 assessment in RAM)"

lrp_rules <- tribble(
  ~stock_id,      ~biomass_metric, ~lrp_rule,    ~lrp_basis,
  "sablefish",    "b_bmsy",        "0.4 x Bmsy", "0.4 x Bmsy (DFO default proxy; the 2024 survey reports this LRP for sablefish)",
  "halibut",      "ssb",           "SSBlim",     "SSBlim from RAM (IPHC limit; the 2024 survey describes the LRP as 0.2 x unfished female spawning biomass)",
  "pcod_5abcd",   "tb",            "TBlim",      "TBlim from RAM (limit reference point used in the assessment)",
  "herring_sog",  "ssb",           "0.3 x SSB0", herring_basis,
  "herring_wcvi", "ssb",           "0.3 x SSB0", herring_basis,
  "herring_cc",   "ssb",           "0.3 x SSB0", herring_basis,
  "herring_prd",  "ssb",           "0.3 x SSB0", herring_basis,
  "herring_hg",   "ssb",           "0.3 x SSB0", herring_basis
)

lrp_vals <- lrp_rules |>
  left_join(select(stocks, stock_id, ram_stockid), by = "stock_id") |>
  left_join(bp, by = "ram_stockid") |>
  mutate(
    lrp_value = case_when(
      lrp_rule == "0.4 x Bmsy" ~ 0.4,
      lrp_rule == "SSBlim"     ~ SSBlim,
      lrp_rule == "TBlim"      ~ TBlim,
      lrp_rule == "0.3 x SSB0" ~ 0.3 * SSB0
    ),
    lrp_unit = if_else(lrp_rule == "0.4 x Bmsy", "ratio to Bmsy", "MT")
  )

cat("\nLRP used for each stock, next to DFO's own description:\n")
lrp_vals |>
  select(stock_id, lrp_rule, lrp_value, lrp_unit) |>
  left_join(select(stocks, stock_id, lrp_description), by = "stock_id") |>
  mutate(lrp_value = round(lrp_value, 2),
         lrp_description = str_trunc(lrp_description, 70)) |>
  print(n = Inf, width = 250)

if (any(is.na(lrp_vals$lrp_value))) {
  warning("Missing LRP for: ", paste(lrp_vals$stock_id[is.na(lrp_vals$lrp_value)], collapse = ", "))
}

# ---- 3. Derived time series: biomass / LRP ----
b_lrp <- timeseries |>
  inner_join(select(lrp_vals, stock_id, biomass_metric, lrp_value),
             by = c("stock_id", "metric" = "biomass_metric")) |>
  transmute(
    stock_id, ram_stockid, year,
    metric = "b_lrp",
    value  = value / lrp_value,
    unit   = "ratio",
    source = "Derived: biomass divided by limit reference point (see stocks$lrp_basis)"
  )

timeseries <- bind_rows(timeseries, b_lrp) |> arrange(stock_id, metric, year)

# ---- 4. Does RAM's latest value agree with DFO's 2024 zone? ----
latest <- b_lrp |>
  group_by(stock_id) |>
  slice_max(year, n = 1) |>
  ungroup() |>
  left_join(select(stocks, stock_id, dfo_status_zone), by = "stock_id") |>
  mutate(
    b_lrp    = round(value, 2),
    ram_says = if_else(value < 1, "Below LRP", "Above LRP"),
    agrees   = case_when(
      is.na(dfo_status_zone)                                    ~ "No DFO zone to compare",
      value <  1 & dfo_status_zone == "Critical"                ~ "Yes",
      value >= 1 & dfo_status_zone %in% c("Cautious", "Healthy") ~ "Yes",
      TRUE                                                       ~ "No: check"
    )
  )

cat("\nLatest RAM value versus DFO 2024 zone:\n")
latest |>
  select(stock_id, year, b_lrp, ram_says, dfo_status_zone, agrees) |>
  print(n = Inf)

# ---- 5. Update stocks and the data dictionary ----
stocks <- stocks |>
  select(-any_of(c("lrp_basis", "lrp_value", "lrp_unit"))) |>
  left_join(select(lrp_vals, stock_id, lrp_basis, lrp_value, lrp_unit), by = "stock_id")

dd <- dd |>
  mutate(description = if_else(
    table == "timeseries" & field == "metric" & !str_detect(description, "b_lrp"),
    paste0(description, "; b_lrp: biomass divided by the limit reference point (derived, approximate)"),
    description
  )) |>
  bind_rows(tribble(
    ~table,   ~field,      ~type,     ~description,                                                          ~source,
    "stocks", "lrp_basis", "text",    "How the limit reference point used for b_lrp was defined, and where it came from", "Project",
    "stocks", "lrp_value", "numeric", "Value of the limit reference point used for b_lrp",                     "RAM Legacy v4.66 / derived",
    "stocks", "lrp_unit",  "text",    "Unit of lrp_value (MT = metric tonnes)",                                 "Project"
  )) |>
  distinct(table, field, .keep_all = TRUE)

# ---- 6. Save ----
write_csv(stocks,     file.path(db_dir, "stocks.csv"), na = "")
write_csv(timeseries, file.path(db_dir, "timeseries.csv"), na = "")
write_csv(dd,         file.path(db_dir, "data_dictionary.csv"), na = "")
saveRDS(list(stocks = stocks, timeseries = timeseries,
             landings = landings, data_dictionary = dd),
        file.path(db_dir, "bc_stock_status_db.rds"))

# ---- 7. Quick check plot ----
p_check <- b_lrp |>
  ggplot(aes(year, value)) +
  geom_hline(yintercept = 1, colour = "#c0392b", linetype = "dashed") +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ stock_id, ncol = 2, scales = "free_x") +
  scale_y_log10() +
  labs(title = "Check: biomass relative to the limit reference point",
       subtitle = "Dashed red line = LRP. Log scale.", x = NULL, y = "Biomass / LRP") +
  theme_minimal()
print(p_check)

cat("\nDone. b_lrp rows added:", nrow(b_lrp), "\n")