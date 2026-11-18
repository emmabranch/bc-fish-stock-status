# 05_herring_stripes.R
# "Biomass stripes" for five BC Pacific herring stocks:
# spawning biomass relative to the limit reference point (LRP = 0.3 x SSB0).

source(here::here("R", "functions.R"))
suppressPackageStartupMessages({
  library(plotly)
  library(scales)
  library(glue)
})

ram        <- load_ram()
stocks     <- read_csv(here("data", "processed", "stocks.csv"), show_col_types = FALSE)
timeseries <- read_csv(here("data", "processed", "timeseries.csv"), show_col_types = FALSE)

herring <- stocks |>
  filter(species == "Pacific herring") |>
  select(stock_id, ram_stockid, dfo_status_zone)

# ---- 1. Unfished spawning biomass (SSB0) from RAM ----
ssb0 <- ram$bioparams_values_views |>
  as_tibble() |>
  filter(stockid %in% herring$ram_stockid) |>
  transmute(ram_stockid = stockid, ssb0 = as.numeric(SSB0))

ssb0_units <- ram$bioparams_units_views |>
  as_tibble() |>
  filter(stockid %in% herring$ram_stockid) |>
  select(ram_stockid = stockid, any_of(c(ssb0_unit = "SSB0")))

cat("\nSSB0 values from RAM (check units match SSB, which is MT):\n")
print(left_join(ssb0, ssb0_units, by = "ram_stockid"))

# ---- 2. Build the stripes data ----
area_labels <- c(
  herring_hg   = "Haida Gwaii",
  herring_prd  = "Prince Rupert District",
  herring_cc   = "Central Coast",
  herring_wcvi = "West Coast Vancouver Island",
  herring_sog  = "Strait of Georgia"
)

stripes <- timeseries |>
  filter(metric == "ssb", stock_id %in% herring$stock_id) |>
  left_join(herring, by = c("stock_id", "ram_stockid")) |>
  left_join(ssb0, by = "ram_stockid") |>
  mutate(
    lrp_t     = 0.3 * ssb0,
    ratio_lrp = value / lrp_t,
    area      = factor(unname(area_labels[stock_id]), levels = rev(area_labels)),
    hover     = glue("{area}, {year}<br>Spawning biomass: {comma(round(value))} t<br>{number(ratio_lrp, accuracy = 0.1)} \u00d7 LRP")
  ) |>
  arrange(stock_id, year)

write_csv(
  stripes |> select(stock_id, year, ssb_t = value, ssb0_t = ssb0, lrp_t, ratio_lrp),
  here("data", "processed", "herring_ssb_lrp.csv")
)

# ---- 3. Static infographic ----
zone_cols <- c(Healthy = "#1a7f4b", Cautious = "#d98c00", Critical = "#c0392b", Uncertain = "grey50")
fill_scale <- scale_fill_gradient2(
  low = "#b2182b", mid = "#f7f7f7", high = "#2166ac", midpoint = 0,
  limits = c(-2, 2), oob = squish,
  breaks = c(-2, -1, 0, 1, 2),
  labels = c("\u00bc\u00d7", "\u00bd\u00d7", "LRP", "2\u00d7", "4\u00d7"),
  name = "Spawning biomass relative to the limit reference point"
)

badges <- stripes |> distinct(area, dfo_status_zone)

p <- ggplot(stripes, aes(x = year, y = area, fill = log2(ratio_lrp))) +
  geom_tile(height = 0.92, width = 1) +
  geom_label(
    data = badges,
    aes(x = 2020, y = area, label = paste0("DFO 2024: ", dfo_status_zone), colour = dfo_status_zone),
    inherit.aes = FALSE, hjust = 0, size = 3.3, fontface = "bold", fill = "white"
  ) +
  scale_colour_manual(values = zone_cols, guide = "none") +
  fill_scale +
  scale_x_continuous(breaks = seq(1950, 2010, 10), limits = c(1950.5, 2033), expand = c(0, 0)) +
  labs(
    title    = "Seventy years of Pacific herring in British Columbia",
    subtitle = "Each stripe is one year of estimated spawning biomass, coloured relative to the limit reference point (0.3 \u00d7 unfished spawning biomass).\nRed years are below the limit. Badges show DFO's 2024 status zone for comparison.",
    x = NULL, y = NULL,
    caption  = "Data: RAM Legacy Stock Assessment Database v4.66 (DFO assessments to 2018); DFO Sustainability Survey for Fisheries 2024.\nUnfished biomass is from the 2018 assessment and is re-estimated by DFO each year. Exploratory personal project, not an official assessment."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid       = element_blank(),
    axis.text.y      = element_text(face = "bold", size = 11),
    legend.position  = "bottom",
    legend.title.position = "top",
    legend.key.width = unit(2.5, "cm"),
    plot.title       = element_text(face = "bold", size = 17),
    plot.subtitle    = element_text(colour = "grey30"),
    plot.caption     = element_text(colour = "grey45", hjust = 0, size = 8.5),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  )

ggsave(here("figures", "herring_stripes.png"), p, width = 12, height = 5.5, dpi = 300, bg = "white")
print(p)

# ---- 4. Interactive version (hover for values) ----
p_int <- ggplot(stripes, aes(x = year, y = area, fill = log2(ratio_lrp), text = hover)) +
  geom_tile(height = 0.92, width = 1) +
  fill_scale +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(panel.grid = element_blank(), legend.position = "none")

p_plotly <- ggplotly(p_int, tooltip = "text") |>
  config(displayModeBar = FALSE)
htmlwidgets::saveWidget(p_plotly, here("figures", "herring_stripes.html"), selfcontained = TRUE)
p_plotly

# ---- 5. Fun facts for the story page ----
cat("\nFun facts by area:\n")
stripes |>
  group_by(area) |>
  summarise(
    years            = n(),
    years_below_lrp  = sum(ratio_lrp < 1),
    lowest_year      = year[which.min(ratio_lrp)],
    lowest_ratio     = round(min(ratio_lrp), 2),
    highest_year     = year[which.max(ratio_lrp)],
    ratio_2018       = round(ratio_lrp[year == max(year)], 2)
  ) |>
  arrange(desc(area)) |>
  print(width = 200)