# 08_map_cards.R
# Builds the interactive stock map (with card popups) and a card grid preview.

for (p in c("leaflet", "htmlwidgets")) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}

source(here::here("R", "functions.R"))
source(here::here("R", "plot_functions.R"))
source(here::here("R", "card_functions.R"))
suppressPackageStartupMessages({
  library(leaflet)
  library(htmlwidgets)
})

db_dir     <- here("data", "processed")
stocks     <- read_csv(file.path(db_dir, "stocks.csv"), show_col_types = FALSE)
timeseries <- read_csv(file.path(db_dir, "timeseries.csv"), show_col_types = FALSE)

# Re-make the "no data" panels with the margin fix
for (id in stocks$stock_id[stocks$data_tier == "D"]) {
  ggsave(here("figures", "stocks", paste0(id, "_trend.png")),
         plot_trend(timeseries, stocks, id), width = 9, height = 6, dpi = 300, device = ragg::agg_png)
}

# ---- 1. Card data and popup HTML ----
cards <- make_card_data(stocks, timeseries) |>
  mutate(
    zone  = coalesce(dfo_status_zone, "Not in survey"),
    col   = unname(zone_cols[zone]),
    label = unname(stock_short[stock_id])
  )
cards$popup <- map_chr(seq_len(nrow(cards)), \(i) as.character(make_card(cards[i, ])))

cat("\nData gaps generated for each stock:\n")
for (i in seq_len(nrow(cards))) {
  cat("\n-", cards$stock_id[i], "\n")
  cat(paste0("    * ", make_gaps(cards[i, ])), sep = "\n")
}

# ---- 2. Map ----
title_html <- paste0(
  "<div style='background:white;padding:8px 12px;border:2px solid #111;",
  "box-shadow:3px 3px 0 #111;font-family:\"Bricolage Grotesque\",sans-serif;max-width:260px;'>",
  "<div style='font-weight:800;font-size:15px;'>BC fish stock status</div>",
  "<div style='font-size:11.5px;color:#5A544A;margin-top:2px;'>Click a pin to open its card. ",
  "Pins mark general areas, not stock boundaries.</div></div>"
)

m <- leaflet(cards, options = leafletOptions(minZoom = 4)) |>
  addProviderTiles(providers$Esri.OceanBasemap, group = "Ocean") |>
  addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Light") |>   # CARTO now needs an API key
  fitBounds(lng1 = -133.4, lat1 = 48.4, lng2 = -122.8, lat2 = 55.0) |>
  addCircleMarkers(
    lng = ~lng, lat = ~lat, radius = 11,
    color = "#111111", weight = 2, fillColor = ~col, fillOpacity = 0.95,
    label = ~label, popup = ~popup,
    popupOptions = popupOptions(maxWidth = 320, minWidth = 290, maxHeight = 440,
                                keepInView = TRUE, autoPanPadding = c(40, 40))
    ) |>
  addLegend(
    position = "bottomright",
    colors = unname(zone_cols[c("Healthy", "Cautious", "Critical", "Not in survey")]),
    labels = c("Healthy", "Cautious", "Critical", "Not in DFO 2024 survey"),
    title = "DFO 2024 status zone", opacity = 0.95   # match the pins
  ) |>
  addLayersControl(baseGroups = c("Ocean", "Light"),
                   options = layersControlOptions(collapsed = TRUE)) |>
  addControl(html = title_html, position = "topleft") |>
  addScaleBar(position = "bottomleft") |>
  addEasyButton(easyButton(
    icon    = "fa-home",
    title   = "Reset view to the whole BC coast",
    onClick = JS("function(btn, map){ map.fitBounds([[48.4, -133.4], [55.0, -122.8]]); }")
  ))

m <- prependContent(m, tags$style(HTML(card_css)))
saveWidget(m, here("figures", "stock_map.html"), selfcontained = TRUE,
           title = "BC fish stock status map")
print(m)

# ---- 3. Card grid preview ----
grid_page <- tagList(
  tags$style(HTML(card_css)),
  tags$style(HTML("body{background:#F3EEE3;padding:24px;font-family:'Bricolage Grotesque',sans-serif;}")),
  tags$h2("BC fish stock cards"),
  div(class = "card-grid", lapply(seq_len(nrow(cards)), \(i) make_card(cards[i, ])))
)
save_html(grid_page, here("figures", "cards_preview.html"))

cat("\nSaved: figures/stock_map.html and figures/cards_preview.html\n")