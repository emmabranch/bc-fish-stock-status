# card_functions.R
# Builds the "trading card" HTML for each stock. Used by the map and the website.
# Requires plot_functions.R to be loaded first (for zone_cols, species_cols, zone_label).

suppressPackageStartupMessages({
  library(htmltools)
})

# ---- Editable extras: map positions and fun facts ----
# Pins mark the general area of each stock, not stock boundaries.
stock_extras <- tribble(
  ~stock_id,      ~lat,   ~lng,    ~location_note,                                 ~fun_fact,
  "sablefish",    51.30, -131.40, "Coastwide stock; pin position is illustrative.", "Often sold as 'black cod', although it is not a true cod.",
  "halibut",      50.20, -129.60, "Coastwide stock; pin position is illustrative.", "The largest flatfish in the North Pacific. As it grows, both eyes end up on one side of its head.",
  "pcod_5abcd",   52.90, -130.60, NA,                                               "Also known as grey cod.",
  "herring_hg",   52.55, -131.35, NA,                                               "Herring spawn in shallow coastal water, laying sticky eggs on kelp and eelgrass.",
  "herring_prd",  54.25, -130.55, NA,                                               "Herring spawn in shallow coastal water, laying sticky eggs on kelp and eelgrass.",
  "herring_cc",   52.10, -128.50, NA,                                               "Herring spawn in shallow coastal water, laying sticky eggs on kelp and eelgrass.",
  "herring_wcvi", 49.10, -126.10, NA,                                               "Herring spawn in shallow coastal water, laying sticky eggs on kelp and eelgrass.",
  "herring_sog",  49.45, -124.55, NA,                                               "Herring spawn in shallow coastal water, laying sticky eggs on kelp and eelgrass.",
  "spot_prawn",   50.42, -125.90, "Coastwide stock; pin position is illustrative.", "Spot prawns start life as males and later change to females.",
  "dungeness",    54.05, -132.40, "Coastwide stock; pin position is illustrative.", "Named after Dungeness, a small town in Washington State."
)

# ---- Card CSS (shared by the map and the website) ----
# The site's cards.css is written from this by R/10_make_site_files.R, so edit here
card_css <- "
.stock-card{font-family:'Bricolage Grotesque',system-ui,sans-serif;
  width:290px;border-radius:0;overflow:hidden;border:2px solid #111;box-shadow:5px 5px 0 #111;
  background:#fff;color:#111;}
.card-grid .stock-card{width:auto;}
.leaflet-popup-content .stock-card{box-shadow:none;border:none;width:290px;}
.leaflet-popup-content{margin:0 !important;}
.leaflet-popup-scrolled{border:none !important;scrollbar-width:thin;}
.leaflet-popup-content-wrapper{padding:0 !important;border-radius:0 !important;border:2px solid #111;box-shadow:4px 4px 0 #111 !important;overflow:hidden;}
.stock-card .card-head{padding:10px 14px;border-bottom:2px solid #111;}
.stock-card .card-species{font-family:'Space Mono',monospace;font-size:10.5px;letter-spacing:.08em;text-transform:uppercase;opacity:.85;}
.stock-card .card-title{font-size:16px;font-weight:700;line-height:1.2;margin-top:2px;}
.stock-card .card-body{padding:10px 14px 12px;font-size:12.5px;line-height:1.4;}
.stock-card .badges{display:flex;gap:6px;flex-wrap:wrap;margin-bottom:8px;}
.stock-card .badge{display:inline-block;padding:2px 8px;border-radius:0;border:1.5px solid #111;font-size:11px;font-weight:700;}
.stock-card .badge.tier{background:#F3EEE3;color:#111;}
.stock-card .stat{font-size:24px;font-weight:800;line-height:1.1;}
.stock-card .stat-label{font-size:11px;color:#5A544A;}
.stock-card .spark{width:100%;height:auto;margin:4px 0 6px;}
.stock-card dl{margin:8px 0 6px;display:grid;grid-template-columns:auto 1fr;gap:3px 10px;}
.stock-card dt{font-family:'Space Mono',monospace;color:#5A544A;font-size:10.5px;}
.stock-card dd{margin:0;font-size:11.5px;}
.stock-card .gaps{background:#FFF4CC;border-left:4px solid #FFD23F;padding:6px 8px;margin-top:6px;font-size:11.5px;}
.stock-card .gaps ul{margin:3px 0 0 16px;padding:0;}
.stock-card .fun{margin-top:8px;font-size:11.5px;color:#5A544A;font-style:italic;}
.stock-card .note{margin-top:6px;font-size:10.5px;color:#5A544A;}
.stock-card a.more{display:inline-block;margin-top:8px;font-weight:700;font-size:12px;color:#2340FF;}
.card-grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(290px,1fr));gap:18px;}
"

# ---- Data for the cards ----
make_card_data <- function(stocks, timeseries) {
  latest_b <- timeseries |>
    filter(metric == "b_lrp") |>
    group_by(stock_id) |>
    slice_max(year, n = 1) |>
    ungroup() |>
    select(stock_id, latest_year = year, latest_b_lrp = value)
  
  # First and last year with a biomass estimate (can differ from RAM's assessment period)
  b_span <- timeseries |>
    filter(metric == "b_lrp", !is.na(value)) |>
    group_by(stock_id) |>
    summarise(b_first = min(year), b_last = max(year), .groups = "drop")

  u_last <- timeseries |>
    filter(metric == "u_umsy") |>
    group_by(stock_id) |>
    summarise(u_last = max(year), .groups = "drop")
  
  stocks |>
    left_join(latest_b, by = "stock_id") |>
    left_join(b_span, by = "stock_id") |>
    left_join(u_last, by = "stock_id") |>
    left_join(stock_extras, by = "stock_id") |>
    mutate(
      manager = case_when(
        co_managed == "Yes" ~ paste0("DFO, co-managed with ", str_remove(co_manager, "\\s*\\(http.*\\)")),
        TRUE ~ "Fisheries and Oceans Canada (DFO)"
      )
    )
}

# ---- Rule-based data gaps ----
make_gaps <- function(s) {
  gaps <- character()
  if (is.na(s$dfo_status_zone)) {
    gaps <- c(gaps, "Not included in DFO's 2024 Sustainability Survey, so there is no official status zone here.")
  }
  if (s$data_tier == "D") {
    gaps <- c(gaps, "No public assessment time series compiled in this project; status relies on DFO's survey response.")
  }
  if (!is.na(s$latest_year) && !is.na(s$dfo_status_zone) && s$latest_year < 2024) {
    gaps <- c(gaps, glue("Time series ends in {s$latest_year}; the DFO zone is from the 2024 survey."))
  }
  if (s$data_tier %in% c("B", "C")) {
    gaps <- c(gaps, "No fishing-pressure ratio relative to MSY, so no Kobe plot.")
  }
  if (!is.na(s$u_last) && !is.na(s$latest_year) && s$u_last < s$latest_year - 2) {
    gaps <- c(gaps, glue("Fishing-pressure ratio available only to {s$u_last}."))
  }
  if (!is.na(s$status_method) && str_detect(s$status_method, regex("expert", ignore_case = TRUE))) {
    gaps <- c(gaps, "DFO status based on expert judgement rather than a model-based assessment.")
  }
  if (!is.na(s$lrp_basis) && str_detect(s$lrp_basis, "RAM")) {
    gaps <- c(gaps, "Limit reference point taken or derived from RAM Legacy; DFO's current value may differ.")
  }
  if (length(gaps) == 0) gaps <- "No major gaps flagged by this project's rules."
  gaps
}

# ---- One card ----
make_card <- function(s, link = NULL, spark_dir = here::here("figures", "sparklines"), show_gaps = TRUE) {
  zone <- coalesce(s$dfo_status_zone, "Not in survey")
  zcol <- zone_cols[[zone]]
  ztxt <- zone_text_on[[zone]]
  scol <- species_cols[[s$species]]
  stxt <- species_text_on[[s$species]]
  spark_path <- file.path(spark_dir, paste0(s$stock_id, ".png"))
  
  stat <- if (!is.na(s$latest_b_lrp)) {
    tagList(
      div(class = "stat", paste0(number(s$latest_b_lrp, accuracy = 0.01), "\u00d7 LRP")),
      div(class = "stat-label", glue("Biomass relative to the limit reference point, {s$latest_year}"))
    )
  } else {
    div(class = "stat-label", "No biomass time series compiled for this stock.")
  }
  
  spark <- if (file.exists(spark_path)) {
    img(class = "spark", src = knitr::image_uri(spark_path),
        alt = paste("Sparkline of biomass relative to the limit reference point for", s$stock_name))
  }
  
  div(
    class = "stock-card",
    div(class = "card-head", style = glue("background:{scol};color:{stxt};"),
        div(class = "card-species", s$species),
        div(class = "card-title", s$stock_name)),
    div(
      class = "card-body",
      div(class = "badges",
          span(class = "badge", style = glue("background:{zcol};color:{ztxt};"), zone_label(s$dfo_status_zone)),
          span(class = "badge tier", glue("Data tier {s$data_tier}"))),
      stat,
      spark,
      tags$dl(
        tags$dt("Evidence"),         tags$dd(s$data_tier_label),
        tags$dt("Managed by"),       tags$dd(s$manager),
        tags$dt("Status method"),    tags$dd(coalesce(s$status_method, "Not reported")),
        tags$dt("RAM assessment period"), tags$dd(coalesce(s$ram_assess_years, "Not in RAM Legacy")),
        tags$dt("Biomass estimates"), tags$dd(if (is.null(s$b_first) || is.na(s$b_first)) "None compiled" else glue("{s$b_first} to {s$b_last}"))
        ),
      if (show_gaps) div(class = "gaps", strong("Data gaps and caveats"),
                         tags$ul(lapply(make_gaps(s), tags$li))),
      
      if (!is.na(s$fun_fact)) div(class = "fun", paste("Fun fact:", s$fun_fact)),
      if (!is.na(s$location_note)) div(class = "note", s$location_note),
      if (!is.null(link)) a(class = "more", href = link, "Full stock page \u2192")
    )
  )
}

# ---- Map (used by the website; links each card to its stock page) ----
make_stock_map <- function(cards, link_prefix = NULL, height = "620px") {
  suppressPackageStartupMessages(library(leaflet))
  
  cards <- cards |>
    mutate(
      zone  = coalesce(dfo_status_zone, "Not in survey"),
      col   = unname(zone_cols[zone]),
      label = unname(stock_short[stock_id])
    )
  cards$popup <- map_chr(seq_len(nrow(cards)), \(i) {
    link <- if (!is.null(link_prefix)) paste0(link_prefix, cards$stock_id[i], ".html") else NULL
    as.character(make_card(cards[i, ], link = link))
  })
  
  title_html <- paste0(
    "<div style='background:white;padding:8px 12px;border:2px solid #111;",
    "box-shadow:3px 3px 0 #111;font-family:\"Bricolage Grotesque\",sans-serif;max-width:250px;'>",
    "<div style='font-weight:800;font-size:14px;'>Click a pin to open its card</div>",
    "<div style='font-size:11.5px;color:#5A544A;margin-top:2px;'>Pins mark general areas, not stock boundaries.</div></div>"
  )
  
  leaflet(cards, height = height, width = "100%", options = leafletOptions(minZoom = 4)) |>
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
}