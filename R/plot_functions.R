# plot_functions.R
# Reusable plotting functions. Load with: source(here::here("R", "plot_functions.R"))

if (!requireNamespace("patchwork", quietly = TRUE)) install.packages("patchwork")
suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(glue)
  library(ggrepel)
  library(plotly)
  library(patchwork)
})

# ---- Shared look ----
zone_cols <- c(
  Healthy = "#1a7f4b", Cautious = "#d98c00", Critical = "#c0392b",
  Uncertain = "#7f8c8d", "Not in survey" = "#8e959c"
)

species_cols <- c(
  "Sablefish"       = "#2c3e50",
  "Pacific halibut" = "#16a085",
  "Pacific cod"     = "#8e44ad",
  "Pacific herring" = "#2471a3",
  "Spot prawn"      = "#e67e22",
  "Dungeness crab"  = "#b03a2e"
)

stock_short <- c(
  sablefish    = "Sablefish",
  halibut      = "Pacific halibut",
  pcod_5abcd   = "Pacific cod (5ABCD)",
  herring_hg   = "Herring: Haida Gwaii",
  herring_prd  = "Herring: Prince Rupert",
  herring_cc   = "Herring: Central Coast",
  herring_wcvi = "Herring: WCVI",
  herring_sog  = "Herring: Strait of Georgia",
  spot_prawn   = "Spot prawn",
  dungeness    = "Dungeness crab"
)

caption_std <- paste(
  "Data: RAM Legacy Stock Assessment Database v4.66; DFO Sustainability Survey for Fisheries 2024.",
  "Exploratory personal project, not an official assessment.",
  sep = "\n"
)

theme_bcstock <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title            = element_text(face = "bold", size = base_size * 1.35),
      plot.subtitle         = element_text(colour = "grey30"),
      plot.caption          = element_text(colour = "grey45", hjust = 0, size = base_size * 0.7),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      panel.grid.minor      = element_blank()
    )
}

fill_lrp <- function(name = "Biomass relative to the limit reference point") {
  scale_fill_gradient2(
    low = "#b2182b", mid = "#f7f7f7", high = "#2166ac", midpoint = 0,
    limits = c(-2, 2), oob = squish, breaks = -2:2,
    labels = c("\u00bc\u00d7", "\u00bd\u00d7", "LRP", "2\u00d7", "4\u00d7"),
    name = name
  )
}

zone_label <- function(zone) {
  if_else(is.na(zone), "Not in DFO 2024 survey", paste0("DFO 2024: ", zone))
}

# ---- 1. Coast-wide stripes ----
plot_stripes <- function(timeseries, stocks, stock_ids = NULL,
                         title = "Biomass relative to the limit reference point, BC stocks") {
  st <- stocks |> filter(!is.na(lrp_value))
  if (!is.null(stock_ids)) st <- st |> filter(stock_id %in% stock_ids)
  lvls <- unname(stock_short[rev(st$stock_id)])
  
  d <- timeseries |>
    filter(metric == "b_lrp", stock_id %in% st$stock_id) |>
    mutate(label = factor(unname(stock_short[stock_id]), levels = lvls))
  
  badges <- st |>
    mutate(
      label = factor(unname(stock_short[stock_id]), levels = lvls),
      zone  = coalesce(dfo_status_zone, "Not in survey"),
      badge = zone_label(dfo_status_zone)
    )
  
  x_end <- max(d$year)
  
  ggplot(d, aes(year, label, fill = log2(value))) +
    geom_tile(height = 0.9, width = 1) +
    geom_label(
      data = badges, aes(x = x_end + 2, y = label, label = badge, colour = zone),
      inherit.aes = FALSE, hjust = 0, size = 3.1, fontface = "bold", fill = "white"
    ) +
    scale_colour_manual(values = zone_cols, guide = "none") +
    fill_lrp() +
    scale_x_continuous(breaks = seq(1950, 2020, 10),
                       limits = c(min(d$year) - 0.5, x_end + 19), expand = c(0, 0)) +
    labs(
      title = title,
      subtitle = paste(
        "Each stripe is one year. Red years are below the stock's limit reference point (LRP); blue years are above it.",
        "Limits are taken or derived from RAM Legacy and are approximate. Badges show DFO's 2024 status zone.",
        sep = "\n"
      ),
      x = NULL, y = NULL, caption = caption_std
    ) +
    theme_bcstock() +
    theme(
      panel.grid = element_blank(),
      axis.text.y = element_text(face = "bold"),
      legend.position = "bottom",
      legend.title.position = "top",
      legend.key.width = unit(2.5, "cm")
    )
}

# ---- 2. Kobe plot data ----
kobe_data <- function(timeseries, stocks) {
  timeseries |>
    filter(metric %in% c("b_bmsy", "u_umsy")) |>
    select(stock_id, year, metric, value) |>
    pivot_wider(names_from = metric, values_from = value) |>
    filter(!is.na(b_bmsy), !is.na(u_umsy)) |>
    left_join(select(stocks, stock_id, species), by = "stock_id") |>
    arrange(stock_id, year)
}

# ---- 3. Static Kobe plot ----
plot_kobe <- function(timeseries, stocks) {
  kd   <- kobe_data(timeseries, stocks)
  xmax <- max(2, max(kd$b_bmsy) * 1.05)
  ymax <- max(2, max(kd$u_umsy) * 1.05)
  ends <- kd |> group_by(stock_id) |> filter(year %in% range(year)) |> ungroup()
  
  ggplot(kd, aes(b_bmsy, u_umsy)) +
    annotate("rect", xmin = 0, xmax = 1,    ymin = 1, ymax = ymax, fill = "#f4c7c3") +
    annotate("rect", xmin = 1, xmax = xmax, ymin = 1, ymax = ymax, fill = "#fbe3b8") +
    annotate("rect", xmin = 0, xmax = 1,    ymin = 0, ymax = 1,    fill = "#fbe3b8") +
    annotate("rect", xmin = 1, xmax = xmax, ymin = 0, ymax = 1,    fill = "#cfe8d5") +
    annotate("text", x = 0.03, y = ymax * 0.97, label = "Low biomass,\nhigh fishing pressure",
             hjust = 0, vjust = 1, size = 3, colour = "grey25", fontface = "italic") +
    annotate("text", x = xmax * 0.98, y = 0.04, label = "High biomass,\nlow fishing pressure",
             hjust = 1, vjust = 0, size = 3, colour = "grey25", fontface = "italic") +
    geom_vline(xintercept = c(0.4, 0.8), linetype = "dashed", colour = "grey35", linewidth = 0.4) +
    annotate("text", x = c(0.4, 0.8), y = ymax * 0.02,
             label = c("0.4 Bmsy\nLRP proxy", "0.8 Bmsy\nUSR proxy"),
             hjust = -0.08, vjust = 0, size = 2.7, colour = "grey30") +
    geom_path(aes(colour = species), linewidth = 0.8,
              arrow = arrow(length = unit(0.25, "cm"), type = "closed")) +
    geom_point(aes(colour = species), size = 1.1) +
    geom_label_repel(data = ends, aes(label = year, colour = species),
                     size = 3, show.legend = FALSE, min.segment.length = 0, seed = 1) +
    scale_colour_manual(values = species_cols, name = NULL) +
    coord_cartesian(xlim = c(0, xmax), ylim = c(0, ymax), expand = FALSE) +
    labs(
      title = "Kobe plot: where each stock has been",
      subtitle = "Biomass and fishing pressure relative to their MSY levels. Arrows point to the most recent year.",
      x = "Biomass relative to Bmsy (B/Bmsy)",
      y = "Fishing pressure relative to Fmsy (F/Fmsy or U/Umsy)",
      caption = paste(
        "Halibut fishing-pressure ratios are available for 2000 to 2013 only. The 0.4 and 0.8 Bmsy lines are DFO default proxies;",
        "they are the official reference points for sablefish but not for halibut, which uses limits based on unfished biomass.",
        caption_std, sep = "\n"
      )
    ) +
    theme_bcstock() +
    theme(legend.position = "top")
}

# ---- 4. Animated, interactive Kobe plot ----
plot_kobe_animated <- function(timeseries, stocks) {
  kd <- kobe_data(timeseries, stocks) |>
    mutate(hover = as.character(glue(
      "<b>{species}</b>, {year}<br>B/Bmsy: {number(b_bmsy, accuracy = 0.01)}<br>F/Fmsy: {number(u_umsy, accuracy = 0.01)}"
    )))
  sp_levels <- sort(unique(kd$species))
  years     <- sort(unique(kd$year))
  
  # Build cumulative frames so each frame shows the path up to that year.
  # Species with no data yet get a placeholder row so plotly keeps traces stable.
  acc <- map_dfr(years, function(y) {
    cur     <- filter(kd, year <= y)
    missing <- setdiff(sp_levels, unique(cur$species))
    bind_rows(
      cur,
      tibble(species = missing, b_bmsy = NA_real_, u_umsy = NA_real_, hover = NA_character_)
    ) |>
      mutate(frame = y)
  }) |>
    mutate(species = factor(species, levels = sp_levels)) |>
    arrange(frame, species, year)
  
  xmax <- max(2, max(kd$b_bmsy) * 1.05)
  ymax <- max(2, max(kd$u_umsy) * 1.05)
  rect <- function(x0, x1, y0, y1, col) {
    list(type = "rect", x0 = x0, x1 = x1, y0 = y0, y1 = y1,
         fillcolor = col, line = list(width = 0), layer = "below")
  }
  
  plot_ly(
    acc, x = ~b_bmsy, y = ~u_umsy, color = ~species,
    colors = unname(species_cols[sp_levels]),
    frame = ~frame, text = ~hover, hoverinfo = "text",
    type = "scatter", mode = "lines+markers", marker = list(size = 6)
  ) |>
    layout(
      shapes = list(
        rect(0, 1, 1, ymax, "#f4c7c3"), rect(1, xmax, 1, ymax, "#fbe3b8"),
        rect(0, 1, 0, 1, "#fbe3b8"),    rect(1, xmax, 0, 1, "#cfe8d5")
      ),
      xaxis  = list(title = "Biomass relative to Bmsy", range = c(0, xmax), zeroline = FALSE),
      yaxis  = list(title = "Fishing pressure relative to Fmsy", range = c(0, ymax), zeroline = FALSE),
      legend = list(orientation = "h", y = 1.08)
    ) |>
    animation_opts(frame = 120, transition = 0, redraw = FALSE) |>
    animation_slider(currentvalue = list(prefix = "Year: ")) |>
    animation_button(label = "Play") |>
    config(displayModeBar = FALSE)
}

# ---- 5. Panel for stocks with no time series ----
plot_no_data <- function(s) {
  ggplot() +
    annotate("text", x = 0, y = 0.25, label = s$stock_name, fontface = "bold", size = 6) +
    annotate("text", x = 0, y = -0.05, size = 4, colour = "grey30",
             label = "No public stock assessment time series compiled in this project.") +
    annotate("text", x = 0, y = -0.3, size = 4, colour = zone_cols[coalesce(s$dfo_status_zone, "Not in survey")],
             fontface = "bold",
             label = glue("{zone_label(s$dfo_status_zone)} (status method: {coalesce(s$status_method, 'not reported')})")) +
    xlim(-1, 1) + ylim(-0.6, 0.6) +
    labs(caption = caption_std) +
    theme_void() +
    theme(plot.caption = element_text(colour = "grey45", hjust = 0, size = 8),
          plot.background = element_rect(fill = "grey97", colour = NA),
          plot.margin = margin(15, 15, 15, 15))
}

# ---- 6. Per-stock trend panel ----
plot_trend <- function(timeseries, stocks, id) {
  s <- stocks |> filter(stock_id == id)
  if (s$data_tier == "D") return(plot_no_data(s))
  
  col  <- species_cols[[s$species]]
  b    <- timeseries |> filter(stock_id == id, metric == "b_lrp")
  catch_metric <- if (any(timeseries$stock_id == id & timeseries$metric == "total_catch")) "total_catch" else "landings"
  xr   <- range(b$year) + c(-0.5, 0.5)
  cdat <- timeseries |> filter(stock_id == id, metric == catch_metric,
                               year >= min(b$year), year <= max(b$year))
  last <- b |> slice_max(year, n = 1)
  ymin <- min(0.2, min(b$value) * 0.9)
  ymax <- max(5, max(b$value) * 1.15)
  
  p1 <- ggplot(b, aes(year, value)) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = ymin, ymax = 1, fill = "#f4c7c3", alpha = 0.6) +
    annotate("text", x = xr[1], y = ymin * 1.15, label = "Below limit reference point",
             hjust = 0, vjust = 0, size = 3, colour = "#922b21") +
    geom_hline(yintercept = 1, colour = "#c0392b", linetype = "dashed") +
    geom_line(colour = col, linewidth = 0.9) +
    geom_point(data = last, colour = col, size = 2.6) +
    geom_label(data = last, aes(label = glue("{year}: {number(value, accuracy = 0.01)}\u00d7 LRP")),
               hjust = 1.08, vjust = -0.3, size = 3.2, colour = col) +
    scale_y_log10(limits = c(ymin, ymax), breaks = c(0.25, 0.5, 1, 2, 4, 8),
                  labels = c("\u00bc\u00d7", "\u00bd\u00d7", "LRP", "2\u00d7", "4\u00d7", "8\u00d7")) +
    scale_x_continuous(limits = xr) +
    labs(
      title = s$stock_name,
      subtitle = glue("Biomass relative to the limit reference point (log scale). {zone_label(s$dfo_status_zone)}."),
      x = NULL, y = NULL
    )
  
  if (nrow(cdat) == 0) {
    return(p1 + labs(caption = caption_std) + theme_bcstock())
  }
  
  p2 <- ggplot(cdat, aes(year, value)) +
    geom_col(fill = col, alpha = 0.55, width = 0.85) +
    scale_y_continuous(labels = label_comma()) +
    scale_x_continuous(limits = xr) +
    labs(subtitle = if (catch_metric == "total_catch") "Total catch (tonnes)" else "Landings (tonnes)",
         x = NULL, y = NULL)
  
  (p1 / p2) +
    plot_layout(heights = c(2.2, 1)) +
    plot_annotation(caption = caption_std) &
    theme_bcstock()
}

# ---- 7. Sparkline for cards ----
plot_sparkline <- function(timeseries, stocks, id) {
  s <- stocks |> filter(stock_id == id)
  b <- timeseries |> filter(stock_id == id, metric == "b_lrp")
  if (nrow(b) == 0) return(NULL)
  ggplot(b, aes(year, value)) +
    geom_hline(yintercept = 1, colour = "#c0392b", linetype = "dashed", linewidth = 0.3) +
    geom_line(colour = species_cols[[s$species]], linewidth = 0.7) +
    geom_point(data = slice_max(b, year, n = 1), colour = species_cols[[s$species]], size = 1.4) +
    scale_y_log10() +
    theme_void()
}