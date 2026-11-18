# 07_make_figures.R
# Makes every figure used by the cards, report, website, and story.

source(here::here("R", "functions.R"))
source(here::here("R", "plot_functions.R"))

db_dir     <- here("data", "processed")
stocks     <- read_csv(file.path(db_dir, "stocks.csv"), show_col_types = FALSE)
timeseries <- read_csv(file.path(db_dir, "timeseries.csv"), show_col_types = FALSE)

dir.create(here("figures", "stocks"),     showWarnings = FALSE, recursive = TRUE)
dir.create(here("figures", "sparklines"), showWarnings = FALSE, recursive = TRUE)

# ---- 1. Coast-wide stripes ----
p_stripes <- plot_stripes(timeseries, stocks)
ggsave(here("figures", "coastwide_stripes.png"), p_stripes,
       width = 12, height = 6.5, dpi = 300, bg = "white")

# ---- 2. Kobe plot, static ----
p_kobe <- plot_kobe(timeseries, stocks)
ggsave(here("figures", "kobe.png"), p_kobe, width = 9, height = 7.5, dpi = 300, bg = "white")

# ---- 3. Kobe plot, animated ----
k_anim <- plot_kobe_animated(timeseries, stocks)
htmlwidgets::saveWidget(k_anim, here("figures", "kobe_animated.html"), selfcontained = TRUE)

# ---- 4. Trend panel for each stock ----
for (id in stocks$stock_id) {
  p <- plot_trend(timeseries, stocks, id)
  ggsave(here("figures", "stocks", paste0(id, "_trend.png")), p,
         width = 9, height = 6, dpi = 200, bg = "white")
}

# ---- 5. Sparklines for cards ----
for (id in stocks$stock_id) {
  p <- plot_sparkline(timeseries, stocks, id)
  if (!is.null(p)) {
    ggsave(here("figures", "sparklines", paste0(id, ".png")), p,
           width = 2.4, height = 0.8, dpi = 200, bg = "transparent")
  }
}

# ---- Show the main ones ----
print(p_stripes)
print(p_kobe)
print(k_anim)

cat("\nFigures written:\n")
print(list.files(here("figures"), recursive = TRUE))