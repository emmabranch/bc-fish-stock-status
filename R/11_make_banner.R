# 11_make_banner.R
# Decorative stripe ribbon for the home page banner:
# average Pacific herring biomass relative to the limit (geometric mean of five areas).

source(here::here("R", "functions.R"))
source(here::here("R", "plot_functions.R"))

timeseries <- read_csv(here("data", "processed", "timeseries.csv"), show_col_types = FALSE)

herring_avg <- timeseries |>
  filter(metric == "b_lrp", str_detect(stock_id, "^herring")) |>
  group_by(year) |>
  summarise(v = exp(mean(log(value))), n = n(), .groups = "drop") |>
  filter(n == 5)

p <- ggplot(herring_avg, aes(year, 1, fill = log2(v))) +
  geom_tile(width = 1, height = 1) +
  fill_lrp() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_void() +
  theme(legend.position = "none")

ggsave(here("figures", "banner_stripes.png"), p, width = 16, height = 0.6, dpi = 200, bg = "transparent")
cat("Saved figures/banner_stripes.png covering", min(herring_avg$year), "to", max(herring_avg$year), "\n")