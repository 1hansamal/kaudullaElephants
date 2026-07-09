# ============================================
# HELPER FUNCTIONS
# Pure functions used by the elephant tracking module.
# No dependency on reactive context - safe to source once at app startup.
# ============================================

shoelace_area <- function(lons, lats) {
  n <- length(lons)
  area <- 0
  for (i in 1:n) {
    j <- ifelse(i == n, 1, i + 1)
    area <- area + lons[i] * lats[j] - lons[j] * lats[i]
  }
  abs(area) / 2
}

haversine_km <- function(lat1, lon1, lat2, lon2) {
  R <- 6371
  to_rad <- pi / 180
  dlat <- (lat2 - lat1) * to_rad
  dlon <- (lon2 - lon1) * to_rad
  a <- sin(dlat / 2)^2 +
    cos(lat1 * to_rad) * cos(lat2 * to_rad) * sin(dlon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R * c
}

compute_bearing <- function(lat1, lon1, lat2, lon2) {
  to_rad <- pi / 180
  dlon   <- (lon2 - lon1) * to_rad
  lat1r  <- lat1 * to_rad
  lat2r  <- lat2 * to_rad
  x <- sin(dlon) * cos(lat2r)
  y <- cos(lat1r) * sin(lat2r) - sin(lat1r) * cos(lat2r) * cos(dlon)
  bearing <- atan2(x, y) / to_rad
  (bearing + 360) %% 360
}

compute_hull <- function(df) {
  if (nrow(df) < 3) return(NULL)
  hull_indices <- chull(df$lon, df$lat)
  hull_lons <- c(df$lon[hull_indices], df$lon[hull_indices][1])
  hull_lats <- c(df$lat[hull_indices], df$lat[hull_indices][1])
  area_degrees2 <- shoelace_area(
    hull_lons[-length(hull_lons)],
    hull_lats[-length(hull_lats)]
  )
  mean_lat <- mean(df$lat)
  area_km2 <- area_degrees2 * 111 * (111 * cos(mean_lat * pi / 180))
  list(lons = hull_lons, lats = hull_lats, area_km2 = area_km2)
}

add_movement_metrics <- function(df) {
  df %>%
    arrange(name, datetime) %>%
    group_by(name) %>%
    mutate(
      prev_lat  = lag(lat),
      prev_lon  = lag(lon),
      prev_time = lag(datetime),
      step_km   = haversine_km(prev_lat, prev_lon, lat, lon),
      hours     = as.numeric(difftime(datetime, prev_time, units = "hours")),
      speed_kmh = ifelse(hours > 0, step_km / hours, NA_real_),
      bearing   = compute_bearing(prev_lat, prev_lon, lat, lon)
    ) %>%
    ungroup() %>%
    select(-prev_lat, -prev_lon, -prev_time)
}

bin_bearings <- function(bearings, n_bins = 16) {
  bin_width  <- 360 / n_bins
  bin_labels <- seq(0, 360 - bin_width, by = bin_width)
  bins <- cut(
    bearings,
    breaks = c(bin_labels, 360),
    labels = bin_labels,
    include.lowest = TRUE,
    right = FALSE
  )
  counts <- table(factor(bins, levels = as.character(bin_labels)))
  data.frame(theta = as.numeric(names(counts)), r = as.numeric(counts))
}

make_rose_plot <- function(bearings, color, name) {
  bd <- bin_bearings(bearings[is.finite(bearings)])
  plot_ly(
    bd, type = "barpolar",
    r = ~r, theta = ~theta,
    marker = list(color = color, line = list(color = "#ffffff", width = 0.5)),
    hovertemplate = paste0("<b>", name, "</b><br>%{theta}°: %{r} fixes<extra></extra>")
  ) %>%
    layout(
      polar = list(
        angularaxis = list(
          tickmode  = "array",
          tickvals  = c(0, 45, 90, 135, 180, 225, 270, 315),
          ticktext  = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
          direction = "clockwise",
          rotation  = 90,
          gridcolor = "rgba(0,0,0,0.1)",
          linecolor = "rgba(0,0,0,0.15)"
        ),
        radialaxis = list(
          gridcolor = "rgba(0,0,0,0.08)",
          linecolor = "rgba(0,0,0,0.1)",
          tickfont  = list(color = "#666", size = 9)
        ),
        bgcolor = "rgba(0,0,0,0)"
      ),
      paper_bgcolor = "rgba(0,0,0,0)",
      font          = list(color = "#333333"),
      showlegend    = FALSE,
      margin        = list(l = 20, r = 20, t = 30, b = 20),
      title         = list(text = name, font = list(size = 12, color = "#333333"))
    ) %>%
    config(displayModeBar = FALSE)
}
