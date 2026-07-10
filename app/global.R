library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(DT)
library(dplyr)
library(sf)
library(readr)
library(plotly)
library(adehabitatHR)
library(leaflet)
library(htmltools)
library(yyjsonr)
library(shiny)
library(readxl)
library(lattice)
library(grid)
library(ggplot2)
library(lubridate)
library(here)


#==============================================================
# 1. DATA
#==============================================================

# ── Colour palette (one per elephant) ─────────────────────────────────────────
ELEPHANT_COLOURS <- c(
  Talatha = "#E63946",
  Pazhani = "#457B9D",
  `recollared female` = "#2A9D8F",
  Rahu = "#F4A261",
  Kasun = "#9B2226",
  Dona = "#6A0572",
  Mina = "#0096C7",
  Illuk = "#52B788",
  Dewmi = "#F77F00",
  Gothami = "#CB4335",
  Wilmini = "#1B4332",
  female_1 = "#B5838D",
  `Tara Devi` = "#D4A017",
  Damien = "#3D405B"
)

# ── Load data ──────────────────────────────────────────────────────────────────
DATA_PATH <- here("data/kaudulla_elephants_clean.csv")

load_data <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE)
  df$datetime <- as.POSIXct(df$datetime, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  df$datetime_sl <- with_tz(df$datetime, "Asia/Colombo") # Sri Lanka time
  # FIX 1: derive the calendar date from datetime (old "%d/%m/%Y" -> all NA)
  df$date_parsed <- as.Date(df$datetime_sl)
  df <- df[!is.na(df$lat) & !is.na(df$lon), ]
  df
}
elephants_df <- load_data(DATA_PATH)

# Summary stats
n_obs <- nrow(elephants_df)
n_animals <- length(unique(elephants_df$name))
date_start <- format(min(elephants_df$date_parsed, na.rm = TRUE), "%d %b %Y")
date_end <- format(max(elephants_df$date_parsed, na.rm = TRUE), "%d %b %Y")

# ── Helper: split a track into segments at large gaps (for the MAP) ──────────
assign_track_segments <- function(d, time_col = "datetime_sl") {
  d <- d[order(d[[time_col]]), ]
  if (nrow(d) < 2) {
    d$seg <- 1L
    return(d)
  }
  gaps <- as.numeric(difftime(d[[time_col]][-1], d[[time_col]][-nrow(d)], units = "secs"))
  med <- median(gaps[gaps > 0], na.rm = TRUE)
  if (!is.finite(med)) {
    med <- 3600
  }
  thr <- max(med * 4, 6 * 3600)
  d$seg <- c(1L, cumsum(gaps > thr) + 1L)
  d
}

# ── Helper: insert NA rows at large gaps (for the LINE CHARTS) ───────────────
# An NA row in the middle of a big gap makes plotly leave a blank space
# instead of joining across the missing period.
insert_gaps <- function(d, time_col = "datetime_sl", cols = c("lat", "lon")) {
  d <- d[order(d[[time_col]]), ]
  if (nrow(d) < 2) {
    return(d)
  }
  gaps <- as.numeric(difftime(d[[time_col]][-1], d[[time_col]][-nrow(d)], units = "secs"))
  med <- median(gaps[gaps > 0], na.rm = TRUE)
  if (!is.finite(med)) {
    med <- 3600
  }
  big <- which(gaps > max(med * 4, 6 * 3600))
  if (!length(big)) {
    return(d)
  }
  na_rows <- d[big, , drop = FALSE]
  na_rows[[time_col]] <- d[[time_col]][big] + gaps[big] / 2
  for (cc in cols) {
    na_rows[[cc]] <- NA_real_
  }
  d <- rbind(d, na_rows)
  d[order(d[[time_col]]), ]
}


kaudulla_elephants_clean_imputed <- read_csv(here(
  "data/kaudulla_elephants_clean_imputed.csv"
))

df_sf <- kaudulla_elephants_clean_imputed |>
  filter(!is.na(lon), !is.na(lat)) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  mutate(
    year = format(datetime, "%Y"),
    month = format(datetime, "%m"),
    year_month = format(datetime, "%Y-%m")
  ) |>
  arrange(name, datetime)

# IMPORTANT: palette MUST be based on FULL dataset (not filtered)
pal <- colorFactor(
  palette = c(
    "#8B0000",
    "#006400",
    "#00008B",
    "#8B4513",
    "#4B0082",
    "#2F4F4F",
    "#800080",
    "#B22222",
    "#556B2F",
    "#191970",
    "#8B008B",
    "#A52A2A",
    "#483D8B",
    "#008080",
    "#5F9EA0",
    "#7B3F00",
    "#3B3D6B",
    "#4A7023",
    "#6A0DAD",
    "#7F1734"
  ),
  domain = df_sf$year_month
)

elephants <- sort(unique(df_sf$name))

# Colors for elephant tracking
elephant_colors <- c(
  "#e6194b",
  "#3cb44b",
  "#ffe119",
  "#4363d8",
  "#f58231",
  "#911eb4",
  "#46f0f0",
  "#f032e6",
  "#bcf60c",
  "#fabebe",
  "#008080",
  "#e6beff",
  "#9a6324",
  "#fffac8",
  "#800000"
)


#==============================================================
# 2. CALENDAR HEATMAP FUNCTION
#==============================================================

calendarHeat <- function(
  dates,
  values,
  colors,
  at = NULL,
  ncolors = 99,
  title,
  date.form = "%Y-%m-%d",
  colorkey = FALSE,
  legend = NULL,
  ...
) {
  require(lattice, quietly = TRUE)
  require(grid, quietly = TRUE)

  if (inherits(dates, c("character", "factor"))) {
    dates <- strptime(dates, date.form)
  }
  caldat <- data.frame(value = values, dates = dates)
  min.date <- as.Date(paste(format(min(dates), "%Y"), "-1-1", sep = ""))
  max.date <- as.Date(paste(format(max(dates), "%Y"), "-12-31", sep = ""))

  caldat <- data.frame(date.seq = seq(min.date, max.date, by = "days"), value = NA)
  dates <- as.Date(dates)
  caldat$value[match(dates, caldat$date.seq)] <- values

  caldat$dotw <- as.numeric(format(caldat$date.seq, "%w"))
  caldat$woty <- as.numeric(format(caldat$date.seq, "%U")) + 1
  caldat$yr <- as.factor(format(caldat$date.seq, "%Y"))
  caldat$month <- as.numeric(format(caldat$date.seq, "%m"))
  yrs <- as.character(unique(caldat$yr))
  d.loc <- as.numeric()
  for (m in min(yrs):max(yrs)) {
    d.subset <- which(caldat$yr == m)
    sub.seq <- seq(1, length(d.subset))
    d.loc <- c(d.loc, sub.seq)
  }
  caldat <- cbind(caldat, seq = d.loc)

  if (!is.null(at)) {
    if (missing(colors)) {
      colors <- c("#D61818", "#FFAE63", "#FFFFBD", "#B5E384")
      calendar.pal <- colorRampPalette(colors, space = "Lab")(length(at) - 1)
    } else {
      if (length(colors) == (length(at) - 1)) {
        calendar.pal <- colors
      } else {
        calendar.pal <- colorRampPalette(colors, space = "Lab")(length(at) - 1)
      }
    }
    my.cuts <- NULL
  } else {
    if (missing(colors)) {
      colors <- c("#D61818", "#FFAE63", "#FFFFBD", "#B5E384")
    }
    calendar.pal <- colorRampPalette(colors, space = "Lab")(ncolors)
    my.cuts <- ncolors - 1
  }

  def.theme <- lattice.getOption("default.theme")
  cal.theme <- function() {
    list(
      strip.background = list(col = "transparent"),
      strip.border = list(col = "transparent"),
      axis.line = list(col = "transparent"),
      par.strip.text = list(cex = 0.8)
    )
  }
  lattice.options(default.theme = cal.theme)
  yrs <- (unique(caldat$yr))
  nyr <- length(yrs)
  #==============================================================
  # PART 2: PLOT RENDERING & POST-GRAPHICS REGION FOCUS
  #==============================================================
  print(
    cal.plot <- levelplot(
      value ~ woty * dotw | yr,
      data = caldat,
      as.table = TRUE,
      aspect = .12,
      layout = c(1, nyr %% 7),
      between = list(x = 0, y = c(0.5, 0.5)),
      strip = TRUE,
      main = ifelse(missing(title), "", title),
      scales = list(
        x = list(
          at = c(seq(2.9, 52, by = 4.42)),
          labels = month.abb,
          alternating = c(1, rep(0, (nyr - 1))),
          tck = 0,
          cex = 0.7
        ),
        y = list(
          at = c(0, 1, 2, 3, 4, 5, 6),
          labels = c(
            "Sunday",
            "Monday",
            "Tuesday",
            "Wednesday",
            "Thursday",
            "Friday",
            "Saturday"
          ),
          alternating = 1,
          cex = 0.6,
          tck = 0
        )
      ),
      xlim = c(0.4, 54.6),
      ylim = c(6.6, -0.6),
      at = at,
      cuts = my.cuts,
      col.regions = calendar.pal,
      xlab = "",
      ylab = "",
      colorkey = colorkey,
      legend = legend,
      subscripts = TRUE
    )
  )

  panel.locs <- trellis.currentLayout()
  for (row in 1:nrow(panel.locs)) {
    for (column in 1:ncol(panel.locs)) {
      if (panel.locs[row, column] > 0) {
        trellis.focus("panel", row = row, column = column, highlight = FALSE)
        xyetc <- trellis.panelArgs()
        subs <- caldat[xyetc$subscripts, ]
        dates.fsubs <- caldat[caldat$yr == unique(subs$yr), ]
        y.start <- dates.fsubs$dotw[1]
        y.end <- dates.fsubs$dotw[nrow(dates.fsubs)]
        dates.len <- nrow(dates.fsubs)
        adj.start <- dates.fsubs$woty[1]

        for (k in 0:6) {
          if (k < y.start) {
            x.start <- adj.start + 0.5
          } else {
            x.start <- adj.start - 0.5
          }
          if (k > y.end) {
            x.finis <- dates.fsubs$woty[nrow(dates.fsubs)] - 0.5
          } else {
            x.finis <- dates.fsubs$woty[nrow(dates.fsubs)] + 0.5
          }
          grid.lines(
            x = c(x.start, x.finis),
            y = c(k - 0.5, k - 0.5),
            default.units = "native",
            gp = gpar(col = "grey", lwd = 1)
          )
        }
        if (adj.start < 2) {
          grid.lines(
            x = c(0.5, 0.5),
            y = c(6.5, y.start - 0.5),
            default.units = "native",
            gp = gpar(col = "grey", lwd = 1)
          )
          grid.lines(
            x = c(1.5, 1.5),
            y = c(6.5, -0.5),
            default.units = "native",
            gp = gpar(col = "grey", lwd = 1)
          )
          grid.lines(
            x = c(x.finis, x.finis),
            y = c(dates.fsubs$dotw[dates.len] - 0.5, -0.5),
            default.units = "native",
            gp = gpar(col = "grey", lwd = 1)
          )
          if (dates.fsubs$dotw[dates.len] != 6) {
            grid.lines(
              x = c(x.finis + 1, x.finis + 1),
              y = c(dates.fsubs$dotw[dates.len] - 0.5, -0.5),
              default.units = "native",
              gp = gpar(col = "grey", lwd = 1)
            )
          }
          grid.lines(
            x = c(x.finis, x.finis),
            y = c(dates.fsubs$dotw[dates.len] - 0.5, -0.5),
            default.units = "native",
            gp = gpar(col = "grey", lwd = 1)
          )
        }
        for (n in 1:51) {
          grid.lines(
            x = c(n + 1.5, n + 1.5),
            y = c(-0.5, 6.5),
            default.units = "native",
            gp = gpar(col = "grey", lwd = 1)
          )
        }
        x.start <- adj.start - 0.5

        if (y.start > 0) {
          grid.lines(
            x = c(x.start, x.start + 1),
            y = c(y.start - 0.5, y.start - 0.5),
            default.units = "native",
            gp = gpar(col = "black", lwd = 1.75)
          )
          grid.lines(
            x = c(x.start + 1, x.start + 1),
            y = c(y.start - 0.5, -0.5),
            default.units = "native",
            gp = gpar(col = "black", lwd = 1.75)
          )
          grid.lines(
            x = c(x.start, x.start),
            y = c(y.start - 0.5, 6.5),
            default.units = "native",
            gp = gpar(col = "black", lwd = 1.75)
          )
          if (y.end < 6) {
            grid.lines(
              x = c(x.start + 1, x.finis + 1),
              y = c(-0.5, -0.5),
              default.units = "native",
              gp = gpar(col = "black", lwd = 1.75)
            )
            grid.lines(
              x = c(x.start, x.finis),
              y = c(6.5, 6.5),
              default.units = "native",
              gp = gpar(col = "black", lwd = 1.75)
            )
          } else {
            grid.lines(
              x = c(x.start + 1, x.finis),
              y = c(-0.5, -0.5),
              default.units = "native",
              gp = gpar(col = "black", lwd = 1.75)
            )
            grid.lines(
              x = c(x.start, x.finis),
              y = c(6.5, 6.5),
              default.units = "native",
              gp = gpar(col = "black", lwd = 1.75)
            )
          }
        } else {
          grid.lines(
            x = c(x.start, x.start),
            y = c(-0.5, 6.5),
            default.units = "native",
            gp = gpar(col = "black", lwd = 1.75)
          )
        }

        if (y.start == 0) {
          if (y.end < 6) {
            grid.lines(
              x = c(x.start, x.finis + 1),
              y = c(-0.5, -0.5),
              default.units = "native",
              gp = gpar(col = "black", lwd = 1.75)
            )
            grid.lines(
              x = c(x.start, x.finis),
              y = c(6.5, 6.5),
              default.units = "native",
              gp = gpar(col = "black", lwd = 1.75)
            )
          } else {
            grid.lines(
              x = c(x.start + 1, x.finis),
              y = c(-0.5, -0.5),
              default.units = "native",
              gp = gpar(col = "black", lwd = 1.75)
            )
            grid.lines(
              x = c(x.start, x.finis),
              y = c(6.5, 6.5),
              default.units = "native",
              gp = gpar(col = "black", lwd = 1.75)
            )
          }
        }
        for (j in 1:12) {
          last.month <- max(dates.fsubs$seq[dates.fsubs$month == j])
          x.last.m <- dates.fsubs$woty[last.month] + 0.5
          y.last.m <- dates.fsubs$dotw[last.month] + 0.5
          grid.lines(
            x = c(x.last.m, x.last.m),
            y = c(-0.5, y.last.m),
            default.units = "native",
            gp = gpar(col = "black", lwd = 1.75)
          )
          if ((y.last.m) < 6) {
            grid.lines(
              x = c(x.last.m, x.last.m - 1),
              y = c(y.last.m, y.last.m),
              default.units = "native",
              gp = gpar(col = "black", lwd = 1.75)
            )
            grid.lines(
              x = c(x.last.m - 1, x.last.m - 1),
              y = c(y.last.m, 6.5),
              default.units = "native",
              gp = gpar(col = "black", lwd = 1.75)
            )
          } else {
            grid.lines(
              x = c(x.last.m, x.last.m),
              y = c(-0.5, 6.5),
              default.units = "native",
              gp = gpar(col = "black", lwd = 1.75)
            )
          }
        }
      }
    }
    trellis.unfocus()
  }
  lattice.options(default.theme = def.theme)
}

# ==============================================================================
# 3. GLOBAL SETTINGS & DATA INGESTION
# ==============================================================================
elephants <- read.csv(here("data/kaudulla_elephants_clean_imputed.csv"))
elephants$datetime <- ymd_hms(elephants$datetime)
elephants$date <- as.Date(elephants$datetime)

elephant_names <- unique(elephants$name)

my_colors2 <- c("#7F0000", "#E34A33", "#FDAE61", "#FFFF99", "#78C679", "#006400")
my_ranges <- c(0, 1, 25, 50, 75, 99.999, 100)
range_labels <- c("0 %", "1 - 25 %", "25 - 50 %", "50 - 75 %", "75 - 99 %", "100 %")

discrete_key <- list(
  space = "right",
  rectangles = list(col = my_colors2, border = "black", size = 2),
  text = list(range_labels, cex = 0.8),
  padding.text = 3,
  columns = 1
)


#==============================================================
# 4. CLIMATE DATA
#==============================================================

climate <- read_excel(here("data/daily_climate.xlsx"))
climate$date <- as.Date(climate$date, origin = "1899-12-30")
dates <- climate$date

my_colors <- c(
  "#FFFF99",
  "#FFCC66",
  "#F5B27A",
  "#FF6F91",
  "#9966CC",
  "#330066",
  "#000000"
)

plot_info <- list(
  "Solar Radiation" = list(
    values = climate$solar_radiation,
    breaks = c(0, 15, 20, 22, 23.5, 25, 26.5, 28),
    labels = c("0-15", "15-20", "20-22", "22-23.5", "23.5-25", "25-26.5", "26.5-28"),
    title = "Daily Solar Radiation Calendar Heatmap"
  ),
  "Rainfall" = list(
    values = climate$rainfall,
    breaks = c(0, 0.3, 0.6, 1.5, 2.5, 4, 10, 55),
    labels = c("0-0.3", "0.3-0.6", "0.6-1.5", "1.5-2.5", "2.5-4", "4-10", "10-55"),
    title = "Daily Rainfall Calendar Heatmap"
  ),
  "Pressure" = list(
    values = climate$pressure,
    breaks = c(0, 100.8, 101.0, 101.1, 101.2, 101.3, 101.4, 101.5),
    labels = c(
      "0-100.8",
      "100.8-101",
      "101-101.1",
      "101.1-101.2",
      "101.2-101.3",
      "101.3-101.4",
      "101.4-101.5"
    ),
    title = "Daily Pressure Calendar Heatmap"
  ),
  "Maximum Temperature" = list(
    values = climate$temp_max,
    breaks = c(0, 26.7, 27.4, 28.1, 28.8, 29.5, 30.2, 31),
    labels = c(
      "0-26.7",
      "26.7-27.4",
      "27.4-28.1",
      "28.1-28.8",
      "28.8-29.5",
      "29.5-30.2",
      "30.2-31"
    ),
    title = "Daily Maximum Temperature Calendar Heatmap"
  ),
  "Earth Skin Temperature" = list(
    values = climate$temp_skin,
    breaks = c(0, 27.2, 27.9, 28.6, 29.3, 30, 30.7, 31.5),
    labels = c(
      "0-27.2",
      "27.2-27.9",
      "27.9-28.6",
      "28.6-29.3",
      "29.3-30",
      "30-30.7",
      "30.7-31.5"
    ),
    title = "Daily Earth Skin Temperature Calendar Heatmap"
  ),
  "Wind Speed" = list(
    values = climate$wind_speed,
    breaks = c(0, 2, 4, 5, 6, 7, 8, 10),
    labels = c("0-2", "2-4", "4-5", "5-6", "6-7", "7-8", "8-10"),
    title = "Daily Wind Speed Calendar Heatmap"
  ),
  "Maximum Wind Speed" = list(
    values = climate$wind_speed_max,
    breaks = c(0, 3, 5, 6, 7, 8, 9, 11),
    labels = c("0-3", "3-5", "5-6", "6-7", "7-8", "8-9", "9-11"),
    title = "Daily Maximum Wind Speed Calendar Heatmap"
  )
)


library(shiny)
library(bslib)
library(leaflet)
library(dplyr)
library(plotly)
library(DT)
library(scales)
library(lubridate)
library(bsicons)

# ---- Source module files ----
source("R/helpers.R")
source("R/data_prep.R")
source("R/mod_elephant_tracking.R")

# ---- Load data once at app startup ----
elephant_data <- load_elephant_data(DATA_PATH)
