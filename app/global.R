library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(DT)
library(dplyr)
library(sf)
library(readr)
library(plotly)
library(leaflet)
library(htmltools)
library(yyjsonr)
library(shiny)
library(readxl)
library(lattice)
library(grid)
library(ggplot2)  
library(lubridate)
library(shinyjs)


#==============================================================
# 1. DATA 
#==============================================================


# ── Colour palette (one per elephant) ─────────────────────────────────────────
ELEPHANT_COLOURS <- c(
  Talatha             = "#E63946",
  Pazhani             = "#457B9D",
  `recollared female` = "#2A9D8F",
  Rahu                = "#F4A261",
  Kasun               = "#9B2226",
  Dona                = "#6A0572",
  Mina                = "#0096C7",
  Illuk               = "#52B788",
  Dewmi               = "#F77F00",
  Gothami             = "#CB4335",
  Wilmini             = "#1B4332",
  female_1            = "#B5838D",
  `Tara Devi`         = "#D4A017",
  Damien              = "#3D405B"
)

# ── Load data ──────────────────────────────────────────────────────────────────
DATA_PATH <- "kaudulla_elephants_clean.csv"

load_data <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE)
  df$datetime    <- as.POSIXct(df$datetime, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  df$datetime_sl <- with_tz(df$datetime, "Asia/Colombo")   # Sri Lanka time
  # FIX 1: derive the calendar date from datetime (old "%d/%m/%Y" -> all NA)
  df$date_parsed <- as.Date(df$datetime_sl)
  df <- df[!is.na(df$lat) & !is.na(df$lon), ]
  df
}
elephants_df <- load_data(DATA_PATH)

# Summary stats
n_obs      <- nrow(elephants_df)
n_animals  <- length(unique(elephants_df$name))
date_start <- format(min(elephants_df$date_parsed, na.rm = TRUE), "%d %b %Y")
date_end   <- format(max(elephants_df$date_parsed, na.rm = TRUE), "%d %b %Y")

# ── Month choices for the global "Month" filter (sorted chronologically) ────
month_lookup <- elephants_df %>%
  mutate(
    month_key   = format(date_parsed, "%Y-%m"),
    month_label = format(date_parsed, "%B %Y")
  ) %>%
  distinct(month_key, month_label) %>%
  arrange(month_key)

month_choices <- setNames(month_lookup$month_key, month_lookup$month_label)
month_choices <- c("All months" = "all", month_choices)

# ── Helper: split a track into segments at large gaps (for the MAP) ──────────
assign_track_segments <- function(d, time_col = "datetime_sl") {
  d <- d[order(d[[time_col]]), ]
  if (nrow(d) < 2) { d$seg <- 1L; return(d) }
  gaps <- as.numeric(difftime(d[[time_col]][-1], d[[time_col]][-nrow(d)], units = "secs"))
  med  <- median(gaps[gaps > 0], na.rm = TRUE)
  if (!is.finite(med)) med <- 3600
  thr  <- max(med * 4, 6 * 3600)
  d$seg <- c(1L, cumsum(gaps > thr) + 1L)
  d
}

# ── Helper: insert NA rows at large gaps (for the LINE CHARTS) ───────────────
# An NA row in the middle of a big gap makes plotly leave a blank space
# instead of joining across the missing period.
insert_gaps <- function(d, time_col = "datetime_sl", cols = c("lat", "lon")) {
  d <- d[order(d[[time_col]]), ]
  if (nrow(d) < 2) return(d)
  gaps <- as.numeric(difftime(d[[time_col]][-1], d[[time_col]][-nrow(d)], units = "secs"))
  med  <- median(gaps[gaps > 0], na.rm = TRUE)
  if (!is.finite(med)) med <- 3600
  big  <- which(gaps > max(med * 4, 6 * 3600))
  if (!length(big)) return(d)
  na_rows <- d[big, , drop = FALSE]
  na_rows[[time_col]] <- d[[time_col]][big] + gaps[big] / 2
  for (cc in cols) na_rows[[cc]] <- NA_real_
  d <- rbind(d, na_rows)
  d[order(d[[time_col]]), ]
}



kaudulla_elephants_clean_imputed <- read_csv("kaudulla_elephants_clean.csv")

df_sf <- kaudulla_elephants_clean_imputed |>
  filter(!is.na(lon), !is.na(lat)) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  mutate(
    year = format(datetime, "%Y"),
    month = format(datetime, "%m"),
    year_month = format(datetime, "%Y-%m"),
    # Week-of-month, always 4 buckets (days 1-7, 8-14, 15-21, 22-end)
    week_of_month = factor(
      paste0("Week ", pmin(ceiling(day(datetime) / 7), 4)),
      levels = c("Week 1", "Week 2", "Week 3", "Week 4")
    )
  ) |>
  arrange(name, datetime)

# IMPORTANT: palette MUST be based on FULL dataset (not filtered)
pal <- colorFactor(
  palette = c(
    "red", # Red
    "#377EB8", # Blue
    "#4DAF4A", # Green
    "#984EA3", # Purple
    "#FF7F00", # Orange
    "#fffac8", # Yellow
    "#A65628", # Brown
    "#F781BF", # Pink
    "#17BECF", # Cyan
    "#000000", # Black
    "blue", # Sky Blue
    "#2F4F4F", # Lime Green
    "#FB9A99", # Light Red
    "#CAB2D6", # Lavender
    "#FDBF6F", # Light Orange
    "#6A3D9A", # Dark Purple
    "#B2DF8A", # Light Green
    "#FF1493", # Deep Pink
    "#00CED1", # Dark Turquoise
    "#FFD000"  # Gold
  ),
  domain = df_sf$year_month
)

# ── Week-of-month colour palette (4 fixed, highly-distinct colours) ──────────
# Since only one month is selectable at a time, there are always at most
# 4 weeks on screen (days 1-7, 8-14, 15-21, 22-end) - one clear colour each.
week_of_month_colors <- c(
  "Week 1" = "#e6194b",  # red
  "Week 2" = "#3cb44b",  # green
  "Week 3" = "#4363d8",  # blue
  "Week 4" = "#f58231"   # orange
)

pal_week <- colorFactor(
  palette = unname(week_of_month_colors),
  domain  = names(week_of_month_colors)
)
elephants <- sort(unique(df_sf$name))

# Colors for elephant tracking
elephant_colors <- c(
  "#e6194b", "#3cb44b", "#ffe119", "#4363d8", "#f58231",
  "#911eb4", "#46f0f0", "#f032e6", "#bcf60c", "#fabebe",
  "#008080", "#e6beff", "#9a6324", "#fffac8", "#800000"
)



kaudulla_elephants_clean_imputed <- read_csv("kaudulla_elephants_clean.csv", show_col_types = FALSE)

all_elephant_names <- unique(kaudulla_elephants_clean_imputed$name)

df_sf_new <- kaudulla_elephants_clean_imputed |>
  filter(!is.na(lon), !is.na(lat)) |>
  mutate(
    name = factor(name, levels = all_elephant_names),
    # Ensure sex is a factor with both levels present
    sex = factor(sex, levels = c("Male", "Female")),
    year_month = format(datetime, "%Y-%m"),
    date_month = as.Date(paste0(year_month, "-01"))
  ) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  arrange(name, datetime)

global_bbox <- st_bbox(df_sf_new)

active_months <- df_sf_new |>
  st_drop_geometry() |>
  distinct(date_month) |>
  arrange(date_month) |>
  pull(date_month)

# --- PRE-RENDERING ENGINE ---
img_dir <- file.path(tempdir(), "elephant_plots")
if (!dir.exists(img_dir)) dir.create(img_dir)

message("Pre-rendering plots...")
sex_colors <- c("Male" = "darkblue", "Female" = "darkred")

for (i in seq_along(active_months)) {
  m_date <- active_months[i]
  month_data <- df_sf_new |> filter(date_month == m_date)
  
  p <- ggplot(month_data) +
    geom_sf(
      aes(color = sex), 
      size = 4.5,          # Increased dot size for better visibility
      alpha = 0.8,
      show.legend = TRUE 
    ) +
    facet_wrap(~ name, ncol = 7, drop = FALSE) + 
    coord_sf(
      xlim = c(global_bbox["xmin"], global_bbox["xmax"]),
      ylim = c(global_bbox["ymin"], global_bbox["ymax"])
    ) +
    scale_color_manual(values = sex_colors, drop = FALSE) +
    labs(
      title = format(m_date, "%Y %b"),
      subtitle = "Elephant GPS locations",
      x = "Longitude", y = "Latitude", color = "Sex:"
    ) +
    # Boosted base size drastically to blow up text dimensions on saved files
    theme_minimal(base_size = 22) + 
    theme(
      plot.title = element_text(face = "bold", size = 32, hjust = 0.5, margin = margin(b = 5)),
      plot.subtitle = element_text(size = 22, hjust = 0.5, color = "gray30", margin = margin(b = 10)),
      
      # Axis Labels and Coordinates
      axis.title = element_text(size = 20, face = "bold"),
      axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5, size = 16, face = "bold"),
      axis.text.y = element_text(size = 16, face = "bold"),
      
      # Elephant Names (Facet Strips)
      strip.text = element_text(size = 20, face = "bold", color = "black"), 
      strip.background = element_rect(fill = "#f0f2f5", color = NA),
      
      # Legend Amplification
      legend.position = "bottom",
      legend.title = element_text(size = 24, face = "bold"),
      legend.text = element_text(size = 22, face = "bold"),
      legend.key.size = unit(1.8, "cm"), 
      
      plot.margin = margin(10, 10, 10, 10)
    ) +
    # Make legend color icons larger & distinct
    guides(color = guide_legend(override.aes = list(size = 7)))
  
  # Adjusted dimensions and slightly dropped DPI to make everything appear dramatically larger relative to the frame
  ggsave(
    filename = file.path(img_dir, paste0("plot_", i, ".png")),
    plot = p, width = 20, height = 13, dpi = 96
  )
}
message("Pre-rendering complete!")





#==============================================================
# 2. CALENDAR HEATMAP FUNCTION
#==============================================================

calendarHeat <- function(dates, 
                         values,
                         colors,
                         at = NULL,           
                         ncolors=99,
                         title,
                         date.form = "%Y-%m-%d", 
                         colorkey = FALSE,      
                         legend = NULL,         
                         ...) {
  require(lattice, quietly = TRUE)
  require(grid, quietly = TRUE)
  
  if (inherits(dates, c("character", "factor"))) {
    dates <- strptime(dates, date.form)
  }
  caldat <- data.frame(value = values, dates = dates)
  min.date <- as.Date(paste(format(min(dates), "%Y"), "-1-1", sep = ""))
  max.date <- as.Date(paste(format(max(dates), "%Y"), "-12-31", sep = ""))
  
  caldat <- data.frame(date.seq = seq(min.date, max.date, by="days"), value = NA)
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
  caldat <- cbind(caldat, seq=d.loc)
  
  if (!is.null(at)) {
    if (missing(colors)) {
      colors <- c("#D61818", "#FFAE63", "#FFFFBD", "#B5E384")
      calendar.pal <- colorRampPalette(colors, space = "Lab")(length(at) - 1)
    } else {
      if(length(colors) == (length(at) - 1)) {
        calendar.pal <- colors
      } else {
        calendar.pal <- colorRampPalette(colors, space = "Lab")(length(at) - 1)
      }
    }
    my.cuts <- NULL 
  } else {
    if (missing(colors)) colors <- c("#D61818", "#FFAE63", "#FFFFBD", "#B5E384")
    calendar.pal <- colorRampPalette(colors, space = "Lab")(ncolors)
    my.cuts <- ncolors - 1
  }
  
  def.theme <- lattice.getOption("default.theme")
  cal.theme <- function() {  
    list(
      strip.background = list(col = "transparent"),
      strip.border = list(col = "transparent"),
      axis.line = list(col="transparent"),
      par.strip.text=list(cex=0.8))
  }
  lattice.options(default.theme = cal.theme)
  yrs <- (unique(caldat$yr))
  nyr <- length(yrs)
  #==============================================================
  # PART 2: PLOT RENDERING & POST-GRAPHICS REGION FOCUS
  #==============================================================
  print(cal.plot <- levelplot(value~woty*dotw | yr, data=caldat,
                              as.table=TRUE,
                              aspect=.14,
                              layout = c(1, nyr%%7),
                              between = list(x=0, y=c(1,1)),
                              strip=TRUE,
                              main = ifelse(missing(title), "", title),
                              scales = list(
                                x = list(
                                  at= c(seq(2.9, 52, by=4.42)),
                                  labels = month.abb,
                                  alternating = c(1, rep(0, (nyr-1))),
                                  tck=0,
                                  cex = 1.1),
                                y=list(
                                  at = c(0, 1, 2, 3, 4, 5, 6),
                                  labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
                                             "Friday", "Saturday"),
                                  alternating = 1,
                                  cex = 0.85,
                                  tck=0)),
                              xlim = c(0.4, 54.6),
                              ylim = c(6.6,-0.6),
                              at = at,                 
                              cuts = my.cuts,           
                              col.regions = calendar.pal, 
                              xlab="" ,
                              ylab="",
                              colorkey = colorkey,    
                              legend = legend,        
                              subscripts=TRUE
  ) )
  
  panel.locs <- trellis.currentLayout()
  for (row in 1:nrow(panel.locs)) {
    for (column in 1:ncol(panel.locs))  {
      if (panel.locs[row, column] > 0) {
        trellis.focus("panel", row = row, column = column, highlight = FALSE)
        xyetc <- trellis.panelArgs()
        subs <- caldat[xyetc$subscripts,]
        dates.fsubs <- caldat[caldat$yr == unique(subs$yr),]
        y.start <- dates.fsubs$dotw[1]
        y.end   <- dates.fsubs$dotw[nrow(dates.fsubs)]
        dates.len <- nrow(dates.fsubs)
        adj.start <- dates.fsubs$woty[1]
        
        for (k in 0:6) {
          if (k < y.start) { x.start <- adj.start + 0.5 } else { x.start <- adj.start - 0.5 }
          if (k > y.end) { x.finis <- dates.fsubs$woty[nrow(dates.fsubs)] - 0.5 } else { x.finis <- dates.fsubs$woty[nrow(dates.fsubs)] + 0.5 }
          grid.lines(x = c(x.start, x.finis), y = c(k -0.5, k - 0.5), default.units = "native", gp=gpar(col = "grey", lwd = 1))
        }
        if (adj.start <  2) {
          grid.lines(x = c( 0.5,  0.5), y = c(6.5, y.start-0.5), default.units = "native", gp=gpar(col = "grey", lwd = 1))
          grid.lines(x = c(1.5, 1.5), y = c(6.5, -0.5), default.units = "native", gp=gpar(col = "grey", lwd = 1))
          grid.lines(x = c(x.finis, x.finis), y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native", gp=gpar(col = "grey", lwd = 1))
          if (dates.fsubs$dotw[dates.len] != 6) {
            grid.lines(x = c(x.finis + 1, x.finis + 1), y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native", gp=gpar(col = "grey", lwd = 1))
          }
          grid.lines(x = c(x.finis, x.finis), y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native", gp=gpar(col = "grey", lwd = 1))
        }
        for (n in 1:51) {
          grid.lines(x = c(n + 1.5, n + 1.5), y = c(-0.5, 6.5), default.units = "native", gp=gpar(col = "grey", lwd = 1))
        }
        x.start <- adj.start - 0.5
        
        if (y.start > 0) {
          grid.lines(x = c(x.start, x.start + 1), y = c(y.start - 0.5, y.start -  0.5), default.units = "native", gp=gpar(col = "black", lwd = 1.75))
          grid.lines(x = c(x.start + 1, x.start + 1), y = c(y.start - 0.5 , -0.5), default.units = "native", gp=gpar(col = "black", lwd = 1.75))
          grid.lines(x = c(x.start, x.start), y = c(y.start - 0.5, 6.5), default.units = "native", gp=gpar(col = "black", lwd = 1.75))
          if (y.end < 6  ) {
            grid.lines(x = c(x.start + 1, x.finis + 1), y = c(-0.5, -0.5), default.units = "native", gp=gpar(col = "black", lwd = 1.75))
            grid.lines(x = c(x.start, x.finis), y = c(6.5, 6.5), default.units = "native", gp=gpar(col = "black", lwd = 1.75))
          } else {
            grid.lines(x = c(x.start + 1, x.finis), y = c(-0.5, -0.5), default.units = "native", gp=gpar(col = "black", lwd = 1.75))
            grid.lines(x = c(x.start, x.finis), y = c(6.5, 6.5), default.units = "native", gp=gpar(col = "black", lwd = 1.75))
          }
        } else {
          grid.lines(x = c(x.start, x.start), y = c( - 0.5, 6.5), default.units = "native", gp=gpar(col = "black", lwd = 1.75))
        }
        
        if (y.start == 0 ) {
          if (y.end < 6  ) {
            grid.lines(x = c(x.start, x.finis + 1), y = c(-0.5, -0.5), default.units = "native", gp=gpar(col = "black", lwd = 1.75))
            grid.lines(x = c(x.start, x.finis), y = c(6.5, 6.5), default.units = "native", gp=gpar(col = "black", lwd = 1.75))
          } else {
            grid.lines(x = c(x.start + 1, x.finis), y = c(-0.5, -0.5), default.units = "native", gp=gpar(col = "black", lwd = 1.75))
            grid.lines(x = c(x.start, x.finis), y = c(6.5, 6.5), default.units = "native", gp=gpar(col = "black", lwd = 1.75))
          }
        }
        for (j in 1:12)  {
          last.month <- max(dates.fsubs$seq[dates.fsubs$month == j])
          x.last.m <- dates.fsubs$woty[last.month] + 0.5
          y.last.m <- dates.fsubs$dotw[last.month] + 0.5
          grid.lines(x = c(x.last.m, x.last.m), y = c(-0.5, y.last.m), default.units = "native", gp=gpar(col = "black", lwd = 1.75))
          if ((y.last.m) < 6) {
            grid.lines(x = c(x.last.m, x.last.m - 1), y = c(y.last.m, y.last.m), default.units = "native", gp=gpar(col = "black", lwd = 1.75))
            grid.lines(x = c(x.last.m - 1, x.last.m - 1), y = c(y.last.m, 6.5), default.units = "native", gp=gpar(col = "black", lwd = 1.75))
          } else {
            grid.lines(x = c(x.last.m, x.last.m), y = c(- 0.5, 6.5), default.units = "native", gp=gpar(col = "black", lwd = 1.75))
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
elephants <- read.csv("kaudulla_elephants_clean.csv")
elephants$datetime <- ymd_hms(elephants$datetime)
elephants$date <- as.Date(elephants$datetime)

elephant_names <- unique(elephants$name)

my_colors2 <- c("#D9D9D9", "#FEE08B", "#D9EF8B", "#91CF60", "#4DAC26", "#006400")
my_ranges <- c(0, 1, 25, 50, 75, 99.999, 100)
range_labels <- c("0 %", "1 - 25 %", "25 - 50 %", "50 - 75 %", "75 - 99 %", "100 %")

discrete_key <- list(
  space = "right",
  rectangles = list(col = my_colors2, border = "black", size = 4),
  text = list(range_labels, cex = 1.1),
  padding.text = 4,
  columns = 1
)



#==============================================================
# 4. CLIMATE DATA
#==============================================================

climate <- read_excel("daily_climate.xlsx")
climate$date <- as.Date(climate$date, origin = "1899-12-30")
dates <- climate$date

my_colors <- c(
  "#FFFF99", "#FFCC66", "#F5B27A", "#FF6F91", 
  "#9966CC", "#330066", "#000000"
)

plot_info <- list(
  "Solar Radiation" = list(
    values = climate$solar_radiation,
    breaks = c(0,15,20,22,23.5,25,26.5,28),
    labels = c("0-15", "15-20", "20-22", "22-23.5", "23.5-25", "25-26.5", "26.5-28"),
    title = "Daily Solar Radiation Calendar Heatmap"
  ),
  "Rainfall" = list(
    values = climate$rainfall,
    breaks = c(0,0.3,0.6,1.5,2.5,4,10,55),
    labels = c("0-0.3", "0.3-0.6", "0.6-1.5", "1.5-2.5", "2.5-4", "4-10", "10-55"),
    title = "Daily Rainfall Calendar Heatmap"
  ),
  "Pressure" = list(
    values = climate$pressure,
    breaks = c(0,100.8,101.0,101.1,101.2,101.3,101.4,101.5),
    labels = c("0-100.8", "100.8-101", "101-101.1", "101.1-101.2", "101.2-101.3", "101.3-101.4", "101.4-101.5"),
    title = "Daily Pressure Calendar Heatmap"
  ),
  "Maximum Temperature" = list(
    values = climate$temp_max,
    breaks = c(0,26.7,27.4,28.1,28.8,29.5,30.2,31),
    labels = c("0-26.7", "26.7-27.4", "27.4-28.1", "28.1-28.8", "28.8-29.5", "29.5-30.2", "30.2-31"),
    title = "Daily Maximum Temperature Calendar Heatmap"
  ),
  "Earth Skin Temperature" = list(
    values = climate$temp_skin,
    breaks = c(0,27.2,27.9,28.6,29.3,30,30.7,31.5),
    labels = c("0-27.2", "27.2-27.9", "27.9-28.6", "28.6-29.3", "29.3-30", "30-30.7", "30.7-31.5"),
    title = "Daily Earth Skin Temperature Calendar Heatmap"
  ),
  "Wind Speed" = list(
    values = climate$wind_speed,
    breaks = c(0,2,4,5,6,7,8,10),
    labels = c("0-2", "2-4", "4-5", "5-6", "6-7", "7-8", "8-10"),
    title = "Daily Wind Speed Calendar Heatmap"
  ),
  "Maximum Wind Speed" = list(
    values = climate$wind_speed_max,
    breaks = c(0,3,5,6,7,8,9,11),
    labels = c("0-3", "3-5", "5-6", "6-7", "7-8", "8-9", "9-11"),
    title = "Daily Maximum Wind Speed Calendar Heatmap"
  )
)



#==============================================================
# 5. UI
#==============================================================
# ==============================================================
# 4B. HOME RANGE / MOVEMENT MODULE DATA (from app (6).R)
# ==============================================================

mcp_tracking_data <- read.csv(DATA_PATH)
mcp_tracking_data$lat <- as.numeric(mcp_tracking_data$lat)
mcp_tracking_data$lon <- as.numeric(mcp_tracking_data$lon)
mcp_tracking_data$datetime <- as.POSIXct(mcp_tracking_data$datetime, tz = "UTC")

mcp_tracking_clean <- mcp_tracking_data %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  arrange(name, datetime)

mcp_unique_elephants <- sort(unique(mcp_tracking_clean$name))
mcp_unique_sexes <- sort(unique(mcp_tracking_clean$sex))

mcp_palette_colors <- c(
  "#FF5252", "#2196F3", "#4CAF50", "#FFC107", "#9C27B0",
  "#FF9800", "#00BCD4", "#8BC34A", "#E91E63", "#3F51B5",
  "#795548", "#607D8B", "#CDDC39", "#F44336"
)
mcp_elephant_colors <- setNames(
  mcp_palette_colors[seq_along(mcp_unique_elephants)],
  mcp_unique_elephants
)

mcp_min_date <- as.Date(min(mcp_tracking_clean$datetime, na.rm = TRUE))
mcp_max_date <- as.Date(max(mcp_tracking_clean$datetime, na.rm = TRUE))

# ── Helper functions ──────────────────────────────────────────────────────────

mcp_shoelace_area <- function(lons, lats) {
  n <- length(lons)
  area <- 0
  for (i in 1:n) {
    j <- ifelse(i == n, 1, i + 1)
    area <- area + lons[i] * lats[j] - lons[j] * lats[i]
  }
  abs(area) / 2
}

mcp_haversine_km <- function(lat1, lon1, lat2, lon2) {
  R <- 6371
  to_rad <- pi / 180
  dlat <- (lat2 - lat1) * to_rad
  dlon <- (lon2 - lon1) * to_rad
  a <- sin(dlat / 2)^2 +
    cos(lat1 * to_rad) * cos(lat2 * to_rad) * sin(dlon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R * c
}

mcp_compute_bearing <- function(lat1, lon1, lat2, lon2) {
  to_rad <- pi / 180
  dlon <- (lon2 - lon1) * to_rad
  lat1r <- lat1 * to_rad
  lat2r <- lat2 * to_rad
  x <- sin(dlon) * cos(lat2r)
  y <- cos(lat1r) * sin(lat2r) - sin(lat1r) * cos(lat2r) * cos(dlon)
  bearing <- atan2(x, y) / to_rad
  (bearing + 360) %% 360
}

mcp_compute_hull <- function(df, ratio = 0.3) {
  # ratio: 0 = tightest/most concave fit, 1 = same as a convex hull
  # 0.2-0.4 tends to look close to a "natural" home-range boundary; tune to taste
  if (nrow(df) < 3) {
    return(NULL)
  }
  
  pts_sf <- df %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  hull_sf <- tryCatch(
    st_concave_hull(st_union(pts_sf), ratio = ratio, allow_holes = FALSE),
    error = function(e) NULL
  )
  
  # fallback to convex hull if GEOS on this machine is too old for concave hulls
  if (is.null(hull_sf) || length(hull_sf) == 0 || st_is_empty(hull_sf)) {
    hull_sf <- st_convex_hull(st_union(pts_sf))
  }
  
  coords <- st_coordinates(hull_sf)
  hull_lons <- coords[, "X"]
  hull_lats <- coords[, "Y"]
  
  # geodesic area straight from sf (accounts for lat/lon curvature properly,
  # more accurate than the shoelace + flat cos(lat) approximation)
  area_km2 <- as.numeric(st_area(hull_sf)) / 1e6
  
  list(lons = hull_lons, lats = hull_lats, area_km2 = area_km2)
}

mcp_add_movement_metrics <- function(df) {
  df %>%
    arrange(name, datetime) %>%
    group_by(name) %>%
    mutate(
      prev_lat  = lag(lat),
      prev_lon  = lag(lon),
      prev_time = lag(datetime),
      step_km   = mcp_haversine_km(prev_lat, prev_lon, lat, lon),
      hours     = as.numeric(difftime(datetime, prev_time, units = "hours")),
      speed_kmh = ifelse(hours > 0, step_km / hours, NA_real_),
      bearing   = mcp_compute_bearing(prev_lat, prev_lon, lat, lon)
    ) %>%
    ungroup() %>%
    select(-prev_lat, -prev_lon, -prev_time)
}

mcp_tracking_clean <- mcp_add_movement_metrics(mcp_tracking_clean)

mcp_bin_bearings <- function(bearings, n_bins = 16) {
  bin_width <- 360 / n_bins
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

mcp_make_rose_plot <- function(bearings, color, name) {
  bd <- mcp_bin_bearings(bearings[is.finite(bearings)])
  plot_ly(
    bd,
    type = "barpolar",
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
      font = list(color = "#333333"),
      showlegend = FALSE,
      margin = list(l = 20, r = 20, t = 30, b = 20),
      title = list(text = name, font = list(size = 12, color = "#333333"))
    ) %>%
    config(displayModeBar = FALSE)
}