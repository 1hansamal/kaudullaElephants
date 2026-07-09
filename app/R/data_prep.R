# ============================================
# DATA LOADING & PREP
# Kept outside the module on purpose: a module should receive its data
# as an argument (or reactive) rather than reach out and load its own
# file. This makes mod_elephant_tracking_Server() reusable/testable
# with any data.frame that has the right shape, and lets an app embed
# multiple instances (e.g. one per park) pointed at different CSVs.
#
# Produces the objects the module needs:
#   tracking_clean    - data.frame with movement metrics added
#   unique_elephants  - sorted character vector of elephant names
#   unique_sexes      - sorted character vector of sex categories
#   elephant_colors   - named vector, elephant name -> hex color
#   min_date, max_date - Date range covered by the data
# ============================================

load_elephant_data <- function(path = here("data/kaudulla_elephants_clean_imputed.csv")) {

  tracking_data <- read.csv(path)
  tracking_data$lat      <- as.numeric(tracking_data$lat)
  tracking_data$lon      <- as.numeric(tracking_data$lon)
  tracking_data$datetime <- as.POSIXct(tracking_data$datetime, tz = "UTC")

  tracking_clean <- tracking_data %>%
    filter(!is.na(lat), !is.na(lon)) %>%
    arrange(name, datetime)

  tracking_clean <- add_movement_metrics(tracking_clean)

  unique_elephants <- sort(unique(tracking_clean$name))
  unique_sexes     <- sort(unique(tracking_clean$sex))

  palette_colors <- c(
    "#FF5252", "#2196F3", "#4CAF50", "#FFC107", "#9C27B0",
    "#FF9800", "#00BCD4", "#8BC34A", "#E91E63", "#3F51B5",
    "#795548", "#607D8B", "#CDDC39", "#F44336"
  )
  elephant_colors <- setNames(
    palette_colors[seq_along(unique_elephants)],
    unique_elephants
  )

  list(
    tracking_clean   = tracking_clean,
    unique_elephants = unique_elephants,
    unique_sexes     = unique_sexes,
    elephant_colors  = elephant_colors,
    min_date         = as.Date(min(tracking_clean$datetime, na.rm = TRUE)),
    max_date         = as.Date(max(tracking_clean$datetime, na.rm = TRUE))
  )
}
