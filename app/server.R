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



# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  all_names <- sort(unique(elephants_df$name))
  female_names <- sort(unique(elephants_df$name[elephants_df$sex == "Female"]))
  male_names <- sort(unique(elephants_df$name[elephants_df$sex == "Male"]))

  # ── Quick-select buttons ─────────────────────────────────────────────────────
  observeEvent(input$btn_all, updateSelectInput(session, "sel_elephants", selected = all_names))
  observeEvent(input$btn_female, updateSelectInput(session, "sel_elephants", selected = female_names))
  observeEvent(input$btn_male, updateSelectInput(session, "sel_elephants", selected = male_names))
  observeEvent(input$btn_clear, updateSelectInput(session, "sel_elephants", selected = character(0)))

  # ── Reactive filtered dataset ────────────────────────────────────────────────
  filtered <- reactive({
    req(input$date_range)

    df <- elephants_df %>%
      filter(
        date_parsed >= input$date_range[1],
        date_parsed <= input$date_range[2]
      )

    if (length(input$sel_elephants) > 0) {
      df <- df %>% filter(name %in% input$sel_elephants)
    } else {
      df <- df[0, ]
    }

    if (!input$show_imputed) df <- df %>% filter(!imputed)

    df
  })

  # ── Aggregated dataset ───────────────────────────────────────────────────────
  agg_data <- reactive({
    df <- filtered()
    lvl <- input$agg_level

    if (lvl == "raw") {
      return(df)
    }

    df %>%
      mutate(
        period = if (lvl == "day") {
          as.POSIXct(floor_date(datetime_sl, "day"))
        } else {
          as.POSIXct(floor_date(datetime_sl, "week"))
        }
      ) %>%
      group_by(name, sex, period) %>%
      summarise(
        lat = mean(lat, na.rm = TRUE),
        lon = mean(lon, na.rm = TRUE),
        imputed = any(imputed),
        .groups = "drop"
      ) %>%
      rename(datetime_sl = period)
  })

  # ── Value boxes ──────────────────────────────────────────────────────────────
  output$vbox_obs <- renderValueBox({
    n <- nrow(filtered())
    valueBox(format(n, big.mark = ","), "GPS Fixes (filtered)",
      icon = icon("location-dot"), color = "green"
    )
  })
  output$vbox_eleph <- renderValueBox({
    valueBox(length(unique(filtered()$name)), "Elephants Selected",
      icon = icon("paw"), color = "olive"
    )
  })
  output$vbox_start <- renderValueBox({
    d <- suppressWarnings(min(filtered()$date_parsed, na.rm = TRUE))
    valueBox(if (is.finite(d)) format(d, "%d %b %Y") else "\u2014", "Data From",
      icon = icon("calendar-day"), color = "teal"
    )
  })
  output$vbox_end <- renderValueBox({
    d <- suppressWarnings(max(filtered()$date_parsed, na.rm = TRUE))
    valueBox(if (is.finite(d)) format(d, "%d %b %Y") else "\u2014", "Data To",
      icon = icon("calendar-check"), color = "teal"
    )
  })

  # ── Helper: single-coordinate plotly scatter ────────────────────────────────
  make_plot <- function(df, y_col, y_title, ref_lines = NULL) {
    elephants_in_data <- unique(df$name)
    col_map <- ELEPHANT_COLOURS[names(ELEPHANT_COLOURS) %in% elephants_in_data]

    p <- plot_ly()

    for (el in elephants_in_data) {
      sub <- df %>%
        filter(name == el) %>%
        arrange(datetime_sl)
      sub <- insert_gaps(sub) # FIX 2: break lines at gaps
      clr <- if (el %in% names(col_map)) col_map[[el]] else "#aaaaaa"

      p <- p %>% add_trace(
        data = sub,
        x = ~datetime_sl,
        y = as.formula(paste0("~", y_col)),
        type = "scatter",
        mode = "lines+markers",
        name = el,
        connectgaps = FALSE, # FIX 2: do NOT join across gaps
        line = list(color = clr, width = 1.5),
        marker = list(
          color   = ~ ifelse(imputed, "#888888", clr),
          size    = ~ ifelse(imputed, 5, 4),
          opacity = 0.85,
          symbol  = ~ ifelse(imputed, "circle-open", "circle"),
          line    = list(color = clr, width = 1)
        ),
        text = ~ paste0(
          "<b>", name, "</b><br>",
          "Time (SL): ", format(datetime_sl, "%d %b %Y %H:%M"), "<br>",
          y_title, ": ", round(get(y_col), 5), "\u00B0<br>",
          "Sex: ", sex, "<br>",
          "Imputed: ", imputed
        ),
        hoverinfo = "text"
      )

      if (input$add_smooth && nrow(sub) > 10) {
        smooth_df <- data.frame(x = as.numeric(sub$datetime_sl), y = sub[[y_col]])
        smooth_df <- smooth_df[!is.na(smooth_df$y), ]
        if (nrow(smooth_df) > 5) {
          lo <- loess(y ~ x, data = smooth_df, span = 0.3)
          smooth_df$yhat <- predict(lo)
          smooth_df$ts <- as.POSIXct(smooth_df$x, origin = "1970-01-01", tz = "Asia/Colombo")
          p <- p %>% add_trace(
            data = smooth_df, x = ~ts, y = ~yhat,
            type = "scatter", mode = "lines",
            name = paste(el, "(smooth)"),
            line = list(color = clr, width = 2.5, dash = "dot"),
            showlegend = FALSE, hoverinfo = "skip"
          )
        }
      }
    }

    if (!is.null(ref_lines)) {
      for (rl in ref_lines) {
        p <- p %>% add_segments(
          x = min(df$datetime_sl, na.rm = TRUE),
          xend = max(df$datetime_sl, na.rm = TRUE),
          y = rl$val, yend = rl$val,
          line = list(color = rl$color, width = 1.5, dash = "dash"),
          name = rl$label, showlegend = TRUE, hoverinfo = "name"
        )
      }
    }

    p %>% layout(
      paper_bgcolor = "#ffffff",
      plot_bgcolor = "#ffffff",
      font = list(color = "#333333", family = "Segoe UI"),
      xaxis = list(
        title = "Date / Time (Asia/Colombo)", gridcolor = "#e5e5e5",
        zerolinecolor = "#dddddd", tickformat = "%b %Y"
      ),
      yaxis = list(title = y_title, gridcolor = "#e5e5e5", zerolinecolor = "#dddddd"),
      legend = list(
        bgcolor = "#ffffff", bordercolor = "#4caf50",
        borderwidth = 1, font = list(size = 11)
      ),
      hoverlabel = list(bgcolor = "#ffffff", font = list(color = "#333333")),
      margin = list(t = 40, b = 60, l = 70, r = 20)
    )
  }

  # ── Helper: dual-axis plot — latitude & longitude overlaid ──────────────────
  make_dual_axis_plot <- function(df) {
    elephants_in_data <- unique(df$name)
    col_map <- ELEPHANT_COLOURS[names(ELEPHANT_COLOURS) %in% elephants_in_data]

    p <- plot_ly()

    for (el in elephants_in_data) {
      sub <- df %>%
        filter(name == el) %>%
        arrange(datetime_sl)
      sub <- insert_gaps(sub) # FIX 2: break lines at gaps
      clr <- if (el %in% names(col_map)) col_map[[el]] else "#aaaaaa"

      # Latitude (left axis, solid, circles)
      p <- p %>% add_trace(
        data = sub, x = ~datetime_sl, y = ~lat,
        type = "scatter", mode = "lines+markers",
        name = paste(el, "\u2013 Lat"), legendgroup = el, yaxis = "y",
        connectgaps = FALSE,
        line = list(color = clr, width = 1.5, dash = "solid"),
        marker = list(
          color = ~ ifelse(imputed, "#888888", clr),
          size = ~ ifelse(imputed, 5, 4),
          symbol = ~ ifelse(imputed, "circle-open", "circle"),
          line = list(color = clr, width = 1)
        ),
        text = ~ paste0(
          "<b>", name, "</b><br>Latitude: ", round(lat, 5), "\u00B0N<br>",
          format(datetime_sl, "%d %b %Y %H:%M"), "<br>Sex: ", sex,
          "<br>Imputed: ", imputed
        ),
        hoverinfo = "text"
      )

      # Longitude (right axis, dotted, triangles)
      p <- p %>% add_trace(
        data = sub, x = ~datetime_sl, y = ~lon,
        type = "scatter", mode = "lines+markers",
        name = paste(el, "\u2013 Lon"), legendgroup = el, yaxis = "y2",
        connectgaps = FALSE,
        line = list(color = clr, width = 1.5, dash = "dot"),
        marker = list(
          color = ~ ifelse(imputed, "#888888", clr),
          size = ~ ifelse(imputed, 5, 4),
          symbol = ~ ifelse(imputed, "triangle-up-open", "triangle-up"),
          line = list(color = clr, width = 1)
        ),
        text = ~ paste0(
          "<b>", name, "</b><br>Longitude: ", round(lon, 5), "\u00B0E<br>",
          format(datetime_sl, "%d %b %Y %H:%M"), "<br>Sex: ", sex,
          "<br>Imputed: ", imputed
        ),
        hoverinfo = "text"
      )

      if (input$add_smooth && nrow(sub) > 10) {
        sm_lat <- data.frame(x = as.numeric(sub$datetime_sl), y = sub$lat)
        sm_lat <- sm_lat[!is.na(sm_lat$y), ]
        if (nrow(sm_lat) > 5) {
          lo <- loess(y ~ x, data = sm_lat, span = 0.3)
          sm_lat$yhat <- predict(lo)
          sm_lat$ts <- as.POSIXct(sm_lat$x, origin = "1970-01-01", tz = "Asia/Colombo")
          p <- p %>% add_trace(
            data = sm_lat, x = ~ts, y = ~yhat,
            type = "scatter", mode = "lines",
            name = paste(el, "Lat smooth"), legendgroup = el, yaxis = "y",
            line = list(color = clr, width = 2.5, dash = "solid"),
            opacity = 0.4, showlegend = FALSE, hoverinfo = "skip"
          )
        }
        sm_lon <- data.frame(x = as.numeric(sub$datetime_sl), y = sub$lon)
        sm_lon <- sm_lon[!is.na(sm_lon$y), ]
        if (nrow(sm_lon) > 5) {
          lo2 <- loess(y ~ x, data = sm_lon, span = 0.3)
          sm_lon$yhat <- predict(lo2)
          sm_lon$ts <- as.POSIXct(sm_lon$x, origin = "1970-01-01", tz = "Asia/Colombo")
          p <- p %>% add_trace(
            data = sm_lon, x = ~ts, y = ~yhat,
            type = "scatter", mode = "lines",
            name = paste(el, "Lon smooth"), legendgroup = el, yaxis = "y2",
            line = list(color = clr, width = 2.5, dash = "dashdot"),
            opacity = 0.4, showlegend = FALSE, hoverinfo = "skip"
          )
        }
      }
    }

    ref_lat <- list(
      list(val = 8.140, color = "#4fc3f7", label = "Kaudulla Tank (Lat ~8.140\u00B0N)"),
      list(val = 8.080, color = "#ef9a9a", label = "S. Boundary (Lat ~8.080\u00B0N)"),
      list(val = 8.220, color = "#ef9a9a", label = "N. Boundary (Lat ~8.220\u00B0N)")
    )
    ref_lon <- list(
      list(val = 80.895, color = "#4fc3f7", label = "Kaudulla Tank (Lon ~80.895\u00B0E)"),
      list(val = 80.950, color = "#ef9a9a", label = "E. Boundary (Lon ~80.950\u00B0E)"),
      list(val = 80.872, color = "#ef9a9a", label = "W. Boundary (Lon ~80.872\u00B0E)")
    )
    x_min <- min(df$datetime_sl, na.rm = TRUE)
    x_max <- max(df$datetime_sl, na.rm = TRUE)

    for (rl in ref_lat) {
      p <- p %>% add_segments(
        x = x_min, xend = x_max, y = rl$val, yend = rl$val, yaxis = "y",
        line = list(color = rl$color, width = 1, dash = "dash"),
        name = rl$label, showlegend = TRUE, hoverinfo = "name"
      )
    }
    for (rl in ref_lon) {
      p <- p %>% add_segments(
        x = x_min, xend = x_max, y = rl$val, yend = rl$val, yaxis = "y2",
        line = list(color = rl$color, width = 1, dash = "dashdot"),
        name = rl$label, showlegend = TRUE, hoverinfo = "name"
      )
    }

    p %>% layout(
      paper_bgcolor = "#ffffff",
      plot_bgcolor = "#ffffff",
      font = list(color = "#333333", family = "Segoe UI"),
      xaxis = list(
        title = "Date / Time (Asia/Colombo)", gridcolor = "#e5e5e5",
        zerolinecolor = "#dddddd", tickformat = "%b %Y", domain = c(0, 1)
      ),
      yaxis = list(
        title = "Latitude (\u00B0N, WGS84)", gridcolor = "#e5e5e5",
        zerolinecolor = "#dddddd",
        titlefont = list(color = "#0277bd"), tickfont = list(color = "#0277bd")
      ),
      yaxis2 = list(
        title = "Longitude (\u00B0E, WGS84)", overlaying = "y", side = "right",
        showgrid = FALSE,
        titlefont = list(color = "#ef6c00"), tickfont = list(color = "#ef6c00")
      ),
      legend = list(
        bgcolor = "#ffffff", bordercolor = "#4caf50",
        borderwidth = 1, font = list(size = 10)
      ),
      hoverlabel = list(bgcolor = "#ffffff", font = list(color = "#333333")),
      margin = list(t = 40, b = 60, l = 70, r = 70)
    )
  }

  # ── Latitude plot ────────────────────────────────────────────────────────────
  output$plot_lat <- renderPlotly({
    df <- agg_data()
    validate(need(nrow(df) > 0, "No data for the selected filters."))
    ref_lines <- list(
      list(val = 8.140, color = "#4fc3f7", label = "Kaudulla Tank (~8.140\u00B0N)"),
      list(val = 8.080, color = "#ef9a9a", label = "S. Park Boundary (~8.080\u00B0N)"),
      list(val = 8.220, color = "#ef9a9a", label = "N. Park Boundary (~8.220\u00B0N)")
    )
    make_plot(df, "lat", "Latitude (\u00B0N, WGS84)", ref_lines)
  })

  # ── Longitude plot ───────────────────────────────────────────────────────────
  output$plot_lon <- renderPlotly({
    df <- agg_data()
    validate(need(nrow(df) > 0, "No data for the selected filters."))
    ref_lines <- list(
      list(val = 80.895, color = "#4fc3f7", label = "Kaudulla Tank (~80.895\u00B0E)"),
      list(val = 80.950, color = "#ef9a9a", label = "E. Park Boundary (~80.950\u00B0E)"),
      list(val = 80.872, color = "#ef9a9a", label = "W. Park Boundary (~80.872\u00B0E)")
    )
    make_plot(df, "lon", "Longitude (\u00B0E, WGS84)", ref_lines)
  })

  # ── Both coordinates (dual y-axis) ──────────────────────────────────────────
  output$plot_both <- renderPlotly({
    df <- agg_data()
    validate(need(nrow(df) > 0, "No data for the selected filters."))
    make_dual_axis_plot(df)
  })

  # ── Monthly aggregation for heat maps ───────────────────────────────────────
  heat_data <- reactive({
    df <- filtered()
    validate(need(nrow(df) > 0, "No data for the selected filters."))

    df <- df %>% mutate(ym = format(datetime_sl, "%Y-%m"))
    months_seq <- format(
      seq(as.Date(format(min(df$datetime_sl), "%Y-%m-01")),
        as.Date(format(max(df$datetime_sl), "%Y-%m-01")),
        by = "month"
      ), "%Y-%m"
    )
    names_seq <- sort(unique(df$name))

    agg <- df %>%
      group_by(name, ym) %>%
      summarise(mlon = mean(lon, na.rm = TRUE), n = n(), .groups = "drop")

    mat_lon <- matrix(NA_real_, length(names_seq), length(months_seq),
      dimnames = list(names_seq, months_seq)
    )
    mat_n <- mat_lon
    for (i in seq_len(nrow(agg))) {
      mat_lon[agg$name[i], agg$ym[i]] <- agg$mlon[i]
      mat_n[agg$name[i], agg$ym[i]] <- agg$n[i]
    }
    list(months = months_seq, names = names_seq, mat_lon = mat_lon, mat_n = mat_n)
  })

  output$heat_lon <- renderPlotly({
    hd <- heat_data()
    plot_ly(
      x = hd$months, y = hd$names, z = hd$mat_lon, type = "heatmap",
      colors = colorRamp(c("#f1faee", "#2a9d8f", "#e9c46a", "#e63946")),
      hoverongaps = FALSE,
      colorbar = list(
        title = "Mean\nLon (\u00B0E)",
        tickfont = list(color = "#333333"),
        titlefont = list(color = "#333333")
      ),
      hovertemplate = "%{y}<br>%{x}<br>Mean lon %{z:.4f}\u00B0E<extra></extra>"
    ) %>%
      layout(
        paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff",
        font = list(color = "#333333", family = "Segoe UI"),
        xaxis = list(title = "Month", tickangle = -45, gridcolor = "#e5e5e5"),
        yaxis = list(title = "", autorange = "reversed", gridcolor = "#e5e5e5"),
        margin = list(t = 20, b = 70, l = 110, r = 20)
      ) %>%
      config(displaylogo = FALSE)
  })

  output$heat_n <- renderPlotly({
    hd <- heat_data()
    plot_ly(
      x = hd$months, y = hd$names, z = hd$mat_n, type = "heatmap",
      colors = colorRamp(c("#f1faee", "#ff9f1c", "#e63946")),
      hoverongaps = FALSE,
      colorbar = list(
        title = "GPS\nFixes",
        tickfont = list(color = "#333333"),
        titlefont = list(color = "#333333")
      ),
      hovertemplate = "%{y}<br>%{x}<br>%{z} fixes<extra></extra>"
    ) %>%
      layout(
        paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff",
        font = list(color = "#333333", family = "Segoe UI"),
        xaxis = list(title = "Month", tickangle = -45, gridcolor = "#e5e5e5"),
        yaxis = list(title = "", autorange = "reversed", gridcolor = "#e5e5e5"),
        margin = list(t = 20, b = 70, l = 110, r = 20)
      ) %>%
      config(displaylogo = FALSE)
  })


  # ── Elephant tracking ───────────────────────────────────────────────────────────────
  # Color palette for tracking
  color_palette_tracking <- colorFactor(
    palette = elephant_colors,
    domain = df_sf$name
  )

  # Tracking Map
  output$tracking_map <- renderLeaflet({
    leaflet(df_sf) %>%
      addProviderTiles(providers$OpenStreetMap, group = "Street Map") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      # POINTS
      addCircleMarkers(
        color = ~ color_palette_tracking(name),
        radius = 4,
        stroke = FALSE,
        fillOpacity = 0.8,
        popup = ~ paste(
          "<b>Date:</b>", datetime,
          "<br><b>Gender:</b>", sex,
          "<br><b>Name:</b>", name
        )
      ) %>%
      # LEGEND
      addLegend(
        pal = color_palette_tracking,
        values = ~name,
        title = "Elephant Name",
        position = "bottomright"
      ) %>%
      # LAYERS CONTROL
      addLayersControl(
        baseGroups = c("Street Map", "Satellite"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })

  # Tracking Data Plot
  output$tracking_plot <- renderPlot({
    ggplot() +
      geom_sf(
        data = df_sf,
        aes(color = factor(name)),
        size = 0.75,
        alpha = 0.5
      ) +
      theme_minimal() +
      scale_color_manual(
        values = elephant_colors,
        name = "Name"
      ) +
      guides(
        color = guide_legend(
          override.aes = list(size = 3, alpha = 1)
        )
      ) +
      labs(
        title = "Tracking Data",
        x = "Longitude",
        y = "Latitude"
      ) +
      theme(
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        legend.position = "right"
      )
  })

  # GPS Tracking Data by Elephant
  output$tracking_by_elephant <- renderPlot({
    # Get unique elephants and their colors
    elephant_names <- unique(df_sf$name)
    n_elephants <- length(elephant_names)

    # Create color mapping for sex
    sex_colors <- c("Male" = "darkblue", "Female" = "darkred")

    # Determine number of columns (max 4)
    n_col <- min(4, n_elephants)

    ggplot() +
      geom_sf(
        data = df_sf,
        aes(color = sex),
        size = 3,
        alpha = 0.5
      ) +
      facet_wrap(~name, ncol = n_col) +
      coord_sf() +
      theme_minimal() +
      scale_color_manual(
        values = sex_colors,
        name = "Sex"
      ) +
      labs(
        title = "GPS Tracking Data by Elephant",
        x = "Longitude",
        y = "Latitude"
      ) +
      theme(
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
        strip.text = element_text(size = 12, face = "bold"),
        legend.position = "bottom"
      )
  })

  # ── Migration & Climate ───────────────────────────────────────────────────────────────

  gps_data_reactive <- reactive({
    req(input$selected_elephant)

    elephants %>%
      filter(name == input$selected_elephant) %>%
      group_by(date) %>%
      summarise(
        valid_records = sum(!is.na(lat) & !is.na(lon)),
        availability = pmin(100, 100 * valid_records / 24),
        .groups = "drop"
      )
  })

  output$calendar_plot <- renderPlot({
    df <- gps_data_reactive()

    validate(
      need(nrow(df) > 0, "No data available for the selected elephant.")
    )

    # We execute the function directly within the renderPlot space
    calendarHeat(
      dates = df$date,
      values = df$availability,
      at = my_ranges,
      colors = my_colors2,
      title = paste("Daily GPS Availability Calendar Heatmap -", input$selected_elephant),
      colorkey = FALSE,
      legend = list(
        right = list(
          fun = draw.key,
          args = list(key = discrete_key)
        )
      )
    )
  })

  # populate year dropdown
  observe({
    updateSelectInput(session, "year",
      choices = sort(unique(df_sf$year))
    )

    updateSelectInput(session, "month",
      choices = sprintf("%02d", 1:12)
    )

    updateSelectInput(session, "elephant",
      choices = sort(unique(df_sf$name))
    )
  })

  # reactive filtered dataset (IMPORTANT FIX)
  df_filtered <- reactive({
    req(input$year, input$month, input$elephant)

    df_sf |>
      filter(
        year == input$year,
        month == input$month,
        name == input$elephant
      )
  })

  output$map <- renderLeaflet({
    dat <- df_filtered()

    # ==========================================================
    # CASE 1: NO DATA → SHOW EMPTY STREET MAP WITH MESSAGE
    # ==========================================================
    if (nrow(dat) == 0) {
      leaflet() |>
        addProviderTiles(providers$OpenStreetMap) |>
        addPopups(
          lng = 80.0,
          lat = 7.0,
          popup = "<b>No elephant data available for selected year & month</b>"
        ) |>
        setView(lng = 80.0, lat = 7.0, zoom = 7)
    }

    # ==========================================================
    # CASE 2: DATA EXISTS → PLOT MAP
    # ==========================================================
    else {
      elephant_list <- sort(unique(dat$name))

      m <- leaflet(dat) |>
        addProviderTiles(providers$OpenStreetMap) # ONLY STREET MAP

      for (e in elephant_list) {
        d <- dat |>
          filter(name == e) |>
          arrange(datetime)

        m <- m |>
          addCircleMarkers(
            data = d,
            group = e,
            color = ~ pal(year_month),
            radius = 5,
            stroke = FALSE,
            fillOpacity = 1,
            popup = ~ paste0(
              "<b>Elephant:</b> ", name,
              "<br><b>Date:</b> ", datetime,
              "<br><b>Year:</b> ", year,
              "<br><b>Month:</b> ", month
            )
          )
      }
      m
    }
  })

  # HEATMAP
  output$summaryTable <- renderTable(
    {
      info <- plot_info[[input$variable]]
      vals <- as.numeric(info$values)

      data.frame(
        Measure = c(
          "Minimum", "First Quantile (Q1)", "Median",
          "Mean", "Third Quantile (Q3)", "Maximum",
          "Standard Deviation", "Missing Values (NA)"
        ),
        Value = c(
          min(vals, na.rm = TRUE),
          quantile(vals, 0.25, na.rm = TRUE),
          median(vals, na.rm = TRUE),
          mean(vals, na.rm = TRUE),
          quantile(vals, 0.75, na.rm = TRUE),
          max(vals, na.rm = TRUE),
          sd(vals, na.rm = TRUE),
          sum(is.na(vals))
        )
      )
    },
    digits = 2,
    colnames = FALSE,
    striped = FALSE, # Turned off to let custom CSS handle row colors
    bordered = FALSE, # Turned off to remove harsh borders
    width = "100%"
  )

  output$calendarPlot <- renderPlot({
    info <- plot_info[[input$variable]]
    discrete_key <- list(
      space = "right",
      rectangles = list(col = my_colors, border = "black", size = 2),
      text = list(info$labels, cex = 0.8),
      padding.text = 3,
      columns = 1
    )

    calendarHeat(
      dates = dates, values = info$values, at = info$breaks, colors = my_colors,
      title = info$title, colorkey = FALSE,
      legend = list(right = list(fun = draw.key, args = list(key = discrete_key)))
    )
  })

  # ── Data table ───────────────────────────────────────────────────────────────
  output$data_table <- renderDT({
    df <- filtered() %>%
      select(name, sex, datetime_sl, lat, lon, imputed) %>%
      mutate(
        datetime_sl = format(datetime_sl, "%d %b %Y %H:%M"),
        lat = round(lat, 6), lon = round(lon, 6)
      ) %>%
      rename(
        Elephant = name, Sex = sex, `Date/Time (SL)` = datetime_sl,
        Latitude = lat, Longitude = lon, Imputed = imputed
      )

    datatable(df,
      options = list(
        pageLength = 20, scrollX = TRUE,
        dom = "Bfrtip", buttons = c("csv", "excel")
      ),
      rownames = FALSE, class = "stripe hover", extensions = "Buttons"
    )
  })
}
