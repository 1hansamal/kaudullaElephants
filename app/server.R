# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  all_names    <- sort(unique(elephants_df$name))
  female_names <- sort(unique(elephants_df$name[elephants_df$sex == "Female"]))
  male_names   <- sort(unique(elephants_df$name[elephants_df$sex == "Male"]))
  
  # ── Quick-select buttons ─────────────────────────────────────────────────────
  observeEvent(input$btn_all,    updateSelectInput(session, "sel_elephants", selected = all_names))
  observeEvent(input$btn_female, updateSelectInput(session, "sel_elephants", selected = female_names))
  observeEvent(input$btn_male,   updateSelectInput(session, "sel_elephants", selected = male_names))
  observeEvent(input$btn_clear,  updateSelectInput(session, "sel_elephants", selected = character(0)))
  
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
    
    if (!is.null(input$sel_month) && input$sel_month != "all") {
      df <- df %>% filter(format(date_parsed, "%Y-%m") == input$sel_month)
    }
    
    df
  })
  
  # ── Aggregated dataset ───────────────────────────────────────────────────────
  agg_data <- reactive({
    df <- filtered()
    lvl <- input$agg_level
    
    if (lvl == "raw") return(df)
    
    df %>%
      mutate(
        period = if (lvl == "day")
          as.POSIXct(floor_date(datetime_sl, "day"))
        else
          as.POSIXct(floor_date(datetime_sl, "week"))
      ) %>%
      group_by(name, sex, period) %>%
      summarise(lat = mean(lat, na.rm = TRUE),
                lon = mean(lon, na.rm = TRUE),
                .groups = "drop") %>%
      rename(datetime_sl = period)
  })
  
  # ── Value boxes ──────────────────────────────────────────────────────────────
  output$vbox_obs <- renderValueBox({
    n <- nrow(filtered())
    valueBox(format(n, big.mark = ","), "GPS Fixes (filtered)",
             icon = icon("location-dot"), color = "green")
  })
  output$vbox_eleph <- renderValueBox({
    valueBox(length(unique(filtered()$name)), "Elephants Selected",
             icon = icon("paw"), color = "olive")
  })
  output$vbox_start <- renderValueBox({
    d <- suppressWarnings(min(filtered()$date_parsed, na.rm = TRUE))
    valueBox(if (is.finite(d)) format(d, "%d %b %Y") else "\u2014", "Data From",
             icon = icon("calendar-day"), color = "teal")
  })
  output$vbox_end <- renderValueBox({
    d <- suppressWarnings(max(filtered()$date_parsed, na.rm = TRUE))
    valueBox(if (is.finite(d)) format(d, "%d %b %Y") else "\u2014", "Data To",
             icon = icon("calendar-check"), color = "teal")
  })
  
  # ── Helper: single-coordinate plotly scatter ────────────────────────────────
  make_plot <- function(df, y_col, y_title, ref_lines = NULL, plot_source = NULL) {
    elephants_in_data <- unique(df$name)
    col_map <- ELEPHANT_COLOURS[names(ELEPHANT_COLOURS) %in% elephants_in_data]
    
    p <- plot_ly(source = plot_source)
    
    for (el in elephants_in_data) {
      sub <- df %>% filter(name == el) %>% arrange(datetime_sl)
      sub <- insert_gaps(sub)                 # FIX 2: break lines at gaps
      clr <- if (el %in% names(col_map)) col_map[[el]] else "#aaaaaa"
      
      p <- p %>% add_trace(
        data = sub,
        x    = ~datetime_sl,
        y    = as.formula(paste0("~", y_col)),
        type = "scatter",
        mode = "lines+markers",
        name = el,
        connectgaps = FALSE,                  # FIX 2: do NOT join across gaps
        line    = list(color = clr, width = 1.5),
        marker  = list(
          color   = clr,
          size    = 4,
          opacity = 0.85,
          symbol  = "circle",
          line    = list(color = clr, width = 1)
        ),
        text = ~paste0(
          "<b>", name, "</b><br>",
          "Time (SL): ", format(datetime_sl, "%d %b %Y %H:%M"), "<br>",
          y_title, ": ", round(get(y_col), 5), "\u00B0<br>",
          "Sex: ", sex
        ),
        hoverinfo = "text"
      )
      
      if (input$add_smooth && nrow(sub) > 10) {
        smooth_df <- data.frame(x = as.numeric(sub$datetime_sl), y = sub[[y_col]])
        smooth_df <- smooth_df[!is.na(smooth_df$y), ]
        if (nrow(smooth_df) > 5) {
          lo <- loess(y ~ x, data = smooth_df, span = 0.3)
          smooth_df$yhat <- predict(lo)
          smooth_df$ts   <- as.POSIXct(smooth_df$x, origin = "1970-01-01", tz = "Asia/Colombo")
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
      plot_bgcolor  = "#ffffff",
      font  = list(color = "#333333", family = "Segoe UI"),
      xaxis = list(title = "Date / Time (Asia/Colombo)", gridcolor = "#e5e5e5",
                   zerolinecolor = "#dddddd", tickformat = "%b %Y"),
      yaxis = list(title = y_title, gridcolor = "#e5e5e5", zerolinecolor = "#dddddd"),
      legend = list(bgcolor = "#ffffff", bordercolor = "#4caf50",
                    borderwidth = 1, font = list(size = 11)),
      hoverlabel = list(bgcolor = "#ffffff", font = list(color = "#333333")),
      margin = list(t = 40, b = 60, l = 70, r = 20)
    )
  }
  
  # ── Helper: dual-axis plot — latitude & longitude overlaid ──────────────────
  make_dual_axis_plot <- function(df, plot_source = NULL) {
    elephants_in_data <- unique(df$name)
    col_map <- ELEPHANT_COLOURS[names(ELEPHANT_COLOURS) %in% elephants_in_data]
    
    p <- plot_ly(source = plot_source)
    
    for (el in elephants_in_data) {
      sub <- df %>% filter(name == el) %>% arrange(datetime_sl)
      sub <- insert_gaps(sub)                 # FIX 2: break lines at gaps
      clr <- if (el %in% names(col_map)) col_map[[el]] else "#aaaaaa"
      
      # Latitude (left axis, solid, circles)
      p <- p %>% add_trace(
        data = sub, x = ~datetime_sl, y = ~lat,
        type = "scatter", mode = "lines+markers",
        name = paste(el, "\u2013 Lat"), legendgroup = el, yaxis = "y",
        connectgaps = FALSE,
        line   = list(color = clr, width = 1.5, dash = "solid"),
        marker = list(color = clr,
                      size = 4,
                      symbol = "circle",
                      line = list(color = clr, width = 1)),
        text = ~paste0("<b>", name, "</b><br>Latitude: ", round(lat, 5), "\u00B0N<br>",
                       format(datetime_sl, "%d %b %Y %H:%M"), "<br>Sex: ", sex),
        hoverinfo = "text"
      )
      
      # Longitude (right axis, dotted, triangles)
      p <- p %>% add_trace(
        data = sub, x = ~datetime_sl, y = ~lon,
        type = "scatter", mode = "lines+markers",
        name = paste(el, "\u2013 Lon"), legendgroup = el, yaxis = "y2",
        connectgaps = FALSE,
        line   = list(color = clr, width = 1.5, dash = "dot"),
        marker = list(color = clr,
                      size = 4,
                      symbol = "triangle-up",
                      line = list(color = clr, width = 1)),
        text = ~paste0("<b>", name, "</b><br>Longitude: ", round(lon, 5), "\u00B0E<br>",
                       format(datetime_sl, "%d %b %Y %H:%M"), "<br>Sex: ", sex),
        hoverinfo = "text"
      )
      
      if (input$add_smooth && nrow(sub) > 10) {
        sm_lat <- data.frame(x = as.numeric(sub$datetime_sl), y = sub$lat)
        sm_lat <- sm_lat[!is.na(sm_lat$y), ]
        if (nrow(sm_lat) > 5) {
          lo <- loess(y ~ x, data = sm_lat, span = 0.3)
          sm_lat$yhat <- predict(lo)
          sm_lat$ts   <- as.POSIXct(sm_lat$x, origin = "1970-01-01", tz = "Asia/Colombo")
          p <- p %>% add_trace(data = sm_lat, x = ~ts, y = ~yhat,
                               type = "scatter", mode = "lines",
                               name = paste(el, "Lat smooth"), legendgroup = el, yaxis = "y",
                               line = list(color = clr, width = 2.5, dash = "solid"),
                               opacity = 0.4, showlegend = FALSE, hoverinfo = "skip")
        }
        sm_lon <- data.frame(x = as.numeric(sub$datetime_sl), y = sub$lon)
        sm_lon <- sm_lon[!is.na(sm_lon$y), ]
        if (nrow(sm_lon) > 5) {
          lo2 <- loess(y ~ x, data = sm_lon, span = 0.3)
          sm_lon$yhat <- predict(lo2)
          sm_lon$ts   <- as.POSIXct(sm_lon$x, origin = "1970-01-01", tz = "Asia/Colombo")
          p <- p %>% add_trace(data = sm_lon, x = ~ts, y = ~yhat,
                               type = "scatter", mode = "lines",
                               name = paste(el, "Lon smooth"), legendgroup = el, yaxis = "y2",
                               line = list(color = clr, width = 2.5, dash = "dashdot"),
                               opacity = 0.4, showlegend = FALSE, hoverinfo = "skip")
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
      p <- p %>% add_segments(x = x_min, xend = x_max, y = rl$val, yend = rl$val, yaxis = "y",
                              line = list(color = rl$color, width = 1, dash = "dash"),
                              name = rl$label, showlegend = TRUE, hoverinfo = "name")
    }
    for (rl in ref_lon) {
      p <- p %>% add_segments(x = x_min, xend = x_max, y = rl$val, yend = rl$val, yaxis = "y2",
                              line = list(color = rl$color, width = 1, dash = "dashdot"),
                              name = rl$label, showlegend = TRUE, hoverinfo = "name")
    }
    
    p %>% layout(
      paper_bgcolor = "#ffffff",
      plot_bgcolor  = "#ffffff",
      font  = list(color = "#333333", family = "Segoe UI"),
      xaxis = list(title = "Date / Time (Asia/Colombo)", gridcolor = "#e5e5e5",
                   zerolinecolor = "#dddddd", tickformat = "%b %Y", domain = c(0, 1)),
      yaxis = list(title = "Latitude (\u00B0N, WGS84)", gridcolor = "#e5e5e5",
                   zerolinecolor = "#dddddd",
                   titlefont = list(color = "#0277bd"), tickfont = list(color = "#0277bd")),
      yaxis2 = list(title = "Longitude (\u00B0E, WGS84)", overlaying = "y", side = "right",
                    showgrid = FALSE,
                    titlefont = list(color = "#ef6c00"), tickfont = list(color = "#ef6c00")),
      legend = list(bgcolor = "#ffffff", bordercolor = "#4caf50",
                    borderwidth = 1, font = list(size = 10)),
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
    make_plot(df, "lat", "Latitude (\u00B0N, WGS84)", ref_lines, plot_source = "lat_plotly")
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
    make_plot(df, "lon", "Longitude (\u00B0E, WGS84)", ref_lines, plot_source = "lon_plotly")
  })
  
  # ── Both coordinates (dual y-axis) ──────────────────────────────────────────
  output$plot_both <- renderPlotly({
    df <- agg_data()
    validate(need(nrow(df) > 0, "No data for the selected filters."))
    make_dual_axis_plot(df, plot_source = "both_plotly")
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # SYNCED MAP — hovering on the Lat/Lon/Both time-series charts moves a
  # marker on the map to that exact GPS fix and draws the path travelled so
  # far, per elephant, so direction of movement is visible at a glance.
  # ══════════════════════════════════════════════════════════════════════════
  
  # ── Robustly turn whatever plotly gives back for the x-hover value into a
  #    POSIXct in Sri-Lanka time ──────────────────────────────────────────────
  parse_hover_time <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.numeric(x)) {
      # plotly sometimes reports epoch milliseconds for datetime axes
      return(as.POSIXct(x / 1000, origin = "1970-01-01", tz = "Asia/Colombo"))
    }
    t <- suppressWarnings(as.POSIXct(x, tz = "Asia/Colombo"))
    if (is.na(t)) {
      t <- suppressWarnings(as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Colombo"))
    }
    t
  }
  
  get_col <- function(nm) {
    if (nm %in% names(ELEPHANT_COLOURS)) unname(ELEPHANT_COLOURS[[nm]]) else "#888888"
  }
  
  # Single reactive "scrubber" position, driven by whichever chart the user
  # is hovering over (Latitude, Longitude, or the combined Both-Coordinates
  # chart) — all three synced maps below react to it.
  hover_time <- reactiveVal(NULL)
  
  observeEvent(event_data("plotly_hover", source = "lat_plotly"), {
    ed <- event_data("plotly_hover", source = "lat_plotly")
    t  <- parse_hover_time(ed$x)
    if (!is.null(t) && !is.na(t)) hover_time(t)
  })
  observeEvent(event_data("plotly_hover", source = "lon_plotly"), {
    ed <- event_data("plotly_hover", source = "lon_plotly")
    t  <- parse_hover_time(ed$x)
    if (!is.null(t) && !is.na(t)) hover_time(t)
  })
  observeEvent(event_data("plotly_hover", source = "both_plotly"), {
    ed <- event_data("plotly_hover", source = "both_plotly")
    t  <- parse_hover_time(ed$x)
    if (!is.null(t) && !is.na(t)) hover_time(t)
  })
  
  # ── Base map (context layer): full, faint tracks for every elephant that
  #    passes the current filters — redrawn only when the filters change,
  #    NOT on every hover (that's handled separately via leafletProxy) ───────
  build_base_sync_map <- function(df) {
    elephants_in_data <- sort(unique(df$name))
    
    m <- leaflet() %>%
      addProviderTiles("CartoDB.Positron", group = "Light") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      addLayersControl(
        baseGroups = c("Light", "Satellite"),
        options    = layersControlOptions(collapsed = TRUE)
      )
    
    for (el in elephants_in_data) {
      sub <- df %>% filter(name == el) %>% arrange(datetime_sl)
      sub <- sub[!is.na(sub$lat) & !is.na(sub$lon), ]
      if (nrow(sub) < 2) next
      m <- m %>% addPolylines(
        data = sub, lng = ~lon, lat = ~lat,
        color = get_col(el), weight = 1.5, opacity = 0.35,
        group = "context"
      )
    }
    
    if (nrow(df) > 0) {
      m <- m %>% fitBounds(
        lng1 = min(df$lon, na.rm = TRUE), lat1 = min(df$lat, na.rm = TRUE),
        lng2 = max(df$lon, na.rm = TRUE), lat2 = max(df$lat, na.rm = TRUE)
      )
    }
    
    if (length(elephants_in_data) > 0) {
      m <- m %>% addLegend(
        "bottomright",
        colors  = vapply(elephants_in_data, get_col, character(1)),
        labels  = elephants_in_data,
        title   = "Elephant", opacity = 0.9
      )
    }
    m
  }
  
  output$sync_map_lat  <- renderLeaflet({ build_base_sync_map(agg_data()) })
  output$sync_map_lon  <- renderLeaflet({ build_base_sync_map(agg_data()) })
  output$sync_map_both <- renderLeaflet({ build_base_sync_map(agg_data()) })
  
  # ── Progress layer: on every hover, redraw (via proxy, no full re-render)
  #    each elephant's path up to the hovered time plus a bold "current
  #    position" marker, so consecutive hover points are joined by a line ──
  update_sync_progress <- function(map_id) {
    df <- agg_data()
    ht <- hover_time()
    
    proxy <- leafletProxy(map_id) %>% clearGroup("progress")
    if (is.null(ht) || nrow(df) == 0) return(invisible(NULL))
    
    elephants_in_data <- sort(unique(df$name))
    
    for (el in elephants_in_data) {
      sub <- df %>%
        filter(name == el, datetime_sl <= ht) %>%
        arrange(datetime_sl)
      sub <- sub[!is.na(sub$lat) & !is.na(sub$lon), ]
      if (nrow(sub) == 0) next
      
      clr <- get_col(el)
      
      if (nrow(sub) >= 2) {
        proxy <- proxy %>% addPolylines(
          data = sub, lng = ~lon, lat = ~lat,
          color = clr, weight = 3, opacity = 0.95,
          group = "progress"
        )
      }
      
      cur <- sub[nrow(sub), ]
      proxy <- proxy %>%
        addCircleMarkers(
          data = cur, lng = ~lon, lat = ~lat,
          radius = 7, color = "#ffffff", weight = 2,
          fillColor = clr, fillOpacity = 1,
          group = "progress",
          popup = ~paste0(
            "<b>", name, "</b><br>",
            format(datetime_sl, "%d %b %Y %H:%M"), "<br>",
            "Lat: ", round(lat, 5), "\u00B0N<br>",
            "Lon: ", round(lon, 5), "\u00B0E"
          ),
          label = ~paste0(name, " \u2014 ", format(datetime_sl, "%d %b %Y %H:%M"))
        )
    }
    invisible(NULL)
  }
  
  observeEvent(hover_time(), {
    update_sync_progress("sync_map_lat")
    update_sync_progress("sync_map_lon")
    update_sync_progress("sync_map_both")
  }, ignoreNULL = FALSE)
  
  # ══════════════════════════════════════════════════════════════════════════
  # LIVE ELEPHANT PATH — a dedicated page: pick ONE elephant, then press the
  # slider's play button to watch its GPS path get drawn frame-by-frame on
  # the map, in perfect time-sync with the Latitude/Longitude charts below.
  # ══════════════════════════════════════════════════════════════════════════
  
  # Data for the chosen elephant only, respecting the sidebar's Date Range /
  # Month filters but NOT the multi-elephant "Select Elephants" checklist
  # (so this page always shows whichever elephant you pick here).
  live_base_data <- reactive({
    req(input$live_elephant, input$date_range)
    df <- elephants_df %>%
      filter(
        name == input$live_elephant,
        date_parsed >= input$date_range[1],
        date_parsed <= input$date_range[2]
      )
    if (!is.null(input$live_month) && input$live_month != "all") {
      df <- df %>% filter(format(date_parsed, "%Y-%m") == input$live_month)
    }
    df %>% arrange(datetime_sl)
  })
  
  # ── Playback slider — rebuilt whenever the elephant/date range changes,
  #    so it always spans exactly that elephant's number of GPS fixes ───────
  output$live_slider_ui <- renderUI({
    df <- live_base_data()
    validate(need(nrow(df) > 0, "No GPS fixes for this elephant in the selected date range."))
    sliderInput(
      "live_frame",
      paste0("Fix 1 of ", nrow(df), " \u2014 drag or press \u25B6 to animate"),
      min = 1, max = nrow(df), value = 1, step = 1, width = "100%",
      animate = animationOptions(interval = 250, loop = FALSE)
    )
  })
  
  # ── Convex-hull (home-range) area at every cumulative fix count, computed
  #    once per elephant/month/date-range change (not on every frame tick) ──
  live_hull_series <- reactive({
    df <- live_base_data()
    n  <- nrow(df)
    areas <- rep(NA_real_, n)
    if (n >= 3) {
      for (i in 3:n) {
        h <- tryCatch(mcp_compute_hull(df[seq_len(i), ]), error = function(e) NULL)
        areas[i] <- if (is.null(h)) NA_real_ else h$area_km2
      }
    }
    areas
  })
  
  # ── Current-position info card ──────────────────────────────────────────
  output$live_info_box <- renderUI({
    df <- live_base_data()
    req(nrow(df) > 0, input$live_frame)
    n   <- min(input$live_frame, nrow(df))
    cur <- df[n, ]
    step_km <- if (n > 1) {
      round(mcp_haversine_km(df$lat[n - 1], df$lon[n - 1], df$lat[n], df$lon[n]), 3)
    } else NA_real_
    hull_km2 <- live_hull_series()[n]
    
    tags$div(
      style = "font-size:12px; line-height:1.9; margin-top:4px;",
      tags$p(tags$b("Elephant: "), cur$name),
      tags$p(tags$b("Time: "), format(cur$datetime_sl, "%d %b %Y %H:%M"), " (SL time)"),
      tags$p(tags$b("Latitude: "), round(cur$lat, 5), "\u00B0N"),
      tags$p(tags$b("Longitude: "), round(cur$lon, 5), "\u00B0E"),
      tags$p(tags$b("Step distance: "), if (is.na(step_km)) "\u2014" else paste0(step_km, " km")),
      tags$p(tags$b("Hull area so far: "), if (is.na(hull_km2)) "\u2014 (need \u2265 3 fixes)" else paste0(round(hull_km2, 3), " km\u00B2")),
      tags$p(tags$b("Progress: "), n, " / ", nrow(df), " fixes")
    )
  })
  
  # ── Base map: full faint track for the chosen elephant — rebuilt only
  #    when the elephant or date range changes, not on every frame ─────────
  output$live_map <- renderLeaflet({
    df <- live_base_data()
    validate(need(nrow(df) > 0, "No GPS fixes for this elephant in the selected date range."))
    clr <- get_col(input$live_elephant)
    
    m <- leaflet() %>%
      addProviderTiles("CartoDB.Positron", group = "Light") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      addLayersControl(
        baseGroups = c("Light", "Satellite"),
        options    = layersControlOptions(collapsed = TRUE)
      ) %>%
      addPolylines(
        data = df, lng = ~lon, lat = ~lat,
        color = clr, weight = 1.5, opacity = 0.25, group = "context"
      ) %>%
      fitBounds(
        lng1 = min(df$lon, na.rm = TRUE), lat1 = min(df$lat, na.rm = TRUE),
        lng2 = max(df$lon, na.rm = TRUE), lat2 = max(df$lat, na.rm = TRUE)
      )
    m
  })
  
  # ── Frame-by-frame progress: fires every time the slider moves (manually
  #    or via the animate/play button), redrawing only the "in-progress"
  #    layer via leafletProxy — this is what makes the path draw live ───────
  observeEvent(input$live_frame, {
    df <- live_base_data()
    req(nrow(df) > 0)
    n   <- min(input$live_frame, nrow(df))
    sub <- df[seq_len(n), ]
    clr <- get_col(input$live_elephant)
    
    proxy <- leafletProxy("live_map") %>% clearGroup("liveprogress")
    
    # Growing convex-hull (home-range) polygon, built only from fixes so far
    if (nrow(sub) >= 3) {
      hull <- tryCatch(mcp_compute_hull(sub), error = function(e) NULL)
      if (!is.null(hull)) {
        proxy <- proxy %>% addPolygons(
          lng = hull$lons, lat = hull$lats,
          color = clr, weight = 1.5, dashArray = "4",
          fillColor = clr, fillOpacity = 0.12,
          group = "liveprogress"
        )
      }
    }
    
    if (nrow(sub) >= 2) {
      proxy <- proxy %>% addPolylines(
        data = sub, lng = ~lon, lat = ~lat,
        color = clr, weight = 3.5, opacity = 0.95, group = "liveprogress"
      )
    }
    cur <- sub[nrow(sub), ]
    proxy %>% addCircleMarkers(
      data = cur, lng = ~lon, lat = ~lat,
      radius = 8, color = "#ffffff", weight = 2,
      fillColor = clr, fillOpacity = 1, group = "liveprogress",
      popup = ~paste0(
        "<b>", name, "</b><br>",
        format(datetime_sl, "%d %b %Y %H:%M"), "<br>",
        "Lat: ", round(lat, 5), "\u00B0N<br>",
        "Lon: ", round(lon, 5), "\u00B0E"
      )
    )
  })
  
  # ── Live home-range (hull) area chart — grows as frames advance ──────────
  output$live_hull_plot <- renderPlotly({
    df <- live_base_data()
    validate(need(nrow(df) >= 3, "Need at least 3 GPS fixes to compute a home-range polygon."))
    req(input$live_frame)
    n     <- min(input$live_frame, nrow(df))
    areas <- live_hull_series()
    clr   <- get_col(input$live_elephant)
    
    full_df <- data.frame(datetime_sl = df$datetime_sl, area_km2 = areas)
    sub_df  <- full_df[seq_len(n), ]
    
    plot_ly() %>%
      add_trace(
        data = full_df, x = ~datetime_sl, y = ~area_km2,
        type = "scatter", mode = "lines", name = "Final",
        line = list(color = clr, width = 1, dash = "dot"),
        opacity = 0.25, hoverinfo = "skip", showlegend = FALSE
      ) %>%
      add_trace(
        data = sub_df, x = ~datetime_sl, y = ~area_km2,
        type = "scatter", mode = "lines+markers", name = "So far",
        line = list(color = clr, width = 2.5),
        marker = list(color = clr, size = 5),
        text = ~paste0(format(datetime_sl, "%d %b %Y %H:%M"), "<br>",
                       "Hull area: ", round(area_km2, 3), " km\u00B2"),
        hoverinfo = "text", showlegend = FALSE
      ) %>%
      layout(
        paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff",
        font  = list(color = "#333333", family = "Segoe UI"),
        xaxis = list(title = "", gridcolor = "#e5e5e5"),
        yaxis = list(title = "Home-range / hull area (km\u00B2)", gridcolor = "#e5e5e5"),
        margin = list(t = 20, b = 40, l = 60, r = 20)
      )
  })
  
  # ── Live Latitude / Longitude vs time charts — revealed up to the
  #    current frame, so the line is drawn in step with the map ────────────
  make_live_plot <- function(df, sub, y_col, y_title, clr) {
    plot_ly() %>%
      add_trace(
        data = df, x = ~datetime_sl, y = as.formula(paste0("~", y_col)),
        type = "scatter", mode = "lines", name = "Full track",
        line = list(color = clr, width = 1, dash = "dot"),
        opacity = 0.25, hoverinfo = "skip", showlegend = FALSE
      ) %>%
      add_trace(
        data = sub, x = ~datetime_sl, y = as.formula(paste0("~", y_col)),
        type = "scatter", mode = "lines+markers", name = "Travelled so far",
        line = list(color = clr, width = 2.5),
        marker = list(color = clr, size = 5),
        text = ~paste0(format(datetime_sl, "%d %b %Y %H:%M"), "<br>",
                       y_title, ": ", round(get(y_col), 5), "\u00B0"),
        hoverinfo = "text", showlegend = FALSE
      ) %>%
      layout(
        paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff",
        font  = list(color = "#333333", family = "Segoe UI"),
        xaxis = list(title = "", gridcolor = "#e5e5e5"),
        yaxis = list(title = y_title, gridcolor = "#e5e5e5"),
        margin = list(t = 20, b = 40, l = 60, r = 20)
      )
  }
  
  output$live_lat_plot <- renderPlotly({
    df <- live_base_data()
    validate(need(nrow(df) > 0, "No data."))
    req(input$live_frame)
    n   <- min(input$live_frame, nrow(df))
    make_live_plot(df, df[seq_len(n), ], "lat", "Latitude (\u00B0N)", get_col(input$live_elephant))
  })
  
  output$live_lon_plot <- renderPlotly({
    df <- live_base_data()
    validate(need(nrow(df) > 0, "No data."))
    req(input$live_frame)
    n   <- min(input$live_frame, nrow(df))
    make_live_plot(df, df[seq_len(n), ], "lon", "Longitude (\u00B0E)", get_col(input$live_elephant))
  })
  
  # ── Small static reference map: Kaudulla Tank & park boundary ──────────────
  # This is the same geographic anchor (tank + N/S/E/W boundary lines) used
  # for every dashed reference line on the Latitude, Longitude, and
  # Both-Coordinates charts above — shown here as an actual map for context.
  output$kaudulla_ref_map <- renderLeaflet({
    
    # Approximate park boundary (rectangle) — matches the reference lines
    # used in make_plot()/make_dual_axis_plot(): lat 8.080-8.220, lon 80.872-80.950
    park_boundary <- data.frame(
      lon = c(80.872, 80.950, 80.950, 80.872, 80.872),
      lat = c(8.080,  8.080,  8.220,  8.220,  8.080)
    )
    
    # Points #1-3 — same three rows at the top of the Key Coordinates table
    ref_points <- data.frame(
      num  = c("1", "2", "3"),
      name = c("Kaudulla Tank (core reference)",
               "Park entrance / safari zone",
               "Kaudulla Wewa (mapped reservoir)"),
      lat  = c(8.140, 8.111, 8.168),
      lon  = c(80.895, 80.886, 80.926),
      note = c("Dry-season water source \u2014 latitude/longitude reference lines pivot around this point.",
               "Southwestern edge of the elephants' core range; jeep safari staging area.",
               "Northeastern shoreline of the reservoir; frequent dry-season gathering point.")
    )
    
    # Boundary edges #4-7 — same four rows at the bottom of the Key Coordinates
    # table. Each is drawn as its own highlighted edge (not just the faint
    # rectangle) with a numbered badge at its midpoint, so every table row has
    # a directly matching feature on the map.
    ref_edges <- list(
      list(num = "4", name = "Southern park boundary",
           lng = c(80.872, 80.950), lat = c(8.080, 8.080),
           mid_lng = 80.911, mid_lat = 8.080),
      list(num = "5", name = "Northern park boundary",
           lng = c(80.872, 80.950), lat = c(8.220, 8.220),
           mid_lng = 80.911, mid_lat = 8.220),
      list(num = "6", name = "Eastern boundary (HEC zone)",
           lng = c(80.950, 80.950), lat = c(8.080, 8.220),
           mid_lng = 80.950, mid_lat = 8.150),
      list(num = "7", name = "Western park boundary",
           lng = c(80.872, 80.872), lat = c(8.080, 8.220),
           mid_lng = 80.872, mid_lat = 8.150)
    )
    
    badge_label <- function(num, cls) {
      htmltools::HTML(paste0("<span class='ref-badge ", cls, "'>", num, "</span>"))
    }
    
    m <- leaflet() |>
      addProviderTiles(providers$OpenStreetMap) |>
      addPolygons(
        data = park_boundary, lng = ~lon, lat = ~lat,
        color = "#2e7d32", weight = 1, dashArray = "6 4",
        fill = TRUE, fillColor = "#2e7d32", fillOpacity = 0.06,
        label = "Kaudulla National Park \u2014 approximate boundary used for all reference lines"
      )
    
    # Boundary edges (#4-7): highlighted orange segment + numbered badge
    for (e in ref_edges) {
      m <- m |>
        addPolylines(
          lng = e$lng, lat = e$lat,
          color = "#c1440e", weight = 4, opacity = 0.85, dashArray = "8 5",
          label = paste0("#", e$num, " \u2014 ", e$name)
        ) |>
        addLabelOnlyMarkers(
          lng = e$mid_lng, lat = e$mid_lat,
          label = badge_label(e$num, "boundary"),
          labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE,
                                      direction = "center", className = "leaflet-div-badge")
        )
    }
    
    # Point features (#1-3): teal circle marker + numbered badge
    m <- m |>
      addCircleMarkers(
        data = ref_points, lng = ~lon, lat = ~lat,
        radius = 9, color = "#0f766e", fillColor = "#4fc3f7",
        fillOpacity = 0.9, stroke = TRUE, weight = 2,
        popup = ~paste0("<b>#", num, " \u2014 ", name, "</b><br>", round(lat, 3), "\u00B0N, ", round(lon, 3), "\u00B0E<br>", note)
      ) |>
      addLabelOnlyMarkers(
        data = ref_points, lng = ~lon, lat = ~lat,
        label = ~lapply(num, badge_label, cls = "core"),
        labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE,
                                    direction = "center", className = "leaflet-div-badge")
      ) |>
      addLegend(
        position = "bottomright",
        colors = c("#0f766e", "#c1440e"),
        labels = c("Key point (badges 1\u20133)", "Boundary line (badges 4\u20137)"),
        opacity = 0.9
      ) |>
      setView(lng = 80.905, lat = 8.15, zoom = 12)
    
    m
  })
  
  # ── Monthly aggregation for heat maps ───────────────────────────────────────
  heat_data <- reactive({
    df <- filtered()
    validate(need(nrow(df) > 0, "No data for the selected filters."))
    
    df <- df %>% mutate(ym = format(datetime_sl, "%Y-%m"))
    months_seq <- format(
      seq(as.Date(format(min(df$datetime_sl), "%Y-%m-01")),
          as.Date(format(max(df$datetime_sl), "%Y-%m-01")), by = "month"), "%Y-%m")
    names_seq <- sort(unique(df$name))
    
    agg <- df %>% group_by(name, ym) %>%
      summarise(mlon = mean(lon, na.rm = TRUE), n = n(), .groups = "drop")
    
    mat_lon <- matrix(NA_real_, length(names_seq), length(months_seq),
                      dimnames = list(names_seq, months_seq))
    mat_n <- mat_lon
    for (i in seq_len(nrow(agg))) {
      mat_lon[agg$name[i], agg$ym[i]] <- agg$mlon[i]
      mat_n[agg$name[i],   agg$ym[i]] <- agg$n[i]
    }
    list(months = months_seq, names = names_seq, mat_lon = mat_lon, mat_n = mat_n)
  })
  
  output$heat_lon <- renderPlotly({
    hd <- heat_data()
    plot_ly(x = hd$months, y = hd$names, z = hd$mat_lon, type = "heatmap",
            colors = colorRamp(c("#f1faee", "#2a9d8f", "#e9c46a", "#e63946")),
            hoverongaps = FALSE,
            colorbar = list(title = "Mean\nLon (\u00B0E)",
                            tickfont = list(color = "#333333"),
                            titlefont = list(color = "#333333")),
            hovertemplate = "%{y}<br>%{x}<br>Mean lon %{z:.4f}\u00B0E<extra></extra>") %>%
      layout(paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff",
             font = list(color = "#333333", family = "Segoe UI"),
             xaxis = list(title = "Month", tickangle = -45, gridcolor = "#e5e5e5"),
             yaxis = list(title = "", autorange = "reversed", gridcolor = "#e5e5e5"),
             margin = list(t = 20, b = 70, l = 110, r = 20)) %>%
      config(displaylogo = FALSE)
  })
  
  output$heat_n <- renderPlotly({
    hd <- heat_data()
    plot_ly(x = hd$months, y = hd$names, z = hd$mat_n, type = "heatmap",
            colors = colorRamp(c("#f1faee", "#ff9f1c", "#e63946")),
            hoverongaps = FALSE,
            colorbar = list(title = "GPS\nFixes",
                            tickfont = list(color = "#333333"),
                            titlefont = list(color = "#333333")),
            hovertemplate = "%{y}<br>%{x}<br>%{z} fixes<extra></extra>") %>%
      layout(paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff",
             font = list(color = "#333333", family = "Segoe UI"),
             xaxis = list(title = "Month", tickangle = -45, gridcolor = "#e5e5e5"),
             yaxis = list(title = "", autorange = "reversed", gridcolor = "#e5e5e5"),
             margin = list(t = 20, b = 70, l = 110, r = 20)) %>%
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
        color = ~color_palette_tracking(name),
        radius = 4,
        stroke = FALSE,
        fillOpacity = 0.8,
        popup = ~paste("<b>Date:</b>", datetime,
                       "<br><b>Gender:</b>", sex,
                       "<br><b>Name:</b>", name)
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
  
  # GPS by elephants
  addResourcePath("pre_rendered", img_dir)
  
  current_idx <- reactiveVal(1)
  is_playing <- reactiveVal(FALSE)
  timer <- reactiveTimer(1000)
  
  observeEvent(input$btn_toggle, {
    is_playing(!is_playing())
    if (is_playing()) {
      updateActionButton(session, "btn_toggle", label = "⏸ Pause")
      removeClass("btn_toggle", "btn-success")
      addClass("btn_toggle", "btn-warning")
    } else {
      updateActionButton(session, "btn_toggle", label = "▶ Play")
      removeClass("btn_toggle", "btn-warning")
      addClass("btn_toggle", "btn-success")
    }
  })
  
  observe({
    if (!is_playing()) return() 
    timer()
    isolate({
      if (current_idx() < length(active_months)) {
        current_idx(current_idx() + 1)
      } else {
        current_idx(1)
      }
    })
  })
  
  observeEvent(input$btn_next, {
    if (current_idx() < length(active_months)) {
      current_idx(current_idx() + 1)
    }
  })
  
  observeEvent(input$btn_prev, {
    if (current_idx() > 1) {
      current_idx(current_idx() - 1)
    }
  })
  
  current_date <- reactive({
    active_months[current_idx()]
  })
  
  output$current_month_ui <- renderText({
    format(current_date(), "%Y %b")
  })
  
  output$elephant_plot <- renderImage({
    list(
      src = file.path(img_dir, paste0("plot_", current_idx(), ".png")),
      contentType = "image/png",
      alt = "Elephant Tracking Map",
      height = "100%",
      width = "auto"
    )
  }, deleteFile = FALSE)
  
  
  
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
                      choices = sort(unique(df_sf$year)))
    
    updateSelectInput(
      session,
      "month",
      choices  = setNames(sprintf("%02d", 1:12), month.name),
      selected = sprintf("%02d", 1:12)[1]
    )
    
    updateSelectInput(session, "elephant",
                      choices = sort(unique(df_sf$name)))
  })
  
  # reactive filtered dataset (IMPORTANT FIX)
  df_filtered <- reactive({
    req(input$year, input$month, input$elephant, input$select_week)
    
    d <- df_sf |>
      filter(
        year == input$year,
        month %in% input$month,
        name == input$elephant
      )
    
    # If one or more SPECIFIC weeks are chosen (i.e. "All Weeks" isn't
    # among the picks), narrow down to just those weeks. Start/end markers
    # in build_migration_map() are computed from whatever is returned here,
    # so they automatically reflect the start/end of the chosen week(s).
    if (!("All Weeks" %in% input$select_week)) {
      d <- d |> filter(week_of_month %in% input$select_week)
    }
    
    d
  })
  
  # Build the migration map for a given filtered dataset (reused for
  # both the live app render AND the "open in new tab" export)
  build_migration_map <- function(dat, show_numbers = FALSE) {
    
    if (nrow(dat) == 0) {
      return(
        leaflet() |>
          addProviderTiles(providers$OpenStreetMap) |>
          addPopups(
            lng = 80.0,
            lat = 7.0,
            popup = "<b>No elephant data available for selected year & month</b>"
          ) |>
          setView(lng = 80.0, lat = 7.0, zoom = 7)
      )
    }
    
    elephant_list <- sort(unique(dat$name))
    
    m <- leaflet(dat) |>
      addProviderTiles(providers$OpenStreetMap)
    
    for (e in elephant_list) {
      
      d <- dat |>
        filter(name == e) |>
        arrange(datetime)
      
      # Points plotted ONE BY ONE in chronological order (not connected by
      # lines), all the SAME fixed size. Direction/order is conveyed via
      # the optional sequence-number labels below, not by point size.
      n_pts <- nrow(d)
      
      for (i in seq_len(n_pts)) {
        row_i <- d[i, ]
        m <- m |>
          addCircleMarkers(
            data = row_i,
            group = e,
            color = ~pal_week(week_of_month),
            radius = 5,
            stroke = FALSE,
            fillOpacity = 1,
            popup = ~paste0(
              "<b>Elephant:</b> ", name,
              "<br><b>Sequence:</b> ", i, " of ", n_pts,
              "<br><b>Date:</b> ", datetime,
              "<br><b>Week:</b> ", week_of_month,
              "<br><b>Year:</b> ", year,
              "<br><b>Month:</b> ", month
            )
          )
        
        # Optional sequence-number label right on top of each point,
        # toggled by the "Show point sequence numbers" checkbox.
        if (isTRUE(show_numbers)) {
          m <- m |>
            addLabelOnlyMarkers(
              data = row_i,
              label = as.character(i),
              labelOptions = labelOptions(
                noHide = TRUE,
                textOnly = TRUE,
                direction = "top",
                offset = c(0, -8),
                style = list(
                  "font-weight" = "bold",
                  "font-size"   = "12px",
                  "color"       = "black",
                  "text-shadow" = "-1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff"
                )
              )
            )
        }
      }
      
      # ── Start / End markers ─────────────────────────────────────
      start_pt <- d[1, ]
      end_pt   <- d[nrow(d), ]
      
      m <- m |>
        addAwesomeMarkers(
          data = start_pt,
          icon = awesomeIcons(icon = "play", library = "fa",
                              markerColor = "green", iconColor = "white"),
          popup = ~paste0("<b>", e, "</b><br>Start: ", datetime)
        ) |>
        addAwesomeMarkers(
          data = end_pt,
          icon = awesomeIcons(icon = "flag-checkered", library = "fa",
                              markerColor = "red", iconColor = "white"),
          popup = ~paste0("<b>", e, "</b><br>End: ", datetime)
        )
    }
    
    m <- m |>
      addLegend(
        pal = pal_week,
        values = dat$week_of_month,
        title = "Week of Month",
        position = "bottomright"
      )
    
    m
  }
  
  output$map <- renderLeaflet({
    build_migration_map(df_filtered(), show_numbers = input$show_seq_numbers)
  })
  
  # ── Open migration map in a separate browser tab ─────────────────
  observeEvent(input$open_map_newtab, {
    m <- build_migration_map(df_filtered(), show_numbers = input$show_seq_numbers)
    export_path <- file.path(tempdir(), "migration_map_popup.html")
    htmlwidgets::saveWidget(m, export_path, selfcontained = TRUE)
    addResourcePath("mapexport", tempdir())
    runjs(sprintf("window.open('mapexport/%s', '_blank');",
                  basename(export_path)))
  })
  
  # HEATMAP
  output$summaryTable <- renderTable({
    info <- plot_info[[input$variable]]
    vals <- as.numeric(info$values)
    yrs  <- format(dates, "%Y")
    
    summary_by_year <- function(v) {
      c(
        min(v, na.rm = TRUE),
        quantile(v, 0.25, na.rm = TRUE),
        median(v, na.rm = TRUE),
        mean(v, na.rm = TRUE),
        quantile(v, 0.75, na.rm = TRUE),
        max(v, na.rm = TRUE),
        sd(v, na.rm = TRUE),
        sum(is.na(v))
      )
    }
    
    yr_levels <- sort(unique(yrs))
    
    out <- data.frame(
      Measure = c(
        "Minimum", "First Quantile (Q1)", "Median",
        "Mean", "Third Quantile (Q3)", "Maximum",
        "Standard Deviation", "Missing Values (NA)"
      )
    )
    
    for (y in yr_levels) {
      out[[y]] <- summary_by_year(vals[yrs == y])
    }
    
    out
  },
  digits = 2,
  colnames = TRUE,   # now show year headers
  striped = FALSE,
  bordered = FALSE,
  width = "100%"
  )
  
  output$calendarPlot <- renderPlot({
    info <- plot_info[[input$variable]]
    calendar_key <- list(
      space = "right",
      rectangles = list(col = my_colors, border = "black", size = 4),
      text = list(info$labels, cex = 1.1),
      padding.text = 4,
      columns = 1
    )
    
    calendarHeat(
      dates = dates, values = info$values, at = info$breaks, colors = my_colors,
      title = info$title, colorkey = FALSE,
      legend = list(right = list(fun = draw.key, args = list(key = calendar_key)))
    )
  })
  
  # ── Data table ───────────────────────────────────────────────────────────────
  output$data_table <- renderDT({
    df <- filtered() %>%
      select(name, sex, datetime_sl, lat, lon) %>%
      mutate(datetime_sl = format(datetime_sl, "%d %b %Y %H:%M"),
             lat = round(lat, 6), lon = round(lon, 6)) %>%
      rename(Elephant = name, Sex = sex, `Date/Time (SL)` = datetime_sl,
             Latitude = lat, Longitude = lon)
    
    datatable(df,
              options = list(pageLength = 20, scrollX = TRUE,
                             dom = "Bfrtip", buttons = c("csv", "excel")),
              rownames = FALSE, class = "stripe hover", extensions = "Buttons")
  })
  # ══════════════════════════════════════════════════════════════════════════
  # Home Range & Speed module (from app (6).R)
  # ══════════════════════════════════════════════════════════════════════════
  
  # ---- Filtered data (now driven by the GLOBAL sidebar: elephant, date, month) ----
  mcp_filtered_data <- reactive({
    req(input$date_range)
    
    df <- mcp_tracking_clean %>%
      filter(
        sex %in% input$mcp_sex_filter,
        as.Date(datetime) >= input$date_range[1],
        as.Date(datetime) <= input$date_range[2]
      )
    
    if (length(input$sel_elephants) > 0) {
      df <- df %>% filter(name %in% input$sel_elephants)
    } else {
      df <- df[0, ]
    }
    
    if (!is.null(input$sel_month) && input$sel_month != "all") {
      df <- df %>% filter(format(datetime, "%Y-%m") == input$sel_month)
    }
    
    df
  })
  
  # ---- Hull + summary ----
  mcp_hull_results <- reactive({
    df <- mcp_filtered_data()
    elephants_present <- sort(unique(df$name))
    hulls <- list()
    summary_rows <- list()
    
    for (elephant in elephants_present) {
      edata <- df %>% filter(name == elephant)
      h <- mcp_compute_hull(edata)
      step_dist <- edata$step_km[is.finite(edata$step_km)]
      step_speed <- edata$speed_kmh[is.finite(edata$speed_kmh)]
      
      hulls[[elephant]] <- h
      
      area_km2_val <- if (is.null(h)) NA_real_ else round(h$area_km2, 3)
      area_hectares_val <- if (is.null(h)) NA_real_ else round(h$area_km2 * 100, 0)
      
      days_tracked <- as.numeric(
        difftime(max(edata$datetime), min(edata$datetime), units = "days")
      )
      
      summary_rows[[elephant]] <- data.frame(
        name          = elephant,
        sex           = edata$sex[1],
        n_points      = nrow(edata),
        area_km2      = area_km2_val,
        area_hectares = area_hectares_val,
        total_dist_km = round(sum(step_dist), 1),
        km_per_day    = round(sum(step_dist) / max(days_tracked, 1), 2),
        max_step_km   = round(suppressWarnings(max(step_dist)), 2),
        avg_speed_kmh = round(suppressWarnings(mean(step_speed)), 2),
        max_speed_kmh = round(suppressWarnings(max(step_speed)), 2),
        days_tracked  = round(days_tracked, 0)
      )
    }
    
    list(
      hulls = hulls,
      summary = if (length(summary_rows) > 0) {
        bind_rows(summary_rows) %>% arrange(desc(area_km2))
      } else {
        data.frame()
      }
    )
  })
  
  # ---- Value boxes ----
  output$mcp_vb_elephants <- renderValueBox({
    valueBox(length(unique(mcp_filtered_data()$name)), "Elephants Shown",
             icon = icon("signs-post"), color = "green"
    )
  })
  
  output$mcp_vb_points <- renderValueBox({
    n <- nrow(mcp_filtered_data())
    txt <- if (n == 0) "\u2014" else format(n, big.mark = ",")
    valueBox(txt, "No. of GPS Fixes", icon = icon("location-dot"), color = "blue")
  })
  
  output$mcp_vb_distance <- renderValueBox({
    d <- mcp_filtered_data()$step_km
    d <- d[is.finite(d)]
    txt <- if (length(d) == 0) "\u2014" else paste0(format(round(sum(d), 1), big.mark = ","), " km")
    valueBox(txt, "Total Distance Covered", icon = icon("route"), color = "purple")
  })
  
  output$mcp_vb_speed <- renderValueBox({
    s <- mcp_filtered_data()$speed_kmh
    s <- s[is.finite(s)]
    txt <- if (length(s) == 0) "\u2014" else paste0(round(mean(s), 2), " km/h")
    valueBox(txt, "Avg. Speed", icon = icon("gauge-high"), color = "teal")
  })
  
  
  
  # ---- Map (base rendered once, contents updated via proxy) ----
  output$mcp_hull_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron", group = "Light") %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "Street") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      setView(
        lng  = mean(mcp_tracking_clean$lon, na.rm = TRUE),
        lat  = mean(mcp_tracking_clean$lat, na.rm = TRUE),
        zoom = 12
      ) %>%
      addScaleBar(position = "bottomleft")
  })
  
  observe({
    df <- mcp_filtered_data()
    res <- mcp_hull_results()
    elephants_present <- sort(unique(df$name))
    
    # Clear every possible hull group (one per elephant) plus points/centers,
    # using their ACTUAL group names -- "hulls"/"points"/"centers" never
    # matched anything that was actually added, so old hulls from previously
    # selected elephants were never being removed.
    all_hull_groups <- paste(mcp_unique_elephants, "- Hull")
    
    proxy <- leafletProxy("mcp_hull_map") %>%
      clearGroup(all_hull_groups) %>%
      clearGroup("All GPS Points") %>%
      clearGroup("All Centers") %>%
      clearControls()
    
    if (length(elephants_present) == 0) {
      return()
    }
    
    for (elephant in elephants_present) {
      edata <- df %>% filter(name == elephant)
      color <- mcp_elephant_colors[[elephant]]
      h <- res$hulls[[elephant]]
      area_info <- res$summary %>% filter(name == elephant)
      
      if (!is.null(h) && nrow(area_info) > 0) {
        popup_text <- paste0(
          "<div style='width:220px;'>",
          "<h4>", elephant, " &mdash; MCP</h4>",
          "<b>Sex:</b> ", area_info$sex, "<br>",
          "<b>Area:</b> ", area_info$area_km2, " km&sup2; (",
          format(area_info$area_hectares, big.mark = ","), " ha)<br>",
          "<b>GPS fixes:</b> ", format(area_info$n_points, big.mark = ","), "<br>",
          "<b>Total distance:</b> ", area_info$total_dist_km, " km<br>",
          "<b>Avg speed:</b> ", area_info$avg_speed_kmh, " km/h<br>",
          "</div>"
        )
        proxy <- proxy %>%
          addPolygons(
            lng = h$lons, lat = h$lats,
            color = color, weight = 2, opacity = 1,
            fillColor = color, fillOpacity = 0.15,
            popup = popup_text,
            group = paste(elephant, "- Hull"),
            layerId = paste0("mcp_hull_", elephant)
          )
      }
      
      proxy <- proxy %>%
        addCircleMarkers(
          data = edata, lng = ~lon, lat = ~lat,
          popup = ~ paste0("<b>", name, "</b><br>", datetime),
          label = elephant, radius = 3,
          color = color, fillColor = color,
          fillOpacity = 0.6, weight = 1, stroke = TRUE,
          group = "All GPS Points"
        ) %>%
        addCircleMarkers(
          lng = mean(edata$lon), lat = mean(edata$lat),
          radius = 7, color = "#FFFFFF",
          fillColor = color, fillOpacity = 1, weight = 2,
          popup = paste("<b>Center:</b>", elephant),
          group = "All Centers"
        )
    }
    
    hull_groups <- paste(elephants_present, "- Hull")
    
    proxy %>%
      addLayersControl(
        baseGroups    = c("Light", "Street", "Satellite"),
        overlayGroups = c(hull_groups, "All GPS Points", "All Centers"),
        options       = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup(c("All GPS Points", "All Centers")) %>%
      addLegend(
        "bottomright",
        colors  = unname(mcp_elephant_colors[elephants_present]),
        labels  = elephants_present,
        title   = "Elephant",
        opacity = 0.8
      )
  })
  
  # ---- Movement timeline ----
  output$mcp_timeline_plot <- renderPlotly({
    df <- mcp_filtered_data() %>%
      filter(!is.na(step_km)) %>%
      arrange(name, datetime) %>%
      group_by(name) %>%
      mutate(cum_dist_km = cumsum(coalesce(step_km, 0))) %>%
      ungroup()
    
    validate(need(nrow(df) > 0, "No data for the selected filters."))
    
    plot_ly(
      df,
      x = ~datetime, y = ~cum_dist_km, color = ~name,
      colors = mcp_elephant_colors[unique(df$name)],
      type = "scatter", mode = "lines",
      hovertemplate = "<b>%{fullData.name}</b><br>%{x}<br>Cumulative: %{y:.1f} km<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "", gridcolor = "rgba(0,0,0,0.08)"),
        yaxis = list(title = "Cumulative distance (km)", gridcolor = "rgba(0,0,0,0.08)"),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(color = "#333333"),
        legend = list(orientation = "h", y = -0.2)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # ---- Area bar chart ----
  output$mcp_area_bar_chart <- renderPlotly({
    res <- mcp_hull_results()$summary %>% filter(!is.na(area_km2))
    validate(need(nrow(res) > 0, "No elephant in this selection has enough GPS fixes for a home range."))
    
    bar_df <- res %>% mutate(name = factor(name, levels = rev(name)))
    
    plot_ly(
      bar_df,
      x = ~area_km2, y = ~name, type = "bar", orientation = "h",
      marker = list(color = unname(mcp_elephant_colors[as.character(bar_df$name)])),
      text = ~ paste0(area_km2, " km²"),
      textposition = "auto",
      textfont = list(color = "#0D0D0D"),
      hovertemplate = "<b>%{y}</b><br>Area: %{x} km²<extra></extra>"
    ) %>%
      layout(
        xaxis = list(
          title     = "Area (km²)",
          gridcolor = "rgba(0,0,0,0.08)",
          range     = c(0, max(bar_df$area_km2, na.rm = TRUE) * 1.15)
        ),
        yaxis = list(title = "", automargin = TRUE),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(color = "#333333"),
        margin = list(l = 10, r = 20, t = 10, b = 40)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # ---- Individual rose plots ----
  output$mcp_rose_individual_ui <- renderUI({
    df <- mcp_filtered_data() %>% filter(is.finite(bearing))
    validate(need(nrow(df) > 0, "No movement data for the selected filters."))
    
    elephants_present <- sort(unique(df$name))
    n <- length(elephants_present)
    n_cols <- 2
    n_rows <- ceiling(n / n_cols)
    plot_height <- paste0(max(250, min(350, 900 / n_rows)), "px")
    
    rows <- lapply(seq_len(n_rows), function(row_i) {
      idx <- ((row_i - 1) * n_cols + 1):min(row_i * n_cols, n)
      fluidRow(
        lapply(elephants_present[idx], function(elephant) {
          column(6, plotlyOutput(
            outputId = paste0("mcp_rose_", gsub("[^A-Za-z0-9]", "_", elephant)),
            height   = plot_height
          ))
        })
      )
    })
    
    tagList(rows)
  })
  
  observe({
    df <- mcp_filtered_data() %>% filter(is.finite(bearing))
    elephants_present <- sort(unique(df$name))
    
    lapply(elephants_present, function(elephant) {
      local({
        el <- elephant
        col <- mcp_elephant_colors[[el]]
        output_id <- paste0("mcp_rose_", gsub("[^A-Za-z0-9]", "_", el))
        
        output[[output_id]] <- renderPlotly({
          edata <- df %>% filter(name == el)
          validate(need(nrow(edata) > 1, paste(el, ": not enough data")))
          mcp_make_rose_plot(edata$bearing, col, el)
        })
      })
    })
  })
  
  # ---- Population rose plot ----
  output$mcp_rose_population <- renderPlotly({
    df <- mcp_filtered_data() %>% filter(is.finite(bearing))
    validate(need(nrow(df) > 0, "No movement data for the selected filters."))
    
    bd <- mcp_bin_bearings(df$bearing)
    
    plot_ly(
      bd,
      type = "barpolar",
      r = ~r, theta = ~theta,
      marker = list(
        color = ~r,
        colorscale = list(
          c(0, "#1a237e"), c(0.25, "#1565C0"),
          c(0.5, "#00BCD4"), c(0.75, "#4CAF50"),
          c(1, "#FF5252")
        ),
        showscale = TRUE,
        colorbar = list(title = "Fixes", tickfont = list(color = "#333333"))
      ),
      hovertemplate = "%{theta}°: %{r} fixes<extra></extra>"
    ) %>%
      layout(
        polar = list(
          angularaxis = list(
            tickmode  = "array",
            tickvals  = c(0, 45, 90, 135, 180, 225, 270, 315),
            ticktext  = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
            direction = "clockwise",
            rotation  = 90,
            gridcolor = "rgba(0,0,0,0.12)",
            linecolor = "rgba(0,0,0,0.2)",
            tickfont  = list(color = "#333333", size = 14)
          ),
          radialaxis = list(
            gridcolor = "rgba(0,0,0,0.08)",
            linecolor = "rgba(0,0,0,0.1)",
            tickfont  = list(color = "#666", size = 10)
          ),
          bgcolor = "rgba(0,0,0,0)"
        ),
        paper_bgcolor = "rgba(0,0,0,0)",
        font = list(color = "#333333"),
        showlegend = FALSE,
        margin = list(l = 60, r = 60, t = 40, b = 40)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # ---- Summary table ----
  output$mcp_summary_table <- renderDT({
    res <- mcp_hull_results()$summary
    validate(need(nrow(res) > 0, "No data for the selected filters."))
    
    res %>%
      select(
        Elephant           = name,
        Sex                = sex,
        `Days Tracked`     = days_tracked,
        `Total Dist. (km)` = total_dist_km,
        `km / day`         = km_per_day,
        `Max Step (km)`    = max_step_km,
        `GPS Fixes`        = n_points,
        `Area (km²)`       = area_km2,
        `Area (ha)`        = area_hectares,
        `Avg Speed (km/h)` = avg_speed_kmh,
        `Max Speed (km/h)` = max_speed_kmh
      ) %>%
      datatable(
        rownames = FALSE,
        options = list(
          pageLength = 14,
          dom        = "ft",
          order      = list(list(3, "desc")), # 0-based: col 3 = Total Dist.
          scrollX    = TRUE
        ),
        class = "display compact"
      )
  })
  
}
