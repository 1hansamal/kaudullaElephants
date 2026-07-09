# ============================================
# MODULE: elephant_tracking
#
# UI:     mod_elephant_tracking_UI(id)
# Server: mod_elephant_tracking_Server(id, tracking_clean, unique_elephants,
#                                       unique_sexes, elephant_colors,
#                                       min_date, max_date)
#
# All data is passed in from the caller rather than loaded inside the
# module - see R/data_prep.R. This module can be instantiated more than
# once in the same app (e.g. one tab per park) as long as each call
# uses a distinct `id`.
# ============================================

# ---- UI ----
mod_elephant_tracking_UI <- function(id) {
  ns <- NS(id)

  tagList(
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        title = "Filters",

        checkboxGroupInput(
          ns("sex_filter"), "Sex",
          choices  = NULL,   # populated in Server via updateCheckboxGroupInput
          selected = NULL
        ),

        selectizeInput(
          ns("elephant_filter"), "Elephants",
          choices  = NULL,   # populated in Server via updateSelectizeInput
          multiple = TRUE,
          options  = list(plugins = list("remove_button"))
        ),

        dateRangeInput(
          ns("date_filter"), "Date range"
        ),

        actionButton(ns("select_all"),  "Select all elephants", class = "btn-sm btn-outline-light w-100 mb-2"),
        actionButton(ns("select_none"), "Clear all",            class = "btn-sm btn-outline-light w-100"),

        hr(),
        helpText(
          "MCP home ranges, GPS movement tracks, and speed/direction metrics ",
          "for elephants tracked at Kaudulla National Park."
        )
      ),

      layout_columns(
        col_widths = c(3, 3, 3, 3),
        value_box(
          title    = "Elephants Shown",
          value    = textOutput(ns("vb_elephants")),
          showcase = bsicons::bs_icon("signpost-split"),
          theme    = "primary"
        ),
        value_box(
          title    = "GPS Fixes",
          value    = textOutput(ns("vb_points")),
          showcase = bsicons::bs_icon("geo-alt-fill"),
          theme    = "info"
        ),
        value_box(
          title    = "Avg. Speed",
          value    = textOutput(ns("vb_speed")),
          showcase = bsicons::bs_icon("speedometer2"),
          theme    = "success"
        ),
        value_box(
          title    = "Total Distance",
          value    = textOutput(ns("vb_distance")),
          showcase = bsicons::bs_icon("rulers"),
          theme    = "warning"
        )
      ),

      navset_card_underline(

        nav_panel(
          "Home Range Map",
          card(
            full_screen = TRUE,
            card_header("Tracking Points & Minimum Convex Polygons"),
            leafletOutput(ns("hull_map"), height = "600px")
          )
        ),

        nav_panel(
          "Movement Timeline",
          card(
            full_screen = TRUE,
            card_header("Cumulative Distance Traveled Over Time"),
            plotlyOutput(ns("timeline_plot"), height = "550px")
          )
        ),

        nav_panel(
          "Speed & Distance",
          card(
            full_screen = TRUE,
            card_header("Home Range Area by Elephant"),
            plotlyOutput(ns("area_bar_chart"), height = "550px")
          )
        ),

        nav_panel(
          "Movement Directions",
          navset_tab(
            nav_panel(
              "Individual Rose Plots",
              card(
                full_screen = TRUE,
                card_header("Movement Direction by Elephant (16 compass sectors)"),
                uiOutput(ns("rose_individual_ui"))
              )
            ),
            nav_panel(
              "Population Rose Plot",
              card(
                full_screen = TRUE,
                card_header("Overall Movement Direction — All Selected Elephants Combined"),
                plotlyOutput(ns("rose_population"), height = "600px")
              )
            )
          )
        ),

        nav_panel(
          "Summary Table",
          card(
            full_screen = TRUE,
            card_header("Per-Elephant Summary"),
            DTOutput(ns("summary_table"))
          )
        )
      )
    )
  )
}

# ---- Server ----
mod_elephant_tracking_Server <- function(id,
                                          tracking_clean,
                                          unique_elephants,
                                          unique_sexes,
                                          elephant_colors,
                                          min_date,
                                          max_date) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Initialize inputs that depend on the data passed in ----
    observe({
      updateCheckboxGroupInput(
        session, "sex_filter",
        choices  = unique_sexes,
        selected = unique_sexes
      )
      updateSelectizeInput(
        session, "elephant_filter",
        choices  = unique_elephants,
        selected = unique_elephants,
        server   = FALSE
      )
      updateDateRangeInput(
        session, "date_filter",
        start = min_date, end = max_date,
        min   = min_date, max = max_date
      )
    }) %>% bindEvent(TRUE, once = TRUE)

    observeEvent(input$select_all, {
      updateSelectizeInput(session, "elephant_filter", selected = unique_elephants)
    })
    observeEvent(input$select_none, {
      updateSelectizeInput(session, "elephant_filter", selected = character(0))
    })

    # ---- Filtered data ----
    filtered_data <- reactive({
      req(input$elephant_filter, input$date_filter)
      tracking_clean %>%
        filter(
          name %in% input$elephant_filter,
          sex  %in% input$sex_filter,
          as.Date(datetime) >= input$date_filter[1],
          as.Date(datetime) <= input$date_filter[2]
        )
    })

    # ---- Hull + summary ----
    hull_results <- reactive({
      df <- filtered_data()
      elephants_present <- sort(unique(df$name))
      hulls <- list()
      summary_rows <- list()

      for (elephant in elephants_present) {
        edata       <- df %>% filter(name == elephant)
        h           <- compute_hull(edata)
        step_dist   <- edata$step_km[is.finite(edata$step_km)]
        step_speed  <- edata$speed_kmh[is.finite(edata$speed_kmh)]

        hulls[[elephant]] <- h

        area_km2_val      <- if (is.null(h)) NA_real_ else round(h$area_km2, 3)
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
        hulls   = hulls,
        summary = if (length(summary_rows) > 0)
          bind_rows(summary_rows) %>% arrange(desc(area_km2))
        else
          data.frame()
      )
    })

    # ---- Value boxes ----
    output$vb_elephants <- renderText({ length(unique(filtered_data()$name)) })

    output$vb_points <- renderText({ comma(nrow(filtered_data())) })

    output$vb_speed <- renderText({
      s <- filtered_data()$speed_kmh
      s <- s[is.finite(s)]
      if (length(s) == 0) return("—")
      paste0(round(mean(s), 2), " km/h")
    })

    output$vb_distance <- renderText({
      d <- filtered_data()$step_km
      d <- d[is.finite(d)]
      paste0(comma(round(sum(d), 0)), " km")
    })

    # ---- Map (base rendered once, contents updated via proxy) ----
    output$hull_map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles("CartoDB.Positron",     group = "Light") %>%
        addProviderTiles("OpenStreetMap.Mapnik", group = "Street") %>%
        addProviderTiles("Esri.WorldImagery",    group = "Satellite") %>%
        setView(
          lng  = mean(tracking_clean$lon, na.rm = TRUE),
          lat  = mean(tracking_clean$lat, na.rm = TRUE),
          zoom = 12
        ) %>%
        addScaleBar(position = "bottomleft")
    })

    observe({
      df <- filtered_data()
      res <- hull_results()
      elephants_present <- sort(unique(df$name))

      proxy <- leafletProxy("hull_map") %>%
        clearGroup("hulls") %>%
        clearGroup("points") %>%
        clearGroup("centers") %>%
        clearControls()

      if (length(elephants_present) == 0) return()

      for (elephant in elephants_present) {
        edata     <- df %>% filter(name == elephant)
        color     <- elephant_colors[[elephant]]
        h         <- res$hulls[[elephant]]
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
              layerId = paste0("hull_", elephant)
            )
        }

        proxy <- proxy %>%
          addCircleMarkers(
            data = edata, lng = ~lon, lat = ~lat,
            popup = ~paste0("<b>", name, "</b><br>", datetime),
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
          colors  = unname(elephant_colors[elephants_present]),
          labels  = elephants_present,
          title   = "Elephant",
          opacity = 0.8
        )
    })

    # ---- Movement timeline ----
    output$timeline_plot <- renderPlotly({
      df <- filtered_data() %>%
        filter(!is.na(step_km)) %>%
        arrange(name, datetime) %>%
        group_by(name) %>%
        mutate(cum_dist_km = cumsum(coalesce(step_km, 0))) %>%
        ungroup()

      validate(need(nrow(df) > 0, "No data for the selected filters."))

      plot_ly(
        df, x = ~datetime, y = ~cum_dist_km, color = ~name,
        colors = elephant_colors[unique(df$name)],
        type = "scatter", mode = "lines",
        hovertemplate = "<b>%{fullData.name}</b><br>%{x}<br>Cumulative: %{y:.1f} km<extra></extra>"
      ) %>%
        layout(
          xaxis  = list(title = "", gridcolor = "rgba(0,0,0,0.08)"),
          yaxis  = list(title = "Cumulative distance (km)", gridcolor = "rgba(0,0,0,0.08)"),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          font   = list(color = "#333333"),
          legend = list(orientation = "h", y = -0.2)
        ) %>%
        config(displayModeBar = FALSE)
    })

    # ---- Area bar chart ----
    output$area_bar_chart <- renderPlotly({
      res <- hull_results()$summary %>% filter(!is.na(area_km2))
      validate(need(nrow(res) > 0, "No elephant in this selection has enough GPS fixes for a home range."))

      bar_df <- res %>% mutate(name = factor(name, levels = rev(name)))

      plot_ly(
        bar_df, x = ~area_km2, y = ~name, type = "bar", orientation = "h",
        marker = list(color = unname(elephant_colors[as.character(bar_df$name)])),
        text = ~paste0(area_km2, " km²"),
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
          yaxis         = list(title = "", automargin = TRUE),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          font          = list(color = "#333333"),
          margin        = list(l = 10, r = 20, t = 10, b = 40)
        ) %>%
        config(displayModeBar = FALSE)
    })

    # ---- Individual rose plots ----
    output$rose_individual_ui <- renderUI({
      df <- filtered_data() %>% filter(is.finite(bearing))
      validate(need(nrow(df) > 0, "No movement data for the selected filters."))

      elephants_present <- sort(unique(df$name))
      n      <- length(elephants_present)
      n_cols <- 3
      n_rows <- ceiling(n / n_cols)
      plot_height <- paste0(max(250, min(350, 900 / n_rows)), "px")

      rows <- lapply(seq_len(n_rows), function(row_i) {
        idx <- ((row_i - 1) * n_cols + 1):min(row_i * n_cols, n)
        fluidRow(
          lapply(elephants_present[idx], function(elephant) {
            column(4, plotlyOutput(
              outputId = ns(paste0("rose_", gsub("[^A-Za-z0-9]", "_", elephant))),
              height   = plot_height
            ))
          })
        )
      })

      tagList(rows)
    })

    observe({
      df <- filtered_data() %>% filter(is.finite(bearing))
      elephants_present <- sort(unique(df$name))

      lapply(elephants_present, function(elephant) {
        local({
          el        <- elephant
          col       <- elephant_colors[[el]]
          output_id <- paste0("rose_", gsub("[^A-Za-z0-9]", "_", el))

          output[[output_id]] <- renderPlotly({
            edata <- df %>% filter(name == el)
            validate(need(nrow(edata) > 1, paste(el, ": not enough data")))
            make_rose_plot(edata$bearing, col, el)
          })
        })
      })
    })

    # ---- Population rose plot ----
    output$rose_population <- renderPlotly({
      df <- filtered_data() %>% filter(is.finite(bearing))
      validate(need(nrow(df) > 0, "No movement data for the selected filters."))

      bd <- bin_bearings(df$bearing)

      plot_ly(
        bd, type = "barpolar",
        r = ~r, theta = ~theta,
        marker = list(
          color     = ~r,
          colorscale = list(
            c(0, "#1a237e"), c(0.25, "#1565C0"),
            c(0.5, "#00BCD4"), c(0.75, "#4CAF50"),
            c(1, "#FF5252")
          ),
          showscale = TRUE,
          colorbar  = list(title = "Fixes", tickfont = list(color = "#333333"))
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
          font          = list(color = "#333333"),
          showlegend    = FALSE,
          margin        = list(l = 60, r = 60, t = 40, b = 40)
        ) %>%
        config(displayModeBar = FALSE)
    })

    # ---- Summary table ----
    output$summary_table <- renderDT({
      res <- hull_results()$summary
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
          options  = list(
            pageLength = 14,
            dom        = "ft",
            order      = list(list(3, "desc")),  # 0-based: col 3 = Total Dist.
            scrollX    = TRUE
          ),
          class = "display compact"
        )
    })

    # ---- Return values (optional, lets a parent app react to this module) ----
    return(
      list(
        filtered_data = filtered_data,
        hull_results  = hull_results
      )
    )
  })
}
