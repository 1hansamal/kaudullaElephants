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


# ==============================================================
# 5. UI
# ==============================================================
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(
    title = tags$span(
      tags$img(
        src = "https://upload.wikimedia.org/wikipedia/commons/1/11/Flag_of_Sri_Lanka.svg",
        height = "22px", style = "margin-right:8px; vertical-align:middle;"
      ),
      "Kaudulla Elephant Tracker"
    ),
    titleWidth = 320
  ),
  dashboardSidebar(
    width = 270,
    tags$div(
      style = "padding:14px 16px 6px; color:#ccc; font-size:12px; line-height:1.5;",
      tags$b("Kaudulla National Park"), tags$br(),
      "North Central Province, Sri Lanka", tags$br(),
      "8\u00B008\u2032N  80\u00B054\u2032E", tags$br(),
      tags$hr(style = "border-color:#444; margin:8px 0;"),
      tags$i("GPS Collar Monitoring Programme"), tags$br(),
      tags$a("Wildlife Department of Sri Lanka",
        href = "https://wildlife.gov.lk", target = "_blank",
        style = "color:#8bc34a;"
      ),
      tags$hr(style = "border-color:#444; margin:8px 0;")
    ),
    sidebarMenu(
      menuItem("Latitude vs Time", tabName = "lat_tab", icon = icon("chart-line")),
      menuItem("Longitude vs Time", tabName = "lon_tab", icon = icon("chart-line")),
      menuItem("Both Coordinates", tabName = "both_tab", icon = icon("layer-group")),
      menuItem("Heat Maps", tabName = "heat_tab", icon = icon("fire")),
      menuItem("Elephant Tracking", tabName = "tracking_tab", icon = icon("map")),
      menuItem("Migration & Climate", tabName = "climate_tab", icon = icon("globe")),
      menuItem("Data Table", tabName = "data_tab", icon = icon("table"))
    ),
    tags$hr(style = "border-color:#444; margin:4px 0;"),
    tags$div(
      style = "padding:0 16px;",
      selectInput(
        "sel_elephants", "Select Elephants",
        choices = sort(unique(elephants_df$name)),
        selected = sort(unique(elephants_df$name)),
        multiple = TRUE
      ),
      tags$div(
        style = "display:flex; gap:4px; margin:-4px 0 8px;",
        actionButton("btn_all", "All", class = "btn-xs", style = "flex:1; font-size:11px;"),
        actionButton("btn_female", "Females", class = "btn-xs", style = "flex:1; font-size:11px;"),
        actionButton("btn_male", "Males", class = "btn-xs", style = "flex:1; font-size:11px;"),
        actionButton("btn_clear", "Clear", class = "btn-xs", style = "flex:1; font-size:11px;")
      ),
      checkboxInput("show_imputed", "Show imputed points", value = TRUE),
      tags$hr(style = "border-color:#444; margin:4px 0;"),
      dateRangeInput(
        "date_range", "Date Range",
        start = min(elephants_df$date_parsed, na.rm = TRUE),
        end = max(elephants_df$date_parsed, na.rm = TRUE),
        min = min(elephants_df$date_parsed, na.rm = TRUE),
        max = max(elephants_df$date_parsed, na.rm = TRUE),
        format = "dd M yyyy"
      ),
      checkboxInput("add_smooth", "Add LOESS smoother", value = FALSE),
      tags$hr(style = "border-color:#444; margin:4px 0;"),
      radioButtons(
        "agg_level", "Time Resolution",
        choices = c(
          "Raw (hourly)" = "raw",
          "Daily mean" = "day",
          "Weekly mean" = "week"
        ),
        selected = "raw",
        inline = FALSE
      )
    ),
    tags$div(
      style = "padding:10px 16px 4px; font-size:10px; color:#888; line-height:1.4;",
      tags$b("Key Literature"), tags$br(),
      "Fernando et al. (2008)", tags$br(),
      "Pastorini et al. (2010)", tags$br(),
      "Ratnayeke et al. (2023)"
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("

/*====================================================
  GLOBAL
====================================================*/
body{
  background:#f5f7fb;
  font-family:'Segoe UI',system-ui,sans-serif;
}

.content-wrapper,
.right-side{
  background:#f5f7fb;
}

/*====================================================
  BOXES
====================================================*/

.box{
  background:white;
  border:none;
  border-radius:14px;
  box-shadow:0 2px 12px rgba(0,0,0,.08);
}

.box-header{
  background:white;
  color:#1e293b;
  border-bottom:1px solid #e5e7eb;
  border-radius:14px 14px 0 0;
}

.box-title{
  color:#0f766e;
  font-size:18px;
  font-weight:700;
}

/*====================================================
  SIDEBAR
====================================================*/

.main-sidebar{
  background:#ffffff;
  border-right:1px solid #e5e7eb;
}

.sidebar-menu>li>a{
    color: #ffffff !important;   /* white text */
  font-weight: 700 !important; /* bold */
  font-size:14px;
  font-weight:500;
  border-radius:10px;
  margin:5px 10px;
}

.sidebar-menu>li>a:hover{
  background:#ecfeff !important;
  color:#0f766e !important;
}

.sidebar-menu>li.active>a{
  background:#0f766e !important;
  color:white !important;
}

.sidebar-menu>li.header{
  color:#64748b !important;
}

/*====================================================
  HEADER
====================================================*/

.main-header .logo{
  background:#0f766e !important;
  color:white !important;
  font-weight:bold;
}

.main-header .navbar{
  background:#0f766e !important;
}

/*====================================================
  VALUE BOXES
====================================================*/

.small-box{
  border-radius:14px;
  box-shadow:0 3px 12px rgba(0,0,0,.08);
}

.info-box{
  background:white;
  border-radius:14px;
  box-shadow:0 3px 10px rgba(0,0,0,.06);
}

/*====================================================
  TEXT
====================================================*/

h4.ref-heading{
  color:#0f766e;
  font-size:15px;
  font-weight:700;
}

p.ref-text{
  color:#475569;
  font-size:13px;
  line-height:1.7;
}

.shiny-text-output{
  color:#334155;
}

/*====================================================
  SECTION TITLES
====================================================*/

.section-title{
  font-size:32px;
  font-weight:800;
  color:#1e293b;
  text-align:center;
  margin:30px 0 20px;
  letter-spacing:-0.5px;
}

.sub-title{
  font-size:24px;
  font-weight:700;
  color:#334155;
  text-align:center;
  margin:25px 0 15px;
}

.section-description{
  font-size:15px;
  color:#64748b;
  text-align:center;
  line-height:1.7;
  max-width:900px;
  margin:0 auto 25px auto;
}

.section-box{
  background:white;
  border-radius:14px;
  padding:20px;
  margin-bottom:25px;
  box-shadow:0 2px 12px rgba(0,0,0,.08);
}


/*====================================================
  SIDEBAR PANEL (inside sidebarLayout)
====================================================*/

.well{
  background:#0f766e !important;
  border:none !important;
  border-radius:12px;
  color:white !important;
  box-shadow:0 3px 10px rgba(0,0,0,.15);
}

.well h4,
.well h5,
.well label,
.well p,
.well .help-block{
  color:white !important;
  font-weight:600;
}

/* SelectInput */

.selectize-control.single .selectize-input{
  background:white !important;
  color:#0f766e !important;
  border-radius:8px;
  border:none;
}

.selectize-dropdown{
  border-radius:8px;
}

.selectize-dropdown .option{
  color:#1e293b;
}

.selectize-dropdown .active{
  background:#0f766e !important;
  color:white !important;
}

/* Table inside sidebar */

.well table{
  color:white;
}

.well td,
.well th{
  color:white !important;
}

/* Horizontal line */

.well hr{
  border-top:1px solid rgba(255,255,255,.35);
}

/* Action buttons */

.well .btn{
  background:white;
  color:#0f766e;
  border:none;
  border-radius:8px;
}

.well .btn:hover{
  background:#ecfdf5;
}

/* Numeric inputs */

.well input{
  border-radius:8px;
}

/*====================================================
  TABLES
====================================================*/

.dataTables_wrapper{
  color:#334155;
}

.dataTables_wrapper .dataTables_filter input{
  background:white;
  border:1px solid #cbd5e1;
  border-radius:8px;
  color:#334155;
}

table.dataTable{
  border-collapse:collapse;
}

table.dataTable tbody tr{
  background:white !important;
}

table.dataTable tbody tr:nth-child(even){
  background:#f8fafc !important;
}

table.dataTable tbody tr:hover{
  background:#ecfeff !important;
}

table.dataTable td{
  padding:10px;
}

/*====================================================
  BUTTONS
====================================================*/

.btn{
  border-radius:8px;
}

.btn-default{
  background:white;
  border:1px solid #cbd5e1;
}

.btn-default:hover{
  background:#ecfeff;
}

/*====================================================
  INPUTS
====================================================*/

.form-control{
  border-radius:8px;
  border:1px solid #cbd5e1;
}

/*====================================================
  LEAFLET
====================================================*/

.leaflet-container{
  border-radius:12px;
  border:1px solid #dbe4ee;
}

/*====================================================
  PLOTLY
====================================================*/

.js-plotly-plot,
.plotly,
.plot-container {
  background: #ffffff !important;   /* clean white */
  border-radius: 12px;
}

/* if plots are inside boxes */
.box-body {
  background: #ffffff !important;
}

/*====================================================
  SCROLLBAR
====================================================*/

::-webkit-scrollbar{
  width:8px;
}

::-webkit-scrollbar-thumb{
  background:#94a3b8;
  border-radius:5px;
}

::-webkit-scrollbar-track{
  background:#f1f5f9;
}


"))
    ),
    tabItems(
      # ── TAB 1 : Latitude vs Time ─────────────────────────────────────────────
      tabItem(
        "lat_tab",
        fluidRow(
          valueBoxOutput("vbox_obs", width = 3),
          valueBoxOutput("vbox_eleph", width = 3),
          valueBoxOutput("vbox_start", width = 3),
          valueBoxOutput("vbox_end", width = 3)
        ),
        fluidRow(
          box(
            title = "\U0001F4CD Latitude vs Time — GPS Collar Data, Kaudulla National Park",
            width = 12, solidHeader = TRUE,
            tags$p(
              style = "color:#666; font-size:11px; margin-bottom:6px;",
              "Kaudulla elephants range roughly between latitudes 8.10\u00B0N and 8.25\u00B0N.",
              "The park boundary lies around 8.08\u00B0\u20138.22\u00B0N (Fernando et al. 2008).",
              "Northward movement often corresponds to the seasonal arrival at",
              "Kaudulla tank when Minneriya dries (Ratnayeke et al. 2023)."
            ),
            plotlyOutput("plot_lat", height = "460px")
          )
        ),
        fluidRow(
          box(
            title = "\U0001F4DA Literature Context — Latitude & Elephant Ranging in Sri Lanka",
            width = 12, solidHeader = TRUE,
            tags$div(
              style = "display:flex; flex-wrap:wrap; gap:20px; padding:4px 0;",
              tags$div(
                style = "flex:1; min-width:220px;",
                tags$h4("Fernando et al. (2008)", class = "ref-heading"),
                tags$p("Home ranges of Sri Lankan elephants averaged 46\u2013103 km\u00B2,
                        with latitudinal movement of 0.1\u00B0\u20130.3\u00B0 correlated with
                        seasonal tank water levels in the dry zone.", class = "ref-text")
              ),
              tags$div(
                style = "flex:1; min-width:220px;",
                tags$h4("Ratnayeke et al. (2023)", class = "ref-heading"),
                tags$p("Kaudulla\u2013Minneriya corridor study showed elephants shift
                        northward (higher latitude) into Kaudulla from May\u2013October
                        when the Minneriya tank partially dries, with peak
                        aggregations in August\u2013September.", class = "ref-text")
              ),
              tags$div(
                style = "flex:1; min-width:220px;",
                tags$h4("Wildlife Department of Sri Lanka", class = "ref-heading"),
                tags$p("The Department's GPS collar programme in Kaudulla National Park
                        (est. 2002, 6,900 ha) monitors movement to inform HEC
                        (Human\u2013Elephant Conflict) mitigation and corridor management.",
                  class = "ref-text"
                ),
                tags$a("wildlife.gov.lk",
                  href = "https://wildlife.gov.lk",
                  target = "_blank", style = "color:#2e7d32; font-size:11px;"
                )
              ),
              tags$div(
                style = "flex:1; min-width:220px;",
                tags$h4("Geographic Reference", class = "ref-heading"),
                tags$p("Latitude 8.10\u00B0\u20138.25\u00B0N (WGS84). Kaudulla tank (reservoir)
                        at ~8.14\u00B0N is a key dry-season water source. The Mahaweli
                        River floodplain at ~8.22\u00B0N forms the northern park boundary.",
                  class = "ref-text"
                )
              )
            )
          )
        )
      ),

      # ── TAB 2 : Longitude vs Time ────────────────────────────────────────────
      tabItem(
        "lon_tab",
        fluidRow(
          box(
            title = "\U0001F4CD Longitude vs Time — GPS Collar Data, Kaudulla National Park",
            width = 12, solidHeader = TRUE,
            tags$p(
              style = "color:#666; font-size:11px; margin-bottom:6px;",
              "Longitudes span 80.87\u00B0\u201380.96\u00B0E. The Kaudulla tank lies near 80.90\u00B0E.",
              "Elephants moving eastward (higher longitude) approach the park's",
              "eastern boundary, which borders agricultural land — a key HEC zone",
              "(Pastorini et al. 2010; Wildlife Dept. Sri Lanka 2023 Annual Report)."
            ),
            plotlyOutput("plot_lon", height = "460px")
          )
        ),
        fluidRow(
          box(
            title = "\U0001F4DA Literature Context — Longitude & East\u2013West Ranging",
            width = 12, solidHeader = TRUE,
            tags$div(
              style = "display:flex; flex-wrap:wrap; gap:20px; padding:4px 0;",
              tags$div(
                style = "flex:1; min-width:220px;",
                tags$h4("Pastorini et al. (2010)", class = "ref-heading"),
                tags$p("Genetic analysis of Sri Lankan elephants confirmed
                        east\u2013west sub-population structure partly driven by
                        the Mahaweli River. Kaudulla elephants belong to the
                        eastern dry-zone meta-population.", class = "ref-text")
              ),
              tags$div(
                style = "flex:1; min-width:220px;",
                tags$h4("Leimgruber et al. (2008)", class = "ref-heading"),
                tags$p("Longitude displacement of >0.05\u00B0 per day indicates
                        long-range foraging excursions beyond the core
                        Kaudulla\u2013Minneriya protected area, with agriculture
                        along the eastern boundary being the primary
                        conflict zone.", class = "ref-text")
              ),
              tags$div(
                style = "flex:1; min-width:220px;",
                tags$h4("HEC Hotspot — Eastern Boundary", class = "ref-heading"),
                tags$p("Longitudes >80.94\u00B0E place elephants near the
                        Giritale\u2013Hingurakgoda road and paddy fields.
                        Wildlife Dept. electric fence lines run along
                        ~80.95\u00B0E on the eastern park edge.", class = "ref-text")
              ),
              tags$div(
                style = "flex:1; min-width:220px;",
                tags$h4("Geographic Reference", class = "ref-heading"),
                tags$p("Longitude 80.87\u00B0\u201380.96\u00B0E (WGS84). Kaudulla tank
                        central axis \u224880.89\u00B0E. National Highway A11
                        (Habarana\u2013Trincomalee) crosses the corridor
                        near 80.93\u00B0E and is a major elephant crossing point.",
                  class = "ref-text"
                )
              )
            )
          )
        )
      ),

      # ── TAB 3 : Both coordinates ─────────────────────────────────────────────
      tabItem(
        "both_tab",
        fluidRow(
          box(
            title = "\U0001F4CD Latitude & Longitude vs Time (Overlaid, Dual Y-Axis)",
            width = 12, solidHeader = TRUE,
            tags$p(
              style = "color:#666; font-size:11px; margin-bottom:6px;",
              "Latitude (solid lines, circles, left axis) and longitude",
              "(dotted lines, triangles, right axis) are plotted on the",
              "same chart per elephant, using matching colours. Correlated",
              "dips in latitude with rising longitude typically indicate",
              "movement toward agricultural areas on the eastern boundary."
            ),
            plotlyOutput("plot_both", height = "600px")
          )
        ),
        fluidRow(
          box(
            title = "\U0001F418 About The Gathering — Kaudulla National Park",
            width = 8, solidHeader = TRUE,
            tags$p(
              style = "color:#444; font-size:12px; line-height:1.7;",
              tags$b("Kaudulla National Park"), " was gazetted in 2002 specifically
              to protect the elephant corridor between Minneriya and Hurulu Eco Park.
              Together these three parks form the 'Trincomalee Elephant Triangle'.",
              tags$br(), tags$br(),
              "Every year between July and October, up to ", tags$b("300\u2013400 elephants"),
              " converge at the Kaudulla and Minneriya tanks in what is known as ",
              tags$b(style = "color:#2e7d32;", "'The Gathering'"),
              " — one of the largest aggregations of Asian elephants in the world
              (Fernando et al. 2008; BBC Wildlife Magazine 2009).",
              tags$br(), tags$br(),
              "This GPS collar dataset documents the movement of ",
              tags$b(style = "color:#2e7d32;", "14 individually identified elephants"),
              " from July 2024 to June 2026, capturing seasonal latitudinal shifts,
              boundary excursions, and corridor use."
            )
          ),
          box(
            title = "\U0001F3DB Wildlife Department Mandate",
            width = 4, solidHeader = TRUE,
            tags$p(
              style = "color:#444; font-size:12px; line-height:1.7;",
              "The Department of Wildlife Conservation of Sri Lanka (DWC),
              under the Ministry of Environment, administers Kaudulla under
              the Fauna and Flora Protection Ordinance (FFPO).",
              tags$br(), tags$br(),
              "The GPS collar programme contributes to:", tags$br(),
              "\u2022 Human\u2013Elephant Conflict (HEC) early warning", tags$br(),
              "\u2022 Corridor integrity assessment", tags$br(),
              "\u2022 Population monitoring", tags$br(), tags$br(),
              tags$a("wildlife.gov.lk",
                href = "https://wildlife.gov.lk",
                target = "_blank", style = "color:#2e7d32;"
              )
            )
          )
        )
      ),


      # ── TAB 4 : Heat Maps ────────────────────────────────────────────────────
      tabItem(
        "heat_tab",
        fluidRow(
          box(
            title = "\U0001F321 Average Longitude by Month — Position Heat Map (blank = no data)",
            width = 12, solidHeader = TRUE,
            tags$p(
              style = "color:#bbb; font-size:11px; margin-bottom:6px;",
              "Cell colour = mean longitude of GPS fixes for that elephant in that",
              "month (warmer = further east, toward the agricultural boundary;",
              "cooler = further west, toward the tank). Blank cells mean the",
              "elephant had no GPS fixes recorded that month within the current filters."
            ),
            plotlyOutput("heat_lon", height = "420px")
          )
        ),
        fluidRow(
          box(
            title = "\U0001F4CA Data Coverage by Month — GPS Fix Count Heat Map",
            width = 12, solidHeader = TRUE,
            tags$p(
              style = "color:#bbb; font-size:11px; margin-bottom:6px;",
              "Cell colour = number of GPS fixes recorded for that elephant in",
              "that month. Darker/blank cells flag months with sparse or missing",
              "collar data — useful for spotting collar failures or animals that",
              "temporarily left monitoring range."
            ),
            plotlyOutput("heat_n", height = "420px")
          )
        )
      ),

      # ── TAB 5 : Elephant Tracking ────────────────────────────────────────────────────
      tabItem(
        "tracking_tab",
        div(
          class = "panel-box",
          div(
            class = "section-title",
            "🗺️ Elephant Tracking Overview"
          ),
          div(
            class = "sub-title",
            "Interactive Map with Satellite View"
          ),
          leafletOutput("tracking_map", height = 600)
        ),
        div(
          class = "panel-box",
          div(
            class = "sub-title",
            "📊 Tracking Data Visualization"
          ),
          plotOutput("tracking_plot", height = 500, width = "100%")
        ),
        div(
          class = "panel-box",
          div(
            class = "sub-title",
            "👥 GPS Tracking Data by Individual Elephant"
          ),
          plotOutput("tracking_by_elephant", height = 800, width = "100%")
        )
      ),


      # ── TAB 6 : Migration & Climate ────────────────────────────────────────────────────
      tabItem(
        "climate_tab",
        div(
          class = "panel-box",
          div(
            class = "section-title",
            "🐘 Elephant Tracking Data Availability"
          ),
          sidebarLayout(
            sidebarPanel(
              selectInput(
                inputId = "selected_elephant",
                label = "Select Elephant Name:",
                choices = elephant_names,
                selected = elephant_names[1]
              ),
              hr(),
              helpText("This heatmap shows the percentage of valid GPS records captured per day (max 24 records/day).")
            ),
            mainPanel(
              plotOutput("calendar_plot", height = "600px")
            )
          )
        ),
        div(
          class = "panel-box",
          div(
            class = "section-title",
            "🐘 Elephant Migration Map"
          ),
          sidebarLayout(
            sidebarPanel(
              selectInput("year", "Select Year", choices = NULL),
              selectInput("month", "Select Month", choices = sprintf("%02d", 1:12)),
              selectInput("elephant", "Select Elephant", choices = NULL)
            ),
            mainPanel(
              leafletOutput("map", height = 600)
            )
          )
        ),
        div(
          class = "panel-box",
          div(
            class = "section-title",
            "🌡 Climate Calendar Analysis"
          ),
          sidebarLayout(
            sidebarPanel(
              selectInput("variable",
                "Climate Variable",
                choices = names(plot_info)
              ),
              h4("Summary Statistics"),
              tableOutput("summaryTable")
            ),
            mainPanel(
              plotOutput("calendarPlot", height = 500)
            )
          )
        )
      ),

      # ── TAB 7 : Data Table ───────────────────────────────────────────────────
      tabItem(
        "data_tab",
        fluidRow(
          box(
            title = "\U0001F4CB GPS Observation Records",
            width = 12, solidHeader = TRUE,
            DTOutput("data_table")
          )
        )
      )
    )
  )
)
