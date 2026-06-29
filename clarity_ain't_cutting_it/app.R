library(shiny)
library(bslib)
library(shinyTime)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(stringr)
library(lcars)

first_date <- Sys.Date() - 90
last_date <- Sys.Date()

ui <- lcarsPage(
  tagList(
    tags$style(HTML("

      @import url(https://fonts.googleapis.com/css2?family=Antonio:wght@300&display=swap);

      div, h1, h2, h3, h4,
      h5, h6, p, body, label,
      .lcars-hdr-title,
      .lcars-box-title,
      .lcars-box-subtitle,
      .lcars-element-addition,
      .lcars-btn,
      .lcars-btn-filtered,
      .lcars-checkbox {
        font-family: Antonio;
      }

      h1, h2, h4, h5, h6, p {
        color: #FFCC66;
      }

      h6 {
        font-size: 15px;
      }

      h3 {
        color: #FF7700;
      }

      h1 {
        text-align: center;
      }

      li, a {
        color: #ffffff;
        font-size: 18px;
      }

      .nav-pills>li.active>a, .nav-pills>li.active>a:focus,
      .nav-pills>li.active>a:hover, .nav-pills>li>a{
        background-color: #CC6699;
        border: 1px solid white;
        border-radius: 10px;
      }

      .nav-pills>li.active>a, .nav-pills>li.active>a:focus, .nav-pills>li.active>a:hover{
        color: #774466;
        border: 1px solid #774466;
        border-radius: 10px;
      }

      .nav>li>a {
        margin-top: 5px;
        margin-bottom: 5px;
        border-radius: 10px;
      }

      .nav {
        margin: auto;
        width: 100%;
        border-radius: 25px;
        background-color: #CC6699;
      }

      .nav-pills {
        padding-left: 16%;
        padding-right: 15%;
        margin-top: 5px;
        margin-bottom: 5px;
      }

      .nav-item {
        margin-right: 2%;
        margin-left: 2%;
        background-color: #CC6699;
      }

      hr {
        border: none;
        margin: 10px;
        background-color: #a9a9a9;
        height: 1px;
      }

      .callout-box {
        border-top: 5px solid #FECC66;
        border-bottom: 5px solid #FECC66;
        border-left: 10px solid #FECC66;
        border-right: 10px solid #FECC66;
        border-radius: 25px;
        padding: 10px;
        margin-bottom: 10px;
        margin-top: 10px;
      }

    "))
  ),

  tags$hr(),
  titlePanel(h1("PRETEND THIS IS A TITLE")),
  tags$hr(),
  tags$hr(),

  tags$br(),

  fluidRow(
    column(3,
        lcarsPill(title = "IMPORTANT NOTES"),
        div(p("Press the 'i' icons at the bottom right of each section for
          more information about the graph.", class = "callout-box")),
        div(p("This is a fun, unique tool for investigating one's own blood
              sugar trends and management, not a replacement for licensed medical
              advice.", class = "callout-box"))
    ),
    column(6,
      lcarsBracket(color = "#FF7700",
        fluidRow(
          column(5,
             lcarsRect(
               color = "#000000",
               height = 52,
               text = h3("UPLOAD DATA HERE:"),
               text_size = 17,
               text_color = "#FFFFDD"
             )
          ),
          column(7,
             fileInput(
               "upload", "",
               buttonLabel = "Import",
               placeholder = "see directions below",
               multiple = TRUE,
               accept = ".xlsx",
               width = 250
             )
          )
        ),
        fluidRow(
          column(2, lcarsRect(color = "#000000", width = "150px")),
          column(3,
             lcarsCheckbox("trial", p("Use Trial Data Instead"),
                           value = FALSE,
                           width = 250)
          ),
          column(6,
            lcarsCheckbox(
              "instructions", p("Show Directions for Uploading Data"),
              width = 400,
              value = FALSE
            )
          )
        )
      )
    ),
    column(3,
             dateRangeInput(
               "interval", h4("Date Range"),
               start = first_date,
               end = last_date,
               min = first_date,
               max = last_date,
               width = 250
             ),
             sliderInput(
               "rect", h4("Target Average BG Range"),
               min = 90, max = 180,
               value = c(100,165),
               step = 5,
               ticks = FALSE,
               width = 250
             )
      )
  ),

  tags$hr(),

# here's the navbar and the main content of the site
  navset_pill(
    nav_panel("Averages",
      tags$hr(),
      tags$br(),
    ),

    nav_spacer(),

    nav_panel("Glycemic Variability",
      tags$hr(),
      tags$br(),
    ),

    nav_spacer(),

    nav_panel("Daily Report",
      tags$hr(),
      tags$br(),

      lcarsBox(
        corners = c(1, 2, 3, 4),
        sides = c(1, 2, 3, 4),
        color = c("#FF9900",  "#FFCC66",  "#FF9900",  "#FF9900"),
        side_color = c( "#FFCC66",  "#FFCC66", "#FF9900", "#FF9900"),
        title_color = "#FFCC66",

        title = "October 25th, 2024",
        left_inputs = inputColumn(
          dateInput("date_to_view", h4("Date"), width = 150),
          timeInput("day_start", h4("Start Time"), width = 150)
        ),

        fluidRow(
          plotOutput("daily", height = "500px",
                     hover = hoverOpts("daily_hover")
          )
        )

      )
    )
  )


)

server <- function(input, output, session) {


  output$daily <- renderPlot ({
    clarity_10_23_01_20 <- clarity_10_23_01_20[-(1:19),]

    # removing the random and very annoying T from all the date/time values
    clarity_10_23_01_20$...2 <- str_replace_all(clarity_10_23_01_20$...2, "T", " ")

    # in the app, time1 will be given by timeInput
    # for now, using randomly selected row
    time1 <- clarity_10_23_01_20$...2[666]
    time2 <- as.character(as.POSIXct(time1) + hours(24))


    clarity <- clarity_10_23_01_20 |>
      filter(...2 >= time1 & ...2 <= time2) |>
      rename(time = ...2, value = ...8) |>
      mutate(time = as.POSIXct(time), value = as.numeric(value))

    extrema <- c(max(clarity$value), min(clarity$value))

    avg <- mean(clarity$value)

    capt <- paste0("Highest Value = ", extrema[1], "      Lowest Value = ", extrema[2])

    daily_overview <- ggplot() +
      geom_hline(aes(yintercept = 180), linetype = "dotted", color = "#ED1D24", linewidth = 0.6) +
      geom_hline(aes(yintercept = 70), linetype = "dotted", color = "#ED1D24", linewidth = 0.6) +
      geom_line(clarity, mapping = aes(x = time, y = value), linewidth = 0.5, color = "#1a1c1a") +
      scale_y_continuous(breaks = seq(40, 400, 60), limits = c(40,400), expand = c(0,0)) +
      scale_x_datetime(
        date_labels = "%d %b, %H:%M",
        date_minor_breaks = "hour",
        expand = c(0, 0)
      ) +
      labs(
        title = "Daily Overview",
        subtitle = "Oct. 24th to 25th",
        x = "Date/Time",
        y = "Blood Glucose (mg/dL)",
        caption = capt
      ) +
      theme_lcars_light() +
      theme(
        panel.grid.major = element_line(linewidth = 0.1, color = "#1a1c1a"),
        panel.grid.minor = element_line(linewidth = 0.05, color = "#444444"),
        panel.border = element_rect(linewidth = 0.75),
        plot.margin = margin(14, 14, 14, 14),
        plot.background = element_rect(color = "#1a1c1a", linewidth = 1.2),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(margin = margin(0, 0, 10, 0), hjust = 0.5),
        text = element_text(family = "sans"),
        axis.title.x = element_text(size = 12, margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 12, margin = margin(t = 0, r = 10, b = 0, l = 0))
      )
    daily_overview
  })


  observeEvent(input$instructions, {
    req(input$instructions == TRUE)
    showModal(
      modalDialog(
        title = h4("HOW TO UPLOAD DEXCOM CGM DATA", style = "color: #000000;"),
        div(
          p("Step 1: Log in to Dexcom Clarity. On the far right side of the 'Overview'
          page, just above the 'Sensor Usage' widget, is an icon that looks like
          a spreadsheet with an arrow pointing right.", style = "color: #000000;"),
          p("Step 2: Click that icon, select a date range, then press export. Dexcom
          only allows downloads of up to 90 days worth of data at a time, so if you
          would like to analyze a longer timespan, repeat this step to download
          multiple 90 day datasets.", style = "color: #000000;"),
          p("Step 3:  The file will automatically open in Numbers. Go to File >
          Export To > Excel to convert it to the correct format.", style = "color: #000000;"),
          p("Step 4: Finally, come back here and hit import! Hold down command
            while selecting files to upload multiple files at once.", style = "color: #000000;")
        ),
        footer = modalButton("OKAY"),
        size = c("m")
      )
    )
    updateCheckboxInput(session, "instructions", value = FALSE)
  })

}

shinyApp(ui = ui, server = server)



overview_dttm1 <- reactiveVal()
overview_dttm2 <- reactiveVal()

observeEvent({
  input$upload
  input$trial
}, {
  overview_dttm1(as.POSIXct(all_data()$date[1], tz = ""))
  overview_dttm2(as.POSIXct(all_data()$date[1], tz = "") + hours(24))

  updateDateInput(session, inputId = "overview_date", value = as_date(all_data()$date[1]))
})


observeEvent({
  input$overview_date
}, {
  all_date_data <- all_data() |>
    select(date) |>
    mutate(date = as.Date(date))

  if (input$overview_date %in% all_date_data$date == TRUE) {
    overview_dttm1(as.POSIXct(input$overview_date) + hours(8))
    overview_dttm2(as.POSIXct(input$overview_date) + hours(32))
  }
  else {
    showModal(
      modalDialog(
        paste("TO DISPLAY GRAPH, PLEASE SELECT A DATE BETWEEN", as.character(first_date()), "AND", as.character(last_date())),
        footer = modalButton("OKAY"),
        size = c("s")
      )
    )
  }
})

output$daily_title <- renderText({
  format(as.Date(overview_dttm1()), "%B %d, %Y")
})

# daily overview graph
output$daily <- renderPlot ({

  overview_data <- all_data()
  overview_dttm1 <- as.POSIXct(overview_dttm1(), tz = "")
  overview_dttm2 <- as.POSIXct(overview_dttm2(), tz = "")

  overview_data <- overview_data |>
    filter(date >= overview_dttm1 & date <= overview_dttm2) |>
    mutate(time = as.POSIXct(date), value = as.numeric(value))

  extrema <- c(max(overview_data$value), min(overview_data$value))

  avg <- mean(overview_data$value)

  capt <- paste0("Highest Value = ", extrema[1], "      Lowest Value = ", extrema[2])

  daily_overview <- ggplot() +
    annotate(
      "rect",
      xmin = overview_dttm1, xmax = overview_dttm2,
      ymin = 70, ymax = 180,
      fill = "#99DDFF",
      alpha = 0.25
    ) +
    geom_line(overview_data, mapping = aes(x = time, y = value), linewidth = 0.5, color = "#1a1c1a") +
    scale_y_continuous(breaks = seq(40, 400, 60), limits = c(40,400), expand = c(0,0)) +
    scale_x_datetime(
      date_labels = "%H:%M",
      date_breaks = "2 hours",
      expand = c(0, 0)
    ) +
    labs(
      x = "Time",
      y = "Blood Glucose (mg/dL)",
      caption = capt
    ) +
    theme_lcars_light() +
    theme_daily_overview()

  daily_overview},
  execOnResize = TRUE
)



# box for the bar charts
lcarsBox(
  corners = c(1, 2, 3, 4),
  color = c("#AA99FF", "#AA99FF", "#AA99FF", "#AA99FF"),
  sides = c(1, 2, 3, 4),

  # info button
  subtitle = uiOutput("help_bars"),

  # bar charts displaying distribution of time in range vs out
  fluidRow(
    column(1, lcarsRect(color = "#000000")),
    column(5, plotOutput("range_time", height = 450)),
    column(5, plotOutput("range_percent", height = 450))
  ),

  right_inputs = inputColumn(
    lcarsRect(height = 200, color = "#7788FF"),
  ),

  # upper and lower limit selectors for the charts
  left_inputs = inputColumn(
    numericInput("high_lim", h4("'High' Limit"), value = 180, width = 150),
    lcarsRect(
      height = 14,
      width = 150,
      color = "#AA99FF",
      round = c("both")
    ),
    numericInput("low_lim", h4("'Low' Limit"), value = 70, width = 150),
    lcarsRect(height = 67, color = "#7788FF"),
  )
)

