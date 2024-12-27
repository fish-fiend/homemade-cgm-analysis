library(shiny)
library(shinyWidgets)
library(tidyverse)
library(ggplot2)
library(gt)
library(lcars)
library(grid)
library(png)
library(readxl)
library(zoo)
library(stats)
library(hpfilter)
library(showtext)
# a lotttt of packages

font_add_google("Antonio", "Antonio")
showtext_auto()

# custom themes
theme_averages_plot <- function() {
  theme(plot.title = element_text(size = 30),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 16),
        axis.text = element_text(size = 16),
        plot.margin = margin(20, 20, 20, 20),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        text = element_text(family = "Antonio")
  )
}


# A1C to eAG conversion chart
  conversion_chart <- data.frame(
    eAG = c(118, 126, 133, 140, 147, 154, 161, 169, 176, 183, 190),
    A1C = c(5.75, 6, 6.25, 6.5, 6.75, 7, 7.25, 7.5, 7.75, 8, 8.25)
  )
  conversion_chart_gt <- conversion_chart |>
    gt()


# functions for reincorporating extreme readings (preventing NAs) by
# replacing "High" or "Low" values with 400 and 40
  highs <- function(x) {
    mutate(x, ...8 = replace(...8, ...8 == "High", "400" ))
  }

  lows <- function(x) {
    mutate(x, ...8 = replace(...8, ...8 == "Low", "40" ))
  }



# function for removing irrelevant data
  reshape <- function(file) {
    file[-(1:19),] |>
      mutate(date = as.Date(...2, '%Y-%m-%d'), value = as.numeric(...8)) |>
      select(date, value) |>
      filter(!is.na(value))
  }


# starter dates for the date range display
  first_date <- Sys.Date() - 90
  last_date <- Sys.Date()




ui <- lcarsPage(force_uppercase = TRUE,

# ideally css would be imported but currently when i try to do so it doesn't
# overwrite the existing css properly.
# something to deal with eventually
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

      #violinLabel {
        text-align: left;
        border-width: 2px;
        border-style: solid;
        border-color: #FFCC66;
        border-radius: 25px;
        width: 210px;
        padding: 10px;
      }

      #instruct {
        margin: 14px;
        border-width: 3px;
        border-style: solid;
        border-color: #FFCC66;
        border-radius: 25px;
        padding: 18px;
      }

      .lcars-btn, .lcars-btn-filtered {
          height: 29px;
          width: 29px;
          padding-left: 6px;
          padding-top: 0px;
          text-align: center;
          border: 2px solid #FFFFCD;
          border-radius: 25px;
          font-size: 14px;
      }

      .modal-footer {
        border-top: 0px;
      }

      .irs {
        font-family: Antonio;
        color: #FFffcd;
      }

      .irs--shiny .irs-min, .irs--shiny .irs-max {
        font-size: 12px;
      }
      .irs--shiny .irs-from, .irs--shiny .irs-to {
        color: #ffffcd
      }
      .irs--shiny .irs-handle {
        top: 23px;
        width: 20px;
        height: 14px;
        border: 1px solid #ffffff;
        background-color: #99ccff;
        box-shadow: 1px 1px 3px rgba(255, 255, 255, 0.3);
        border-radius: 8px;
        z-index: 2;
        }

      .irs--shiny .irs-bar {
        top: 23px;
        height: 14px;
        border-top: 1px solid #ffffff;
        border-bottom: 1px solid #ffffff;
        border-left: 1px solid #ffffff;
        background: #428bca;
        cursor: s-resize;
        z-index: 2;
      }


      .irs--shiny .irs-line {
        top: 23px;
        height: 14px;
        background: #000000;
        background-color: #000000;
        border: 1px solid #cccccc;
        border-radius: 8px;
        overflow: visible;
      }

      .irs--shiny .irs-handle.state_hover, .irs--shiny .irs-handle:hover {
        background: #428bca;
      }
    "))
   ),



# title
  lcarsHeader("[INSERT TITLE]",
              color = "#FF9900",
              title_right = FALSE),

# header section — intro and customizable settings for the plot
  lcarsSweep(reverse = TRUE, color = "#99CCFF",
    title = "DISCLAIMERS", left_width = 0.36,


    left_inputs = inputColumn(
      div(
        br(),
        p("This page should be used at full width so that the elements align
          properly."),
        p("None of the graphics will render until data is uploaded."),
        p("Press the 'i' icons at the bottom right of each section for
          more information about interacting with the plots."),
        style = "font-size: 16px; text-align: left;"
      )
    ),

    column_inputs = inputColumn(
      lcarsRect(color = "#99CCFF", width = 150, round = c("right")),
    ),

# custom inputs for the plot (date range and ideal average range)
    right_inputs = inputColumn(
          lcarsPill(
            title = "DAILY AVERAGE GRAPH SETTINGS",
            height = 30,
            color = "#CCCC55"
          ),
          div(br()),
          fluidRow(
            column(1, lcarsRect(color = "#000000")),
            column(5,
              dateRangeInput(
                  "interval", h4("Date Range"),
                  start = first_date,
                  end = last_date,
                  min = first_date,
                  max = last_date,
                  width = 250
              )
            ),
            column(5,
              sliderInput(
                  "rect", h4("Target Average BG Range"),
                  min = 90, max = 180,
                  value = c(100,165),
                  step = 5,
                  ticks = FALSE,
                  width = 250
              )
            )
          )
    )
  ),

# bracket for uploading data
lcarsBracket(
  color = "#FF7700",
  fluidRow(
    column(6,
           lcarsRect(
             color = "#000000",
             height = 52,
             text = h3("UPLOAD DATA TO DISPLAY GRAPHS:"),
             text_size = 17,
             text_color = "#FFFFDD"
           )
    ),
    column(6,
           fileInput(
             "upload", "",
             buttonLabel = "Import",
             placeholder = "see directions below",
             multiple = TRUE,
             accept = ".xlsx",
             width = 250
           )
    )
  )
),

# toggleable directions for downloading/uploading data
  fluidRow(
    column(3,
        lcarsCheckbox(
          "instructions", p("Instructions for Downloading and Uploading Data"),
          width = 400,
          value = FALSE
          )
    ),
    column(12, htmlOutput("instructions"))
  ),




# box for displaying the plot, toggles, overall average, and conversion chart
  lcarsBox(
    corners = c(1, 2, 3, 4),
    sides = c(1, 3, 4),
    color = c("#CC6699",  "#FF9900",  "#FFCC66",  "#cc99cc"),
    side_color = c( "#FF9900",  "#000000", "#FFCC66", "#CC6699"),

# displays overall average at the top of the box
    title = textOutput("overall_average"),
    title_color = "atomic-tangerine",

    subtitle = uiOutput("help_averages"),

# creates the buttons on the left side which control the lines on the plot
    left_inputs = inputColumn(
      lcarsToggle(
        inputId = "davg",
        label = h6("Daily Averages:"),
        value = TRUE,
        true_color = "#5577ee",
        false_color = "#EE4444",
        background_color = "#FFDD99"
      ),
      lcarsToggle(
        inputId = "smavg",
        label = h6("Smooth Average:"),
        value = TRUE,
        true_color = "#5577ee",
        false_color = "#EE4444",
        background_color = "#FFDD99"
      ),
      lcarsRect(
        height = 0,
        width = 150,
        color = "#000"
      ),
      lcarsRect(
        height = 63,
        width = 150,
        color = "#CC6699"
      ),
      lcarsRadioToggle("date_lines", h6("X-Axis Lines/Labels:"),
        c("Months" = "m", "Weeks" = "w", "Days" = "d"),
        label_color = "#CCCC55",
        width = 150
      )
    ),


# the actual plot itself
    fluidRow(
      column(12,
          plotOutput("averages_plot", height = "500px",
            dblclick = "click_averages",
            brush = brushOpts(
              "brush_averages",
              resetOnNew = TRUE
            )
          )
      )
    ),


# displays the conversion chart (a1c to eag) on the right side
    right_inputs = inputColumn(
      lcarsRect(
        "eAG = estimated average glucose",
        text_size = 10,
        round = "left",
        height = 21
      ),
      lcarsRect(
        tableOutput("a1c_eag"),
        round = c("both"),
        color = "#000000",
        text_color = "#FFCC66",
        text_size = 14,
        height = 380,
        width = 140
      ),
      lcarsRect(
        height = 100,
        color = "golden-tanoi"
      )
    )
  ),

# box for the violin plot
  lcarsBox(
    subtitle = uiOutput("help_violins"),

    corners = c(1, 2, 3, 4),
    color = c("#CCCC55", "#CCCC55", "#CCCC55", "#CCCC55"),
    sides = c(4),
    side_color = c("#000000", "#000000", "#000000", "#000000"),

    fluidRow(
      column(9,
        plotOutput("glycemic_var", height = 550, click = "violin_click")
      ),
      inputColumn(
        column(3,
          lcarsRect(height = 50, color = "#000000"),
          dateRangeInput(
            "range", h4("Date Range"),
             start = Sys.Date() - 7,
             end = Sys.Date(),
             width = 210
          ),
          lcarsRect(height = 15, color = "#000000"),
          htmlOutput("violin_label")
        )
      ),
    )
  ),

# box for the bar charts
  lcarsBox(
    subtitle = uiOutput("help_bars"),

    corners = c(1, 2, 3, 4),
    color = c("#AA99FF", "#AA99FF", "#AA99FF", "#AA99FF"),
    sides = c(1, 2, 3, 4),

# bar charts displaying distribution of time in range vs out
    fluidRow(
      column(1, lcarsRect(color = "#000000")),
      column(5, plotOutput("range_time", height = 450)),
      column(5, plotOutput("range_percent", height = 450))
    ),

# all style no substance
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
      numericInput("low_lim", h4("'Low' Limit"), value = 80, width = 150),
      lcarsRect(height = 67, color = "#7788FF"),
    )
  ),


# The aesthetics of this app are based on the LCARS computer interface
# from Star Trek because I am an unrepentant nerd. Credit for the theme
# and custom widgets goes to Matthew Leonawicz
# https://github.com/leonawicz/lcars/tree/master?tab=readme-ov-file

)






server <- function(input, output, session) {



# creates info buttons
  output$help_averages <- renderUI ({
    lcarsButton(
      "help_averages",
      label = "",
      icon = icon(name = "info", style = "color: #FFFFCD;"),
      color = "#222222",
      hover_color = "eggplant"
    )
  })

  output$help_violins <- renderUI ({
    lcarsButton(
      "help_violins",
      label = "",
      icon = icon(name = "info", style = "color: #FFFFCD;"),
      color = "#222222",
      hover_color = "eggplant"
    )
  })

  output$help_bars <- renderUI ({
    lcarsButton(
      "help_bars",
      label = "",
      icon = icon(name = "info", style = "color: #FFFFCD;"),
      color = "#222222",
      hover_color = "eggplant"
    )
  })

# content for info modals
  observeEvent(input$help_averages, {
      showModal(
        modalDialog(
          title = h4("INFO — DAILY AVERAGES",  style = "color: #000000;"),
          div(
            p("- each point represents the average of all CGM readings from a given day", style = "color: #000000;"),
            p("- drag the mouse to highlight an area on the graph, then double
              click to zoom in——double click again to reset", style = "color: #000000;"),
            p("- use the 'X-Axis Scale' buttons to adjust labels on the x-axis", style = "color: #000000;"),
            p("- the date range above this plot changes the range of the graph
              and the slider adjusts the shaded areas", style = "color: #000000;"),
            hr(),
            p("Disclaimer: the overall average is a comprehensive estimate based
              on all available CGM data but it is not necessarily an accurate
              predictor of A1C %. The recommended use of the conversion chart is
              as a reference for determining individual goals.", style = "color: #000000; font-size: 12px;")
          ),
          footer = modalButton("OKAY"),
          size = c("m")
        )
      )
  })

  observeEvent(input$help_violins, {
    showModal(
      modalDialog(
        title = h4("INFO — DAILY GLYCEMIC VARIATION",  style = "color: #000000;"),
        div(
          p("- each object is a sort of density plot——the width represents the
            number of observations at a certain blood sugar level", style = "color: #000000;"),
          p("- click on a shape to reveal the average glucose value and standard
            deviation of data from that day", style = "color: #000000;"),
          hr(),
          p("Disclaimer: recommended date range is around 10 days or less.", style = "color: #000000; font-size: 12px;")
        ),
        footer = modalButton("DISMISS"),
        size = c("m")
      )
    )
  })

  observeEvent(input$help_bars, {
    showModal(
      modalDialog(
        title = h4("INFO — TIME IN RANGE CHARTS", style = "color: #000000;"),
        div(
          p("- the plot on the left shows the distribution of time spent very high,
            high, in range, low, and very low", style = "color: #000000;"),
          p("- the high and low limits can be adjusted with the numeric inputs
            to the left", style = "color: #000000;"),
          p("- the plot on the right shows the percent of days where the daily
            average is at or lower than the limit specified with the slider at
            the top right of the page", style = "color: #000000;")
        ),
        footer = modalButton("OKAY"),
        size = c("m")
      )
    )
  })


# instructions for downloading/uploading raw data (beneath the bracket)
  output$instructions <- renderUI ({
    if (input$instructions == TRUE){
      div(id = "instruct",
        h4("HOW TO DOWNLOAD RAW DEXCOM CGM DATA:"),
        p("Step 1: Log in to Dexcom Clarity. On the far right side of the 'Overview'
          page, just above the 'Sensor Usage' widget, is an icon that looks like
          a spreadsheet with an arrow pointing right."),
        p("Step 2: Click that icon, select a date range, then press export. Dexcom
          only allows downloads of up to 90 days worth of data at a time, so if you
          would like to analyze a longer timespan, repeat this step to download
          multiple 90 day datasets."),
        p("Step 3:  The file will automatically open with Numbers. Go to File >
          Export To > Excel to convert it to the correct format."),
        p("Step 4: Name the file whatever you want, then come back here and hit
          import! Hold down command to select multiple files at once (4 is the maximum)."),
        style = "font-size: 16px;"
      )
    }
  })

# conversion chart in the right column of the first box
  output$a1c_eag <- renderTable ({
    conversion_chart_gt
  })


# tidying the uploaded data
  all_data <- eventReactive(input$upload, {

# reads the uploaded files to create a list of data frames with the same names
# as the original files + ".xlsx"
    files <- setNames(
      lapply(input$upload$datapath, read_excel),
      sapply(input$upload$name, basename))

# interpolates values outside of the CGM's range (over 400 and under 40)
    files <- lapply(files, highs)
    files <- lapply(files, lows)

# tidies up the data
    files <- lapply(files, reshape)

# merges the uploaded files, dependent on the number of inputs
# there's probably a better way to do this but that's not a high priority rn
    if (length(files) == 1) {
      all_data <- files[[input$upload$name[[1]] ]]
    }
    if (length(files) == 2) {
      all_data <- files[[input$upload$name[[1]] ]] |>
        full_join(files[[input$upload$name[[2]] ]], join_by(date, value))
    }
    if (length(files) == 3) {
      all_data <- files[[input$upload$name[[1]] ]] |>
        full_join(files[[input$upload$name[[2]] ]], join_by(date, value)) |>
        full_join(files[[input$upload$name[[3]] ]], join_by(date, value))
    }
    if (length(files) == 4) {
      all_data <- files[[input$upload$name[[1]] ]] |>
        full_join(files[[input$upload$name[[2]] ]], join_by(date, value)) |>
        full_join(files[[input$upload$name[[3]] ]], join_by(date, value)) |>
        full_join(files[[input$upload$name[[4]] ]], join_by(date, value))
    }
    all_data <- all_data
  })


# creates a new data frame that includes daily averages and smoothed data
  averages <- eventReactive(input$upload, {
    averages <- all_data()
    averages <- averages |>
      group_by(date) |>
      summarize(daily_avg = mean(value))

    smoother <- hp2(averages, lambda = 1000)

    averages <- mutate(averages, smoother = smoother$daily_avg)
  })


# estimates overall average bg value
  overall_average <- eventReactive(input$upload, {
    overall_average <- round(mean(as.vector(averages()$daily_avg)))
  })

# overall average title at the top of the first box
  output$overall_average <- renderText ({
    req(input$upload)
    paste("OVERALL AVERAGE =", overall_average(), "mg/dL")
  })


# finds earliest date of observations
  first_date <- eventReactive(input$upload, {
    averages()$date[1]
  })

# finds most recent date of observations
  last_date <- eventReactive(input$upload, {
    averages_inverse <- arrange(averages(), desc(date))
    last_date <- averages_inverse$date[1]
  })

# updates the initial start/end dates of the date range input in the top right based
# on the full range of uploaded data
  observeEvent(input$upload, {
    updateDateRangeInput(
      session,
      "interval",
      start = first_date(),
      end = last_date(),
      min = first_date(),
      max = last_date()
    )
  })


# reactive values that control the averages graph's x-axis/range
  xmin_averages <- reactiveVal()
  xmax_averages <- reactiveVal()

# when date range is manually changed, the range of the averages graph is
# determined by those inputs
  observeEvent(input$interval, {
    xmin_averages(input$interval[1])
    xmax_averages(input$interval[2])
  })

# when the graph is brushed and double clicked, range of the graph is changed
# to fit the minimum and maximum x values of the brushed area
  observeEvent(input$click_averages, {
    brush <- input$brush_averages
      if(!is.null(brush)) {
        xmin_averages(as.Date(brush$xmin, format = "%Y-%m-%d"))
        xmax_averages(as.Date(brush$xmax, format = "%Y-%m-%d"))
      }
      else {
        xmin_averages(input$interval[1])
        xmax_averages(input$interval[2])
      }
  })


# reactive variables to change the scale and labels of the x-axis of the averages graph
  averages_breaks <- reactiveVal()
  averages_labels <- reactiveVal()

  observeEvent(input$date_lines, {
    if(input$date_lines == "m") {
      averages_breaks("months")
      averages_labels("%b")
    }
    if(input$date_lines == "w") {
      averages_breaks("weeks")
      averages_labels("%b-%d")
    }
    if(input$date_lines == "d") {
      averages_breaks("days")
      averages_labels("%m-%d")
    }
  })


# the plot thickens!
# (time for the daily averages graph)
  output$averages_plot <- renderPlot({
     req(input$upload)

#   this first part creates a geom-less plot to function as a base for the conditionals
#   so that there's less copy + pasting
    averages_plot <- ggplot(averages(), aes(x = date)) +
      annotate(
        "rect",
        xmin = xmin_averages(), xmax = xmax_averages(),
        ymin = 90, ymax = input$rect[1],
        fill = "#ffcc66",
        alpha = 0.25
      ) +
      annotate(
        "rect",
        xmin = xmin_averages(), xmax = xmax_averages(),
        ymin = input$rect[1], ymax = input$rect[2],
        fill = "#99DDFF",
        alpha = 0.25
      ) +
      annotate(
        "rect",
        xmin = xmin_averages(), xmax = xmax_averages(),
        ymin = input$rect[2], ymax = 320,
        fill = "#ffcc66",
        alpha = 0.25
      ) +
      labs(
        title = "Average Daily Blood Glucose",
        x = "Date",
        y = "Value (mg/dL)"
      ) +
      scale_x_date(limits = c(xmin_averages(), xmax_averages()),
                              date_breaks = averages_breaks(), date_labels = averages_labels()) +
      scale_y_continuous(limits = c(90, 320), breaks = seq(90, 320, 20)) +
      scale_color_manual(
        "Method",
        values = c("#cc6699", "#cc99cc"),
        labels = c("Smooth Average", "Daily Averages")
      ) + theme_lcars_light() +
      theme_averages_plot()


# now this bit tests which buttons have been toggled to determine which extra
# conditions to add to the original empty plot

# daily averages on, spline on (DEFAULT)
    if (input$davg == TRUE & input$smavg == TRUE){
      averages_plot <- averages_plot +
        geom_line(aes(y = daily_avg, color = "#cc99cc")) +
        geom_line(aes(y = smoother, color = "#cc6699"), linewidth = 0.92)
    }
# daily averages on, spline off
    if(input$davg == TRUE & input$smavg == FALSE){
      averages_plot <- averages_plot +
        geom_line(aes(y = daily_avg, color = "#cc99cc")) +
        geom_line(aes(y = smoother, color = "#cc6699"), alpha = 0)
    }
# daily averages off, spline on
    if(input$davg == FALSE & input$smavg == TRUE){
      averages_plot <- averages_plot +
        geom_line(aes(y = daily_avg, color = "#cc99cc"), alpha = 0) +
        geom_line(aes(y = smoother, color = "#cc6699"), linewidth = 0.92)
    }
# all lines off (empty plot)
    else {
      averages_plot <- averages_plot +
        geom_line(aes(y = daily_avg, color = "#cc99cc"), alpha = 0) +
        geom_line(aes(y = smoother, color = "#cc6699"), alpha = 0)
    }
    averages_plot},
    execOnResize = TRUE
  )


# glycemic variation violin plot time

# creates reactive values that will be used to determine the range of the x-axis
  maxi <- reactiveVal()
  mini <- reactiveVal()

# assigns initial values to select the most recent week of available data
# executes upon upload of files
    observeEvent(input$upload, {
      maxi(last_date() - 1)
      mini(last_date() - 8)
    })

# assigns minimum and maximum dates based on user selection
# prompts a warning modal if date range exceeds 30 days
# executes upon date range input
    observeEvent(input$range, {
      if ((input$range[2]-input$range[1]) > 30) {
        showModal(
          modalDialog(
            "PLEASE SELECT A SMALLER DATE RANGE INTERVAL!",
            footer = modalButton("OKAY"),
            size = c("s")
          )
        )
      }
      else {
        maxi(input$range[2])
        mini(input$range[1])
      }
    })

# changes the date range starter values to display those initial reactive values
# until new ones are input
  observeEvent(input$upload, {
    updateDateRangeInput(
      session,
      "range",
      start = mini(),
      end = maxi(),
    )
  })


# aforementioned data handling
# selects observations between the range inputs from the overall data
# executes when either new files are uploaded OR the date range is manually adjusted
  violin_data <- eventReactive({
                    input$range
                    input$upload
                 },
  {violin_data <- all_data()

   violin_data <- violin_data |>
     filter(between(date, input$range[1], input$range[2])) |>
     group_by(date) |>
     mutate(value = as.numeric(value), date = as.factor(date))
  })


# violin plot
  output$glycemic_var <- renderPlot({
    req(input$upload)

    glycemic_var <- ggplot(violin_data(), aes(x = date, y = value, fill = date)) +
      geom_violin() +
      labs(
        title = "Daily Glycemic Variation",
        y = "Glucose Value (mg/dL)",
        x = "Date"
      ) +
      scale_y_continuous(limits = c(40, 400), breaks = seq(40, 400, 30)) +
      scale_fill_manual(
        values = c("#EE5555", "#FF9900",  "#FFCC66", "#CCCC55", "#99CCFF", "#AAAAFF", "#cc99cc", "#CC6699",
                 "#EE5555", "#FF9900",  "#FFCC66", "#CCCC55", "#99CCFF", "#AAAAFF", "#cc99cc", "#CC6699",
                 "#EE5555", "#FF9900",  "#FFCC66", "#CCCC55", "#99CCFF", "#AAAAFF", "#cc99cc", "#CC6699",
                 "#EE5555", "#FF9900",  "#FFCC66", "#CCCC55", "#99CCFF", "#AAAAFF", "#cc99cc", "#CC6699")
      ) +
      theme_lcars_dark() +
      theme(
        plot.title = element_text(size = 30, margin = margin(b = 23)),
        axis.title = element_text(size = 20),
        axis.title.x = element_text(margin = margin(t = 15)),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.text = element_text(size = 17),
        plot.margin = margin(0, 10, 0, 15),
        legend.position = "none",
        text = element_text(family = "Antonio")
      )
    glycemic_var},
    execOnResize = TRUE
  )

# next part creates a reactive readout beside the plot that displays the mean
# and standard deviation of whichever day the user clicks on

# first create the necessary reactive variables
  violin_date <- reactiveVal(NULL)
  violin_sd <- reactiveVal(NULL)
  violin_mean <- reactiveVal(NULL)


# assign value depending on selected day
  observeEvent(input$violin_click, {

    violin_data <- violin_data()
    averages <- averages()
    selection <- nearPoints(
                    violin_data,
                    input$violin_click,
                    xvar = "date",
                    maxpoints = 5,
                    threshold = 45
                    )

    violin_info_df <- violin_data |>
      filter(date == as.factor(selection$date[1]))

    mean <- averages |>
      filter(date == as.Date(selection$date[1], '%Y-%m-%d'))

    if(nrow(selection) == 0) {
      violin_date(NULL)
      violin_sd(NULL)
      violin_mean(NULL)
    }
    else {
      violin_date(selection$date[1])
      violin_sd(paste("Standard Deviation =", round(sd(violin_info_df$value), digits = 1), "mg/dL"))
      violin_mean(paste("Mean =", round(mean$daily_avg[1]), "mg/dL"))
    }
  })


# reactive variable for the display of the daily mean and sd
  label <- reactiveVal(NULL)

# creates html object to display mean and sd
  observe({
    if(!is.null(input$violin_click)) {
      label(
        div(id = "violinLabel",
            h4(strong(violin_date())),
            br(),
            p(violin_mean()),
            p(violin_sd()),
        )
      )
    }
    else {
      label(div(id = "violinLabel",
              p("click the graph to display daily mean and standard deviation")
              )
      )
    }
  })

  output$violin_label <- renderUI(label())



# stacked bar chart showing the ratio of overall time spent high, in range, and low
# adjusts with numeric inputs on the side
  output$range_time <- renderPlot({
    bar_time <- all_data()

    bar_time <- bar_time |>
      mutate(class = "time", range = value) |>
      mutate(range = replace(range, range > 250, 4)) |>
      mutate(range = replace(range, range > input$high_lim & range != 4, 3 )) |>
      mutate(range = replace(range, range <= input$high_lim & range >= input$low_lim, 2 )) |>
      mutate(range = replace(range, range < input$low_lim & range >= 50, 1)) |>
      mutate(range = replace(range, range < 50 & range >= 40, 0)) |>
      mutate(range = as.character(range))


    range_time <- ggplot(bar_time, aes(x = class, y = value, fill = range)) +
      geom_bar(position = position_fill(reverse = TRUE), stat = "identity") +
      scale_fill_manual(
        values = c("#BC4411","#ffcc66", "#99CCFF", "#CD99CE", "#cc6699"),
        breaks = c("4", "3", "2", "1", "0"),
        labels = c(
          "Very High (> 250 mg/dL)",
          paste("High (>", input$high_lim, "mg/dL)"),
          "In Range",
          paste("Low (<", input$low_lim, "mg/dL)"),
          "Very Low (< 50 mg/dL)"
          )
      ) +
      labs(
        caption = "Time in Range ",
        x = NULL,
        y = NULL
      ) +
      theme_lcars_dark() +
      theme(
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0.5, size = 28, margin = margin(t = 15)),
        legend.text = element_text(size = 17),
        axis.text.x = element_blank(),
        plot.margin = margin(32, 50, 28, 28),
        text = element_text(family = "Antonio")
      )
    range_time},
    execOnResize = TRUE
  )


# stacked bar chart showing percentage of days in range
# adjusts with the slider
  output$range_percent <- renderPlot({
    bar_averages <- averages()

    bar_averages <- bar_averages |>
      mutate(class = "time", in_range = daily_avg) |>
      mutate(in_range = replace(in_range, in_range > input$rect[2], ">" )) |>
      mutate(in_range = replace(in_range, in_range <= input$rect[2] & in_range != ">", "<"))

    range_percent <- ggplot(bar_averages, aes(x = class, y = daily_avg, fill = in_range), alpha = 0.98) +
      geom_bar(position = position_fill(reverse = TRUE), stat = "identity") +
      scale_fill_manual(
        values = c("#ffcc66", "#99CCFF" ),
        breaks = c(">", "<"),
        labels = c(
          paste("Daily Avg. >", input$rect[2], "mg/dL"),
          paste("Daily Avg. <", input$rect[2], "mg/dL"))
      ) +
      labs(
        caption = "Days in Range",
        x = NULL,
        y = NULL
      ) +
      theme_lcars_dark() +
      theme(
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0.5, size = 28, margin = margin(t = 15)),
        legend.text = element_text(size = 17),
        axis.text.x = element_blank(),
        plot.margin = margin(28, 35, 28, 28),
        text = element_text(family = "Antonio")
      )
    range_percent},
    execOnResize = TRUE
  )
}

# "#3366CD"

shinyApp(ui, server)
