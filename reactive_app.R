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
# a lotttt of packages


theme_averages_plot <- function() {

  theme(plot.title = element_text(size = 22),
        axis.title = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        plot.margin = margin(20, 20, 20, 20),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
  )
}

# A1C to eAG conversion chart
  conversion_chart <- data.frame(
    eAG = c(118, 126, 133, 140, 147, 154, 161, 169, 176, 183, 190),
    A1C = c(5.75, 6, 6.25, 6.5, 6.75, 7, 7.25, 7.5, 7.75, 8, 8.25)
  )
  conversion_chart_gt <- conversion_chart |>
    gt()


# function for removing irrelevant data
  reshape <- function(file) {
    file[-(1:19),] |>
      mutate(date = as.Date(...2, '%Y-%m-%d'), value = as.numeric(...8)) |>
      select(date, value) |>
      filter(!is.na(value))
  }

# functions for reincorporating extreme readings (preventing NAs)
# replacing "High" or "Low" values with 400 and 40 to get a slightly more
# accurate estimate
  highs <- function(x) {
    mutate(x, ...8 = replace(...8, ...8 == "High", "400" ))
  }

  lows <- function(x) {
    mutate(x, ...8 = replace(...8, ...8 == "Low", "40" ))
  }

# starter dates for the date range display
  first_date <- Sys.Date() - 90
  last_date <- Sys.Date()




ui <- lcarsPage(force_uppercase = TRUE,

# css
  tagList(
    tags$style(HTML("
      @import url(https://fonts.googleapis.com/css2?family=Antonio:wght@300&display=swap);

      h4, h5, p {
        color: #FFCC66;
        font-family: Antonio;
      }
      h6 {
        color: #FFCC66;
        font-family: Antonio;
        font-size: 15px;
      }
      h3 {
        color: #FF7700;
        font-family: Antonio;
      }
      div {
        font-family: Antonio;
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

    "))
  ),

# title
# "B.R.A.T. (Blood-sugar Readings: Average Trends)"
  lcarsHeader("[INSERT TITLE HERE]",
              color = "#FF9900",
              title_right = FALSE),

# header section — intro and customizable settings for the plot
  lcarsSweep(reverse = TRUE, color = "#99CCFF",
    title = "Info", expand = c(0, 500),

# as of yet useless but aesthetically pleasing center divider
    inputColumn(
      lcarsRect(color = "#9999FF", height = 24, round = c("both")),
      lcarsRect(color = "#cc99cc", height = 24, round = c("both")),
      lcarsButton("button", "", color = "golden-tanoi"),
      lcarsRect(color = "#EE5555", height = 24, round = c("both"))
    ),

# needed something to fill the space on the left side
    left_inputs = inputColumn(
      column(12,
        div(
          br(),
          h4("After data is uploaded, a graph will appear in the box below, as well
          as an overall estimated daily average and bar plot which shows the
          percentage of days with an average value within your selected ideal range.
          The breadth of this range is adjustable using the slider to the right."),
          h4("The chart on the right side of the box displays a rough estimate of A1C
          based on average blood sugar. The overall average displayed at the top
          of the box will not necessarily be compatible with this chart as A1C
          is determined by only the last 90 days of blood sugar behaviour. It's
          best used as a general indication of blood sugar control, not a predictor
          of a lab value."),
          br(),
          h5("Disclaimer: None of the graphics will render unless you upload data first. Also
           you have to use this page at full width or nothing aligns properly. Enjoy! "),
        )
      )
    ),

# custom inputs for the plot (date range and ideal average range)
    right_inputs = inputColumn(
      lcarsPill(
        title = "GRAPH SETTINGS",
        height = 30,
        color = "#CCCC55"
      ),
      div(br()),
      dateRangeInput(
        "interval", h4("Date Range"),
        start = first_date,
        end = last_date,
        min = first_date,
        max = last_date
      ),
      div(br()),
      sliderInput(
        "rect", h4("'Ideal' Daily Average Upper Limit"),
        min = 110, max = 180,
        value = c(165),
        step = 5
      )
    )
  ),


# bracket for uploading data
  lcarsBracket(
    color = "#FF7700",
    fluidRow(
      column(5,
        lcarsRect(
            color = "#000000",
            height = 52,
            text = h3("UPLOAD DATA TO DISPLAY GRAPH:"),
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


# toggleable instructions below bracket
  fluidRow(
    column(3,
        lcarsCheckbox(
          "instructions", p("Instructions for Downloading and Uploading Data"),
          width = 400,
          value = FALSE
        )
    ),
    column(8,
        htmlOutput("instructions")
    )
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

# creates the buttons on the left side which control the lines on the plot
    left_inputs = inputColumn(
      lcarsToggle(
        inputId = "mavg",
        label = h6("Show Moving Average:"),
        value = TRUE,
        false_color = "#EE4444"
      ),
      lcarsRect(
        height = 10,
        width = 150,
        color = "#CC6699",
        round = c("both")
      ),
      lcarsToggle(
        inputId = "davg",
        label = h6("Show Daily Average:"),
        value = TRUE,
        false_color = "#EE4444"
      ),
      lcarsRect(
        height = 170,
        color = "#CC6699"
      ),
    ),

# the actual plot itself
    fluidRow(
      column (12, plotOutput("averages_plot_app", height = 450))
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
        height = 50,
        color = "golden-tanoi"
      )
    )
  ),

# box for the violin plot
  lcarsBox(
    corners = c(1, 2, 3, 4),
    color = c("#CCCC55", "#CCCC55", "#CCCC55", "#CCCC55"),
    sides = c(4),
    side_color = c("#000000", "#000000", "#000000", "#000000"),

    left_inputs =
      dateRangeInput(
        "range", h4("Date Range"),
        start = Sys.Date() - 7,
        end = Sys.Date(),
        width = 150
    ),
    fluidRow(
      column(9,
        plotOutput("glycemic_var", height = 560, click = "violin_click")
      ),
      column(3,
        lcarsRect(height = 175, color = "#000000"),
        htmlOutput("violin_label")
      ),
    )
  ),

# box for the bar charts
  lcarsBox(
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
    left_inputs = inputColumn(
      lcarsRect(height = 225, color = "#7788FF"),
    ),

# upper and lower limit selectors for the charts
    right_inputs = inputColumn(
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


# shoutout to the creator of the lcars package
  div(br(), h6("The aesthetics of this app are based on the LCARS computer interface
         from Star Trek because I am an unrepentant nerd. Credit for the theme
         and custom widgets goes to Matthew Leonawicz!
         https://github.com/leonawicz/lcars/tree/master?tab=readme-ov-file"))
)





server <- function(input, output, session) {

# first step is tidying the uploaded data
  all_data <- eventReactive(input$upload, {

#   reads the uploaded files to create a list of data frames with the same names
#   as the original files + ".xlsx"
    files <- setNames(
      lapply(input$upload$datapath, read_excel),
      sapply(input$upload$name, basename))

#   interpolates values outside of the CGM's range (over 400 and under 40)
    files <- lapply(files, highs)
    files <- lapply(files, lows)

#   tidies up the data
    files <- lapply(files, reshape)

#   merges the uploaded files, dependent on the number of inputs
#   there's probably a better way to do this but that's not a high priority rn
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


# calculates the daily averages and adds a new column for the moving average
  averages <- eventReactive(input$upload, {
    averages <- all_data()

    averages <- averages |>
      group_by(date) |>
      summarize(daily_avg = mean(value)) |>
      mutate(ma_12 = rollmean(daily_avg, k = 12, fill = NA, align = "right"))
  })


# estimates overall average bg value
  overall_average <- eventReactive(input$upload, {
    overall_average <- round(mean(as.vector(averages()$daily_avg)))
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
  observe({
    input$upload

    updateDateRangeInput(
      session,
      "interval",
      start = first_date(),
      end = last_date(),
      min = first_date(),
      max = last_date()
    )
  })


# instructions for downloading/uploading raw data (beneath the bracket)
  output$instructions <- renderUI ({
    if (input$instructions == TRUE){
      div(
        br(),
        h4("HOW TO DOWNLOAD YOUR RAW CGM DATA IN THE CORRECT FORMAT:"),
        p("Step 1: Log into Dexcom Clarity (at https://clarity.dexcom.com/). On the
          far right side of the Overview page, just above the 'Sensor Usage' widget,
          is an icon that looks like a spreadsheet with an arrow pointing right."),
        p("Step 2: Click that icon, select a date range, then press export. The
          data will automatically download in Numbers. 90 days is the automatic
           date range limit so if you would like to analyze a longer timespan,
           you can download multiple 90 day datasets (up to 4) and upload them all
           simultaneously to this app."),
        p("Step 3: Open it in numbers then go to File > Export To > Excel."),
        p("Step 4: Name the file whatever you want, then come back here and hit
          import! Hold down command while clicking on files to select
          multiple files at once."),
        br()
      )
    }
  })


# overall average title at the top of the first box
  output$overall_average <- renderText ({
    req(input$upload)
    paste("OVERALL AVERAGE =", overall_average(), "mg/dL")
  })

# conversion chart in the right column of the first box
  output$a1c_eag <- renderTable ({
    conversion_chart_gt
  })


# the plot thickens!
# (time for the daily averages graph)
  output$averages_plot_app <- renderPlot({
     req(input$upload)

#   this first part creates a geom-less plot to function as a base for the conditionals
#   so that there's less copy + pasting
    averages_plot_app <- ggplot(averages(), aes(x = date)) +
      annotate(
        "rect",
        xmin = input$interval[1], xmax = input$interval[2],
        ymin = 100, ymax = input$rect,
        fill = "#99CCFF",
        alpha = 0.25
      ) +
      annotate(
        "rect",
        xmin = input$interval[1], xmax = input$interval[2],
        ymin = input$rect, ymax = 320,
        fill = "#ffcc66",
        alpha = 0.25
      ) +
      labs(
        title = "Average Daily Blood Glucose",
        x = "Date",
        y = "Value (mg/dL)"
      ) +
      scale_x_date(limits = c(input$interval[1], input$interval[2]), date_breaks = "months", date_labels = "%b") +
      scale_y_continuous(limits = c(100, 320), breaks = seq(100,320,20)) +
      scale_color_manual(
        "Method",
        values = c("#cc6699", "#cc99cc"),
        labels = c("Moving Average", "Daily Average")
      ) + theme_lcars_light() +
      theme_averages_plot()


# now this bit tests which buttons have been toggled to determine which extra
# conditions to add to the original empty plot

#   both lines toggled on
    if (input$mavg == TRUE & input$davg == TRUE){
      averages_plot_app <- averages_plot_app +
        geom_line(aes(y = daily_avg, color = "#cc99cc")) +
        geom_line(aes(y = ma_12, color = "#cc6699"), linewidth = 0.92)
    }
#   moving average on, daily averages off
    if(input$mavg == TRUE & input$davg == FALSE){
      averages_plot_app <- averages_plot_app +
        geom_line(aes(y = daily_avg, color = "#cc99cc"), alpha = 0) +
        geom_line(aes(y = ma_12, color = "#cc6699"), linewidth = 0.92)
    }
#   daily averages on, moving average off
    if(input$davg == TRUE & input$mavg == FALSE){
      averages_plot_app <- averages_plot_app +
        geom_line(aes(y = daily_avg, color = "#cc99cc")) +
        geom_line(aes(y = ma_12, color = "#cc6699"), alpha = 0)
    }
#   both lines off (empty plot)
    else {
      averages_plot_app <- averages_plot_app +
        geom_line(aes(y = daily_avg, color = "#cc99cc"), alpha = 0) +
        geom_line(aes(y = ma_12, color = "#cc6699"), alpha = 0)
    }
    averages_plot_app
  })


# glycemic variation violin plot time

# creates reactive values that will be used to determine the range of the x-axis
  maxi <- reactiveVal()
  mini <- reactiveVal()

#   assigns initial values to select the most recent week of available data
#   executes upon upload of files
    observeEvent(input$upload, {
      maxi(last_date())
      mini(last_date() - 7)
    })

#   assigns minimum and maximum dates based on user selection
#   executes upon date range input
    observeEvent(input$range, {
      maxi(input$range[2])
      mini(input$range[1])
    })

# changes the date range starter values to display those initial reactive values
# until new ones are input
  observe({
    input$upload

    updateDateRangeInput(
      session,
      "range",
      start = mini(),
      end = maxi(),
    )
  })

# creates a list of multiple conditions so that the data handling will re-execute
# when either new files are uploaded OR the date range is manually adjusted
  range_or_upload <- reactive({
    list(input$range, input$upload)
  })

# aforementioned data handling
# selects observations between the range inputs
  violin_data <- eventReactive(range_or_upload(), {
    violin_data <- all_data()

    violin_data <- violin_data |>
      filter(date >= mini() & date <= maxi()) |>
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
        plot.title = element_text(size = 23, margin = margin(b = 25)),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(margin = margin(t = 15)),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.text = element_text(size = 12),
        plot.margin = margin(0, 10, 0, 15),
        legend.position = "none"
      )
    glycemic_var
  })

# next part creates a reactive readout beside the plot that displays the mean
# and standard deviation of whichever day the user clicks on

  violin_sd <- reactiveVal(NULL)
  violin_mean <- reactiveVal(NULL)
  violin_date <- reactiveVal(NULL)

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

    violin_date(selection$date[1])

    violin_info_df <- violin_data |>
      filter(date == as.factor(selection$date[1]))

    mean <- averages |>
      filter(date == as.Date(selection$date[1], '%Y-%m-%d'))

    if(nrow(selection) == 0) {
      violin_sd(NULL)
      violin_mean(NULL)
    }
    else {
      violin_sd(paste("Standard Deviation =", round(sd(violin_info_df$value), digits = 1), "mg/dL"))
      violin_mean(paste("Mean =", round(mean$daily_avg[1]), "mg/dL"))
    }
  })

  label <- reactiveVal(NULL)

  observe({
    if(!is.null(input$violin_click)) {
      label(
        div(id = "violinLabel",
            h4(strong(violin_date())),
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
        caption = "Overall Time in Range ",
        x = NULL,
        y = NULL
      ) +
      theme_lcars_dark() +
      theme(
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0.5, size = 16, margin = margin(t = 15)),
        legend.text = element_text(size = 12),
        axis.text.x = element_blank(),
        plot.margin = margin(32, 28, 28, 28)
      )
    range_time
  })

# stacked bar chart showing percentage of days in range
# adjusts with the slider
  output$range_percent <- renderPlot({
    bar_averages <- averages()

    bar_averages <- bar_averages |>
      mutate(class = "time", in_range = daily_avg) |>
      mutate(in_range = replace(in_range, in_range > input$rect, ">" )) |>
      mutate(in_range = replace(in_range, in_range <= input$rect & in_range != ">", "<"))

    range_percent <- ggplot(bar_averages, aes(x = class, y = daily_avg, fill = in_range), alpha = 0.98) +
      geom_bar(position = position_fill(reverse = TRUE), stat = "identity") +
      scale_fill_manual(
        values = c("#ffcc66", "#99CCFF" ),
        breaks = c(">", "<"),
        labels = c(
          paste("Daily Avg. >", input$rect, "mg/dL"),
          paste("Daily Avg. <", input$rect, "mg/dL"))
      ) +
      labs(
        caption = "Days in Range",
        x = NULL,
        y = NULL
      ) +
      theme_lcars_dark() +
      theme(
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0.5, size = 16, margin = margin(t = 15)),
        legend.text = element_text(size = 12),
        axis.text.x = element_blank(),
        plot.margin = margin(28, 28, 28, 28),
      )
  range_percent
  })
}

"#3366CD"

shinyApp(ui, server)
