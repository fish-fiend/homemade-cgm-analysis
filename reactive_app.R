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


# A1C to eAG conversion chart
  conversion_chart <- data.frame(
    eAG = c(126, 133, 140, 147, 154, 161, 169, 176, 183, 190, 197),
    A1C = c(6, 6.25, 6.5, 6.75, 7, 7.25, 7.5, 7.75, 8, 8.25, 8.5)
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

  
# title
# "B.R.A.T. (Blood-sugar Readings: Average Trends)"
  lcarsHeader("[INSERT TITLE HERE]", 
              color = "#FF9900", 
              title_right = FALSE),
  

# header section — intro and customizable settings for the plot
  lcarsSweep(reverse = TRUE, color = "#99CCFF",

# as of yet useless but aesthetically pleasing center divider
    inputColumn(
      lcarsRect(color = "#cc99cc", height = 21, round = c("both")),
      lcarsRect(color = "#EE4444", height = 21, round = c("both")),
      lcarsButton("button", "", color = "golden-tanoi", height = 25),
      lcarsRect(color = "#3366cc", height = 21, round = c("both"))
    ),

# needed something to fill the space on the left side
    left_inputs = inputColumn(
      column(12,
        htmlOutput("intro")
      )
    ),

# custom inputs for the plot (date range and ideal average range)
    right_inputs = inputColumn(
      lcarsPill(
        title = "GRAPH SETTINGS",
        height = 30
      ),
      div(br()),
      dateRangeInput(
        "interval", "Date Range",
        start = first_date,
        end = last_date,
        min = first_date, 
        max = last_date
      ),
      div(br()),
      sliderInput(
        "rect", "'Ideal' Daily Average Upper Limit",
        min = 110, max = 180,
        value = c(170),
        step = 5
      )
    ),
    chooseSliderSkin("Modern", color = "#cc99cc")
  ),
  

# bracket for uploading data
  lcarsBracket(
    fluidRow(
      column(5,
        lcarsRect(
            color = "#000000",
            height = 52,
            text = "UPLOAD DATA TO DISPLAY GRAPH:",
            text_size = 17,
            text_color = "#FFFFED"
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
    column(8,
        lcarsCheckbox(
          "instructions", "Instructions for Downloading and Uploading Data",
          width = 400,
          value = FALSE
        )
    ),
    column(12,
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
        label = "Display Moving Average",
        value = TRUE,
        false_color = "#EE4444"
      ),
      lcarsToggle(
        inputId = "davg",
        label = "Display Daily Average",
        value = TRUE,
        false_color = "#EE4444"
      )
    ),
    
# the actual plot itself
    fluidRow(
      column (12, plotOutput("averages_plot_app"))
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
        text_color = "#FFFFDD",
        text_size = 14,
        height = 380,
        width = 140
      )
    )
  ),

# box for the bar chart (and eventually something else too)
  lcarsBox( 
    corners = c(1, 2, 3, 4),
    color = c("#9999FF", "#9999FF", "#9999FF", "#9999FF"),
    sides = c(1, 2, 3, 4),
    
    fluidRow(
      column(5, plotOutput("range_percent"))
    ),
  ),

# shoutout to the creator of the lcars package
  div(br(), h6("The aesthetics of this app are based on the LCARS computer interface
         from Star Trek because I am an unrepentant nerd. Credit for the theme
         and custom widgets goes to Matthew Leonawicz!
         https://github.com/leonawicz/lcars/tree/master?tab=readme-ov-file"))
)





server <- function(input, output, session) {

# first section is for tidying data and creating reactive variables 
  
  averages <- eventReactive(input$upload, {
    
# reads the uploaded files to create a list of data frames with the same names
# as the original files + ".xlsx"   
    files <- setNames(
      lapply(input$upload$datapath, read_excel), 
      sapply(input$upload$name, basename))
    
# interpolation of values outside of the CGM's range (over 400 and under 40)
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
    
# calculates the daily averages
    averages <- all_data |>
      group_by(date) |>
      summarize(daily_avg = mean(value))
    
# adds a new column for the moving average    
    averages <- averages |>
      mutate(ma_10 = rollmean(daily_avg, k = 10, fill = NA, align = "right"))
  })
    
  
# calculates overall average
  overall_average <- eventReactive(input$upload, {
    overall_average <- round(mean(as.vector(averages()$daily_avg)))
  })
    
# takes earliest date of observations 
  first_date <- eventReactive(input$upload, {
    averages()$date[1]
  })
    
# takes most recent date of observations
  last_date <- eventReactive(input$upload, {
    averages_inverse <- arrange(averages(), desc(date))
    last_date <- averages_inverse$date[1]
  })
  

# second section is for. everything else
  
  
# updates the start/end dates of the date range input in the top right based
# on the reactive variables created above
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

#yapppinnggggggg to fill space
  output$intro <- renderUI ({
          div(
        h4("INTRO"), 
        p("After data is uploaded, a graph will appear in the box below, as well
          as an overall estimated daily average and bar plot which compares the
          percent of days with an average value within your selected ideal range.
          The breadth of this range is adjustable using the slider to the right."),
        p("The chart on the right side of the box displays a rough estimate of A1C
          based on average blood sugar. The overall average displayed at the top
          of the box will not necessarily be compatible with this chart as A1C 
          is determined by only the last 90 days of blood sugar behaviour."),
        br(),
        h6("Disclaimer: None of the graphics will render unless you upload data first. Also
           you have to use it at full width or nothing aligns properly. Enjoy! ")
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
  
  
# text for the overall average title at the top of the box
  output$overall_average <- renderText ({
    req(input$upload)
    paste("OVERALL AVERAGE =", overall_average(), "mg/dL")
  })
  
# conversion chart
  output$a1c_eag <- renderTable ({
    conversion_chart_gt
  })
 
  
# the plot thickens 
  output$averages_plot_app <- renderPlot({
     req(input$upload)
    
# this first part creates a geom-less plot to function as a base for the conditionals 
# so that there's less copy + pasting
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
# maybe: geom_hline(yintercept = overall_average(), color = "blue", linetype = "dotted", linewidth = 1) +
      scale_color_manual(
        "Method", 
        values = c("#cc6699", "#cc99cc"), 
        labels = c("Moving Average", "Daily Average")
      ) + theme_lcars_light() +
      theme(
        plot.title = element_text(size = 22),
        axis.title = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        plot.margin = margin(25, 10, 25, 25)
      )
    
    
# this bit tests which buttons have been toggled to determine which extra
# conditions to add to the original empty plot 
# (so far unsuccessful in removing the obvious redundancy 
# without fucking up the whole thing)(i don't know why)
    
# both lines toggled on
    if (input$mavg == TRUE & input$davg == TRUE){
      averages_plot_app <- averages_plot_app +
        geom_line(aes(y = daily_avg, color = "#cc99cc")) +
        geom_line(aes(y = ma_10, color = "#cc6699"), linewidth = 0.92) + 
        theme_lcars_light() +
        theme(
          plot.title = element_text(size = 22),
          axis.title = element_text(size = 15),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 12),
          plot.margin = margin(20, 20, 20, 20)
        )
    }
# moving average on and daily averages off
    if(input$mavg == TRUE & input$davg == FALSE){
      averages_plot_app <- averages_plot_app + 
        geom_line(aes(y = daily_avg, color = "#cc99cc"), alpha = 0) +
        geom_line(aes(y = ma_10, color = "#cc6699"), linewidth = 0.92) + 
        theme_lcars_light() +
        theme(
          plot.title = element_text(size = 22),
          axis.title = element_text(size = 15),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 12),
          plot.margin = margin(20, 20, 20, 20)
        )
    }
# daily averages on and moving average off
    if(input$davg == TRUE & input$mavg == FALSE){
      averages_plot_app <- averages_plot_app +
        geom_line(aes(y = daily_avg, color = "#cc99cc")) +
        geom_line(aes(y = ma_10, color = "#cc6699"), alpha = 0) + 
        theme_lcars_light() +
        theme(
          plot.title = element_text(size = 22),
          axis.title = element_text(size = 15),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 12),
          plot.margin = margin(20, 20, 20, 20)
        )
    }
# both lines off (empty plot)
    else {
      averages_plot_app <- averages_plot_app + 
        geom_line(aes(y = daily_avg, color = "#cc99cc"), alpha = 0) +
        geom_line(aes(y = ma_10, color = "#cc6699"), alpha = 0) + 
        theme_lcars_light() +
        theme(
          plot.title = element_text(size = 22),
          axis.title = element_text(size = 15),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 12),
          plot.margin = margin(20, 20, 20, 20)
        )
    }
    averages_plot_app
  })
  
  
# stacked bar chart showing percentage of days in range — adjusts with the slider
  output$range_percent <- renderPlot({
    bar_averages <- averages()
  
    bar_averages <- bar_averages |>
      mutate(class = "time", in_range = daily_avg) |>
      mutate(in_range = replace(in_range, in_range > input$rect, ">" )) |>
      mutate(in_range = replace(in_range, in_range <= input$rect & in_range != ">", "<"))
    
    range_percent <- ggplot(bar_averages, aes(x = class, y = daily_avg, fill = in_range)) +
      geom_bar(position = position_fill(reverse = TRUE), stat = "identity") +
      scale_fill_manual(
        values = c("#ffcc66", "#99CCFF" ), 
        breaks = c(">", "<"),
        labels = c(
          paste("Daily Average >", input$rect, "mg/dL"), 
          paste("Daily Average <", input$rect, "mg/dL"))
      ) +
      labs(
        caption = "Percentage of Days 'In-Range'",
        x = NULL,
        y = NULL
      ) +
      theme_lcars_dark() +
      theme(
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 0.5, size = 16, margin = margin(t = 15)),
        legend.text = element_text(size = 12),
        axis.text.x = element_blank(),
        plot.margin = margin(28, 28, 28, 32),
      )
  range_percent
  })
}


shinyApp(ui, server)
