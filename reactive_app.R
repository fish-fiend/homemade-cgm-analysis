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
# yeah ik thats a lot of packages


# A1C to eAG conversion chart
  conversion_chart <- data.frame(
    eAG = c(126, 133, 140, 147, 154, 161, 169, 176, 183, 190, 197),
    A1C = c(6, 6.25, 6.5, 6.75, 7, 7.25, 7.5, 7.75, 8, 8.25, 8.5)
  )
  conversion_chart_gt <- conversion_chart |>
    gt()

  
# function for cleaning up data
  reshape <- function(cgm_data) {
    cgm_data[-(1:19),] |>
      mutate(date = as.Date(...2, '%Y-%m-%d'), value = as.numeric(...8)) |>
      select(date, value)
  }
  
  
# initial dates for the date range display
  first_date <- Sys.Date() - 90
  last_date <- Sys.Date()

  

ui <- lcarsPage(
  
# title
  lcarsHeader("homemade blood sugar analysis", 
              color = "#FF9900", 
              title_right = FALSE),
  
# header section — intro and customizable settings for the plot
  lcarsSweep(reverse = TRUE, color = "#99CCFF",

# as of yet useless but aesthetically pleasing center divider
    inputColumn(
      lcarsRect(color = "#cc99cc", height = 25, round = c("both")),
      lcarsRect(color = "#EE4444", height = 25, round = c("both")),
      lcarsButton("button", "", color = "golden-tanoi"),
      lcarsRect(color = "#3366cc", height = 25, round = c("both"))
    ),

# yappingggg
# needed something to fill the space on the left side
    left_inputs = inputColumn(
      div(
        h3("Intro"), 
        p("In my own experience as a diabetic, I've found Dexcom's
          lack of tools for visualizing longterm blood sugar trends to be 
          dissapointing. I intended to create a simple, non-reactive plot of my
          own data as an easy way to practive my R skills. It's probably obvious 
          that I got a bit carried away. Enjoy!"),
        br(),  
        p("After data is uploaded, a plot will appear in the box below.
          The blue shaded region on the plot represents the range that you would
          like your daily average blood sugar to stay within. The
          breadth of this range is adjustable using the slider to the right.
          The idea is that the chart of A1C to average blood glucose next to the 
          graph can be used in conjuction with the 'overall average' value 
          displayed over the graph to help you determine your ideal range."),
        br(),
        h6("Disclaimer: Use with your own discretion. I am not a medical 
            professional and this is not a professional tool. Or authorized by 
            Dexcom. Which is hopefully a non-issue.")
      )
    ),


# fun stuff! custom inputs for the plot (date range and ideal average range)
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
    chooseSliderSkin("Modern", color = "#cc99cc"),
    left_width = 0.6
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
            "data_upload", "",
            buttonLabel = "Import",
            placeholder = "see directions below",
            width = 250
        )
      )
    )
  ),
  
  
# toggleable instructions below bracket
  fluidRow(
    column(2,
        lcarsCheckbox(
          "instructions", "Instructions for Downloading and Uploading Data",
          width = 150,
          value = FALSE
        )
    ),
    column(12,
        htmlOutput("instructions")
    )
  ),
  
  
  
# box for displaying the plot, toggles, overall average, and conversion chart
  lcarsBox(
    corners = c(1, 2, 3),
    sides = c(1, 3, 4),
    color = c("#CC6699",  "#FF9900",  "#FFCC66",  "#FFCC66"),
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

# shoutout to the creator of the lcars package
  div(br(), h6("The aesthetics of this app are based on the LCARS computer interface
         from Star Trek because I am an unrepentant nerd. Credit for the theme
         and custom widgets goes to Matthew Leonawicz!
         https://github.com/leonawicz/lcars/tree/master?tab=readme-ov-file"))
)




server <- function(input, output, session) {


# first section is for tidying data and creating reactive variables (data frame 
# to populate my graph with, overall average value to display at the top, dates 
# to automatically adjust the axes)
  
# reads the file, cleans it up and calculates averages
  averages <- eventReactive(input$data_upload, {
    
    cgm_data <- read_excel(input$data_upload$datapath)
    
    cgm_data$...8[cgm_data$...8 == 'High'] <- '400'
    cgm_data$...8[cgm_data$...8 == 'Low'] <- '40'
    
    cgm_data <- reshape(cgm_data)
    
    cgm_averages <- cgm_data |>
      group_by(date) |>
      summarize(daily_avg = mean(value))
    
    averages <- cgm_averages |>
      mutate(ma_10 = rollmean(daily_avg, k = 10, fill = NA, align = "right"))
  })
    
# calculates overall average from available data
  overall_average <- eventReactive(input$data_upload, {
    overall_average <- round(mean(as.vector(averages()$daily_avg)))
  })
    
# earliest date of observations 
  first_date <- eventReactive(input$data_upload, {
    averages()$date[1]
  })
    
# most recent date of observations
  last_date <- eventReactive(input$data_upload, {
    averages_inverse <- arrange(averages(), desc(date))
    last_date <- averages_inverse$date[1]
  })
  

# second section is for. everything else
# by which i mean actual outputs
  
# updates the start/end dates of the date range input in the top right based
# on the reactive variables created above
  observe({
    input$data_upload
    
    updateDateRangeInput(
      session,
      "interval",
      start = first_date(),
      end = last_date(),
      min = first_date(),
      max = last_date()
    )
  })

# renders instructions for downloading/uploading raw data (beneath the bracket)
  output$instructions <- renderUI ({
    if (input$instructions == TRUE){
      div(
        br(),
        p("How to Download Raw CGM Data in the Correct Format"),
        h6("Step 1: Log into Dexcom Clarity at https://clarity.dexcom.com/). On the
        far right side of the Overview page, just above the 'Sensor Usage' widget, 
        is an icon that looks like a spreadsheet with an arrow pointing right."),
        h6("Step 2: Click that icon, select a date range, then press export. The 
        data will automatically download in Numbers."),
        h6("Step 3: Open it in numbers then go to File > Export To > Excel"),
        h6("Step 4: Name the file whatever you want, then come back here and hit 
        import!"),
        br()
      )
    }
  })
  
# renders text for the overall average title at the top of the box
  output$overall_average <- renderText ({
    req(input$data_upload)
    paste("OVERALL AVERAGE =", overall_average(), "mg/dL")
  })
  
# renders the conversion chart
  output$a1c_eag <- renderTable ({
    conversion_chart_gt
  })
 

# the plot thickens 
# please bear with me its about to get messy
  output$averages_plot_app <- renderPlot({
    
    req(input$data_upload)
    
# this part creates a geom-less plot to function as a base for the conditionals 
# to build on so that there's less copy + pasting
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
      theme(
        plot.title = element_text(size = 22),
        axis.title = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        plot.margin = margin(25, 10, 25, 25)
      )
    
    
# this bit tests which buttons have been toggled to determine which extra
# conditions to add to the original plot 
# (so far unsuccessful in removing the obvious redundancy in the style 
# settings without fucking up the whole thing)(sigh)
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
}


shinyApp(ui, server)
