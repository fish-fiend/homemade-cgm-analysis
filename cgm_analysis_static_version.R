library(shiny)
library(shinyWidgets)
library(tidyverse) 
library(ggplot2)
library(gt)
library(lcars)
library(grid)
library(png)
library(readxl)




#A1C to eAG conversion chart
  conversion_chart <- data.frame(
    eAG = c(126, 133, 140, 147, 154, 161, 169, 176, 183, 190, 197),
    A1C = c(6, 6.25, 6.5, 6.75, 7, 7.25, 7.5, 7.75, 8, 8.25, 8.5)
   )

  conversion_chart_gt <- conversion_chart |>
    gt() |>
    tab_header(
      title = md("**A1C to eAG Conversion Chart**"),
      subtitle = "(eAG = estimated average glucose)"
    ) |>
    opt_stylize(
      style = 4,
      color = "cyan"
    )



#customizable settings for graph
#CURRENTLY NOT WORKING!!! :[
date_range <- dateRangeInput(
            "interval", "Date Range",
            start = first_date,
            end = last_date,
            min = first_date, 
            max = last_date
            )
slider <- sliderInput(
           "rect", "'Ideal' Daily Average Upper Limit",
            min = 110, max = 180,
            value = c(170),
            step = 5
            )


  ui <- lcarsPage(

#title
    lcarsHeader("blood sugar analysis for diabetics, by a diabetic", 
                color = "#FF9900", 
                title_right = FALSE),
#first page
  lcarsSweep(
      reverse = TRUE,
      color = "#99CCFF",
      inputColumn(
        lcarsRect(color = "#cc99cc", height = 25, round = c("both")),
        lcarsRect(color = "#EE4444", height = 25, round = c("both")),
        lcarsButton("button", "this is a button btw", color = "golden-tanoi"),
        lcarsRect(color = "#3366cc", height = 25, round = c("both"))
       ),

      left_inputs = inputColumn(
        div(
          h3("Intro"), 
          p("Among the many things to dislike about Dexcom, I've found
            their lack of tools for visualizing longterm blood sugar trends to be 
            dissapointing. So, I thought a fun way to practice my very beginner R 
            skills would be creating a customizable plot of daily averages over 
            time with a moving average overlay and a shaded 'ideal range'."),
          p("I didn't intend to make this publically available, but it turned out 
            to be a much more effective way to identify connections between my life
            and blood sugar than I hoped it would be. I figured there must be at
            least one other person who might find it helpful."),
          br(),
          h5("Features to look forward to:"),
          h6("ability to convert units from mg/dL to mmol/L,  percent of days in 
           range (as a plot of some kind?), compatability with data from 
           other CGM brands"),
        )
      ),

      right_inputs = inputColumn(
          lcarsPill(
            title = "GRAPH SETTINGS",
            height = 30
          ),
         div(br()),
         date_range,
         div(br()),
         slider
      ),
         chooseSliderSkin("Modern", color = "#cc99cc"),
         left_width = 0.6
    ),


#uploading data bracket
  lcarsBracket(
    fluidRow(
      column(5,
             lcarsRect(
               color = "#000000",
               height = 52,
               text = "UPLOAD DATA HERE:",
               text_size = 18,
               text_color = "#FFFFED"
             )
      ),
      column(6,
             fileInput(
               "data_upload", "",
               buttonLabel = "Import",
               placeholder = "Excel Sheets Only!!"
             )
      ),
    )
  ),


#toggleable instructions
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
  
    

#second page
  lcarsBox(
    fluidRow(
      column (12, plotOutput("averages_plot_app"))
    ),
    
    title = textOutput("overall_average"),
    title_color = "atomic-tangerine",
    
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
    
    right_inputs = inputColumn(
      lcarsRect(
        "eAG = estimated average glucose",
        text_size = 10,
        round = "left", 
        height = 21),
      lcarsRect(
        tableOutput("a1c_eag"), 
          round = c("both"), 
          color = "#000000",
          text_color = "#FFFFDD",
          text_size = 14,
          height = 380,
          width = 140
      )
    ),
    
    corners = c(1, 2, 3),
    sides = c(1, 3, 4),
    color = c("#CC6699",  "#FF9900",  "#FFCC66",  "#FFCC66"),
    side_color = c( "#FF9900",  "#000000", "#FFCC66", "#CC6699")
  ),
  div(br(), h6("The aesthetics of this app are based on the LCARS computer interface
         from star trek because I am an unrepentant nerd. Credit for the theme
         and custom widgets goes to Matthew Leonawicz!
         https://github.com/leonawicz/lcars/tree/master?tab=readme-ov-file"),
      h6("And a special shoutout to my sister for peer pressuring me into trying
          R."))
)




server <- function(input, output) {

  output$instructions <- renderUI ({
    if (input$instructions == TRUE){
      div(
        br(),
        p("How to Download Raw CGM Data in the Correct Format"),
        h6("Step 1: Log into Dexcom Clarity at https://clarity.dexcom.com/). On the
        far left side of the Overview page, just above the 'Sensor Usage' widget, 
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
  
  
  output$overall_average <- renderText ({
    paste("OVERALL AVERAGE =", total_avg, "mg/dL")
  })
  
  
  output$a1c_eag <- renderTable ({
    conversion_chart_gt
  })

  
  output$averages_plot_app <- renderPlot({


#geom-less plot
    averages_plot_app <- ggplot(daily_averages, aes(x = date)) +
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
    
    
#conditionals
    
    if (input$mavg == TRUE & input$davg == TRUE){
      averages_plot_app <- averages_plot_app +
        geom_line(aes(y = daily_avg, color = "#cc99cc")) +
        geom_line(aes(y = ma_12, color = "#cc6699"), linewidth = 0.92) + 
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
        geom_line(aes(y = ma_12, color = "#cc6699"), linewidth = 0.92) + 
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
        geom_line(aes(y = ma_12, color = "#cc6699"), alpha = 0) + 
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
        geom_line(aes(y = ma_12, color = "#cc6699"), alpha = 0) + 
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