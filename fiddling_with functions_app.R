library(shiny)
library(tidyverse)
library(readxl)
library(stats)
library(zoo)


reshape <- function(cgm_data) {
  cgm_data[-(1:19),] |>
    mutate(date = as.Date(...2, '%Y-%m-%d'), value = as.numeric(...8)) |>
    select(date, value) |>
    filter(!is.na(value))
}

ui <- fluidPage(
  fileInput(
    "upload", "",
    buttonLabel = "Import",
    placeholder = "see directions below",
    multiple = TRUE,
    accept = ".xlsx",
    width = 250 
  ),
  tableOutput("head")
)

server <- function(input, output, session) {
  
# creates a list of data frames with the same names as the original files + ".xlsx"
# then cleans the data
  averages <- eventReactive(input$upload, {
    files <- setNames(
              lapply(input$upload$datapath, read_excel), 
              sapply(input$upload$name, basename))
    
    files <- lapply(files, reshape)
    
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
    
    averages <- all_data |>
      group_by(date) |>
      summarize(daily_avg = mean(value))
    
    averages <- averages |>
      mutate(ma_10 = rollmean(daily_avg, k = 10, fill = NA, align = "right"))
  })
  
  output$head <- renderTable({
    averages()
  })

}

shinyApp(ui, server)