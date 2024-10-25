library(tidyverse)

highs <- function(x) {
  x$...8 <- replace(x$...8, x$...8 == "High", "400" )
}

lows <- function(x) {
  x$...8 <- replace(x$...8, x$...8 == "Low", "40" )
}

victim_02$...8 <- highs(victim_02)
victim_02$...8 <- lows(victim_02)



interpol <- function(y) {
  y$...8 <- highs(y)
  y$...8 <- lows(y)
  View(victim_02)
}


# read all the uploaded files
all_files <- reactive({
  req(input$upload)
  purrr::map(input$upload$datapath, read_csv) %>%
    purrr::set_names(input$upload$name)
})

#select a row in DT files and display the corresponding table
output$selected_file_table <- renderTable({
  req(input$upload)
  req(input$files_rows_selected)
  
  all_files()[[
    input$upload$name[[input$files_rows_selected]]
  ]]
})

