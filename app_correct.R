library(shiny)
library(tidyverse)

# Define UI
ui <- fluidPage(titlePanel('DNA Workbook Calculator'),
                sidebarLayout(
                  sidebarPanel(
                    fileInput(inputId = 'input_workbook', label = 'Upload DNA Extraction Workbook', multiple = FALSE, accept = c('.txt', 'tsv')),
                    selectInput(inputId = 'input_method', label = 'Select Sequencing Prep Method', choices = c('full', 'quarter'), selected = 'quarter', multiple = FALSE)
                  ),
                  mainPanel(tableOutput(outputId = 'output_workbook'))
                )
)

# Define server function  
server <- function(input, output, session) {
  
  df_workbook <- reactive({read_tsv(input$input_workbook$datapath)})
  
  target_volume <- reactive({
    
    if_else(input$input_method == 'quarter', 7.5, 30)
    
  })
  
  target_conc <- reactive({
    
    min(df_workbook()$DNA_Conc)
    
  })
  
  target_ng <- reactive({
    
    target_volume() * target_conc()
    
  })
  
  df_workbook_updated <- reactive({
    
    df_workbook() %>%
      mutate(DNA_Volume = target_ng() / DNA_Conc) %>%
      mutate(DNA_Volume = round(DNA_Volume, 1)) %>%
      mutate(Water_Volume = target_volume() - DNA_Volume)
    
  })
  
  output$output_workbook <- renderTable(df_workbook_updated())
  
}

# Create Shiny object
shinyApp(ui = ui, server = server)