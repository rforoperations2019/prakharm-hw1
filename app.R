#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(plotly)
library(tools)
library(shinythemes)

arrest_data <- read.csv("arrest_data.csv")


ui <- fluidPage( theme = shinytheme("flatly"),
  
  
  
   #Title
   titlePanel("Arrest Data for Pittsburgh"),
   
   
   #Sidebar 
   sidebarLayout(
      
     sidebarPanel(
        
        
        #X-axis variable
        selectInput(inputId = "x", 
                    label = "X-axis:",
                    choices = c( "Council District" = "COUNCIL_DISTRICT", 
                                 "Public Works Division" = "PUBLIC_WORKS_DIVISION",
                                 "Cases" = "PK"),
                    selected = "Cases"),
        
        
        #Y-axis variable
        selectInput(inputId = "y", 
                    label = "Y-axis:",
                    choices = c("Council District" = "COUNCIL_DISTRICT", 
                                "Public Works Division" = "PUBLIC_WORKS_DIVISION"
                                ), 
                    selected = "PUBLIC_WORKS_DIVISION"),
        
        
        #Color defining z-variable
        selectInput(inputId = "z", 
                    label = "Color by:",
                    choices = c("Gender" = "GENDER", 
                                "Race" = "RACE"),
                    selected = "GENDER"),
        
        #Alpha value
        sliderInput(inputId = "alpha", 
                    label = "Alpha:", 
                    min = 0, max = 1, 
                    value = 0.5),
        
        
        #Size of scatter plot points
        sliderInput(inputId = "size", 
                    label = "Size:", 
                    min = 0, max = 5, 
                    value = 2),
        
        
        #Checkbox to toggle on/off data table
        checkboxInput(inputId = "show_data",
                      label = "Show data table",
                      value = TRUE),
        

        
        #Checkbox to select council districts
        checkboxGroupInput(inputId = "selected_type",
                           label = "Search by council districts:",
                           choices = c("1", "2", "3","4","5","6","7","8","9"),
                           selected = "1"),
        
        
        #Selecting the size of the sample
        numericInput(inputId = "n_samp", 
                     label = "Sample size:", 
                     min = 1, max = nrow(arrest_data), 
                     value = 1000),
        
        
        #Download Button to download the current datatable being displayed
        downloadButton("downloadData", "Download Data")
         
      ),
      

      mainPanel(
        
        tabsetPanel(
          
          tabPanel("Data Table", br(), br(), uiOutput(outputId = "n"),br(), br(),DT::dataTableOutput("DataTable")),
          
          tabPanel("Scatterplot",br(), br(), plotOutput(outputId = "scatterplot")),
          
          tabPanel("Donut Chart",br(), br(), plotlyOutput(outputId = "donut")),
          
          tabPanel("Histogram", br(), br(),plotlyOutput(outputId = "histogram")),
          
          tabPanel("Pie Chart", br(), br(),plotlyOutput(outputId = "piechart"))
          
          
        )
       
        #Scatterplot
        #plotOutput(outputId = "scatterplot"),
        #br(),br(),
        
        #Histogram
        #plotlyOutput(outputId = "histogram"),
        #br(), br(),
        
        #Donut chart
        #plotlyOutput(outputId = "donut"),
        #br(), br(),
        
        #Piechart
        #plotlyOutput(outputId = "piechart"),
        #br(), br(),
        
        # Printing the number of observations plotted
        #uiOutput(outputId = "n"),
        #br(), br(), br(),    # a little bit of visual separation
        
        #to see data table
        #DT::dataTableOutput("DataTable")
          
      )
   )
)


server <- function(input,output,session) {
   
  # Subsetting data for filtering based on selection of council district
  arrest_data_subset <- reactive({
    req(input$selected_type) # ensure availablity of value before proceeding
    filter(arrest_data, COUNCIL_DISTRICT %in% input$selected_type)
  })
  
  # Updating the size of the sample subset
  observe({
    updateNumericInput(session, 
                       inputId = "n_samp",
                       value = min(1000, nrow(arrest_data_subset())),
                       max = nrow(arrest_data_subset())
    )
  })
  
  # A new dataframe that subsets the sample based on the size requested
  arrest_data_sample <- reactive({ 
    req(input$n_samp) 
    sample_n(arrest_data_subset(), input$n_samp)
  })
  
  # server logic for the scatterplot 
  output$scatterplot <- renderPlot({
    ggplot(data = arrest_data_sample(),aes_string(x = input$x,y=input$y,
                                              color=input$z))+
      geom_point(alpha = input$alpha, size = input$size)
  })
  
  #server logic for the histogram
  output$histogram <- renderPlotly({
    p <- plot_ly(arrest_data_sample(), x = ~AGE, y = ~GENDER) %>%
      add_histogram2d() %>%
    layout(title = "Hover over the graph to look at number of cases by age and gender")
    
    #p <- p %>% add_histogram2d(colorscale = "Blues")
    
  })
  
  
  #server logic for the donut chart
  output$donut <- renderPlotly({
    p <- arrest_data_sample() %>%
      group_by(COUNCIL_DISTRICT) %>%
      summarize(count = n()) %>%
      plot_ly(labels = ~COUNCIL_DISTRICT, values = ~count) %>%
      add_pie(hole = 0.6) %>%
      layout(title = "Donut chart depicting percentage of cases by council district",  showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  #server logic for the piechart
  output$piechart <- renderPlotly({
    p <- plot_ly(arrest_data_sample(), labels = ~COUNCIL_DISTRICT, type = 'pie',
                 textposition = 'inside',
                 textinfo = 'label+percent',
                 insidetextfont = list(color = '#FFFFFF'),
                 marker = list(colors = colors,
                               line = list(color = '#FFFFFF', width = 1)),
                 showlegend = TRUE) %>%
      layout(title = 'Pie chart depicting percentage of cases by council district',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  #To print the number of cases plotted, by council districts
  output$n <- renderUI({
    types <- arrest_data_sample()$COUNCIL_DISTRICT %>% 
      factor(levels = input$selected_type) 
    counts <- table(types)
    
    HTML(paste("There are", counts,"cases from Council District", input$selected_type, "in this dataset. <br>"))
  })
  
  # To toggle on/off displaying the data table
  output$DataTable <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(data = arrest_data_sample()[, 1:7], 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
    }
  )
  
  #Download csv
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui = ui, server = server)

