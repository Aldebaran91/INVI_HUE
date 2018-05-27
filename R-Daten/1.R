library(shiny)

ui <- fluidPage(
  selectInput(inputId = "cmbData",
              label = "Choose a dataset:", 
              choices = c(
                "Fertility",
                "Agriculture",
                "Education",
                "Catholic",
                "Infant")),
  plotOutput("hist"),
  verbatimTextOutput("stats")
)

server <- function(input, output) {
  datasetInput <- reactive({
    switch(input$cmbData,
           "Fertility" = swiss$Fertility,
           "Agriculture" = swiss$Agriculture,
           "Education" = swiss$Education,
           "Catholic" = swiss$Catholic,
           "Infant" = swiss$Infant.Mortality)
  })
  output$hist <- renderPlot({
    hist(datasetInput(),
    main = input$cmbData)
  })
  output$stats <- renderPrint({
    summary(datasetInput())
  })
}

shinyApp(ui = ui, server = server)