library(shiny)

ui <- fluidPage(
  sidebarPanel(
    radioButtons(inputId = "cmbData",
                label = "Choose a dataset:", 
                choices = c(
                  "Fertility",
                  "Agriculture",
                  "Education",
                  "Catholic",
                  "Infant")),
    sliderInput("bw_adjust", label = "Bandwidth adjustment:",
                min = 0.2, max = 2, value = 1, step = 0.2)),
  mainPanel(
    plotOutput("hist"),
    plotOutput("box"),
    verbatimTextOutput("stats"))
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
    hist(datasetInput(), probability = T,
    main = input$cmbData)
    dens <- density(datasetInput(), adjust = input$bw_adjust)
    lines(dens, col = "blue")
  })
  
  output$box <- renderPlot({
    boxplot(datasetInput(), horizontal = T)
  })
  
  output$stats <- renderPrint({
    summary(datasetInput())
  })
}

shinyApp(ui = ui, server = server)