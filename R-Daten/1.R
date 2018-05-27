library(shiny)
d <- swiss
fertility <- d[,1, drop = F]
agriculture <- d[,2, drop = F]
education <- d[,4, drop = F]
catholic <- d[,5, drop = F]
infant <- d[,6, drop = F]

ui <- fluidPage(
  selectInput(inputId = "cmbData",
              label = "Choose a dataset:", 
              choices = c(
                "Fertility",
                "Agriculture",
                "Education",
                "Catholic",
                "Infant")),
  plotOutput("hist")
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
    hist(datasetInput())
  })
}

shinyApp(ui = ui, server = server)