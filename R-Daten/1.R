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
    radioButtons(inputId = "cmbVis",
                 label = "Choose a visualization:", 
                 choices = c(
                   "Plot",
                   "Boxplot",
                   "Histogramm",
                   "Matrix",
                   "Mosaic")),
    sliderInput("bw_adjust", label = "Bandwidth adjustment for hist:",
                min = 0.2, max = 2, value = 1, step = 0.2)),
  mainPanel(
    plotOutput("out"))
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
  
  output$out <- renderPlot({
    if (input$cmbVis == "Plot"){
      plot(datasetInput())
    } else if (input$cmbVis == "Boxplot") {
      boxplot(datasetInput(), horizontal = T)
    } else if (input$cmbVis == "Histogramm") {
      hist(datasetInput(), probability = T,
           main = input$cmbData)
      dens <- density(datasetInput(), adjust = input$bw_adjust)
      lines(dens, col = "blue")
    } else if (input$cmbVis == "Matrix") {
      panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
      {
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        r <- abs(cor(x, y))
        txt <- format(c(r, 0.123456789), digits = digits)[1]
        txt <- paste0(prefix, txt)
        if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
        text(0.5, 0.5, txt, cex = cex.cor * r)
      }
      pairs(swiss, lower.panel = panel.smooth, upper.panel = panel.cor)
    } else if (input$cmbVis == "Mosaic") {
      ## TODO
      #library(vcd)
      #mosaic(as.numeric(unlist(swiss)))
    }
  })
}

shinyApp(ui = ui, server = server)