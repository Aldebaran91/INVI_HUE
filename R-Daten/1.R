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
                   "Infant",
                   "Education & Infant",
                   "Education & Catholic")),
    radioButtons(inputId = "cmbVis",
                 label = "Choose a visualization:", 
                 choices = c(
                   "Plot",
                   "4er Plot fü lm" = "4Plot",
                   "Boxplot",
                   "Histogramm (r=mean, b=median)" = "Histogramm",
                   "Matrix")),
    sliderInput("bw_adjust", label = "Bandwidth adjustment for hist:",
                min = 0.2, max = 2, value = 1, step = 0.2)),
  mainPanel(
    plotOutput("out", height = 800))
)


server <- function(input, output) {
  datasetInput <- reactive({
    switch(input$cmbData,
       "Fertility" = swiss$Fertility,
       "Agriculture" = swiss$Agriculture,
       "Education" = swiss$Education,
       "Catholic" = swiss$Catholic,
       "Infant" = swiss$Infant.Mortality,
       "Education & Infant" = swiss$Education~swiss$Infant.Mortality,
       "Education & Catholic" = swiss$Education~swiss$Catholic)
  })
  
  output$out <- renderPlot({
    if (input$cmbVis == "Plot"){
      plot(datasetInput())
      if (input$cmbData == "Education & Infant" ||
          input$cmbData == "Education & Catholic"){
        abline(lm(datasetInput()))
      }
    } else if (input$cmbVis == "4Plot") {
      par(mfrow=c(2,2))
      plot(lm(datasetInput()))
    } else if (input$cmbVis == "Boxplot") {
      boxplot(datasetInput(), horizontal = T)
    } else if (input$cmbVis == "Histogramm") {
      hist(datasetInput(), probability = T,
           main = input$cmbData)
      dens <- density(datasetInput(), adjust = input$bw_adjust)
      lines(dens, col = "blue")
      abline(v = mean(datasetInput()), col="red", lwd=3, lty=2)
      abline(v = median(datasetInput()), col="blue", lwd=3, lty=2)
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
    }
  })
}

shinyApp(ui = ui, server = server)