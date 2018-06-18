library(shiny)
library(utils)
library(psych)

ui <- fluidPage(
  sidebarPanel(
    radioButtons(inputId = "cmbData",
                 label = "Choose a dataset:", 
                 choices = c(
                   "Population",
                   "Income",
                   "Illiteracy",
                   "Life.Exp",
                   "Murder",
                   "HS Grade",
                   "Frost")),
    radioButtons(inputId = "cmbVis",
                 label = "Choose a visualization:", 
                 choices = c(
                   "Plot",
                   "Boxplot",
                   "Histogramm (r=mean, b=median)" = "Histogramm",
                   "Matrix")),
    sliderInput("bw_adjust", label = "Bandwidth adjustment for hist:",
                min = 0.2, max = 2, value = 1, step = 0.2)),
  mainPanel(
    plotOutput("out", height = 800))
)


server <- function(input, output) {
  s <- as.data.frame(state.x77)
  s$Area <- NULL
  datasetInput <- reactive({
    switch(input$cmbData,
           "Population" = s$Population,
           "Income" = s$Income,
           "Illiteracy" = s$Illiteracy,
           "Life.Exp" = s$`Life Exp`,
           "Murder" = s$Murder,
           "HS Grade" = s$`HS Grad`,
           "Frost" = s$Frost)
  })
  
  output$out <- renderPlot({
    if (input$cmbVis == "Plot"){
      plot(datasetInput())
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
      
      abline(v = mean(datasetInput()), col="red")
      abline(v = median(datasetInput()), col="blue")
      abline(v = getmode(datasetInput()), col="green")
      
      legend("topright", legend=c("Mean", "Median", "Mode"),
             col=c("red", "blue", "green"), lty=1)
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
      pairs(s, lower.panel = panel.smooth, upper.panel = panel.cor)
    }
  })
}

# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

shinyApp(ui = ui, server = server)