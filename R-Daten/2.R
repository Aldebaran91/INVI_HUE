library(shiny)
library(utils)
library(psych)
library(moments)

ui <- fluidPage(
  
  tabsetPanel(id = "tabselected",
              tabPanel("Dataset Summary",
                       sidebarLayout(
                         sidebarPanel(
                           radioButtons(inputId = "cmbData",
                                        label = "Choose a dataset:", 
                                        choices = c(
                                          "Population",
                                          "Income",
                                          "Illiteracy",
                                          "Life.Exp",
                                          "Murder",
                                          "HS.Grad",
                                          "Frost")),
                           radioButtons(inputId = "cmbVis",
                                        label = "Choose a visualization:", 
                                        choices = c(
                                          "Q-Q Plot",
                                          "Boxplot",
                                          "Histogramm (r=mean, b=median)" = "Histogramm")),
                           sliderInput("bw_adjust", label = "Bandwidth adjustment for Histogram:",
                                       min = 0.2, max = 2, value = 1, step = 0.2), 
                           sliderInput("bins",
                                       "Number of bins in Histogram:",
                                       min = 1,
                                       max = 50,
                                       value = 20)
                         ),
                         mainPanel(
                           
                           helpText("Summary"), verbatimTextOutput("summary"),
                           helpText("Expectation"), verbatimTextOutput("expectation"), 
                           helpText("Variance"), verbatimTextOutput("variance"), 
                           helpText("Skewness"), verbatimTextOutput("skewness"), 
                           helpText("Kurtosis"),verbatimTextOutput("kurtosis"), plotOutput("out")))),
              tabPanel("Regression", plotOutput("pairs"),plotOutput("reg1"),plotOutput("reg2"),plotOutput("reg3"),plotOutput("reg5")),
              tabPanel("Scatterplots", 
                       sidebarLayout(
                         sidebarPanel(
                           radioButtons(inputId = "cmbData1",
                                        label = "Choose a dataset:", 
                                        choices = c(
                                          "Population",
                                          "Income")),
                           radioButtons(inputId = "cmbData2",
                                        label = "Choose a dataset:", 
                                        choices = c(
                                          "Murder",
                                          "Illiteracy",
                                          "HS.Grad",
                                          "Life.Exp",
                                          "Frost"))
                         ),
                       mainPanel(plotOutput("scatterplot"))
                       ))
  )
)



server <- function(input, output) {
  s <- as.data.frame(state.x77)
  colnames(s)[4] <- "Life.Exp"                   
  colnames(s)[6] <- "HS.Grad"  
  s$Area <- NULL
  datasetInput <- reactive({
    switch(input$cmbData,
           "Population" = s$Population,
           "Income" = s$Income,
           "Illiteracy" = s$Illiteracy,
           "Life.Exp" = s$Life.Exp,
           "Murder" = s$Murder,
           "HS Grade" = s$HS.Grad,
           "Frost" = s$Frost)
  })
  
  output$out <- renderPlot({
    if (input$cmbVis == "Q-Q Plot"){
      qqnorm(s[,input$cmbData],
             
             main=paste("Q-Q Plot for all States with the variable", input$cmbData),
             ylab="Norm-Distribution",
             xlab=input$cmbData)
      qqline(s[,input$cmbData])
    } else if (input$cmbVis == "Boxplot") {
      boxplot(s[,input$cmbData],horizontal = TRUE,
              col = "Grey",
              ylab="",
              xlab=input$cmbData)
      
      abline(v=mean(state.x77[,colnames(state.x77) %in% input$cmbData]),
             col="red")
      abline(v=median(state.x77[,colnames(state.x77) %in% input$cmbData]),
             col="black")
    } else if (input$cmbVis == "Histogramm") {
      hist(s[,input$cmbData], probability = T, breaks = input$bins,
           main = input$cmbData, xlab = input$cmbData)
      dens <- density(s[,input$cmbData], adjust = input$bw_adjust)
      lines(dens, col = "blue")
      
      abline(v = mean(datasetInput()), col="red")
      abline(v = median(datasetInput()), col="blue")
      abline(v = getmode(datasetInput()), col="green")
      
      legend("topright", legend=c("Mean", "Median", "Mode"),
             col=c("red", "blue", "green"), lty=1)
    } 
  })
  
  lml <- lm(Murder~Population+Illiteracy+Income,data = s)
  
  output$pairs <- renderPlot({
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
  })
  
  output$reg1 <- renderPlot({
    plot(lml,1)
  })
  
  output$reg2 <- renderPlot({
    plot(lml,2)
  })
  
  output$reg3 <- renderPlot({
    plot(lml,3)
  })
  
  output$reg4 <- renderPlot({
    plot(lml,4)
  })
  
  output$reg5 <- renderPlot({
    plot(lml,5)
  })
  
  
  output$summary <- renderPrint({
    summary(s[, input$cmbData])
  })
  
  output$kurtosis <- renderPrint({
    kurtosis(s[,input$cmbData])
  })
  output$expectation <- renderPrint({
    mean(s[,input$cmbData])
  })
  output$variance <- renderPrint({
    var(s[,input$cmbData])
  })
  output$skewness <- renderPrint({
    skewness(s[,input$cmbData])
  })
  
  output$scatterplot <- renderPlot({
    # Render a scatterplot
    plot(log(s[,input$cmbData1])~s[,input$cmbData2],
         
         main=paste("Scatterplot over all States with the variables", input$cmbData1," and ", input$cmbData2),
         ylab=input$cmbData1,
         xlab=input$cmbData2)
    abline(lm(log(s[,input$cmbData1])~s[,input$cmbData2]))
  })
}


# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

shinyApp(ui = ui, server = server)
