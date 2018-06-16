library(shiny)
library(moments)

ui <- fluidPage(
  sidebarPanel(
    radioButtons(inputId = "cmbVis",
                 label = "Choose a visualization:", 
                 choices = c(
                  "Plot",
                  "Boxplot",
                  "Histogramm",
                  "QQ Norm")),
    sliderInput("bw_adjust", label = "Bandwidth adjustment for hist:",
                min = 0.2, max = 2, value = 1, step = 0.2)),
  mainPanel(
    plotOutput("out", height = 800))
)


server <- function(input, output) {
  
  output$out <- renderPlot({
    if (input$cmbVis == "Plot"){
      m <- mean(LakeHuron, na.rm=TRUE,zero=TRUE)
      med <- median(LakeHuron)
      
      plot(LakeHuron)
      qqline(med, col=1)
      qqline(m, col=2)
      qqline(m+mad(LakeHuron), col=3)
      qqline(m-mad(LakeHuron), col=3)
    } else if (input$cmbVis == "Boxplot") {
      boxplot(LakeHuron, horizontal = T)
    } else if (input$cmbVis == "Histogramm") {
      hist(LakeHuron, probability = T,
           main = input$cmbData)
      dens <- density(LakeHuron, adjust = input$bw_adjust)
      lines(dens, col = "blue")
    } else if (input$cmbVis == "QQ Norm") {
      qqnorm(LakeHuron)
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
      pairs(cbind(LakeHuron, time(LakeHuron)), lower.panel = panel.smooth, upper.panel = panel.cor)
    }
  })
}

shinyApp(ui = ui, server = server)