library(shiny)
library(ggplot2)

ui <- fluidPage(
  sidebarPanel(
    radioButtons(inputId = "cmbData",
                 label = "Choose a dataset:", 
                 choices = c(
                   "Mosaic" = -1,
                   "Alter Allgemein" = 0,
                   "Alter - Gestorben" = 1,
                   "Alter - Überlebt" = 2,
                   "Alter - Überlebt Erwachsene" = 3,
                   "Alter - Überlebt Kinder" = 4,
                   "Alter - Gestorben Erwachsene" = 5,
                   "Alter - Gestorben Kinder" = 6,
                   "Alter - Sterberate 3. Klasse" = 7,
                   "Alter - Sterberate Erwachsene" = 8,
                   "Geschlecht - Überlebt/Gestorben" = 9,
                   "Geschlecht - Überlebt" = 10,
                   "Geschlecht - Überlebt Erwachsene" = 11,
                   "Geschlecht - Überlebt Kinder" = 12,
                   "Geschlecht - Gestorben Erwachsene" = 13,
                   "Geschlecht - Gestorben Kinder" = 14,
                   "Geschlecht - Mehr Männer als Frauen überlebt" = 15,
                   "Klasse Allgemein" = 16,
                   "Klasse - 3. & Crew" = 17,
                   "Klasse - 3. & Crew M/W" = 18,
                   "Klasse - Gestorben Erwachsene" = 19,
                   "Klasse - Gestorben Kinder" = 20))),
  mainPanel(
    plotOutput("out", height = 800))
)

server <- function(input, output) {
  # Allgemein
  library(ggplot2)
  library(vcd)
  t <- as.data.frame(Titanic)
  adult <- subset(t, Age == 'Adult')
  child <- subset(t, Age == 'Child')
  died <- subset(t, Survived == 'No')
  survived <- subset(t, Survived == 'Yes')
  adult_died <- subset(adult, Survived == 'No')
  adult_survived <- subset(adult, Survived == 'Yes')
  child_died <- subset(child, Survived == 'No')
  child_survived <- subset(child, Survived == 'Yes')
  
  output$out <- renderPlot({
    if (input$cmbData == -1){
      mosaic(Titanic, fit = TRUE)
    } else if (input$cmbData == 0){
      ggplot(t, aes(x = Class, y = Freq)) +
        geom_col(width = 0.5) +
        facet_wrap(~Age) + 
        ggtitle('Alter - Allgemein')
    } else if (input$cmbData == 1) {
      ggplot(died, aes(x = Class, y = Freq)) +
        geom_col(width = 0.5) +
        facet_wrap(~Age) + 
        ggtitle('Alter - Gestorben')
    } else if (input$cmbData == 2) {
      ggplot(survived, aes(x = Class, y = Freq)) +
        geom_col(width = 0.5) +
        facet_wrap(~Age) +
        ggtitle('Alter - Überlebt')
    } else if (input$cmbData == 3) {
      ggplot(adult_survived, aes(x = Class, y = Freq)) +
        geom_col(width = 0.5) +
        ggtitle('Alter - Überlebt (Erwachsene)')
    } else if (input$cmbData == 4) {
      ggplot(child_survived, aes(x = Class, y = Freq)) +
        geom_col(width = 0.5) +
        ggtitle('Alter - Überlebt (Kinder)')
    } else if (input$cmbData == 5) {
      ggplot(adult_died, aes(x = Class, y = Freq)) +
        geom_col(width = 0.5) +
        ggtitle('Alter - Gestorben (Erwachsene)')
    } else if (input$cmbData == 6) {
      ggplot(child_died, aes(x = Class, y = Freq)) +
        geom_col(width = 0.5) +
        ggtitle('Alter - Gestorben (Kinder)')
    } else if (input$cmbData == 7) {
      child_3rd = subset(child, child$Class == '3rd')
      ggplot(child_3rd, aes(x = Survived, y = Freq)) +
        geom_col(width = 0.5) +
        ggtitle('Alter - Überlebt/Gestorben (Kinder, 3. Klasse)')
    } else if (input$cmbData == 8) {
      ggplot(adult, aes(x = Survived, y = Freq)) +
        geom_col(width = 0.5) +
        facet_wrap(~Class) +
        ggtitle('Alter - Überlebt/Gestorben (Erwachsene)')
    } else if (input$cmbData == 9) {
      ggplot(t, aes(x = Survived, y = Freq, fill = Sex)) +
        geom_col(width = 0.5) +
        ggtitle('Geschlecht - Allgemein')
    } else if (input$cmbData == 10) {
      ggplot(survived, aes(x = Sex, y = Freq)) +
        geom_col(width = 0.5) +
        ggtitle('Geschlecht - Überlebt')
    } else if (input$cmbData == 11) {
      ggplot(adult_survived, aes(x = Class, y = Freq, fill= Age)) +
        geom_col(width = 0.5) +
        facet_wrap(~Sex) +
        ggtitle('Geschlecht - Überlebt (Erwachsene)')
    } else if (input$cmbData == 12) {
      ggplot(child_survived, aes(x = Class, y = Freq, fill= Age)) +
        geom_col(width = 0.5) +
        facet_wrap(~Sex) +
        ggtitle('Geschlecht - Überlebt (Kinder)')
    } else if (input$cmbData == 13) {
      ggplot(adult_died, aes(x = Class, y = Freq, fill= Age)) +
        geom_col(width = 0.5) +
        facet_wrap(~Sex) +
        ggtitle('Geschlecht - Gestorben (Erwachsene)')
    } else if (input$cmbData == 14) {
      ggplot(child_died, aes(x = Class, y = Freq, fill= Age)) +
        geom_col(width = 0.5) +
        facet_wrap(~Sex) +
        ggtitle('Geschlecht - Gestorben (Kinder)')
    } else if (input$cmbData == 15) {
      ggplot(survived, aes(x = Sex, y = Freq)) +
        geom_col(width = 0.5) +
        ggtitle('Geschlecht - Überlebt insgesamt')
    } else if (input$cmbData == 16) {
      ggplot(t, aes(x = Class, y = Freq, fill = Age)) +
        geom_col(width = 0.5) +
        facet_wrap(~Survived) +
        ggtitle('Klassen - Allgemein')
    } else if (input$cmbData == 17) {
      newTbl <- aggregate(survived$Freq, by=list(Class=survived$Class), FUN=sum)
      died <- aggregate(died$Freq, by=list(Class=died$Class), FUN=sum)
      newTbl$Lost <- died$x
      newTbl$Total <- newTbl$x + died$x
      newTbl$Survival <- newTbl$x / newTbl$Total
      
      ggplot(newTbl, aes(x = Class, y = Survival)) +
        geom_col(width = 0.5) +
        ggtitle('Klassen - Überlebt (Erwachsene)')
    } else if (input$cmbData == 18) {
      newTbl <- aggregate(survived$Freq, by=list(Class=survived$Class, Sex=survived$Sex), FUN=sum)
      died <- aggregate(died$Freq, by=list(Class=died$Class, Sex=died$Sex), FUN=sum)
      newTbl$Lost <- died$x
      newTbl$Total <- newTbl$x + died$x
      newTbl$Survival <- newTbl$x / newTbl$Total
      
      ggplot(newTbl, aes(x = Class, y = Survival)) +
        geom_col(width = 0.5) +
        facet_wrap(~Sex) +
        ggtitle('Klassen - Überlebt (Erwachsene)')
    } else if (input$cmbData == 19) {
      ggplot(adult_died, aes(x = Class, y = Freq)) +
        geom_col(width = 0.5) +
        ggtitle('Klassen - Gestorben (Erwachsene)')
    } else if (input$cmbData == 20) {
      ggplot(child_died, aes(x = Class, y = Freq)) +
        geom_col(width = 0.5) +
        ggtitle('Klassen - Gestorben (Kinder)')
    }
  })
}

shinyApp(ui = ui, server = server)