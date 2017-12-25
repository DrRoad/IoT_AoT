library(shiny)
library(shinythemes)
library(dplyr)
library(devtools)
library(plotly)
library(ggplot2)
library(ggthemes)
library(reshape)
library(reshape2)
library(rsconnect)
library(tidyverse)
library(gridExtra)
library(readr)

df <- read.csv("./data_P20A0.csv", header=TRUE, stringsAsFactors = FALSE)
variable <- c("x21", "x20", "x96","x71","x4","x49")
df <- df[c("t", variable)]


# UI
ui <-  fluidPage(
  titlePanel("IoT sensor data"),
  sidebarLayout(
    sidebarPanel(
#  sliderInput("t", "time", min="auto", max="auto"),
#  sliderInput("value", "value of sensor data", min="auto", max="auto"),
  selectInput("variable", "variable", choices = variable, selected = "x21", multiple = TRUE)
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("plot",
               fluidRow(
                 plotOutput("plot", width = "600px"),
                 verbatimTextOutput("summary"))
      )),
    tabPanel("Summary",  verbatimTextOutput("summary1"))
  )
))

server <- function(input, output){
        
        d <- reactive({
          subset(df,
                 variable==input$variable
                 )
          })
        
        output$plot <- renderPlot({
        dd <- d()
        dd <- reshape2::melt(dd, id ="t")
        g1 <- ggplot(dd, aes(x=t, y=value, colour=variable)) + geom_line() + geom_smooth() 
        g2 <- ggplot(dd, aes(x=as.factor(t), fill=variable, colour=variable)) + geom_density(alpha=0.1) 
        g3 <- ggplot(dd, aes(x=variable, y=value)) + geom_boxplot() 
        grid.arrange(g1,g2,g3)
        })

    output$summary <- renderPrint({
      dd <- d()
      summary(dd)})
}

shinyApp(ui = ui, server = server)