business <- read.csv("data/pittsburgh-restaurants.csv", 
                     stringsAsFactors=FALSE)
yelp.clusters <- read.csv("data/just-pittsburgh-clusters.csv", 
                          stringsAsFactors=FALSE)
source("helpers.R")

library(shiny)
library(datasets)

# Define server logic 
shinyServer(function(input, output) {
	selectedData <- reactive({
		business[, c(input$xcol, input$ycol)]
	})

	output$value <- renderPrint({recommendation(c(input$restaurant1, 
                                input$restaurant2, input$restaurant3), 
                               c(input$rating1, input$rating2, input$rating3))})
})
