library(shiny)
library(datasets)

business <- read.csv("data/pittsburgh-restaurants.csv", 
                     stringsAsFactors=FALSE)
yelp.clusters <- read.csv("data/just-pittsburgh-clusters.csv", 
                          stringsAsFactors=FALSE)

# Define UI for dataset viewer application
shinyUI(fluidPage(
  
  titlePanel("Which restaurants have you been to in Pittsburgh?"),

  sidebarLayout(
    sidebarPanel(
      selectInput("restaurant1", "Restaurant 1:", 
                  choices = c("", sort(business$name))),
      sliderInput("rating1", "How many stars?", 
                  min=1, max=5, value=3),            
      selectInput("restaurant2", "Restaurant 2:", 
                  choices = c("", sort(business$name))),
      sliderInput("rating2", "How many stars?", 
                  min=1, max=5, value=3),          
      selectInput("restaurant3", "Restaurant 3:", 
                  choices = c("", sort(business$name))),
      sliderInput("rating3", "How many stars?", 
                  min=1, max=5, value=3),            
      submitButton("Recommendation!")
    ),
    
    mainPanel(
      h4("Restaurant Recommendation"),
      hr(),
  fluidRow(column(5, verbatimTextOutput("value")))
    )
  )
))
