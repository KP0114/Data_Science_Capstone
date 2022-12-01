library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  theme = bslib::bs_theme(bootswatch = "superhero"),
  # Application title
  titlePanel("Next Word Predictor"),
  h6("Prediction may take some time"),
  
  # Sidebar with a slider input for number of n gram
  sidebarLayout(
    sidebarPanel(
      
      sliderInput(
        inputId =  "Ngram", 
        label = "Select N for Ngram:", 
        min = 1,
        max = 20,
        value = 3,
        step = 1
      ),
      textInput("inputString", "Enter a partial sentence here",value = "Type here"),
      submitButton("Submit", icon("refresh"))
      
    ),
    mainPanel(
      h2("Predicted Next Word"),
      strong("Sentence Input:"),
      tags$style(type='text/css', '#text1 {background-color: rgba(0,0,0,0); color: white;}'), 
      textOutput('text1'),
      br(),
      strong("Sentences with Next Word Candidates:"),
      textOutput("prediction"),
      br(),
      strong("Note:"),
      tags$style(type='text/css', '#text2 {background-color: rgba(0,0,0,0); color: white;}'),
      textOutput('text2')
    )
  )
))

