# Load libraries without warnings and messages
suppressPackageStartupMessages(c(
  library(data.table),
  library(stringr),
))

# Load the prediction model, which also load the data from the n-grams
source("Predict_for_app.R")

# word prediction service
shinyServer(function(input, output) {
  nextWord <- reactive({
    t <- input$text
    tt <- reFreshen(t)
    #wCount <- length(tt)
    nextWord <- predicted_words(tt)
    nextWord <- getNextWord(wCount,tt)})
  
  output$nextWords <- renderPrint(nextWord())
  output$inputWords <- renderText({input$text}, quoted = FALSE)
  
})