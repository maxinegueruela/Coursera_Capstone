# Load the prediction model, which also load the data from the n-grams
# Load libraries without warnings and messages
suppressPackageStartupMessages(c(
  library(data.table),
  library(stringr),
  library(tm)))
  #library(NLP),
  #library(tidyr),
  #library(stringi),
  #library(dplyr)))
  #library(tidytext),
  #


#the next explanation of why use the "source" code outside "server" comes from https://shiny.rstudio.com/tutorial/written-tutorial/lesson5/
#Source scripts, load libraries, and read data sets at the beginning of app.R outside of the server function. Shiny will only run this code once, which is all you need to set your server up to run the R expressions contained in server.
source("./Predict_for_app_s_15_v2.R")

server <- function(input, output) {
  
  output$npredictedWords <- renderText({
    predicted_words(input$user_input)
  })}

  
 
