ui <- fluidPage(
  # App title ----
  titlePanel(em("Text prediction app")),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      h5(em("Type three words to give information to the app to predict the next possible words")),
      
      # Input: Slider for the number of bins ----
      
      h5(em("The best predictions will be made using two or three words from the user")),
      textInput("user_input", h4("Enter your words"), value = ""),
      br(),
      #img(src = "coursera.png", height = 50, width = 90),
      #img(src = "johnhopkins.png", height = 50, width = 90),
      p("Coursera site ",
        a("Link", href = "https://www.coursera.org/specializations/jhu-data-science")),
      br(),
      p("For more information about the app and the code go to ",
        a("GitHub", href = "https://github.com/maxinegueruela/Coursera_Capstone")),),
      
    
    # Main panel for displaying outputs ----
    mainPanel(
      h3("Suggested next words:"),
      h3(em(span(textOutput("npredictedWords"), style="color:green")))
      )),
    
    
  )

  
