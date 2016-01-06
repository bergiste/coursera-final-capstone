library(shiny)
shinyUI(   fluidPage(
    headerPanel("Word Predict App"
    ),
    sidebarPanel(
        h3("Introducton"),
        p("This application predicts the next possible word in a phrase or sentence. To use it, simply type word(s) on the text field to the right of the screen
          and up to 4 possible next words will display in buttons below the field. Click on your intended match to add it to the field."),
        h3("Technical Details"),
        p("The application uses natural language proccessing models, namely, n-grams and Katz's back-off model")
    ),
    mainPanel(
        p("Type a word, or incomplete phrase/sentence:"),
        textInput("inputTxt", "Type in word(s) below:", width = "90%"),
        uiOutput("words"),
        br(),
        wellPanel(
            p("This application was created as an academic project for the Capstone Course of the Coursera Data Science Specialization."),
            p("Source code is freely available at https://github.com/bergiste/coursera-final-capstone"),
            h4("Author:"),
            p("Jose Bergiste")
        )
        
    )
)) 