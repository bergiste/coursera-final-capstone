library(shiny)
shinyUI(   fluidPage(
    headerPanel("Word Predict App"
    ),
    sidebarPanel(
        h3("Introducton"),
        p("This application predicts the next possible word in a phrase or sentence. To use it, simply type word(s) on the text field on the screen
          and up to 4 possible next words will display in buttons below the field. Click on your intended match to add it to the field."),
        p("This application was created as an academic project for the Capstone Course of the Coursera Data Science Specialization. We used natural language proccessing models, namely, n-grams, Markov model, and Katz's back-off model to produce the predictions.")
        
    ),
    mainPanel(
        h3("What Do You Want to Say?"),
        textInput("inputTxt", "Type in word(s) below:", width = "90%"),
        uiOutput("words"),
        br(),
        wellPanel(
            h4("Technical Details"),
            HTML("<p>Source code is freely available at <a href='https://github.com/bergiste/coursera-final-capstone' target='_blank'>https://github.com/bergiste/coursera-final-capstone</a></p>"),
            HTML("<p>A detailed application architecture presentation available at <a href='http://rpubs.com/bergiste/data-science-project' target='_blank'>http://rpubs.com/bergiste/data-science-project</a></p>"),
            h4("Author:"),
            p("Jose Bergiste")
        )
        
    )
)) 