library(shiny)
shinyUI(   fluidPage(
    headerPanel(
        h1("Word Predict App")
    ),
    sidebarPanel(
        h3("test"),
        p("description")
    ),
    mainPanel(
        p("Type a word, or incomplete phrase/sentence:"),
        textInput("inputTxt", "Type in word(s) below:"),
        p(span(textOutput("predictions"))),
        uiOutput("word2")
        
    )
)) 