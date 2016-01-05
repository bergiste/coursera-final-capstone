library(shiny)

source("~/ACADEMICS/datascience/Final Capstone/model.R")

shinyServer(
    function(input, output){
        prediction <- reactive({
            nextWordPredictor(input$inputTxt)
        })

        output$predictions <- renderText({prediction()})
        
        #output$word2 <- renderUI(renderText("prediction()"))
    
})