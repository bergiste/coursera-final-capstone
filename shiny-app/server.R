library(shiny)
source("~/ACADEMICS/datascience/Final Capstone/model.R")
shinyServer(
    function(input, output){
        prediction <- eventReactive(input$goButton,{
            nextWordPredictor(input$inputTxt)
        })

        output$predictions <- renderPrint({prediction()})
    
})