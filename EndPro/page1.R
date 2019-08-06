library(shiny)
library(caret)
library(kernlab)
library(dplyr)

ui <- fluidPage(
  titlePanel("Breast Cancer"),
  
  #html
  sidebarLayout(
    sidebarPanel(
      helpText(h4("Please enter the following information in the text box.")),
      
      numericInput("age", "AGE","",min = 1,max = 100),
      numericInput("bmi", "BMI","",min = 1,max = 100,step = 0.01),
      numericInput("glucose","Glucose","",min = 0)
      
    ),
    mainPanel(
      textOutput("value")
   
    )
  ),
  submitButton("Prediction",width = '32.3%'),
  
  #style
  tags$style("#value {font-size:40px;}")
  
)

server <- function(input, output) {
  
  state<-reactiveValues()
  err<-reactiveValues()
  
  observe({
    if(is.na(input$age || input$bmi || input$glucose)){
      renderText({"aoishdailk"})
    }
    else{
      temp <- matrix(c(input$age,input$bmi,input$glucose),ncol=3,byrow=FALSE)
      colnames(temp) <- c('Age','BMI','Glucose')
      
      svm_Model <- readRDS('data/model_svm.rds')
      
      pred <- predict(svm_Model, temp)
      
      state$pre <- ifelse(pred == "Benign","Absence of Breast Cancer Disease.","Presence of Breast Cancer Disease.")
    }
  })
  
  output$value <- renderText({state$pre})
  
  
}

shinyApp(ui, server)