library(shiny)
library(shinydashboard)
library(maps)
library(dplyr)
library(leaflet)
library(ggplot2)
library(tidyverse)
library(DT)
library(plotly)
library(corrplot)
library(caret)
library(stargazer)
library(shinyWidgets)
source("split_train_test1.R")
source("plsda.fit.R")
source("predict.plsda.R")
source("graphs.R")
source('dummies.R')
dd <- iris

shinyServer(function(input, output, session) {
  
  
  
  data <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$qt
        )
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
    
  })
 
  
  output$contents <- renderTable({
    data()
  })
  
  
  output$xvariable <- renderUI({
    
    req(data())
    xa<-colnames(data())
    selectInput(inputId = "xvar",
                label = "Select X variable",
                choices = xa,
                multiple=TRUE)
   
    
  })
  output$yvariable <- renderUI({
    req(data())
    
    ya<-colnames(data())
   
    selectInput(inputId = "yvar",
                label = "Select Y variable",
                choices = ya,
                multiple=FALSE)
    
  })
  
  
  
  
  
  
  
  
  
  
  ###########################################################
 

  InputDataset <- reactive({
    iris
  })
  
  
  InputDataset_model <- reactive({
    req(data(),input$xvar,input$yvar)
   
   
    dt <-data()
   
  })
  newData=eventReactive(input$Slider1,{
    req(data(),input$xvar,input$yvar)
    df=split_train_test(data(),(input$Slider1/100))
  })
  
  #creation train and test
  train=reactive({
    train=newData()$Train
    return(train)
  })
  
  test=reactive({
    test=newData()$Test
    return(test)
  })
  

  output$Summ <-
    renderPrint(
      stargazer(
        data(),
        type = "text",
        title = "Descriptive statistics",
        digits = 1,
        out = "table1.txt"
      )
    )
  output$Summ_old <- renderPrint(summary(data()))
  output$structure <- renderPrint(str(data()))
  

  

  
  
  output$cntTrain <-
    renderText(paste("Train Data:", dim(train())[1], "records"))
  output$cntTest <-
    renderText(paste("Test Data:", dim(test())[1], "records"))
  
  output$Data <- renderDT(data())
  
  
  cormat <- reactive({
    round(cor(InputDataset()), 1)
  })
  output$Corr <-
    renderPlot(corrplot(
      cormat(),
      type = "lower",
      order = "hclust",
      method = "number"
    ))
  
  
  #Code section for Pls Regression-----------------------------------------------------------------------------
  
  #call function FIT
  formul <- reactive({
    as.formula(paste(input$yvar,"~",".",sep=" "))
  })
  
  resFit=eventReactive(input$fit,{
   req(data(),train(),input$xvar,input$yvar)
    
   
    if(is.null(input$xvar)){
      newtrain=train()
    }else{
      
      newtrain=train()[,c(input$xvar,input$yvar)]
     
      
    }
    res=plsda.fit(formula=formul(),data=newtrain,ncomp=input$ncomp,center = input$center,reduce = input$reduce)
    sum=summary.pls(res)
    return(res)
  })
  
  

  output$Fit=renderTable({
    
    resFit()$classement
   
         },rownames=TRUE) 
  

  #call function Predict
  resPred=eventReactive(input$pd,{
    req(resFit(),test(),input$xvar,input$yvar)
    if(is.null(input$xvar)){
      
      y=which(colnames(test())==input$yvar) #remove  y 
      newtest=test()[,-y]
    }else{
      newtest=test()[,input$xvar] 
    }
    respred=predict.plsda(resFit(),newtest)
    return(respred)
    
  })
  
  
  #Result Predict
  output$Pred=renderTable({
    
    resPred()
    
  }) 
  
  
  #Plots individuals_plot
  ind_plot <- eventReactive(input$ip,{
    graph=individuals_plot(resFit(),Axe1=input$comp1,Axe2=input$comp2)
  
  })
  #show graph 
  output$Comp <- renderPlotly({
    ind_plot()
    
  })
  
  
  #Plots Selection of components
  scree_plot <- eventReactive(input$ip,{
    graph1=plsda_scree_plot(resFit())
    
  })
  #show graph 
  output$scree <- renderPlotly({
    scree_plot()
    
  })
  
  
})


