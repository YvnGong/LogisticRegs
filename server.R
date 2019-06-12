library(shiny)
library("shinydashboard", lib.loc="/usr/lib64/R/library")
library(png)
library(shinyBS)
library(V8)
library(shinyjs)
library(car)
library(discrimARTs)
library(leaflet)
library(raster)
library(DT)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(plotly)
library(data.table)


#bankc for challenge bank

bankc <- 
  read.csv("ChallengeOutput.csv")
bankc = 
  data.frame(lapply(bankc, as.character), stringsAsFactors = FALSE)


# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {

  ##########################Go buttons##################################### 
  observeEvent(input$infoex,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = " Move the sliders to see their effect on the diagnostic plots.",
      type = "info"
    )
  })
  
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "Click on desired square, answer the question, then hit submit and go to next question.",
      type = "info"
    )
  })
  observeEvent(input$go,{
    updateTabItems(session,"tabs","explore")
  })
  
  observeEvent(input$start,{
    updateTabItems(session,"tabs","instruction")
  })
  
  observeEvent(input$begin,{
    updateTabItems(session, "tabs", "qqq")
  })
 ############################Gray out buttons###############################
 
 
  observeEvent(input$start, {
    updateButton(session, "answer", disabled = TRUE)
  })
  
  observeEvent(input$challenge, {
    updateButton(session, "answer", disabled = FALSE)
  })
  
  observeEvent(input$answer, {
    updateButton(session, "answer", disabled=TRUE)
  })
  
  observeEvent(input$begin, {
    updateButton(session, "submit", disabled = TRUE)
  })
  
  
#############################plot outputs#################################
  df<-function(b0, b1, sampleSize){
    intercept <-as.numeric(b0)
    bet <- as.numeric(b1)
    x <- rnorm(as.numeric(sampleSize))
    pr <- exp(x * bet) / (1 + exp(x * bet))
    failures <- rbinom(as.numeric(sampleSize), 1, pr)
    df = data.frame(x,failures)
    return(df)
  }
  
  output$logplot<-renderPlotly({
    df = df(input$b0, input$b1, input$sampleSize)
    theme_set(theme_bw())
    
    p <- ggplot(aes(x=x,y=failures),data = df)+
      geom_smooth(aes(linetype="fitted probability"),method = 'glm', size = 1.5, color="maroon", 
                  method.args=list(family='binomial'), se=FALSE, level=input$ci)+
      geom_ribbon(aes(linetype="confidence interval"),stat="smooth", method="glm", alpha=0.15, 
                  method.args=list(family='binomial'))+
      geom_point()+
      ylab('observed Bernoulli')+
      xlab('explanatory variables')+
      ggtitle("Logistic Regression Model \n")+
      scale_linetype_manual(values=c("fitted probability", "confidence interval"))+
      theme(
      plot.title = element_text(color="black", size=15, face="bold"),
      axis.title.x = element_text(color="black", size = 15),
      axis.title.y = element_text(color="black", size = 15)
    )

    p<-
      ggplotly(p)%>%
      layout(hovermode = 'x', legend = list(x = 0.1, y = 0.5))
  })
  
  # output$citable<-renderTable({
  #   df = df(input$b0, input$b1, input$sampleSize)
  #   logit <- glm(failures ~ x, family=binomial, data=df)
  #   citable<-data.table(confint(logit, level = input$ci))
  #   citable<-cbind(CI=c("Intercept", "x"), citable)
  #   citable
  # })
  
  output$residualPlot<-renderPlot({
    df = df(input$b0, input$b1, input$sampleSize)
    logit <- glm(failures ~ x, family=binomial, data=df)
    if(input$residualType == "pearson"){
      plot(residuals(logit, type="pearson"), type="b", main="Pearson Res - Logit")
    }
    else{
      plot(residuals(logit, type="deviance"), type="b", main="Deviance Res - Logit")  #Residual Deviance
    }
  })
  

  ##### Draw the Hangman Game#####
  
  output$distPlot <- renderUI({
    
    mistake = 0
    ## Background
    if(mistake == 0){
      img(src = "treegame1.jpg", width = 500)
      
    }
    
    ## Head
    else if(mistake == 1 ) {
      img(src = "treegame2.jpg", width = 500)
    }
    
    ## Arms
    else if(mistake == 2) {
      img(src = "treegame3.jpg", width = 500)
    }
    
    ## Body
    else if(mistake == 3 ) {
      img(src = "treegame4.jpg", width = 500)
    }
    
    
    ## Legs
    else if(mistake == 4) {
      img(src = "treegame5.jpg", width = 500)
    }
    
  })
  
  })


