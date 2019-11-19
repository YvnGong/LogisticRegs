library(shiny)
library("shinydashboard", lib.loc="/usr/lib64/R/library")
library(png)
library(shinyBS)
library(V8)
library(shinyjs)
# library(car)
library(discrimARTs)
library(leaflet)
library(raster)
library(DT)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(plotly)
library(data.table)
library(ResourceSelection)
library(Stat2Data)


#bank for question
bank <- read.csv("questionbank.csv")
bank = data.frame(lapply(bank, as.character), stringsAsFactors = FALSE)

source("helpers.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  #Initialized learning  locker connection
  connection <- rlocker::connect(session, list(
    base_url = "https://learning-locker.stat.vmhost.psu.edu/",
    auth = "Basic ZDQ2OTNhZWZhN2Q0ODRhYTU4OTFmOTlhNWE1YzBkMjQxMjFmMGZiZjo4N2IwYzc3Mjc1MzU3MWZkMzc1ZDliY2YzOTNjMGZiNzcxOThiYWU2",
    agent = rlocker::createAgent()
  ))
  
  # Setup demo app and user.
  currentUser <- 
    connection$agent
  
  if(connection$status != 200){
    warning(paste(connection$status, "\nTry checking your auth token.")) 
  }
  
  ##########################Go buttons##################################### 
  observeEvent(input$infoex,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = " Move the sliders to see their effect on the diagnostic plots.",
      type = NULL
    )
  })
  
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "This app explores Simple Logistic Regression with simulated data and real data",
      type = NULL
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
  
  observeEvent(input$goMul,{
    updateTabItems(session,"tabs","Multiple")
  })
  
#####################Processing sign#######################
  observeEvent(input$goButtonMul, {
    # When the button is clicked, wrap the code in a call to `withBusyIndicatorServer()`
    withBusyIndicatorServer("goButtonMul", {
      Sys.sleep(1)
    })
  })
  
  observeEvent(input$goButton, {
    # When the button is clicked, wrap the code in a call to `withBusyIndicatorServer()`
    withBusyIndicatorServer("goButton", {
      Sys.sleep(1)
    })
  })
  
 ############################Gray out buttons###############################
  # observeEvent(input$start, {
  #   updateButton(session, "answer", disabled = TRUE)
  # })
  
  # observeEvent(input$challenge, {
  #   updateButton(session, "answer", disabled = FALSE)
  # })
  
  # observeEvent(input$answer, {
  #   updateButton(session, "answer", disabled=TRUE)
  # })

  
  
#############################plot outputs#################################
  df<-function(b0, b1, sampleSize){
    intercept <-as.numeric(b0)
    bet <- as.numeric(b1)
    x <- rnorm(as.numeric(sampleSize))
    pr <- exp(x * bet) / (1 + exp(x * bet))
    y <- rbinom(as.numeric(sampleSize), 1, pr)
    df = data.frame(x,y)
    return(df)
  }

  ##########common objects
  commonDf<-reactive({
    df(input$b0, input$b1, input$sampleSize)
  })
  
  output$logplot<-renderPlotly({
    input$goButton
    df = isolate(commonDf())
    theme_set(theme_bw())
    p <- ggplot(aes(x=x,y=y),data = df)+
      geom_smooth(aes(linetype="fitted probability"),method = 'glm', size = 1, color="maroon", 
                  method.args=list(family='binomial'), se=FALSE)+
      geom_ribbon(aes(linetype="confidence\n interval"),stat="smooth", method="glm", alpha=0.15, 
                  level=input$ci, method.args=list(family='binomial'))+
      geom_point()+
      ylab('Observed Bernoulli')+
      xlab('explanatory variables')+
      ggtitle("Logistic Regression Model \n")+
      scale_linetype_manual(values=c("fitted probability", "confidence interval"))+
      theme(
        plot.title = element_text(color="black", size=15, face="bold"),
        axis.text = element_text(color="black", size = 12),
        axis.title.x = element_text(color="black", size = 15),
        axis.title.y = element_text(color="black", size = 15)
      )
    
    p<-
      ggplotly(p)%>%
      layout(legend = list(x = 0.7, y = 0.15))
  })
  
  output$residualPlot<-renderPlot({
    input$goButton
    df = isolate(commonDf())
    logit <- glm(y ~ x, family=binomial, data=df)
    if(input$residualType == "pearson"){
      plot(residuals(logit, type="pearson"), type="b", 
           main="Pearson Res- logit", ylab= "Pearson Residual", 
           cex.axis = 1.3, cex.lab = 1.5, 
           cex.main =1.5, pch=16, las=1)
    }
    else{
      plot(residuals(logit, type="deviance"), 
           type="b", main="Deviance Res- logit", ylab= "Deviance Residual", 
           cex.axis = 1.3, cex.lab = 1.5, 
           cex.main =1.5, pch=16,las=1)  
    }
  })
  
  ##### goodness of fit#####
  HLresult<-function(){
    input$goButton
    df = isolate(commonDf())
    mod <- glm (y~x, data=df, family = binomial)
    hl <- hoslem.test(mod$y, fitted(mod))
    return(hl)
  }
  
  output$lemeshowTest<-renderPrint({
    hl<-HLresult()
    hl
  }
  )
  
  output$lemeshowDF<-renderTable({
    hl<-HLresult()
    hs<-data.frame(hl$statistic, hl$parameter, hl$p.value)
    names(hs)<-c('χ2', 'df', 'p-value')
    rownames(hs)<-NULL
    hs
  }, striped = TRUE, width = "100%", align = 'c', hover = TRUE, bordered = TRUE)
  
  output$obsexpDF<-renderTable({
    hl<-HLresult()
    hob<-data.frame(cbind(hl$expected, hl$observed))
    hob<-setDT(hob, keep.rownames = TRUE)[]
    names(hob)<-c("interval","number of 0s expected", "number of 1s expected", 
                  "number of 0s in group", "number of 1s in group")
    hob
  }, striped = TRUE, width = "100%", align = 'c', hover = TRUE, bordered = TRUE, rownames = TRUE)
  
  output$obsexp<-renderPrint({
    hl<-HLresult()
    cbind(hl$expected, hl$observed)
  })
  
  

  #####Multiple Graph
  df2<-function(b0, b1, b2, sampleSize){
    x1 = rnorm(sampleSize)           
    x2 = rnorm(sampleSize)
    z = b0+b1*x1+b2*x2        # linear combination with a bias
    pr = 1/(1+exp(-z))         # pass through an inv-logit function
    y = rbinom(sampleSize,1,pr)      # bernoulli response variable
    df = data.frame(y=y,x1=x1,x2=x2)
    return(df)
  }
  
  ##########common objects
  commonDf2<-reactive({
    df2(input$b02, input$b12, input$b2, input$sampleSize2)
  })
  
  #####read in datatable############
  data(MedGPA)
  data("Titanic")
  data("Leukemia")
  ###empircal logit plot############
  output$empericalLogitPlot<-
    renderPlot({
      if (input$datatable == 'MedGPA'){
        if (input$MedYvar == 'Acceptance'){
          if(input$MedXvar == 'GPA'){
            emplogitplot1(Acceptance~GPA, ngroups=input$ngroups, out=TRUE, data=MedGPA, main="Empirical Logit Plot")
          }
          else if(input$MedXvar == 'MCAT'){
            emplogitplot1(Acceptance~MCAT, ngroups=input$ngroups, out=TRUE, data=MedGPA, main="Empirical Logit Plot")
          }
          else if(input$MedXvar == 'BCPM'){
            emplogitplot1(Acceptance~BCPM, ngroups=input$ngroups, out=TRUE, data=MedGPA, main="Empirical Logit Plot")
          }
        }
      }
      else if (input$datatable == 'Titanic'){
        if (input$TitanicYvar == 'Survived'){
          if(input$TitanicXvar == 'Age'){
            emplogitplot1(Survived~Age,ngroups=input$ngroups, out=TRUE, data=Titanic, main="Empirical Logit Plot")
          }
      }}
      else if (input$datatable == 'Leukemia'){
          if (input$LeukemiaYvar == 'Status'){
            if(input$LeukemiaXvar == 'Blasts'){
              emplogitplot1(Status~Blasts, ngroups=input$ngroups, out=TRUE, data=Leukemia, main="Empirical Logit Plot")
            }
            else if(input$LeukemiaXvar == 'Age'){
              emplogitplot1(Status~Age, ngroups=input$ngroups, out=TRUE, data=Leukemia, main="Empirical Logit Plot")
            }
            else if(input$LeukemiaXvar == 'Infil(perceptage of infiltrate)'){
              emplogitplot1(Status~Infil, ngroups=input$ngroups, out=TRUE, data=Leukemia, main="Empirical Logit Plot")
            }
          }
      }
        
    })
  
  output$mulPlot<-renderPlotly({
    input$goButtonMul
    df = isolate(commonDf2())
    theme_set(theme_bw())
    p <- ggplot(aes(x=x1,y=y),data = df)+
      geom_smooth(aes(linetype="X1's fitted\n probability"),method = 'glm', size = 1, color="maroon", 
                  method.args=list(family='binomial'), se=FALSE)+
      geom_smooth(aes(x=x2,y=y, linetype="X2's fitted\n probability "), data=df, method = 'glm', size = 1, color="lightblue",
                  method.args=list(family='binomial'), se=FALSE)+
      geom_ribbon(aes(linetype="confidence\n interval"),stat="smooth", method="glm", alpha=0.15, 
                  level=input$ci2, method.args=list(family='binomial'))+
      geom_point(color="maroon")+
      # geom_smooth(aes(x=x2,y=y, linetype="fitted probability "), data=df, method = 'glm', size = 1, color="lightblue",
      #             method.args=list(family='binomial'), se=FALSE)+
      geom_ribbon(aes(x=x2,y=y, linetype="confidence\n interval"), data=df,stat="smooth", method="glm", alpha=0.15,
                  level=input$ci2, method.args=list(family='binomial'))+
      geom_point(aes(x=x2,y=y), data=df, color="lightblue", alpha=0.4)+
      ylab('Observed Bernoulli')+
      xlab('explanatory variables')+
      ggtitle("Multiple Logistic Regression \n")+
      scale_linetype_manual(values=c("X1's fitted\n probability","X2's fitted\n probability" ,"confidence\n interval"))+
      theme(
        plot.title = element_text(color="black", size=15, face="bold"),
        axis.text = element_text(color="black", size = 12),
        axis.title.x = element_text(color="black", size = 15),
        axis.title.y = element_text(color="black", size = 15)
      )
    
    p<-
      ggplotly(p)%>%
      layout(legend = list("left"))
  })
  
  output$multix<-renderPlot({
    input$goButtonMul
    df<-isolate(commonDf2())
    p<-glm(y~x1+x2,data=df,family="binomial")
    par(mfrow=c(1,3))
    # plot(p,which=c(4,2,1), add.smooth = getOption("add.smooth"), 
    #      las=1, cex.caption=1.5, cex.axis=1.3, cex.lab=1.7)
    plot(p,which=1, add.smooth = getOption("add.smooth"), 
         las=1, cex.caption=1.5, cex.axis=1.3, cex.lab=1.7)
    legend("topleft", legend="fitted line", col="red", lty=1:2, cex=1.5, box.lty=0)
    #second and third plot
    plot(p,which=c(4,2), las=1, cex.caption=1.5, cex.axis=1.3, cex.lab=1.7)
  })
  
  #####Multiple Goodness of fit
  HLresult2<-function(){
    input$goButtonMul
    df<-isolate(commonDf2())
    mod<-glm(y~x1+x2,data=df,family="binomial")
    hl<-hoslem.test(mod$y, fitted(mod), g=10)
    return(hl)
  }
  
  output$lemeshowDF2<-renderTable({
    hl<-HLresult2()
    hs<-data.frame(hl$statistic, hl$parameter, hl$p.value)
    names(hs)<-c('χ2', 'df', 'p-value')
    rownames(hs)<-NULL
    hs
  }, striped = TRUE, width = "100%", align = 'c', hover = TRUE, bordered = TRUE)
  
  output$obsexpDF2<-renderTable({
    hl<-HLresult2()
    hob<-data.frame(cbind(hl$expected, hl$observed))
    hob<-setDT(hob, keep.rownames = TRUE)[]
    names(hob)<-c("interval","number of 0s expected", "number of 1s expected", 
                  "number of 0s in group", "number of 1s in group")
    hob
  }, striped = TRUE, width = "100%", align = 'c', hover = TRUE, bordered = TRUE, rownames = TRUE)
  

  
  ######TIMER########
  timer<-reactiveVal(1)
  active<-reactiveVal(FALSE)

  # observer that invalidates every second. If timer is active, decrease by one.
  observe({
    invalidateLater(1000, session)
    isolate({
      if(active())
      {
        timer(timer()-1)
        if(timer()<1)
        {
          active(FALSE)
          
          randnum<-sample(1:6, 1)
          newvalue<-score()+isolate(randnum)
          score(newvalue)

          if(as.numeric(score())>=20){
            output$dice<-renderUI({
              Sys.sleep(1)
              img(src = "congrats.png", width = '60%')
            })
            updateButton(session, "nextq", disabled = TRUE)
            updateButton(session,"submit", disabled = TRUE)
            updateButton(session, "restart", disabled = FALSE)
          }

          else{
            updateButton(session, "nextq", disabled = FALSE)
            if(randnum == 1){
              output$dice<-renderUI({
                Sys.sleep(1)
                img(src = "21.png",width = '30%')
              })
            }
            else if(randnum == 2){
              output$dice<-renderUI({
                Sys.sleep(1)
                img(src = "22.png",width = '30%')
              })
            }
            else if(randnum == 3){
              output$dice<-renderUI({
                Sys.sleep(1)
                img(src = "23.png",width = '30%')
              })
            }
            else if(randnum == 4){
              output$dice<-renderUI({
                Sys.sleep(1)
                img(src = "24.png",width = '30%')
              })
            }
            else if(randnum == 5){
              output$dice<-renderUI({
                Sys.sleep(1)
                img(src = "25.png",width = '30%')
              })
            }
            else if(randnum == 6){
              output$dice<-renderUI({
                Sys.sleep(1)
                img(src = "26.png",width = '30%')
              })
            }
            
          }
          
          
        }
      }
    })
  })
  
  #####Rlocker observe Event##
  # Gets current page address from the current session
  getCurrentAddress <- function(session){
    return(paste0(
      session$clientData$url_protocol, "//",
      session$clientData$url_hostname,
      session$clientData$url_pathname, ":",
      session$clientData$url_port,
      session$clientData$url_search
    ))
  }
  
  # Pulls corresponding answer values from question bank and returns its text
  
  getResponseText <- function(index, answer){
    if(answer == 'A'){
      key = 3
    } else if(answer == 'B'){
      key = 4
    } else {
      key = 5
    }
    return(bank[index, key])
  }
  
  observeEvent(input$ci,{
    interacted_statement <- rlocker::createStatement(
      list(
        verb = list(
          display = "interacted"
        ),
        object = list(
          id = paste0(getCurrentAddress(session)),
          name = 'confidence interval',
          description = 'single logistic Regression'
        ),
        result = list(
          success = NULL,
          response = input$ci
          # response = paste(paste('SampleSize:',input$sampleSize, "beta0:",
          #                        input$b0, "beta1:",input$b1, "confidence interval:", input$ci))
        )
      )
    )
    
    # Store statement in locker and return status
    status <- rlocker::store(session, interacted_statement)
    
    # print(interacted_statement) # remove me
    # print(status) # remove me
  })
  
  #####Buttons Handle#######
  observeEvent(input$nextq,{
    value$answerbox <- value$index
    index_list$list=index_list$list[-1]   
    value$index<-index_list$list[1]
    value$answerbox<-value$index
    

    updateButton(session, "nextq", disabled = TRUE)
    updateButton(session,"submit", disabled = FALSE)
    
    
    if(value$index %in% c(11:16)){
      updateSelectInput(session,"answer", "pick an answer from below", c("","A", "B"))
    }
    else{
      updateSelectInput(session,"answer", "pick an answer from below", c("","A", "B", "C"))
    }
    
    output$mark <- renderUI({
      img(src = NULL,width = 30)
    })
  })
  
  
  observeEvent(input$submit,{
    updateButton(session,"submit", disabled = TRUE)
    answer<-isolate(input$answer)
    if (any(answer == ans[value$index,1])){
      output$dice<-renderUI({
        img(src = "newdice1.gif", width = '30%')
      })
      active(TRUE)
    }
    else{
      if(length(index_list$list) == 1){
        updateButton(session, "nextq", disabled = TRUE)
        updateButton(session,"submit", disabled = TRUE)
      }
      else{
        updateButton(session,"submit", disabled = TRUE)
        updateButton(session, "nextq", disabled = FALSE)
      }
    }

    output$mark <- renderUI({
      if (any(answer == ans[value$index,1])){
        img(src = "correct.png",width = 30)
      }
      else{
        ig<-img(src = "incorrect.png",width = 30)
        w<-paste("You picked", answer, ", The correct answer is", ans[value$index, 1])
        HTML(paste(ig, w), sep = ' ')
      }
    })
  })
  
  observeEvent(input$submit,{
    answer<-isolate(input$answer)
    statement <- rlocker::createStatement(
      list(
        verb = list(
          display = "answered"
        ),
        object = list(
          id = paste0(getCurrentAddress(session), "#", value$index),
          name = paste('Question', value$index),
          description = bank[value$index, 2]
        ),
        result = list(
          success = any(answer == ans[value$index,1]),
          response = paste(getResponseText(value$index, answer))
        )
      )
    )
    
    # Store statement in locker and return status
    status <- rlocker::store(session, statement)
    
    # print(statement) # remove me
    # print(status) # remove me
  })
  
  observeEvent(input$restart,{
    updateButton(session, "submit", disabled = FALSE)
    updateButton(session,"restart",disable =FALSE)
    updateSelectInput(session,"answer", "pick an answer from below", c("","A", "B", "C"))
    index_list$list<-c(index_list$list,sample(2:14,13,replace=FALSE))
    value$index <- 1
    value$answerbox = value$index
    ans <- as.matrix(bank[1:16,6])
    index_list<-reactiveValues(list=sample(1:16,10,replace=FALSE))
    output$mark <- renderUI({
      img(src = NULL,width = 30)
    })
  })
  
  ####mark at the beginning
  output$mark <- renderUI({
    img(src = NULL,width = 30)
  })
  
  
  #####Question Part########
  value <- reactiveValues(index =  1, mistake = 0,correct = 0)
  ans <- as.matrix(bank[1:16,6])
  index_list<-reactiveValues(list=sample(1:16,10,replace=FALSE))
  
  output$question <- renderUI({
    h4(bank[value$index, 2])
  })
  
  output$options <- renderUI({
    if(value$index == 11){
      str1 <- paste("A.", bank[value$index, 3])
      str2 <- paste("B.", bank[value$index, 4])
      HTML(paste(str1, str2, sep = '<br/>'))
    }
    else if(value$index %in% c(12:16)){
      Apic<-
        img(src = bank[value$index, 3], 
            width = "50%")
      Bpic<-
        img(src = bank[value$index, 4], 
            width = "50%")
      str1 <- paste("A.", Apic)
      str2 <- paste("B.", Bpic)
      HTML(paste(str1, str2, sep = '<br/>'))
    }
    
    else if(value$index %in% c(1:10)){
      str1 <- paste("A.", bank[value$index, 3])
      str2 <- paste("B.", bank[value$index, 4])
      str3 <- paste("C.", bank[value$index, 5])
      HTML(paste(str1, str2, str3, sep = '<br/>'))
    }
    else{
      h4("reach the end")
    }
  })
  
  output$gameplot1<-renderUI(
    img(src = bank[value$index, 3], 
        width = "100%", height = "107%", style = "text-align: center")
  )
  
  ##### Draw the Hangman Game#####
  
  score <- reactiveVal(0)  
  
  output$dice<-renderUI({
    img(src = "21.png",width = '30%')
  })
  
  output$gamescore<-renderUI({
    h2("Your cumulative score is", score())
  })
  
  observeEvent(input$restart,{
    newvalue<-score()-score()
    score(newvalue)
    output$dice<-renderUI({
      img(src = "21.png", width = '30%')
    })
    # updateButton(session, "roll", disabled = FALSE)
    # updateButton(session, "stop", disabled = TRUE)
    # updateButton(session, "restart", disabled = TRUE)
  })
  
  })


