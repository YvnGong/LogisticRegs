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
library(ResourceSelection)

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
  
  output$mulPlot<-renderPlotly({
    input$goButtonMul
    df = isolate(commonDf2())
    theme_set(theme_bw())
    p <- ggplot(aes(x=x1,y=y),data = df)+
      geom_smooth(aes(linetype="X1's fitted probability"),method = 'glm', size = 1, color="maroon", 
                  method.args=list(family='binomial'), se=FALSE)+
      geom_smooth(aes(x=x2,y=y, linetype="X2's fitted probability "), data=df, method = 'glm', size = 1, color="lightblue",
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
      scale_linetype_manual(values=c("X1's fitted probability","X2's fitted probability" ,"confidence interval"))+
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
  

  ##### Draw the Hangman Game#####
  score <- reactiveVal(0)  
  
  output$dice<-renderUI({
    img(src = "first.png",width = '60%')
  })
  
  output$gamescore<-renderUI({
    h2("Your cumulative score is", score())
  })
  
  observeEvent(input$restart,{
    newvalue<-score()-score()
    score(newvalue)
    output$dice<-renderUI({
      img(src = "first.png", width = '60%')
    })
    updateButton(session, "roll", disabled = FALSE)
    updateButton(session, "stop", disabled = TRUE)
    updateButton(session, "restart", disabled = TRUE)
  })
  
  observeEvent(input$roll,{
    output$dice<-renderUI({
      img(src = "rolling15x.gif", width = '60%')
    })
  })
  
  observeEvent(input$roll,{
    updateButton(session, "roll", disabled = TRUE)
    updateButton(session, "stop", disabled = FALSE)
  })
  
  observeEvent(input$stop,{
    updateButton(session, "stop", disabled = TRUE)
    updateButton(session, "roll", disabled = FALSE)
    updateButton(session, "restart", disabled = FALSE)
  })
  
  observeEvent(input$stop,{
    randnum<-sample(1:6, 1)
    newvalue<-score()+isolate(randnum)
    score(newvalue)
    
    if(as.numeric(score())>=50){
      output$dice<-renderUI({
        Sys.sleep(1)
        img(src = "congrats.jpg", width = '60%')
      })
      updateButton(session, "roll", disabled = TRUE)
      updateButton(session, "stop", disabled = TRUE)
      updateButton(session, "restart", disabled = FALSE)
    }
    
    else{
      if(randnum == 1){
        output$dice<-renderUI({
          Sys.sleep(1)
          img(src = "one.png",width = '60%')
        })
      }
      else if(randnum == 2){
        output$dice<-renderUI({
          Sys.sleep(1)
          img(src = "two.png",width = '60%')
        })
      }
      else if(randnum == 3){
        output$dice<-renderUI({
          Sys.sleep(1)
          img(src = "three.png",width = '60%')
        })
      }
      else if(randnum == 4){
        output$dice<-renderUI({
          Sys.sleep(1)
          img(src = "four.png",width = '60%')
        })
      }
      else if(randnum == 5){
        output$dice<-renderUI({
          Sys.sleep(1)
          img(src = "five.png",width = '60%')
        })
      }
      else if(randnum == 6){
        output$dice<-renderUI({
          Sys.sleep(1)
          img(src = "six.png",width = '60%')
        })
      }
    }
    
  })
  
  })


