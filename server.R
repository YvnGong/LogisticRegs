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
  output$logplot<-renderPlot({
    intercept <-as.numeric(input$b0)
    bet <- as.numeric(input$b1)
    x <- rnorm(as.numeric(input$sampleSize))
    pr <- exp(x * bet) / (1 + exp(x * bet))
    failures <- rbinom(as.numeric(input$sampleSize), 1, pr)
    df = data.frame(x,failures)
    ggplot(aes(x=x,y=failures),data = df)+geom_smooth(method = 'glm', method.args=list(family='binomial'))+
      geom_point()+
      ylab('y')+
      ggtitle("Logistic Regression Model")
  })
  
  output$residualPlot<-renderPlot({
    intercept <-as.numeric(input$b0)
    bet <- as.numeric(input$b1)
    x <- rnorm(as.numeric(input$sampleSize))
    pr <- exp(x * bet) / (1 + exp(x * bet))
    failures <- rbinom(as.numeric(input$sampleSize), 1, pr)
    df = data.frame(x,failures)
    logit <- glm(failures ~ x, family=binomial, data=df)
    if(input$residualType == "pearson"){
      plot(residuals(logit, type="pearson"), type="b", main="Pearson Res - Logit")
    }
    else{
      plot(residuals(logit, type="deviance"), type="b", main="Deviance Res - Logit")  #Residual Deviance
    }
  })
  
#  #  observeEvent(input$go | input$submitD, {
#  #    output$plots=
#  #      renderPlot({
#  #       
#  #      if(input$model== "Model 1"){
#  #        nonex<- rnorm(input$n,3,input$x1v)
#  #        nonex2<- rnorm(input$n,3,input$x2v)
#  #        e<- rnorm(input$n,0,.2**2)
#  #        y<- (rnorm(input$n,3,input$yv))**2
#  #        
#  #        logx<- log(nonex)
#  #        logx2<-log(nonex2)
#  #        logy<- log(y)
#  #        
#  #        expx<- sqrt(nonex)
#  #        expx2<- sqrt(nonex2)
#  #        expy<- sqrt(y)
#  #      } else if (input$model == "Model 2"){
#  #        nonex<- rnorm(input$n,3,input$x1v)**2
#  #        nonex2<- rnorm(input$n,3,input$x2v)**2
#  #        e<- rnorm(input$n,0,.2**2)
#  #        y<- (rnorm(input$n,3,input$yv))
#  #        
#  #        logx<- log(nonex)
#  #        logx2<-log(nonex2)
#  #        logy<- log(y)
#  #        
#  #        expx<- sqrt(nonex)
#  #        expx2<- sqrt(nonex2)
#  #        expy<- sqrt(y)
#  #      }else{
#  #        nonex<- rnorm(input$n,3,input$x1v)**2
#  #        nonex2<- rnorm(input$n,3,input$x2v)**2
#  #        e<- rnorm(input$n,0,.2**2)
#  #        y<- (rnorm(input$n,3,input$yv))**2
#  #        
#  #        logx<- log(nonex)
#  #        logx2<-log(nonex2)
#  #        logy<- log(y)
#  #        
#  #        expx<- sqrt(nonex)
#  #        expx2<- sqrt(nonex2)
#  #        expy<- sqrt(y)
#  #      }
#  #        
#  #        for (i in c(input$x)){ for(j in c(input$x2)) {for( k in  c(input$y)){
#  #          if (any(i == "none")& any(j== "none")&any(k== "none")){
#  #            par(mfrow=c(2,2))
#  #            model= lm(y~nonex+nonex2+e)
#  #            plot(model)
#  #          } 
#  #          else if (any(i == "none")& any(j== "logx2")&any(k== "none")) {
#  #            par(mfrow=c(2,2))
#  #            model2= lm(y~nonex+logx2+e)
#  #            plot(model2)
#  #          }
#  #          
#  #          else if(any(i == "none")& any(j== "sqrtx2")&any(k== "none")){
#  #            par(mfrow=c(2,2))
#  #            model3= lm(y~nonex+expx2+e)
#  #            plot(model3)
#  #          }
#  #         
#  #           else if(any(i == "logx")& any(j== "none")&any(k== "none")){
#  #            par(mfrow=c(2,2))
#  #            model4= lm(y~logx+nonex2+e)
#  #            plot(model4)
#  #          }
#  #          
#  #          else if(any(i == "logx")& any(j== "logx2")&any(k== "none")){
#  #            par(mfrow=c(2,2))
#  #            model5= lm(y~logx+logx2+e)
#  #            plot(model5)
#  #          }
#  #          
#  #          else if(any(i == "logx")& any(j== "sqrtx2")&any(k== "none")){
#  #            par(mfrow=c(2,2))
#  #            model6= lm(y~logx+expx2+e)
#  #            plot(model6)
#  #          }
#  #          
#  #          else if(any(i == "sqrtx")& any(j== "none")&any(k== "none")){
#  #            par(mfrow=c(2,2))
#  #            model7= lm(y~expx+nonex2+e)
#  #            plot(model7)
#  #          }
#  #          
#  #          else if(any(i == "sqrtx")& any(j== "logx2")&any(k== "none")){
#  #            par(mfrow=c(2,2))
#  #            model8= lm(y~expx+logx2+e)
#  #            plot(model8)
#  #          }
#  #          
#  #          else if(any(i == "sqrtx")& any(j== "sqrtx2")&any(k== "none")){
#  #            par(mfrow=c(2,2))
#  #            model9= lm(y~expx+expx2+e)
#  #            plot(model9)
#  #          }
#  #          
#  #          else if(any(i == "none")& any(j== "nonex2")&any(k== "logy")){
#  #            par(mfrow=c(2,2))
#  #            model10= lm(logy~nonex+nonex2+e)
#  #            plot(model10)
#  #          }
#  #          
#  #          else if(any(i == "none")& any(j== "logx2")&any(k== "logy")){
#  #            par(mfrow=c(2,2))
#  #            model11= lm(logy~nonex+logx2+e)
#  #            plot(model11)
#  #          }
#  #          
#  #          else if(any(i == "none")& any(j== "sqrtx2")&any(k== "logy")){
#  #            par(mfrow=c(2,2))
#  #            model12= lm(logy~nonex+expx2+e)
#  #            plot(model12)
#  #          }
#  #          
#  #          else if(any(i == "logx")& any(j== "nonex2")&any(k== "logy")){
#  #            par(mfrow=c(2,2))
#  #            model13= lm(logy~logx+nonex2+e)
#  #            plot(model13)
#  #          }
#  #          
#  #          else if(any(i == "logx")& any(j== "logx2")&any(k== "logy")){
#  #            par(mfrow=c(2,2))
#  #            model14= lm(logy~logx+logx2+e)
#  #            plot(model14)
#  #          }
#  #          
#  #          else if(any(i == "logx")& any(j== "sqrtx2")&any(k== "logy")){
#  #            par(mfrow=c(2,2))
#  #            model15= lm(logy~logx+expx2+e)
#  #            plot(model15)
#  #          }
#  #          
#  #          else if(any(i == "sqrtx")& any(j== "nonex2")&any(k== "logy")){
#  #            par(mfrow=c(2,2))
#  #            model16= lm(logy~expx+nonex2+e)
#  #            plot(model16)
#  #          }
#  #          
#  #          else if(any(i == "sqrtx")& any(j== "logx2")&any(k== "logy")){
#  #            par(mfrow=c(2,2))
#  #            model17= lm(logy~expx+logx2+e)
#  #            plot(model17)
#  #          }
#  #          
#  #          else if(any(i == "sqrtx")& any(j== "sqrtx2")&any(k== "logy")){
#  #            par(mfrow=c(2,2))
#  #            model18= lm(logy~expx+expx2+e)
#  #            plot(model18)
#  #          }
#  #          
#  #          else if(any(i == "none")& any(j== "nonex2")&any(k== "sqrty")){
#  #            par(mfrow=c(2,2))
#  #            model19= lm(expy~nonex+nonex2+e)
#  #            plot(model19)
#  #          }
#  #          
#  #          else if(any(i == "none")& any(j== "logx2")&any(k== "sqrty")){
#  #            par(mfrow=c(2,2))
#  #            model20= lm(expy~nonex+logx2+e)
#  #            plot(model20)
#  #          }
#  #          
#  #          else if(any(i == "none")& any(j== "sqrtx2")&any(k== "sqrty")){
#  #            par(mfrow=c(2,2))
#  #            model21= lm(expy~nonex+expx2+e)
#  #            plot(model21)
#  #          }
#  #          
#  #          else if(any(i == "logx")& any(j== "none")&any(k== "sqrty")){
#  #            par(mfrow=c(2,2))
#  #            model22= lm(expy~logx+nonex2+e)
#  #            plot(model22)
#  #          }
#  #          
#  #          else if(any(i == "logx")& any(j== "logx2")&any(k== "sqrty")){
#  #            par(mfrow=c(2,2))
#  #            model23= lm(expy~logx+logx2+e)
#  #            plot(model23)
#  #          }
#  #          
#  #          else if(any(i == "logx")& any(j== "sqrtx2")&any(k== "sqrty")){
#  #            par(mfrow=c(2,2))
#  #            model24= lm(expy~logx+expx2+e)
#  #            plot(model24)
#  #          }
#  #          
#  #          else if(any(i == "sqrtx")& any(j== "none")&any(k== "sqrty")){
#  #            par(mfrow=c(2,2))
#  #            model25= lm(expy~expx+nonex2+e)
#  #            plot(model25)
#  #          }
#  #          
#  #          else if(any(i == "sqrtx")& any(j== "logx2")&any(k== "sqrty")){
#  #            par(mfrow=c(2,2))
#  #            model26= lm(expy~expx+logx2+e)
#  #            plot(model26)
#  #          }
#  #          
#  #          else {
#  #            par(mfrow=c(2,2))
#  #            model27= lm(expy~expx+expx2+e)
#  #            plot(model27)
#  #          }
#  #             }}} 
#  #      })
#  #      
#  #        
#  # ####end of observeeventsubmit     
#  #  })
#   
#   # index <- reactiveValues(index=7)
#   # 
#   # ######################### This section is to output a new activity ############################
#   # 
#   #  observeEvent(input$challenge | input$go, {
#   #   index$index <- sample(1:20,1, replace=FALSE, prob=NULL)
#   #   
#   #   output$challenges <- renderUI ({
#   #     if (index$index == 1){
#   #       h4(bankc[1,2])
#   #     } 
#   #     else if (index$index == 2){
#   #       h4(bankc[2,2])
#   #     }
#   #     else if (index$index == 3){
#   #       h4(bankc[3,2])
#   #     }
#   #     else if (index$index == 4){
#   #       h4(bankc[4,2])
#   #     }
#   #     else if (index$index == 5){
#   #       h4(bankc[5,2])
#   #     }
#   #     else if (index$index == 6){
#   #       h4(bankc[6,2])
#   #     }
#   #     else if (index$index == 7){
#   #       h4(bankc[7,2])
#   #     }
#   #     else if (index$index == 8){
#   #       h4(bankc[8,2])
#   #     }
#   #     else if (index$index == 9){
#   #       h4(bankc[9,2])
#   #     }
#   #     else if (index$index == 10){
#   #       h4(bankc[10,2])
#   #     }
#   #     else if (index$index == 11){
#   #       h4(bankc[11,2])
#   #     }
#   #     else if (index$index == 12){
#   #       h4(bankc[12,2])
#   #     }
#   #     else if (index$index == 13){
#   #       h4(bankc[13,2])
#   #     }
#   #     else if (index$index == 14){
#   #       h4(bankc[14,2])
#   #     }
#   #     else if (index$index == 15){
#   #       h4(bankc[15,2])
#   #     }
#   #     else if (index$index == 16){
#   #       h4(bankc[16,2])
#   #     }
#   #     else if (index$index == 17){
#   #       h4(bankc[17,2])
#   #     }
#   #     else if (index$index == 18){
#   #       h4(bankc[18,2])
#   #     }
#   #     else if (index$index == 19){
#   #       h4(bankc[19,2])
#   #     }
#   #     else if (index$index == 20){
#   #       h4(bankc[20,2])
#   #     }
#   #     
#   #   }
#   #   )
#     
#      })
#   
#   ########################## Output for answer box when nothing is in the box #############################
#   
#   observeEvent(input$challenge,{
#     output$answers <- renderText("Please hit the view feedback button for feedback")
#   }) 
#   
#   observeEvent(input$go,{
#     output$answers <- renderText("Please hit the view feedback button for feedback")
#   })  
#    
#   ###################### output of the answers ################################
#   
#   observeEvent(input$answer,{
#    
#   
#     output$answers <- renderUI ({
#       if (index$index == 1){
#         h4(bankc[1,3])
#       } 
#       else if (index$index == 2){
#         h4(bankc[2,3])
#       }
#       else if (index$index == 3){
#         h4(bankc[3,3])
#       }
#       else if (index$index == 4){
#         h4(bankc[4,3])
#       }
#       else if (index$index == 5){
#         h4(bankc[5,3])
#       }
#       else if (index$index == 6){
#         h4(bankc[6,3])
#       }
#       else if (index$index == 7){
#         h4(bankc[7,3])
#       }
#       else if (index$index == 8){
#         h4(bankc[8,3])
#       }
#       else if (index$index == 9){
#         h4(bankc[9,3])
#       }
#       else if (index$index == 10){
#         h4(bankc[10,3])
#       }
#       else if (index$index == 11){
#         h4(bankc[11,3])
#       }
#       else if (index$index == 12){
#         h4(bankc[12,3])
#       }
#       else if (index$index == 13){
#         h4(bankc[13,3])
#       }
#       else if (index$index == 14){
#         h4(bankc[14,3])
#       }
#       else if (index$index == 15){
#         h4(bankc[15,3])
#       }
#       else if (index$index == 16){
#         h4(bankc[16,3])
#       }
#       else if (index$index == 17){
#         h4(bankc[17,3])
#       }
#       else if (index$index == 18){
#         h4(bankc[18,3])
#       }
#       else if (index$index == 19){
#         h4(bankc[19,3])
#       }
#       else if (index$index == 20){
#         h4(bankc[20,3])
#       } 
#       
#     }
#     )
#    }
#  )
#  
# 
#   
#   
# #######TICTAC
#   
#   #### question bank ####
#   bank <- read.csv('bank.csv', stringsAsFactors = FALSE)
#   bank <- bank[,c(1,2,4:7,9,16)]
#   bank[17:18,2] <- gsub('11',"\\\\",gsub("([\\\\])",'1',bank[17:18,2]))
#   Qs <- nrow(bank)
#   
#   
#   ######## MY SERVER CODE ##########
#   
#  
#   X.icon <- makeIcon(iconUrl = 'X.PNG', iconWidth= 90)
#   O.icon <- makeIcon(iconUrl= 'O.PNG',iconWidth= 105)
#   value <- matrix(rep(-3,9),3,3)
#   values <- list(value) # 
#   container <- c() # contains right or wrong answers after submit button is pressed 
#   XsAndOs <- list()
#   resolved <- c(rep(FALSE, Qs))
#   
#   #lists <- reactiveValues(container <- c(),values <- list(),value <- NULL)
#   
#   sr1.1=Polygon(cbind(c(0.5,0.5,3.5,3.5), c(2.5,1.5,1.5,2.5))) # inner, middle horizontal lines
#   sr1.2=Polygon(cbind(c(0.5,0.5,3.5,3.5), c(2.5,3.5,3.5,2.5))) # inner, horizontal lines
#   sr1.3=Polygon(cbind(c(0.5,0.5,3.5,3.5), c(0.5,1.5,1.5,0.5))) # inner, horizontal lines
#   sr2=Polygon(cbind(c(1.5,1.5,2.5,2.5), c(3.5,0.5,0.5,3.5)))
#   sr3=Polygon(cbind(c(0.5,0.5,0.5,3.5), c(0.52,3.47,0.5,0.5))) #outer 
#   sr4=Polygon(cbind(c(3.5,3.5,0.5,3.5), c(0.52,3.47,3.5,3.5))) #square
#   srs1.1=Polygons(list(sr1.1), 's1.1')
#   srs1.2=Polygons(list(sr1.2), 's1.2')
#   srs1.3=Polygons(list(sr1.3), 's1.3')
#   srs2=Polygons(list(sr2), 's2')
#   srs3=Polygons(list(sr3), 's3')
#   srs4=Polygons(list(sr4), 's4')
#   #srs1.2,srs1.3
#   spp = SpatialPolygons(list(srs1.1,srs2,srs3,srs4), 1:4)
#   #spp = SpatialPolygons(list(srs3,srs4), 1:2)
#   
#   r <- raster(xmn = 0.5, xmx = 3.5, ymn = 0.5, ymx = 3.5, nrows = 3, ncols = 3)
#   values(r) <- matrix(1:9, nrow(r), ncol(r), byrow = TRUE)
#   crs(r) <- CRS("+init=epsg:4326")
#   new.board <- leaflet(options = leafletOptions(zoomControl = FALSE, doubleClickZoom = FALSE, minZoom = 7, maxZoom = 7)) %>%  addPolygons(data=spp) %>% addRasterImage(r, colors="Set3")
#   # %>% addTiles()
#   line <- function(){
#     #recall, in the value matrix, zero's represent O's, or wrong answers
#     for(i in 1:3){
#       total.1 <- 0 ; total.2 <- 0
#       for(j in 1:3){
#         total.1 <- total.1 + value[i, j]
#         total.2 <- total.2 + value[j, i]
#       }
#       if(total.1==0 | total.2==0 | total.1==3 | total.2==3){
#         break
#       }
#     }
#     total.3 <- value[1, 1] + value[2, 2] + value[3, 3]
#     total.4 <- value[1, 3] + value[2, 2] + value[3, 1]
#     
#     #if the game has been won:
#     if(total.1==0 | total.2==0 | total.3==0 | total.4==0 | total.1==3 | total.2==3 | total.3==3 | total.4==3){
#       #place.na[!is.na(place.na)] <<- NA
#       if(total.1==0 | total.2==0 | total.3==0 | total.4==0){
#         warning('LOSE')
#         return("You lost try again!")  #title(sub=list("You Are a Loser !", col="darkblue", font=2, cex=2.5), line=2)
#       }else{
#         warning('WIN')
#         return("You Win! Game Over!")  #title(sub=list("You Win ! Game Over !", col="red", font=2, cex=2.5), line=2)
#       }
#     }
#     
#     
#     
#     
#     #if the previous is true (if the game is over) then this will fire through as well
#     #this will also fire through if the board is full, regardless of whether the previous if() has executed
#     #i dont know why these two statements should both be here
#    
#      if(length(which(value!=-3))==9){
#       if(total.1==0 | total.2==0 | total.3==0 | total.4==0 | total.1==3 | total.2==3 | total.3==3 | total.4==3){
#         #if(total.1==0 | total.2==0 | total.3==0 | total.4==0){
#         #  title(sub=list("You Are a Loser !", col="darkblue", font=2, cex=2.5), line=2)
#         #}else{
#         #  title(sub=list("You Win ! Game Over !", col="orange", font=2, cex=2.5), line=2)
#         #}
#       }else{
#         warning('RESTART')
#         return("Draw ! Please try again !")  #title(sub=list("Draw ! Please try again !", col="blue", font=2, cex=2.5), line=2)
#       }
#     }
#     return(NULL)
#   }
#   
#   
#   
#   v <- reactiveValues(doPlot = FALSE)
#   observeEvent(input$image_click, priority = 7, {
#     validate(need(is.null(game()), label='Game is over'))
#     
#     #input$image_click will return coordinates when clicked, not a logical value
#     if(!is.null(input$image_click)){
#       v$doPlot <- TRUE
#     }
#   })
#   
#   #object that contains the coordinates returned from clicking the image
#   coords <- eventReactive(input$image_click, {
#     validate(need(is.null(game()), label='Game is over'))
#     
#     validate(need(!is.null(input$image_click), 'need to click image'))
#     input$image_click
#   })
#   
#   
#   #### keep track of clicks on A: the plot, B: the submit button, C: the next button ####
#   
#   
#   clicks <- reactiveValues(A=0,B=0,C=0)
#   observeEvent(input$image_click, priority = 6, {
#     validate(need(is.null(game()), label='Game is over'))
#     mouse.at <- coords()
#     output$warning <- renderText('')
#     #we dont return an error for out of bounds clicks
#     if(!(mouse.at[[1]] > 3.5 | mouse.at[[1]] < 0.5 | mouse.at[[2]] > 3.5 | mouse.at[[2]] < 0.5)){
#       if(!is.null(input$image_click) & clicks$A==clicks$B){
#         #we allow the user to switch his tic tac toe selection (before a corresponding answer) here
#         clicks$A <- (clicks$A+1)
#         warning(clicks$A, "A")
#       }
#     }else{
#       if(clicks$A == clicks$B){
#         output$warning <- renderText('Please click a valid square')
#       }
#     }
#   })
#   observeEvent(input$submit, priority = 6, {
#     
#     #make sure the player selected an answer when he pressed submit
#     num <- as.character(numbers$question[length(numbers$question)])
#     #     this has been hard coded to correspond with the number of questions in the question bank
#     ans <- if(num==1){input$'1'}else if(num==2){input$'2'}else if(num==3){input$'3'}else if(num==4){input$'4'}else if(num==5){input$'5'}else if(num==6){input$'6'}else if(num==7){input$'7'}else if(num==8){input$'8'}else if(num==9){input$'9'}else if(num==10){input$'10'}else if(num==11){input$'11'}else if(num==12){input$'12'}else if(num==13){input$'13'}else if(num==14){input$'14'}else if(num==15){input$'15'}else if(num==16){input$'16'}else if(num==17){input$'17'}else if(num==18){input$'18'}else if(num==19){input$'19'}else if(num==20){input$'20'}else if(num==21){input$'21'}else if(num==22){input$'22'}else if(num==23){input$'23'}else if(num==24){input$'24'}else if(num==25){input$'25'}else if(num==26){input$'26'}else if(num==27){input$'27'}else if(num==28){input$'28'}
#     validate(need((ans %in% c("A", "B", "C", "D", seq(0:1000))), label='please select one of the multiple choice responses'))
#     warning(ans, 'tit')
#     if(!is.null(input$submit)){clicks$B <- (clicks$B+1)}
#     warning(clicks$B, 'b')
#   })
#   observeEvent(input$nextButton, priority = 6, {
#     if(!is.null(input$nextButton)){clicks$C <- (clicks$C+1)}
#     warning(clicks$C, 'c')
#   })
#   
#   
#   #called in the next observer below 
#   ID <- reactive({
#     validate(need(is.null(game()), label='Game is over'))
#     
#     validate(
#       need(!is.null(input$image_click), 'click image') # because this will always come first
#     )
#     if(clicks$A>=clicks$B){
#       if(clicks$A>clicks$B){
#         "choice"
#       }else if(clicks$A==clicks$B){
#         #if the first few clicks are not valid squares, we must make sure theres no errors in the next observe handler below
#         if(clicks$A==0){
#           "choice"
#         }else{
#           "answer"
#         }
#       }
#     }
#   })
#   
#   observe({
#     warning(XsAndOs)
#   })
#   
#   game <- reactiveVal(NULL)
#  
#    #contains the board as it changes throughout the game
#   #will store markers in XsAndOs list
#   #as this event fires, only one marker will be added to the XsAndOs list each time
#   #the first if statement will store an empty marker, when a box is chosen. Then,
#   #then the second will overwrite it with an X or an O, when a question is answered
#   
#   observeEvent({input$submit
#     input$image_click}, priority = 0,{
#       validate(need(is.null(game()), label='Game is over'))
#       
#       mouse.at <- coords()
#       ID <- ID()
#       
#       #if the user enters an invalid click after he already entered his choice on the board, it will not affect the outcome of an answer submission
#       
#       if(clicks$A > clicks$B | clicks$A == 0){
#         validate(
#           need(!(mouse.at[[1]] > 3.5 | mouse.at[[1]] < 0.5 | mouse.at[[2]] > 3.5 | mouse.at[[2]] < 0.5), label='please enter a valid click')
#         )
#       }else if(clicks$A == clicks$B & ID == "answer" & XsAndOs[[ifelse(length(XsAndOs)==0,'',length(XsAndOs))]][3] %in% c(0,1) ){
#         #or, if the user enters an invalid click before entering a valid click, it will not crash the app
#         if(length(XsAndOs) == clicks$A){
#           validate(
#             need(!(mouse.at[[1]] > 3.5 | mouse.at[[1]] < 0.5 | mouse.at[[2]] > 3.5 | mouse.at[[2]] < 0.5), label='please enter a valid click')
#           )
#         }
#       }
#       
#      
#      warning(ID)
#       
#       #statement that checks the ID return from either of the input triggering events. choice or answer
#       
#       if(ID=="choice"){
#         mouse.at[[1]] <- round(mouse.at[[1]])
#         mouse.at[[2]] <- round(mouse.at[[2]])
#         x<<-mouse.at[[1]]
#         y<<-mouse.at[[2]]
#         
#         #     should this be length(XsAndOs) ?  6/14/18
#         
#         XsAndOs[[(length(container)+1)]] <<- c(mouse.at[[2]],(mouse.at[[1]]-.3),NULL)
#         leafletProxy("image", session) %>% addMarkers(lng=XsAndOs[[length(XsAndOs)]][1], lat=XsAndOs[[length(XsAndOs)]][2], layerId = "dummy")
#         
#       }else if(ID=="answer"){
#         value <<- values[[length(values)]]
#         if(container[length(container)]==0){
#           value[x, y] <<- 0
#         }else if(container[length(container)]==1){
#           value[x, y] <<- 1
#         }
#         values[[1]] <<- matrix(-3,3,3)
#         values[[(length(values)+1)]] <<- value
#         temp <<- which((values[[length(values)]] - values[[ (length(values)-1) ]])!=0)
#         values[[length(values)]][temp] <<- container[length(container)]
#         
#         #plotting part, this will overwrite what previously passed through the first if statement in this observe handler
#         
#         if(temp<4){
#           XsAndOs[[(length(XsAndOs))]][3] <<- ifelse(matrix(values[[length(values)]],3,3)[[temp]]==0, 0, 1)
#         }else if(temp<7){ 
#           XsAndOs[[(length(XsAndOs))]][3] <<- ifelse(matrix(values[[length(values)]],3,3)[[temp]]==0, 0, 1)
#         }else if(temp<10){
#           XsAndOs[[(length(XsAndOs))]][3] <<- ifelse(matrix(values[[length(values)]],3,3)[[temp]]==0, 0, 1)
#         }else{
#           stop("OH NO")
#         }
#         
#         leafletProxy("image", session)  %>% removeMarker(layerId = "dummy")
#         warning(XsAndOs[[length(XsAndOs)]][3])
#         if(!(XsAndOs[[length(XsAndOs)]][3] %in% c(0,1))){stop('VERY BAD')}
#         if(XsAndOs[[length(XsAndOs)]][3]==1){
#           leafletProxy("image", session)  %>% addMarkers(y,x+.56,icon = X.icon)
#         }else{
#           leafletProxy("image", session)  %>% addMarkers(y,x+.56,icon = O.icon)
#         }
#         
#         temp <- line()
#         game(temp) #reactiveVal syntax
#         message(temp)
#       }
#     }
#   )  
#   
#   #### alerts user of status of game (if it is over) ####
#   #return from line() function
#   output$gameMessage <- renderText({
#     validate(need(!is.null(game), label = 'Game is over'))
#     
#     game <- game()
#     game
#   })
#   
#   
#   ####render image of tic tac toe board####
#   output$image <- renderLeaflet({
#     new.board
#     #  out = out %>% addMarkers(lng=temp[[1]],lat=temp[[2]], icon=tryCatch(ifelse(temp[[3]]==1, X.icon, O.icon), error=function(e)NULL))
#   })
#   
#   #### go button ####
#   observeEvent(input$go, priority=1, {
#     updateTabItems(session, "tabs", "qqq")
#   })
#   observe({
#     validate(
#       need(is.null(input$image_click), message='')
#     )
#     output$directions <- renderText({"Begin by selecting the square on the tic-tac-toe grid you would like."})
#   })
#   
#   ####start over; new game####
#   observeEvent(input$reset, priority = 5,{
#     if(!is.null(game())){
#       game(NULL)
#     }
#     leafletProxy("image", session) %>% clearMarkers()
#     
#     value <<- matrix(rep(-3,9),3,3)
#     values <<- list(value)
#     container <<- c()
#     XsAndOs <<- list()
#     #need to use scoping operator because i didnt create the (above) as reactive objects
#     clicks$A <- 0
#     clicks$B <- 0
#     clicks$C <- 0
#     
#     
#     #create arbitrary cut off of 9 questions (i would say its reasonable) keep in mind the quesiton bank has 18 questions
#     #if more than 9 questions have been answered, then we reset the question bank so that all questions can be drawn from
#    
#      if(length(which(answers() %in% c("correct","incorrect")))>9){
#       numbers$question <- c()
#     }else{
#       #put the incorrectly answered questions back into play
#       if("incorrect" %in% unique(answers())){
#         if("correct" %in% unique(answers())){
#           #numbers$question <- numbers$question[which((answers()=="correct"))] #we assign the taken questions to be only the correctly answered ones
#           numbers$question <- which(answers()=="correct") #we assign the taken questions to be only the correctly answered ones
#         }else{
#           numbers$question <- c() #since no answers were correct, all questions are in play
#         }
#       }
#       
#       #get rid of most recent question (but if it was answered correctly, it still will not be included)
#       numbers$question <- numbers$question[-length(numbers$question)]
#     }
#     
#     #resample to get new random question for when the game is restarted (when image is clicked)
#     
#     space<-c(1:Qs)
#     numbers$question[(length(numbers$question)+1)] <- sample(space[-tryCatch(if(numbers$question){numbers$question}, error=function(e) 29)], 1)
#     warning(numbers$question)
#     
#     updateButton(session, 'nextButton',  disabled = TRUE, style= 'danger')
#     output$directions <- renderText({"Begin by selecting the square on the plot you would like"})
#   })
#  
#    #temporary place holders, will be used as a logical pass to the conditional panel containing the renderUI output
#   
#   observeEvent(input$image_click, {
#     validate(need(is.null(game()), label='Game is over'))
#     
#     output$temp <- renderText({'1'})
#   })
#   observeEvent(input$reset, priority = 8, {
#     output$temp <- renderText({'2'})
#   })
#   
#   
#   #### enact game over mode ####
#   observeEvent(input$submit, priority = -1, {
#     validate(need(!is.null(game()), label='Game is over'))
#     
#     #make sure the player selected an answer when he pressed submit
#     
#     num <- as.character(numbers$question[length(numbers$question)])
#     
#     #     this has been hard coded to correspond with the number of questions in the question bank
#     
#     ans <- if(num==1){input$'1'}else if(num==2){input$'2'}else if(num==3){input$'3'}else if(num==4){input$'4'}else if(num==5){input$'5'}else if(num==6){input$'6'}else if(num==7){input$'7'}else if(num==8){input$'8'}else if(num==9){input$'9'}else if(num==10){input$'10'}else if(num==11){input$'11'}else if(num==12){input$'12'}else if(num==13){input$'13'}else if(num==14){input$'14'}else if(num==15){input$'15'}else if(num==16){input$'16'}else if(num==17){input$'17'}else if(num==18){input$'18'}else if(num==19){input$'19'}else if(num==20){input$'20'}else if(num==21){input$'21'}else if(num==22){input$'22'}else if(num==23){input$'23'}else if(num==24){input$'24'}else if(num==25){input$'25'}else if(num==26){input$'26'}else if(num==27){input$'27'}else if(num==28){input$'28'}
#     validate(need((ans %in% c("A", "B", "C", "D", seq(0:1000))), label='please select one of the multiple choice responses'))
#     
#     updateButton(session, 'nextButton', 
#                   disabled = TRUE)
#     updateButton(session, 'submit', 
#                   disabled = TRUE)
#     output$directions <- renderText({"If you would like to play again, press 'Start new game'!"})
#   })
#   
#   
#   ######## QUESTIONS #########
#   
#   
#   
#   
#   #### random question ####
#   
#   numbers <- reactiveValues(question = c())
#   observeEvent(input$image_click, once=TRUE, priority = 9, {
#     validate(need(is.null(game()), label='Game is over'))
#     
#     numbers$question[1] <- sample(1:Qs, 1)
#     updateButton(session, 'nextButton',  disabled = TRUE)
#     output$directions <- renderText({"Now answer the question and press submit"})
#   })
#   observeEvent(input$nextButton, priority = 4, {
#     space <- c(1:Qs)
#     numbers$question[(length(numbers$question)+1)] <- sample(space[-tryCatch(numbers$question, error=function(e) 29)], 1)
#     updateButton(session, 'nextButton',  disabled = TRUE)
#     output$directions <- renderText({"Now select another square on the board"})
#     if(clicks$A == (clicks$C + 1)){
#       updateButton(session, 'submit',  disabled = FALSE)
#       output$directions <- renderText({"Now answer the question and press submit"})
#     }
#   })
#   
#   
#   ####output random question####
#   output$CurrentQuestion <- renderUI({
#     warning(numbers$question)
#     num <- as.character(numbers$question[length(numbers$question)])
#     warning(num)
#     temp <- NULL
#     
#     # THIS WAS NOT AS DYNAMIC
#     #if(num %in% c(1,2)){
#     #  numericInput(inputId = (num), bank[num, 2], min=0,max=100, val=0)
#     #}else if(num %in% c(3,4,7,10:12)){ 
#     #  radioButtons(inputId = (num), label=(bank[num, 2]), choiceNames=c(bank[num, 3], bank[num, 4]), choiceValues = c("A", "B"),  selected = character(0))
#     #}else if(num %in% c(5,6,8,9,13:16)){
#     #  radioButtons(inputId = (num), label=ifelse(is.null(temp), bank[num, 2], temp), choiceNames=c(bank[num, 3], bank[num, 4], bank[num, 5], bank[num, 6]), choiceValues = c("A", "B", "C", "D"), selected = character(0))
#     #}else if(num %in% c(17,18)){
#     #  withMathJax(
#     #    h4(sprintf(
#     #      bank[num, 2]
#     #    ))
#     #  )
#     #  #temp <- ""
#     # }
#     
#     
#     
#     if(num %in% c(17,18)){
#       #this mathjax call is hard coded for specific questions in the CSV
#       withMathJax(
#         h4(sprintf(
#           bank[num, 2]
#         ))
#       )
#     }else if(!(FALSE %in% unique(as.vector(bank[num,3:6]=='')))){
#       numericInput(inputId = (num), bank[num, 2], min=0,max=1000, val=0)
#     }else if(!(FALSE %in% unique(as.vector(bank[num,5:6]=='')))){
#       radioButtons(inputId = (num), label=(bank[num, 2]), choiceNames=c(bank[num, 3], bank[num, 4]), choiceValues = c("A", "B"),  selected = character(0))
#     }else if(bank[num,6]==''){
#       radioButtons(inputId = (num), label=(bank[num, 2]), choiceNames=c(bank[num, 3], bank[num, 4], bank[num, 5]), choiceValues = c("A", "B", "C"), selected = character(0))
#     }else{
#       radioButtons(inputId = (num), label=(bank[num, 2]), choiceNames=c(bank[num, 3], bank[num, 4], bank[num, 5], bank[num, 6]), choiceValues = c("A", "B", "C", "D"), selected = character(0))
#     }
#   })
#   
#   #hard coded observer for specific questions in the csv
#   output$CurrentQuestion.extra <- renderUI({
#     num <- as.character(numbers$question[length(numbers$question)])
#     if(num == 3){
#       img(src="CIQ3.png",height = 150,width = 500,align = "middle")
#     }else if(num == 4){
#       img(src="CIQ4.png",height = 150,width = 400,align = "middle")
#     }
#     else if(num == 19){
#       img(src="CIQ19.png",height = 150,width = 400,align = "middle")
#     }
#     else if(num == 20){
#       img(src="CIQ20.png",height = 150,width = 400,align = "middle")
#     }
#     else if(num == 21){
#       img(src="CIQ21.png",height = 150,width = 400,align = "middle")
#     }
#     else if(num == 22){
#       img(src="CIQ22.png",height = 150,width = 400,align = "middle")
#     }
#     else if(num == 23){
#       img(src="CIQ3.png",height = 150,width = 400,align = "middle")
#     }
#     else if(num == 24){
#       img(src="CIQ19.png",height = 150,width = 400,align = "middle")
#     }
#     else if(num == 25){
#       img(src="CIQ19.png",height = 150,width = 400,align = "middle")
#     }
#     else if(num == 26){
#       img(src="CIQ22.png",height = 150,width = 400,align = "middle")
#     }
#     else if(num == 27){
#       img(src="CIQ19.png",height = 150,width = 400,align = "middle")
#     }
#     else if(num == 28){
#       img(src="CIQ22.png",height = 150,width = 400,align = "middle")
#     }
#     else if(num == 17){
#       radioButtons(inputId = (num), label='', choiceNames=c(bank[num, 3], bank[num, 4], bank[num, 5]), choiceValues = c("A", "B", "C"), selected = character(0))
#     }else if(num == 18){
#       radioButtons(inputId = (num), label='', choiceNames=c(bank[num, 3], bank[num, 4], bank[num, 5], bank[num, 6]), choiceValues = c("A", "B", "C", "D"), selected = character(0))
#     }
#   })  
#   
#   
#   ####logical flow of answering questions; some extra code to deal with button disabling sequence####
#   observeEvent(input$submit, {
#     #make sure the player selected an answer when he pressed submit
#     num <- as.character(numbers$question[length(numbers$question)])
#     #     this has been hard coded to correspond with the number of questions in the question bank
#     ans <- if(num==1){input$'1'}else if(num==2){input$'2'}else if(num==3){input$'3'}else if(num==4){input$'4'}else if(num==5){input$'5'}else if(num==6){input$'6'}else if(num==7){input$'7'}else if(num==8){input$'8'}else if(num==9){input$'9'}else if(num==10){input$'10'}else if(num==11){input$'11'}else if(num==12){input$'12'}else if(num==13){input$'13'}else if(num==14){input$'14'}else if(num==15){input$'15'}else if(num==16){input$'16'}else if(num==17){input$'17'}else if(num==18){input$'18'}else if(num==19){input$'19'}else if(num==20){input$'20'}else if(num==21){input$'21'}else if(num==22){input$'22'}else if(num==23){input$'23'}else if(num==24){input$'24'}else if(num==25){input$'25'}else if(num==26){input$'26'}else if(num==27){input$'27'}else if(num==28){input$'28'}
#     validate(need((ans %in% c("A", "B", "C", "D", seq(0:1000))), label='please select one of the multiple choice responses'))
#     
#     updateButton(session, 'submit', disabled = TRUE)
#     
#     updateButton(session, 'nextButton',disabled = FALSE)
#     
#     output$directions <- renderText({"Now press the next button"})
#   })
#   observeEvent(input$image_click, {
#     validate(need(is.null(game()), label='Game is over'))
#     
#     if(!(clicks$A > (clicks$C + 1))){
#       updateButton(session, 'submit', disabled = FALSE)
#       output$directions <- renderText({"Now answer the question and press submit"})
#     }
#   })
#   
#   
#   ####checks answer####
#   answers <- reactiveVal(c(rep('', Qs)))
#   observeEvent(input$submit, {
#     #make sure the player selected an answer when he pressed submit
#     num <- as.character(numbers$question[length(numbers$question)])
#     #     this has been hard coded to correspond with the number of questions in the question bank
#     ans <- if(num==1){input$'1'}else if(num==2){input$'2'}else if(num==3){input$'3'}else if(num==4){input$'4'}else if(num==5){input$'5'}else if(num==6){input$'6'}else if(num==7){input$'7'}else if(num==8){input$'8'}else if(num==9){input$'9'}else if(num==10){input$'10'}else if(num==11){input$'11'}else if(num==12){input$'12'}else if(num==13){input$'13'}else if(num==14){input$'14'}else if(num==15){input$'15'}else if(num==16){input$'16'}else if(num==17){input$'17'}else if(num==18){input$'18'}else if(num==19){input$'19'}else if(num==20){input$'20'}else if(num==21){input$'21'}else if(num==22){input$'22'}else if(num==23){input$'23'}else if(num==24){input$'24'}else if(num==25){input$'25'}else if(num==26){input$'26'}else if(num==27){input$'27'}else if(num==28){input$'28'}
#     validate(need((ans %in% c("A", "B", "C", "D", seq(0:1000))), label='please select one of the multiple choice responses'))
#     
#     temp <- answers()
#     num <- as.character(numbers$question[length(numbers$question)])
#     #this has been hard coded to correspond with the number of questions in the question bank
#     ans <- if(num==1){input$'1'}else if(num==2){input$'2'}else if(num==3){input$'3'}else if(num==4){input$'4'}else if(num==5){input$'5'}else if(num==6){input$'6'}else if(num==7){input$'7'}else if(num==8){input$'8'}else if(num==9){input$'9'}else if(num==10){input$'10'}else if(num==11){input$'11'}else if(num==12){input$'12'}else if(num==13){input$'13'}else if(num==14){input$'14'}else if(num==15){input$'15'}else if(num==16){input$'16'}else if(num==17){input$'17'}else if(num==18){input$'18'}else if(num==19){input$'19'}else if(num==20){input$'20'}else if(num==21){input$'21'}else if(num==22){input$'22'}else if(num==23){input$'23'}else if(num==24){input$'24'}else if(num==25){input$'25'}else if(num==26){input$'26'}else if(num==27){input$'27'}else if(num==28){input$'28'}
#     if(ans == bank[num, 8]){
#       temp2 <- "correct"
#     }else{
#       temp2 <- "incorrect"
#     }
#     temp[num] <- temp2
#     answers(temp)
#     container[(length(container)+1)] <<- ifelse(temp[num] == "correct", 1, 0)
#   })
#   
#   
#   
 })
#  
# 
# 


