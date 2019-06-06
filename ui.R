library(shiny)
library("shinydashboard", lib.loc="/usr/lib64/R/library")
library(png)
library(shinyBS)
library(V8)
library(shinyjs)

library(discrimARTs)
library(leaflet)
library(raster)
library(DT)

library(RColorBrewer)
library(car)
library(rgdal)
library(shinyWidgets)

library(rlocker)
#smiles

#This app will be used to explore and play around with the assumptions and diagnostics of regression


shinyUI <- dashboardPage(
                         dashboardHeader(title = "Regression Assumptions and Diagnostics",
                                         titleWidth = 200),
                         #adding prereq pages and game pages
                         dashboardSidebar(
                           width = 220,

                           sidebarMenu(id="tabs",

                                       menuItem("Prerequisites", tabName= "prereq", icon=icon("book")),
                                       menuItem("Overview",tabName = "instruction", icon = icon("dashboard")),
                                       menuItem("Explore",tabName = "explore", icon = icon("wpexplorer")),
                                       menuItem("Game", tabName = "qqq", icon= icon("gamepad"))

                           )
                         ),

                        ####################### Button and slider bar color edits ######################################
                        ######### Could be combined but left separate so easily understood#####################

                          dashboardBody(
                           tags$head(
                             tags$link(rel = "stylesheet", type = "text/css", href = "Feature.css"), #customised style sheet
                             tags$style(HTML('#start{background-color: #ffb6c1')),
                             tags$style(HTML('#go{background-color: #ffb6c1')),
                             tags$style(HTML('#submitD{background-color: #ffb6c1')),
                             tags$style(HTML('#start{border-color:#ffb6c1')),
                             tags$style(HTML('#go{border-color: #ffb6c1')),
                             tags$style(HTML('#submitD{border-color: #ffb6c1')),
                             tags$style(HTML('#begin{background-color: #ffb6c1')),
                             tags$style(HTML('#begin{border-color: #ffb6c1')),
                             tags$style(HTML('#challenge{background-color: #ffb6c1')),
                             tags$style(HTML('#challenge{border-color: #ffb6c1')),
                             tags$style(HTML('#answer{background-color: #ffb6c1')),
                             tags$style(HTML('#answer{border-color: #ffb6c1')),
                             tags$style(HTML('#submit{background-color: #ffb6c1')),
                             tags$style(HTML('#submit{border-color: #ffb6c1')),
                             tags$style(HTML('#nextButton{background-color: #ffb6c1')),
                             tags$style(HTML('#nextButton{border-color: #ffb6c1')),
                             tags$style(HTML('#reset{background-color: #ffb6c1')),
                             tags$style(HTML('#reset{border-color: #ffb6c1')),
                             tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #ffb6c1}")),
                             tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #ffb6c1")),
                             tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #ffb6c1")),
                             tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: #ffb6c1")),
                             tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {border-color: #ffb6c1")),
                             tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {border-color: #ffb6c1")),
                             tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {border-color: #ffb6c1")),
                             tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {border-color: #ffb6c1"))
                           ),

                           tabItems(
                             tabItem(tabName = "instruction",

                                     tags$a(href='http://stat.psu.edu/',tags$img(src='logo.png', align = "left", width = 180)),
                                    br(),br(),br(),
                                    h3(strong("About:")),
                                    h4("This app will allow you to explore how to read diagnostic plots while interacting with different transformations
       to help you better understand the assumptions of regression."),
                                    br(),
                                    h3(strong("Instructions:")),
                                    h4(tags$li("Each 'Mystery Model' on the exploration page is generated with variables or their transformations being the response (Y) 
      or the predictor variables (X1, X2).")),
                                    h4(tags$li("Watch how diagnostic plots change when you adjust the predictors and response variables using different transformations. 
       Note that transforming the y variable will effect certain plots more, and transforming the x variable will effect other 
                                       plots more.")),
                                    h4(tags$li('You also have the option to change the variances of each term, and the sample size.')),
                                    h4(tags$li('The instructions in the activity provide some ideas for exploration.')),
                                    h4(tags$li("In the game, the object is to win at tic-tac-toe where you are playing X's.  Select a square, then answer the question.
       If you get the question correct, an X goes in the square.  If you get it wrong, an O goes in the square.")),
                                    div(style = "text-align: center",
                                        bsButton(inputId = "go", label =  "Go to Exploration",icon("bolt"), style= "danger", size= "large", class="circle grow")
                                    ),
                                    br(),
                                    h3(strong("Acknowledgements:")),
                                    h4("This app was developed and coded by TJ McIntyre, with the help of Ryan Voyack.")

                             ),

                             #Adding pre-requisites page to remove background from instructions page

                             tabItem(tabName="prereq",
                                     h3(strong("Background: Assumptions and Diagnostic Plots in Regression")),
                                     h4(tags$li("Transforming the x values is appropriate when non-linearity is the only problem 
       (i.e., the independence, normality, and equal variance conditions are met). Transforming the y values should be
                                        considered when non-normality and/or unequal variances are the problems with the model.")),
                                     h4(tags$li("The Fitted vs Residuals plot can be used to check the assumption of linearity (any location on the x axis, the 
       average residual should be close to 0) and it can also be used to check the assumption of equal variances 
                                        (at any location on the x axis, the variability of the residual should be similar).")),
                                     h4(tags$li("The Normal Q-Q plot can be used to check the assumption of normal errors: i.e. the majority of the points should 
       be a straight line. Skewness can also be seen by this plot.  See the ", a(href='https://rstudio.aws.science.psu.edu:3838/Boast/Regression/QQ_plot/', 'Q-Q plot')," app for further exploration.")),
                                     h4(tags$li("The Scale-Location plot can be used to check the assumption of equal variances, at any location of the x axis,
        the upper bound of the residuals should be similar.")),
                                     h4(tags$li("The Cook's Distance plot shows the values of leverage, standardized residuals, and Cook's Distance of each data point
       which can be used to determine high leverage points, outliers and influential points.")),br(),
                                     
                                    div(style = "text-align: center",bsButton("start","Go to the overview",icon("bolt"),style = "danger",size = "large",class="circle grow"))


                             ),


                           tabItem(tabName = "explore",
                                   div(style="display: inline-block;vertical-align:top;",
                                       tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 19)),
                                       circleButton("infoex",icon = icon("info"), status = "myClass",size = "xs")
                                   ),
                                   fluidRow(
                                     column(h2("Transformations, Sample size, and Variances vs. Diagnostic plots"), 
                                            width = 12),
                                     #img(src = "pink.jpg", width = 40, offset = 30), 
                                     column(h4("Each model is generated with Y as the response variable and X1 
                                               and X2 being the predictor variables."), width = 11)
                                     ),

                                     br(),

                                  sidebarLayout(
                                    sidebarPanel(

                                      selectInput("model", "Select Mystery Model:", choices= c('Model 1', 'Model 2', 'Model 3')),

                                      sliderInput("n", "Sample Size:", min = 10, max=510, value= 50, step= 10),

                                      bsPopover(id= "n",title= "Sample Size Input", content="Number of observations pulled from the normal distribution",
                                                placement= "top", trigger = "click", options = NULL),

                                      selectInput("x", "Transformation on X1:", choices=c('logx', 'sqrtx',  "none"), selected= "nonex"),

                                      #bsPopover(id= "x",title= "Transformation Hint", content="Transform X when non-linearity is in question",
                                              #  placement= "top", trigger = "click", options = NULL),

                                      sliderInput("x1v", "x1 Variance:", min = 0, max=20, value= 2, step= 1),

                                      bsPopover(id= "x1v",title= "Variance for X1", content="Variance for X1 normal distribution",
                                                placement= "top", trigger = "click", options = NULL),

                                      selectInput("x2", "Transformation on X2:", choices=c('logx2', 'sqrtx2',  "none"),selected ="nonex2"),

                                      #bsPopover(id= "x2",title= "Transformation Hint", content="Transform X when non-linearity is in question",
                                               # placement= "top", trigger = "click", options = NULL),

                                      sliderInput("x2v", "x2 Variance:", min = 0, max=20, value= 2, step= 1),

                                      bsPopover(id= "x2v",title= "Variance for X2", content="Variance for X2 normal distribution",
                                                placement= "top", trigger = "click", options = NULL),

                                      selectInput('y', 'Transformation on Y:', choices=c('logy','sqrty',  "none"), selected = "logy"),

                                      sliderInput("yv", "Y Variance:", min = 0, max=20, value= 2, step= 1),

                                      bsPopover(id= "yv",title= "Variance for Y", content="Variance for Y normal distribution",
                                                placement= "top", trigger = "click", options = NULL),

                                      #bsPopover(id= "y",title= "Transformation Hint", content="Transform Y when non-normality or unequal variances are in question",
                                                #placement= "top", trigger = "click", options = NULL),

                                      bsButton("submitD", "See Results", style = "danger", icon("retweet"), size = "median"),
                                      #bsButton("submitD", "Results for a New Sample", style = "danger", icon("retweet"), size = "median"),
                                      
                                      br(),
                                      br(),

                                   bsButton("begin","Go to the Game",icon("bolt"),style = "danger",size = "median",class="circle grow")


                                    ),
                                    mainPanel(
                                      plotOutput("plots"),

                                      br(),

                                      fluidRow(
                                        column(h3("Adjust the inputs to complete the activity:"), width = 12)),
                                    wellPanel(

                                       fluidRow(column(uiOutput("challenges"), width = 12))
                                   ),

                                   fluidRow(column(h3("Feedback:"), width = 12)),

                                    wellPanel(
                                      fluidRow(column(uiOutput("answers"), width = 12))
                                    ),
                                      bsButton("challenge", "New Activity", style= "danger"),
                                      bsButton("answer", "View Feedback", style= "danger")

                                    ))

                                  ),


                            tabItem(tabName = "qqq",
                                    div(style="display: inline-block;vertical-align:top;",
                                        tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 19))
                                    ),
                                    div(style="display: inline-block;vertical-align:top;",
                                        circleButton("info",icon = icon("info"), status = "myClass",size = "xs")
                                    ),
                                   fluidRow(column(h2("Tic-Tac-Toe"), width = 12),
                                            #column(1,img(src = "pink.jpg", width = 20)),
                                            column(h4("You will get an X for a correct answers and 
                                                     the computer will get an O for a wrong answer."), width = 11)),
                                   br(), 
                                   fluidRow(
                                     column(width = 6,
                                            leafletOutput('image', width = "100%", height = 400),
                                            br(),
                                            textOutput("warning"),
                                            textOutput("gameMessage")
                                     ),
                                     column(6,
                                            conditionalPanel("output.temp != 2",
                                                             conditionalPanel("input.image_click",
                                                                              uiOutput("CurrentQuestion"),
                                                                              uiOutput("CurrentQuestion.extra"),
                                                                              br(),
                                                                              br(),
                                                                              br()
                                                             ),
                                                             textOutput("directions"),
                                                             br()
                                            )
                                     ),
                                     column(2,
                                            bsButton(inputId = 'submit', label = 'Submit Answer', style= 'danger')
                                     ),
                                     column(1,
                                            bsButton(inputId = "nextButton",label = "Next Question", style= 'danger')
                                     )
                                   ),
                                   fluidRow(
                                     column(width=12, offset = 5,
                                             br(),
                                             bsButton(inputId="reset", label="Start new game", style= 'danger')
                                     )
                                   ),
                                   fluidRow(
                                     column(width=12, offset = 5,
                                            ''
                                            #useShinyjs(),
                                            #extendShinyjs(text = jsResetCode),
                                            #actionButton(inputId='JSreset', label="RESET"),
                                            #tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode", function(message) {console.log(message) eval(message.code);});')))
                                     )
                                   )

                           )




                         )
                         )



)
