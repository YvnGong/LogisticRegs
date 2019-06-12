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
library(ggplot2)
library(plotly)

#library(rlocker)
#smiles

shinyUI <- dashboardPage(
                         dashboardHeader(title = "Logistic Regression",
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
                                   # div(style="display: inline-block;vertical-align:top;",
                                   #     tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 19)),
                                   #     circleButton("infoex",icon = icon("info"), status = "myClass",size = "xs")
                                   # ),
                                   h3(strong("Logistic Regression")),
                                   h4("This app will allow you to explore how to create and interprete logistic regression."),
                                   br(),
                                   sidebarLayout(
                                     sidebarPanel(
                                       sliderInput("sampleSize", "Sample Size:",
                                                   min = 0, max = 300, value = 150
                                       ),
                                       sliderInput("b0", "β0 (intercept):",
                                                   min = -10, max = 10, value = 0
                                       ),
                                       sliderInput("b1", "β1 (coefficient):",
                                                   min = -10, max = 10, value = 3
                                       ),
                                       sliderInput("ci", "confidence interval level",
                                                   min = 0, max = 0.999, value = 0.95),
                                       selectInput(inputId="residualType", label = "Residual Type",
                                                   choices = c("deviance", "pearson"), selected="deviance")
                                     ),
                                     mainPanel(
                                       plotlyOutput("logplot", width = "100%"),
                                       br(),
                                       #tableOutput("citable"),
                                       #p("Best results are no patterns or residual values > |2|"),
                                       plotOutput("residualPlot", width = "100%")
                                     )
                                   )
                                  ),

                            tabItem(tabName = "qqq",
                                    h3("Game Section"),
                                    p("Introduction is still working in progress"),
                                    
                                    sidebarLayout(
                                      sidebarPanel(
                                        
                                      ),
                                      
                                      mainPanel(
                                        br(),
                                        
                                        fluidRow(
                                          column(6, offset = 2,
                                                 uiOutput("distPlot", width = "100%"))),
                                        
                                        br(),
                                        br()
                                      )
                                    )
                                    
                           )
                         )
                         )

)
