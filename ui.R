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
library(rlocker)

library(dplyr)
library(shinycssloaders)

source("helpers.R") 

sliderInput2 <- function(inputId, label, min, max, value, step=NULL, from_min, from_max){
  x <- sliderInput(inputId, label, min, max, value, step)
  x$children[[2]]$attribs <- c(x$children[[2]]$attribs, 
                               "data-from-min" = from_min,
                               "data-from-shadow" = TRUE)
  x
}
sliderInput3 <- function(inputId, label, min, max, value, step=NULL, from_min, from_max){
  x <- sliderInput(inputId, label, min, max, value, step)
  x$children[[2]]$attribs <- c(x$children[[2]]$attribs, 
                               "data-from-max" = from_max,
                               "data-from-shadow" = TRUE)
  x
}

shinyUI <- dashboardPage(
                         dashboardHeader(title = "Logistic Regression",
                                         tags$li(class="dropdown",
                                                 tags$a(href="https://shinyapps.science.psu.edu/",
                                                        icon("home", lib="font-awesome"))),
                                         tags$li(class="dropdown",
                                                 actionLink("info", icon("info"), class="myClass")),
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
                             tags$style(HTML('#submit{color: white')),
                             tags$style(HTML('#go{background-color: #ffb6c1')),
                             tags$style(HTML('#goMul{background-color: #ffb6c1')),
                             tags$style(HTML('#goButton{background-color: #ffb6c1')),
                             tags$style(HTML('#goButtonMul{background-color: #ffb6c1')),
                             tags$style(HTML('#submitD{background-color: #ffb6c1')),
                             tags$style(HTML('#start{border-color:#ffb6c1')),
                             tags$style(HTML('#go{border-color: #ffb6c1')),
                             tags$style(HTML('#goMul{border-color: #ffb6c1')),
                             tags$style(HTML('#goButton{border-color: #ffb6c1')),
                             tags$style(HTML('#goButtonMul{border-color: #ffb6c1')),
                             tags$style(HTML('#submitD{border-color: #ffb6c1')),
                             tags$style(HTML('#begin{background-color: #ffb6c1')),
                             tags$style(HTML('#begin{border-color: #ffb6c1')),
                             tags$style(HTML('#challenge{background-color: #ffb6c1')),
                             tags$style(HTML('#challenge{border-color: #ffb6c1')),
                             tags$style(HTML('#answer{background-color: #ffb6c1')),
                             tags$style(HTML('#answer{border-color: #ffb6c1')),
                             tags$style(HTML('#submit{background-color: #f78b9b')),
                             tags$style(HTML('#submit{border-color: #f78b9b')),
                             tags$style(HTML('#nextButton{background-color: #ffb6c1')),
                             tags$style(HTML('#nextButton{border-color: #ffb6c1')),
                             tags$style(HTML('#reset{background-color: #ffb6c1')),
                             tags$style(HTML('#reset{border-color: #ffb6c1')),
                             tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #ffb6c1}")),
                             tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #ffb6c1")),
                             tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #ffb6c1")),
                             tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: #ffb6c1")),
                             tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: #ffb6c1")),
                             tags$style(HTML(".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {background: #ffb6c1")),
                             tags$style(HTML(".js-irs-6 .irs-single, .js-irs-6 .irs-bar-edge, .js-irs-6 .irs-bar {background: #ffb6c1")),
                             tags$style(HTML(".js-irs-7 .irs-single, .js-irs-7 .irs-bar-edge, .js-irs-7 .irs-bar {background: #ffb6c1")),
                             tags$style(HTML(".js-irs-8 .irs-single, .js-irs-8 .irs-bar-edge, .js-irs-8 .irs-bar {background: #ffb6c1")),
                             tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {border-color: #ffb6c1")),
                             tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {border-color: #ffb6c1")),
                             tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {border-color: #ffb6c1")),
                             tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {border-color: #ffb6c1")),
                             tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {border-color: #ffb6c1")),
                             tags$style(HTML(".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {border-color: #ffb6c1")),
                             tags$style(HTML(".js-irs-6 .irs-single, .js-irs-6 .irs-bar-edge, .js-irs-6 .irs-bar {border-color: #ffb6c1")),
                             tags$style(HTML(".js-irs-7 .irs-single, .js-irs-7 .irs-bar-edge, .js-irs-7 .irs-bar {border-color: #ffb6c1")),
                             tags$style(HTML(".js-irs-8 .irs-single, .js-irs-8 .irs-bar-edge, .js-irs-8 .irs-bar {border-color: #ffb6c1"))
                           ),

                           tabItems(
                             #Adding pre-requisites page to remove background from instructions page
                             
                             tabItem(tabName="prereq",
                                     h3(strong("Background: Logistic Regression Analysis")),
                                     br(),
                                     h4(tags$li("The logistic regression model explains the relationship 
                                                between one (or more) explanatory variable and the binary outcome.")),
                                     
                                     br(),
                                     withMathJax(),
                                     h4(tags$li("In the logistic regression the constant \\(\\beta_0\\)
                                                moves the curve left and right and the slope
                                                \\(\\beta_1\\) defines the steepness of the curve.")),
                                     div(style="font-size: 1.6em", helpText('$${ln({p\\over1-p})} = {\\beta_0+\\beta_1x}$$')),
                                     h4(tags$li("Deviance Residual and Pearson Residual check the model fit. Best 
                                                results are no patterns or no extremely large residuals ")),
                                     h4(tags$li("Hosmer and Lemeshow test check the goodness of fit in the model 
                                                where data is divided into recommended 10 groups. The p-value can 
                                                determine the significance of the result.")),
                                     br(),
                                     h4(tags$li("Hosmer-Lemeshow Test Statstics")),
                                     div(style="font-size: 1.6em", helpText('$${\\sum_{i=1}^g}{\\sum_{j=1}^2}{{(obs_{ij} - exp_{ij})^2} 
                                                                            \\over exp_{ij}}$$')),
                                     br(),
                                     br(),
                                     div(style = "text-align: center",bsButton("start","Go to the overview",
                                                                               icon("bolt"),style = "danger",
                                                                               size = "large",class="circle grow"))
                                     
                                     ),
                             
                             tabItem(tabName = "instruction",

                                     tags$a(href='http://stat.psu.edu/',tags$img(src='logo.png', align = "left", width = 180)),
                                    br(),br(),br(),
                                    h3(strong("About:")),
                                    h4("This app allows you to explore how different factors can affect the outcome of the Logistic Regression Model and Multiple Logistic Model."),
                                    br(),
                                    h3(strong("Instructions:")),
                                    h4(tags$li("There are two types of logistic model, Single Logistic Regression and Multiple Logistic Regression, included in this app.")),
                                    h4(tags$li("For each model, adjust the sliders to change the sample size and corresponding beta coefficients.")),
                                    h4(tags$li("Click New Data button to generate plot. Watch the change of plot when drag the slider of confidence interval.")),
                                    h4(tags$li("Each Logistic Regression plot is made on a random sample.")),
                                    h4(tags$li("After working with the explore section, you can start the game to test your understanding of the concepts.")),
                                    br(),
                                    div(style = "text-align: center",
                                        bsButton(inputId = "go", label =  "Explore", icon("bolt"), style= "danger", size= "large", class='circle grow')
                                    ),
                                    
                                    br(),
                                    h3(strong("Acknowledgements:")),
                                    h4("This app was developed and coded by Yiyun Gong and Ruisi Wang.")

                             ),

                             

                           tabItem(tabName = "explore",
                                   
                                   # div(style="display: inline-block;vertical-align:top;",
                                   #     tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 19)),
                                   #     circleButton("infoex",icon = icon("info"), status = "myClass",size = "xs")
                                   # ),
                                   
                                   tabsetPanel(
                                     type = 'tabs',
                                     tabPanel(
                                       ######Single Regression
                                       'Single Logistic Regression',
                                       # h4(tags$li("For each model, ")),
                                       # h4(tags$li("")),
                                       # h4(tags$li("Each Logistic Regression plot is made on a random sample.")),
                                       # h4(tags$li("After working with the explore section, you can start the game to test your understanding of the concepts.")),
                                      h3(strong("Single Logistic Regression")),
                                      h4(tags$li("Adjust the sliders to change the sample size and corresponding 
                                                 beta coefficients.")),
                                      h4(tags$li("Click 'New Data' button to generate plot.")),
                                      br(),
                                      
                                       sidebarLayout(
                                         sidebarPanel(
                                           sliderInput2("sampleSize", "Sample Size:",
                                                        min = 0, 
                                                        max = 300, 
                                                        value = 150, 
                                                        step = 1, 
                                                        from_min = 2
                                           ),
                                           sliderInput("b0", "β0 (intercept):",
                                                       min = -10, max = 10, value = 0
                                           ),
                                           sliderInput("b1", "β1 (coefficient):",
                                                       min = -10, max = 10, value = 3
                                           ),
                                           # sliderInput("ci", "confidence interval level",
                                           #             min = 0, max = 0.999, value = 0.95),
                                           sliderInput3("ci", "confidence interval level:",
                                                        min = 0, 
                                                        max = 1, 
                                                        value = 0.95, 
                                                        step = 0.01,  
                                                        from_max = 0.99
                                           ),
                                           selectInput(inputId="residualType", label = "Residual Type",
                                                       choices = c("deviance", "pearson"), selected="deviance"),
                                           br(),
                                           actionButton("goButton", "New Data", icon("paper-plane"),
                                                        class = "btn btn-lg", style="color: #fff", class="circle grow"),
                                           br()
                                           
                                         ),
                                         mainPanel(
                                           plotlyOutput("logplot", width = "98%", height = "300px")%>% withSpinner(color="#ffb6c1"),
                                           
                                           br(),
                                           tableOutput("citable"),
                                           plotOutput("residualPlot", width = "100%",height = "330px")%>% withSpinner(color="#ffb6c1"),
                                           tags$style(type='text/css', '#lemeshowTest, #obsexp {background-color: rgba(219,193,195,0.20); 
                                                      color: maroon; text-align: center}', '#title{color: blackl; padding-left:2.5em; font-size: 22px}'), 
                                           
                                           br(),
                                           h3(strong(id='title', "Hosmer and Lemeshow goodness of fit test")),
                                           #br(),
                                           tableOutput("lemeshowDF"),
                                           tableOutput("obsexpDF"),
                                           #verbatimTextOutput("lemeshowTest"),
                                           #verbatimTextOutput("obsexp"),
                                           bsPopover("lemeshowDF"," ","The Hosmer-Lemeshow Test is a goodness of fit test for the logistic model. Here is the result of the Hosmer-Lemeshow Test for ten groups. Number of subgroups, g, usually uses the formula g > P + 1. P is number of covariates. Degree of freedom equals g-2. ", trigger = "hover",place="left"),
                                           bsPopover("obsexpDF"," ","There are 10 rows meaning g=10.", trigger = "hover",place="left")
                                           )
                                       )
                                     ),
                                     
                                     tabPanel("Multiple Logistic Regression",
                                              h3(strong("Multiple Logistic Regression")),
                                              h4(tags$li("Adjust the sliders to change the sample size and corresponding 
                                                 beta coefficients.")),
                                              h4(tags$li("After working with the explore section, you can start the game to test your understanding.")),
                                              br(),
                                              
                                              sidebarLayout(
                                                sidebarPanel(
                                                  sliderInput2("sampleSize2", "Sample Size:",
                                                               min = 0, 
                                                               max = 300, 
                                                               value = 150, 
                                                               step = 1, 
                                                               from_min = 10
                                                  ),
                                                  sliderInput("b02", "β0 (intercept):",
                                                              min = -10, max = 10, value = 2
                                                  ),
                                                  sliderInput("b12", "β1 (coefficient):",
                                                              min = -10, max = 10, value = 8
                                                  ),
                                                  sliderInput("b2", "β2 (coefficient):",
                                                              min = -10, max = 10, value = -8
                                                  ),
                                                  sliderInput3("ci2", "confidence interval level:",
                                                               min = 0, 
                                                               max = 1, 
                                                               value = 0.95, 
                                                               step = 0.01,  
                                                               from_max = 0.99
                                                  ),
                                                  
                                                  br(),
                                                  actionButton("goButtonMul", "New Data", icon("paper-plane"),
                                                                                   class = "btn btn-lg", style="color: #fff", class="circle grow"),
                                                  br(),
                                                  br(),
                                                  bsButton(inputId = "begin", label="Game Time!", icon("gamepad"), 
                                                           class='btn btn-lg', style= "danger", class="circle grow")
                                                ),
                                                
                                                mainPanel(
                                                  plotlyOutput("mulPlot", height = "300px")%>% withSpinner(color="#ffb6c1"),
                                                  br(),
                                                  br(),
                                                  plotOutput("multix")%>% withSpinner(color="#ffb6c1")
                                                  # br(),
                                                  # tags$style(type='text/css', '#lemeshowTest2, #obsexp2 {background-color: rgba(219,193,195,0.20); 
                                                  #            color: maroon;text-align: center}'), 
                                                  # # br(),
                                                  # div(style="text-align: center", h3(id='title', "Hosmer and Lemeshow goodness of fit (GOF) test")),
                                                  # br(),
                                                  # tableOutput("lemeshowDF2"),
                                                  # tableOutput("obsexpDF2")
                                                  )
                                              )
                                              )
                                   )),

                            tabItem(tabName = "qqq",
                                    h2(strong("Game Section")),
                                    wellPanel(
                                      style = "background-color: #ffd0d7; border:1px solid #ffb6c1",
                                      tags$li("Practice the following questions. Once you got one question right, you would get a chance to roll the dice."),
                                      tags$li("In each turn 10 questions will be randomly draw from the bank."),
                                      tags$li("Once the total of the dice roll reach 20. You Win!")
                                    ),
                                    br(),
                                    h3(strong("Problems")),
                                    sidebarLayout(
                                      sidebarPanel(
                                        id="sidebar",
                                        tags$head(tags$style(
                                          HTML("#sidebar{background-color:#ffd4d0; border:1px solid #ffd4d0}")
                                        )),
                                        width = 6,
                                        uiOutput("question"),
                                        uiOutput("options"),
                                        br(),
                                        
                                        selectInput("answer", "pick an answer from below", c("","A", "B", "C")),
                                        uiOutput("mark")
                                      ),
                                      
                                      mainPanel(
                                        width = 6,
                                        br(),
                                        tags$head(tags$style(HTML(mycss))),
                                        fluidRow(
                                          column(12, align="center", uiOutput('gamescore')),
                                          column(12, align="center", div(id = "plot-container",
                                                         tags$img(src = "spinner.gif",
                                                                  id = "loading-spinner"),
                                                         uiOutput("dice", width = "100%")
                                          ))
                                        ),
                                        
                                          # br(),
                                        # fluidRow(
                                        #   # column(6, actionButton("roll", "roll")),
                                        #   # column(12, align="center", actionButton("stop", "stop"))
                                        #   # column(5, align="left", bsButton("restart", "restart", style="danger", disabled = TRUE))
                                        #   ),
                                        # br(),
                                        br()
                                      )
                                    ),
                                    fluidRow(
                                      column(6, align="center",
                                             div(style="display: inline-block", actionButton(inputId = 'submit', label = 'Submit')),
                                             div(style="display: inline-block;vertical-align:top; width: 30px;",HTML("<br>")),
                                             div(style="display: inline-block", bsButton(inputId = "nextq",label = "Next", style='warning', disabled = TRUE)),
                                             div(style="display: inline-block;vertical-align:top; width: 30px;",HTML("<br>")),
                                             div(style="display: inline-block", bsButton(inputId = "restart",label = "Restart", style="danger")))
                                    )
                           )
                         )
                         )

)
