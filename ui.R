#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#   Student: Daniel Blei       ID: x16151704
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(shiny)
data <- read.csv("datafile.csv",header=T,stringsAsFactors = F,  na.strings=c(NA,"NA"," NA",""))
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(waffle)



appCSS <- "
#loading-content {
position: absolute;
background: #000000;
opacity: 1;
z-index: 100;
left: 0;
right: 0;
padding-top: 200px;
height: 100%;
text-align: center;
color: #FFFFFF;
}
"



ui <- fluidPage(
    useShinyjs(),
  inlineCSS(appCSS),
  
 
  div(
    id = "loading-content",
    h2("Loading...")
  ),
  
  hidden(
    div(
      id = "app-content",
      
      
  
      dashboardPage( skin = "blue",
      dashboardHeader(title = "Trip Advisor Analysis", titleWidth = 230),
      dashboardSidebar(
         sidebarMenu(
              menuItem("Data Overall", tabName = "dashboard", icon = icon("list-alt")),
              menuItem("Restaurants/City", tabName = "Graphic1", icon= icon("bar-chart-o")),
              menuItem("Cuisine Style/City", tabName = "Graphic2", icon = icon("list-alt")),
              menuItem("Price Range/City", tabName = "Graphic3", icon = icon("pie-chart"))
                       )),
      dashboardBody(
        tabItems(
              tabItem(tabName = "dashboard",h3("Summary of Attributes",align = "center"),
                      fluidRow(
                        box(solidHeader = F, status = "primary", width = "60px", height = "60px",
                            selectInput("Variables", "Select a dataset attribute:", choices=colnames(data[,c(-1,-5)]))
                          ),
                        box(solidHeader = F, status = "primary", width = "600px",align = "center",
                          verbatimTextOutput("summary"))
                       
                        
                     
              
                    )),
                         
              tabItem(tabName = "Graphic1", h3("Cities Average",align = "center"),
                      fluidRow(
                        box(status = "primary",width = "12",collapsible=T,uiOutput('snpselect')),
                        box(status = "primary", width = "12",
                                   plotOutput("DashImage"))
                   
                                 )),
                         
                  tabItem(tabName = "Graphic2", h3("Cuisine Style Analysis", align = "center"),
                                 fluidRow(
                      box(status = "primary",collapsible=T,width=12,uiOutput("Cusine")),
                     box(status = "primary",collapsible=F,width=12,verbatimTextOutput("BoxPlot")) )),
              
              tabItem(tabName = "Graphic3", h3("Price Range Analysis", align = "center"),
                      fluidRow(
                        box(status = "primary",collapsible=T,width=12,uiOutput("price")),
                        box(status = "primary",collapsible=F,
                            width=12,align = "center",verbatimTextOutput("Percentage"),plotOutput("BoxPlot2")) ))
                       )
                     )
)


    
  )
)
)