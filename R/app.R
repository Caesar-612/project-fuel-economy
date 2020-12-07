library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(plotly)
source("fuel_data.R")
library(shinythemes)
library(tidyverse)
library(reactlog)
library(lazyeval)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(titleWidth = 380,
                  title = "Fuel Economy"),
  
  dashboardSidebar(
    width = 380,
    sidebarMenu(
      
      menuItem("Introduction", tabName = "Introduction", icon = icon("file-code-o")),
      menuItem("EDA", icon = icon("far fa-chart-bar"),
               menuSubItem("Distribution of GHG Score/MPG/Fuel Cost", tabName = "distribution"),
               menuSubItem("Averge GHG/MPG/Energy Consumption", tabName = "comparisionFuel"),
               menuSubItem("Cartype recommend", tabName = "recommendedCartypes")
      ),
      menuItem("Statistical Analysis", tabName = "StatisticalAnalysis", icon = icon("fad fa-chart-line"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem(tabName = "Introduction",
              h1(align = "center","Introduction"),
              fluidRow(
                box(width = 12, "The aim of the application is to find a friendly cartype for customers by analyzing the GHG Score,
                        Fuel Cost and Energy Cost in terms of manufactures, fuel types, model year, car types")
              ),
              br(),
              br(),
              fluidRow(
                box(width = 12, align="left", background = "black",
                    h2("Contents"),
                    h3("1. Exploratory Data Analysis"),
                    h4("1.1 Distribution of GHG Score or MPG or Fuel Cost"),
                    h4("1.2 Non-Electricity vs. Electricity: Average GHG Score or MPG or Fuel Cost"),
                    h4("1.3 Cartype Recommendation"),
                    h3("2. Statistical Analysis - Linear Regression")
                )
              )
      ),
      
      tabItem(tabName = "comparisionFuel",class = "active",
              h1(align = "center",
                 "Averge GHG/MPG/Energy Consumption"),
              br(),
              br(),
              fluidRow(
                column(width = 2,
                       box(
                         title = "Settings", width = NULL,height = 520, solidHeader = TRUE, status = "primary",
                         prettyCheckboxGroup(
                           inputId = "fuel_type_1",
                           label = "Choose:",
                           choices = fuelType_list_without_E,
                           selected =fuelType_list_without_E,
                           icon = icon("check-square-o"),
                           status = "primary",
                           outline = TRUE,
                           animation = "jelly"
                         ),
                         br(),
                         sliderTextInput(
                           inputId = "year",
                           label = "Choose model year range:",
                           choices = year_list,
                           selected = c(1984, 2021),
                           grid = TRUE
                         ),
                         br(),
                         prettyCheckboxGroup(
                           inputId = "roadType",
                           label = "Choose Road Type:",
                           choices = roadType,
                           selected = roadType,
                           icon = icon("check-square-o"),
                           status = "primary",
                           outline = TRUE,
                           animation = "jelly"
                         )
                       )
                ),
                column(width = 5,
                       tabBox(title = "Non-Electricity",width = NULL,height = 520,
                              tabPanel("MPG", plotOutput("non_ele_aveMPG")),
                              tabPanel("GHG", plotOutput("non_ele_aveGHG")),
                              tabPanel("Fuel Cost", plotOutput("non_ele_aveCost"))
                       )
                       
                ),
                
                column(width = 5,
                       tabBox(title = "Electricity",width = NULL,height = 520,
                              tabPanel("Electricity (kwh)", plotOutput("ele_aveELE")),
                              tabPanel("GHG", plotOutput("ele_aveGHG")),
                              tabPanel("Electricity Cost", plotOutput("ele_aveCost"))
                       )
                )
              )
      ),
      tabItem(tabName = "distribution",
              h1(align = "center",
                 "Distribution of GHG/MPG/Fuel Cost"),
              fluidRow(
                box(width = 12, align = "left", height = "200px",
                    pickerInput("fuelType", h4("Fuel Type"), choices=fuelType_list, selected = fuelType_list, options = list(`actions-box` = TRUE),multiple = T),
                    pickerInput("make", h4("Manufacture"), choices=manufacture_list, selected = manufacture_list, options = list(`actions-box` = TRUE),multiple = T)
                ),
                
              ),
              fluidRow(
                
                tabBox(height = "520px",
                       
                       tabPanel("GHG",
                                dataTableOutput("ghgTable", height = "520px")
                       ),
                       tabPanel("MPG",
                                dataTableOutput("mpgTable", height = "520px")),
                       
                       tabPanel("Annual Fuel Cost",
                                dataTableOutput("costTable", height = "520px"))
                ),
                
                tabBox(
                  tabPanel("GHG",
                           plotlyOutput("distGHG", height = "520px", width = "600px")),
                  tabPanel("MPG",
                           plotlyOutput("distMPG", height = "520px", width = "600px")),
                  tabPanel("Annual Fuel Cost",
                           plotlyOutput("distCost", height = "520px", width = "600px"))
                )
                
              )
              
      ),
      tabItem(tabName = "recommendedCartypes",class = "active",
              h1(align = "center",
                 "Cartype Recommendation"),
              hr(),
              fluidRow(
                
                column(width = 3,
                       selectInput("fuel_Type", "What fuel type are you looking for?", choices =  fuel_1$fuelType),
                       selectInput("fuel_Type_1", "What another fuel type are you looking for?", choices =  fuel_1$fuelType),
                       selectInput("year", "whcih year?", choices = fuel_year$year, selected = "2020"),
                       checkboxInput("green", "Green House Score"),
                       checkboxInput("trany", "transmission of the car"),
                       checkboxInput("cost", "fuel cost"),
                       sliderInput("bins",
                                   "TOP ?",
                                   min = 5,
                                   max = 50,
                                   value = 5)
                       
                ),
                
                
                
                column(width = 4,
                       plotOutput("colplot")
                       
                ),
                column(width = 4,
                       plotOutput("plot_1")
                ),
              )
      ),
      
      tabItem(tabName = "StatisticalAnalysis",
              h1(align = "center",
                 "Statistical Analysis"),
              tags$h2("Linear Regression"),
              br(),
              
              fluidRow(
                column(width=3,
                       varSelectInput("value_1", "X value?", data =  fuel_2),
                       varSelectInput("value_2", "Y value?", data =  fuel_2),
                       checkboxInput("log", "better to observe"),
                       numericInput("xvalue", "Predict Y base on X value", value = 10, min = 0, max = 100),
                       verbatimTextOutput("plot_4")
                ),
                
                column(width=9,
                       plotOutput("plot_2"),
                       verbatimTextOutput("plot_3")
                       
                )
              )
              
              
              
      )
      
      
    )
    
  )
  
  
  
)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
}
# Run the application 
shinyApp(ui = ui, server = server)


