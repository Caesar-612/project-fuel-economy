library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(plotly)
source("fuel_data.R")
library(shinythemes)
library(tidyverse)
library(reactlog)
library(DT)
library(lazyeval)
library(plotly)
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
            )
    )
))
# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
}
# Run the application 
shinyApp(ui = ui, server = server)
