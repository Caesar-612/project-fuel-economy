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
                
                column(width = 2,
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
                
                
                
                column(width = 5,
                       plotOutput("colplot")
                       
                ),
                column(width = 5,
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
    
  ))
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  observeEvent(input$fuel_type_1, {
    if(!(is.null(input$fuel_type_1))){
      years <- vehicles %>% 
        filter(Fuel_Type %in% input$fuel_type_1) %>% 
        select(Model_Year) %>% distinct(Model_Year) %>% pull(Model_Year) %>% sort()
      first_year <- years[1]
      last_year <- tail(years, n=1)
      updateSliderTextInput(session, inputId = "year",
                            choices = years,
                            selected = c(first_year, last_year)
      )
    }else{
      updateSliderTextInput(session, inputId = "year",
                            choices = c(0,0),
                            selected = c(0,0)
      )
    }
  }, ignoreInit = TRUE,ignoreNULL = F)
  nonE_pldt_MPG <- reactive({
    if(!(is.null(input$fuel_type_1)) & !(is.null(input$roadType))){
      vhc_change_nonElec %>% filter(Model_Year >= input$year[1] & Model_Year <= input$year[2] & Fuel_Type %in% input$fuel_type_1 & Road_Type %in% input$roadType) %>%
        group_by(Fuel_Type, Road_Type) %>%
        summarise(avg_MPG = round(mean(MPG), 2))
    }else if(!(is.null(input$roadType))){
      vhc_change_nonElec %>% filter(Road_Type %in% input$roadType) %>%
        group_by(Road_Type) %>%
        summarise(avg_MPG = round(mean(MPG), 2))
    }else if(!(is.null(input$fuelType))){
      vhc_change_nonElec %>% filter(Fuel_Type %in% input$fuel_type_1) %>%
        group_by(Fuel_Type) %>%
        summarise(avg_MPG = round(mean(MPG), 2))
    }else{
      # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
      vhc_change_nonElec %>%
        group_by(Fuel_Type) %>%
        summarise(avg_MPG = round(mean(MPG), 2))
    }
  })
  nonE_pldt_GHG <- reactive({
    if(!(is.null(input$fuel_type_1)) & !(is.null(input$roadType))){
      vhc_change_nonElec %>% filter(Model_Year >= input$year[1] & Model_Year <= input$year[2] & Fuel_Type %in% input$fuel_type_1 & Road_Type %in% input$roadType) %>%
        group_by(Fuel_Type, Road_Type) %>%
        summarise(avg_GHG = round(mean(GHG), 2))
    }else if(!(is.null(input$roadType))){
      vhc_change_nonElec %>% filter(Road_Type %in% input$roadType) %>%
        group_by(Road_Type) %>%
        summarise(avg_GHG = round(mean(GHG), 2))
    }else if(!(is.null(input$fuelType))){
      vhc_change_nonElec %>% filter(Fuel_Type %in% input$fuel_type_1) %>%
        group_by(Fuel_Type) %>%
        summarise(avg_GHG = round(mean(GHG), 2))
    }else{
      # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
      vhc_change_nonElec %>%
        group_by(Fuel_Type) %>%
        summarise(avg_MPG = round(mean(MPG), 2))
    }
  })
  nonE_pldt_Cost <- reactive({
    if(!(is.null(input$fuel_type_1)) & !(is.null(input$roadType))){
      vhc_change_nonElec %>% filter(Model_Year >= input$year[1] & Model_Year <= input$year[2] & Fuel_Type %in% input$fuel_type_1 & Road_Type %in% input$roadType) %>%
        group_by(Fuel_Type, Road_Type) %>%
        summarise(avg_Cost = round(mean(Annual_Fuel_Cost), 2))
    }else if(!(is.null(input$roadType))){
      vhc_change_nonElec %>% filter(Road_Type %in% input$roadType) %>%
        group_by(Road_Type) %>%
        summarise(avg_Cost = round(mean(Annual_Fuel_Cost), 2))
    }else if(!(is.null(input$fuelType))){
      vhc_change_nonElec %>% filter(Fuel_Type %in% input$fuel_type_1) %>%
        group_by(Fuel_Type) %>%
        summarise(avg_Cost = round(mean(Annual_Fuel_Cost), 2))
    }else{
      # xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
      vhc_change_nonElec %>%
        group_by(Fuel_Type) %>%
        summarise(avg_MPG = round(mean(MPG), 2))
    }
  })
  output$non_ele_aveMPG <- renderPlot({
    ggplot(nonE_pldt_MPG(), aes(x=Fuel_Type, y=avg_MPG, fill=Road_Type)) +
      geom_bar(position = position_dodge(), stat="identity")+
      xlab("Fuel Type") + ylab("Average MPG") +
      theme_minimal()+
      theme(legend.title = element_blank(), axis.title.x = element_blank(), legend.position="bottom", legend.box = "horizontal") +
      ggtitle("Average MPG")+
      geom_text(size = 3,position = position_dodge(width= 1), aes(y=avg_MPG-3,label = avg_MPG), angle=0) +
      scale_x_discrete(limits=unique(nonE_pldt_MPG()$Fuel_Type))
    
  }, height = 450)
  output$non_ele_aveGHG <- renderPlot({
    ggplot(nonE_pldt_GHG(), aes(x=Fuel_Type, y=avg_GHG, fill=Road_Type)) +
      geom_bar(position = position_dodge(), stat="identity")+
      xlab("Fuel Type") + ylab("Average GHG") +
      theme_minimal()+
      theme(legend.title = element_blank(), axis.title.x = element_blank(), legend.position="bottom", legend.box = "horizontal") +
      ggtitle("Average GHG")+
      geom_text(size = 3,position = position_dodge(width= 1), aes(y=avg_GHG,label = avg_GHG), angle=0) +
      scale_x_discrete(limits=unique(nonE_pldt_GHG()$Fuel_Type))
  }, height = 450)
  output$non_ele_aveCost <- renderPlot({
    ggplot(nonE_pldt_Cost(), aes(x=Fuel_Type, y=avg_Cost, fill=Road_Type)) +
      geom_bar(position = position_dodge(), stat="identity")+
      xlab("Fuel Type") + ylab("Average Cost") +
      theme_minimal()+
      theme(legend.title = element_blank(), axis.title.x = element_blank(), legend.position="bottom", legend.box = "horizontal") +
      ggtitle("Average Fuel Cost")+
      geom_text(size = 3,position = position_dodge(width= 1), aes(y=avg_Cost-1,label = avg_Cost), angle=45) +
      scale_x_discrete(limits=unique(nonE_pldt_Cost()$Fuel_Type))
    
  }, height = 450)
  
  E_pldt_elecomsump <- reactive({
    if(!(is.null(input$fuel_type_1)) & !(is.null(input$roadType))){
      vhc_change_E %>% filter(Model_Year >= input$year[1] & Model_Year <= input$year[2] & Road_Type %in% input$roadType) %>%
        group_by(Road_Type) %>%
        summarise(avg_MPG = round(mean(Electricity_Consumption), 2)) 
    }else if(is.null(input$fuel_type_1) & !(is.null(is.null(input$roadType)))){
      vhc_change_E %>% filter(Road_Type %in% input$roadType) %>% 
        group_by(Road_Type) %>%
        summarise(avg_MPG = round(mean(Electricity_Consumption), 2)) 
    }else {
      vhc_change_E %>% filter(Road_Type %in% roadType) %>% 
        group_by(Fuel_Type) %>%
        summarise(avg_MPG = round(mean(Electricity_Consumption), 2))
    }
    
  })
  E_pldt_GHG <- reactive({
    if(!(is.null(input$fuel_type_1)) & !(is.null(input$roadType))){
      vhc_change_E %>% filter(Model_Year >= input$year[1] & Model_Year <= input$year[2] & Road_Type %in% input$roadType) %>%
        group_by(Road_Type) %>%
        summarise(avg_GHG = round(mean(GHG), 2)) 
    }else if(is.null(input$fuel_type_1) & !(is.null(is.null(input$roadType)))){
      vhc_change_E %>% filter(Road_Type %in% input$roadType) %>% 
        group_by(Road_Type) %>%
        summarise(avg_GHG = round(mean(GHG), 2)) 
    }else {
      vhc_change_E %>% filter(Road_Type %in% roadType) %>% 
        group_by(Fuel_Type) %>%
        summarise(avg_GHG = round(mean(GHG), 2))
    }
    
  })
  E_pldt_Cost <- reactive({
    if(!(is.null(input$fuel_type_1)) & !(is.null(input$roadType))){
      vhc_change_E %>% filter(Model_Year >= input$year[1] & Model_Year <= input$year[2] & Road_Type %in% input$roadType) %>%
        group_by(Road_Type) %>%
        summarise(avg_Cost = round(mean(Annual_Fuel_Cost), 2)) 
    }else if(is.null(input$fuel_type_1) & !(is.null(is.null(input$roadType)))){
      vhc_change_E %>% filter(Road_Type %in% input$roadType) %>% 
        group_by(Road_Type) %>%
        summarise(avg_Cost = round(mean(Annual_Fuel_Cost), 2)) 
    }else {
      vhc_change_E %>% filter(Road_Type %in% roadType) %>% 
        group_by(Fuel_Type) %>%
        summarise(avg_Cost = round(mean(Annual_Fuel_Cost), 2))
    }
    
  })
  output$ele_aveELE <- renderPlot({
    
    if(!(is.null(input$roadType))){
      E_plot <- ggplot(E_pldt_elecomsump(), aes(x=Road_Type, y=avg_MPG, fill=Road_Type)) +
        geom_bar(position = position_dodge(), stat="identity")+
        xlab("Road Type") + ylab("Average Electricity Consumption (kwh/100 miles)") +
        theme_minimal()+
        theme(legend.title = element_blank(), axis.title.x = element_blank(), legend.position="bottom", legend.box = "horizontal") +
        ggtitle("Electricity Consumption in kwh/100 miles (City/Highway/Combined)")
      
      E_plot + geom_text(size = 5,position = position_dodge(width= 0.9), aes(y=avg_MPG-3,label = avg_MPG), angle=0) +
        scale_x_discrete(limits=unique(E_pldt_elecomsump()$Road_Type))
    }else{
      E_plot <- ggplot(vhc_change_E %>% filter(Road_Type %in% roadType) %>% 
                         group_by(Fuel_Type) %>%
                         summarise(avg_MPG = round(mean(Electricity_Consumption), 2)), aes(x=Fuel_Type, y=avg_MPG, color=Fuel_Type)) +
        geom_bar(position = position_dodge(), stat="identity")+
        xlab("Fuel Type") + ylab("Average Electricity Consumption (kwh/100 miles)") +
        theme_minimal()+
        theme(legend.title = element_blank(), axis.title.x = element_blank(), legend.position="bottom", legend.box = "horizontal") +
        ggtitle("Electricity_Consumption in kwh/100 miles (City/Highway/Combined)")
      
      E_plot + geom_text(size = 5,position = position_dodge(width= 0.9), aes(y=avg_MPG-3,label = avg_MPG), angle=0) +
        scale_x_discrete(limits=unique(E_pldt_elecomsump()$Fuel_Type))
    }
  }, height = 450)
  output$ele_aveGHG <- renderPlot({
    
    if(!(is.null(input$roadType))){
      E_plot <- ggplot(E_pldt_GHG(), aes(x=Road_Type, y=avg_GHG, fill=Road_Type)) +
        geom_bar(position = position_dodge(), stat="identity")+
        xlab("Road Type") + ylab("Average GHG") +
        theme_minimal()+
        theme(legend.title = element_blank(), axis.title.x = element_blank(), legend.position="bottom", legend.box = "horizontal") 
      
      E_plot + geom_text(size = 5,position = position_dodge(width= 0.9), aes(y=avg_GHG-3,label = avg_GHG), angle=0) +
        scale_x_discrete(limits=unique(E_pldt_GHG()$Road_Type))
    }else{
      E_plot <- ggplot(vhc_change_E %>% filter(Road_Type %in% roadType) %>% 
                         group_by(Fuel_Type) %>%
                         summarise(avg_GHG = round(mean(GHG), 2)), aes(x=Fuel_Type, y=avg_GHG, color=Fuel_Type)) +
        geom_bar(position = position_dodge(), stat="identity")+
        xlab("Fuel Type") + ylab("GHG") +
        theme_minimal()+
        theme(legend.title = element_blank(), axis.title.x = element_blank(), legend.position="bottom", legend.box = "horizontal")
      
      E_plot + geom_text(size = 5,position = position_dodge(width= 0.9), aes(y=avg_MPG-3,label = avg_MPG), angle=0) +
        scale_x_discrete(limits=unique(E_pldt_GHG()$Fuel_Type))
    }
  },height = 450)
  output$ele_aveCost <- renderPlot({
    
    if(!(is.null(input$roadType))){
      E_plot <- ggplot(E_pldt_Cost(), aes(x=Road_Type, y=avg_Cost, fill=Road_Type)) +
        geom_bar(position = position_dodge(), stat="identity")+
        xlab("Road Type") + ylab("Average Annual Fuel Cost") +
        theme_minimal()+
        theme(legend.title = element_blank(), axis.title.x = element_blank(), legend.position="bottom", legend.box = "horizontal") 
      
      E_plot + geom_text(size = 5,position = position_dodge(width= 0.9), aes(y=avg_Cost-3,label = avg_Cost), angle=0) +
        scale_x_discrete(limits=unique(E_pldt_Cost()$Road_Type))
    }else{
      E_plot <- ggplot(E_pldt_Cost(), aes(x=Fuel_Type, y=avg_Cost, color=Fuel_Type)) +
        geom_bar(position = position_dodge(), stat="identity")+
        xlab("Fuel Type") + ylab("Annual Fuel Cost") +
        theme_minimal()+
        theme(legend.title = element_blank(), axis.title.x = element_blank(), legend.position="bottom", legend.box = "horizontal")
      
      E_plot + geom_text(size = 5,position = position_dodge(width= 0.9), aes(y=avg_Cost-3,label = avg_Cost), angle=0) +
        scale_x_discrete(limits=unique(E_pldt_Cost()$Fuel_Type))
    }
  },height = 450)
  ###########################################################################
  observeEvent(input$fuelType, {
    if(!(is.null(input$fuelType))){
      makes <- vehicles %>% 
        filter(Fuel_Type %in% input$fuelType) %>% 
        select(Manufacture) %>% distinct(Manufacture) %>% pull(Manufacture) %>% sort()
      updatePickerInput(session, inputId = "make",
                        choices = makes,
                        selected = makes[1]
      )
    }else{
      updatePickerInput(session, inputId = "make",
                        choices = "",
                        selected = ""
      )
    }
  }, ignoreInit = TRUE,ignoreNULL = F)
  ghgTable <- reactive(({
    if(!(is.null(input$fuelType)) & is.null(input$make)){
      vehicles %>% select(Manufacture, Fuel_Type, Model, Model_Year, GHG) %>%
        filter(Fuel_Type %in% input$fuelType) %>%
        distinct(.keep_all = True) %>%
        # group_by(Fuel_Type, Manufacture) %>%
        arrange(desc(GHG), desc(Fuel_Type),desc(Manufacture))
    }else{
      vehicles %>% select(Manufacture, Fuel_Type, Model, Model_Year, GHG) %>%
        filter(Fuel_Type %in% input$fuelType & Manufacture %in% input$make) %>%
        distinct(.keep_all = True) %>%
        # group_by(Fuel_Type, Manufacture) %>%
        arrange(desc(GHG),desc(Fuel_Type), desc(Manufacture))
    }
  }))
  mpgTable <- reactive(({
    if(!(is.null(input$fuelType)) & is.null(input$make)){
      vhc_change_nonElec %>% select(Manufacture, Fuel_Type, Model, Model_Year, MPG) %>%
        filter(Fuel_Type %in% input$fuelType) %>%
        distinct(.keep_all = True) %>%
        arrange(desc(MPG), desc(Fuel_Type),desc(Manufacture))
    }else{
      vhc_change_nonElec %>% select(Manufacture, Fuel_Type, Model, Model_Year, MPG) %>%
        filter(Fuel_Type %in% input$fuelType & Manufacture %in% input$make) %>%
        distinct(.keep_all = True) %>%
        # group_by(Fuel_Type, Manufacture) %>%
        arrange(desc(MPG),desc(Fuel_Type), desc(Manufacture))
    }
  }))
  costTable <- reactive(({
    if(!(is.null(input$fuelType)) & is.null(input$make)){
      vehicles %>% select(Manufacture, Fuel_Type, Model, Model_Year, Annual_Fuel_Cost) %>%
        filter(Fuel_Type %in% input$fuelType) %>%
        distinct(.keep_all = True) %>%
        arrange(desc(Annual_Fuel_Cost), desc(Fuel_Type),desc(Manufacture))
    }else{
      vehicles %>% select(Manufacture, Fuel_Type, Model, Model_Year, Annual_Fuel_Cost) %>%
        filter(Fuel_Type %in% input$fuelType & Manufacture %in% input$make) %>%
        distinct(.keep_all = True) %>%
        arrange(desc(Annual_Fuel_Cost),desc(Fuel_Type), desc(Manufacture))
    }
  }))
  output$ghgTable <- renderDataTable(DT::datatable({
    ghg_table <-ghgTable()
    ghg_table
    
  }))
  output$mpgTable <- renderDataTable(DT::datatable({
    mpg_table <-mpgTable()
    mpg_table
    
  }))
  output$costTable <- renderDataTable(DT::datatable({
    cost_table <-costTable()
    cost_table
    
  }))
  
  hist_d_ghg <- reactive({
    if(!(is.null(input$make))){
      vehicles %>% select(Manufacture, Model, Model_Year, Fuel_Type, GHG) %>%
        filter(GHG!= -1 & Fuel_Type %in% input$fuelType & Manufacture %in% input$make)
    }else{
      vehicles %>% select(Manufacture, Model, Model_Year, Fuel_Type, GHG) %>%
        filter(GHG!= -1 & Fuel_Type %in% input$fuelType)
    }
    
  })
  hist_d_mpg <- reactive({
    if(!(is.null(input$make))){
      vhc_change_nonElec %>% select(Manufacture, Model, Model_Year, Fuel_Type, MPG) %>%
        filter(Fuel_Type %in% input$fuelType & Manufacture %in% input$make)
    }else{
      vhc_change_nonElec %>% select(Manufacture, Model, Model_Year, Fuel_Type, MPG) %>%
        filter(Fuel_Type %in% input$fuelType)
    }
    
  })
  hist_d_cost <- reactive({
    if(!(is.null(input$make))){
      vhc_change_nonElec %>% select(Manufacture, Model, Model_Year, Fuel_Type, Annual_Fuel_Cost) %>%
        filter(Fuel_Type %in% input$fuelType & Manufacture %in% input$make)
    }else{
      vhc_change_nonElec %>% select(Manufacture, Model, Model_Year, Fuel_Type, Annual_Fuel_Cost) %>%
        filter(Fuel_Type %in% input$fuelType)
    }
    
  })
  output$distGHG <- renderPlotly({
    ghg_hist <- plot_ly(hist_d_ghg(), x = ~GHG, type = "histogram",
                        marker = list(color = "lightgray", 
                                      line = list(color = "darkgray", width = 2)),
                        hovertemplate = paste(
                          "%{xaxis.title.text}: %{x}<br>",
                          "Count: %{y:,.0f}<br><extra></extra>")) %>%
      layout(barmode="overlay",bargap=0.1,
             title = "Distribution of GHG",hovermode="unified")
    
    ghg_hist
  })
  output$distMPG <- renderPlotly({
    ghg_hist <- plot_ly(hist_d_mpg(), x = ~MPG, type = "histogram",
                        marker = list(color = "lightgray", 
                                      line = list(color = "darkgray", width = 2)),
                        hovertemplate = paste(
                          "%{xaxis.title.text}: %{x}<br>",
                          "Count: %{y:,.0f}<br><extra></extra>")) %>%
      layout(barmode="overlay",bargap=0.1,
             title = "Distribution of MPG",hovermode="unified")
    
    ghg_hist
  })
  output$distCost <- renderPlotly({
    ghg_hist <- plot_ly(hist_d_cost(), x = ~Annual_Fuel_Cost, type = "histogram",
                        marker = list(color = "lightgray", 
                                      line = list(color = "darkgray", width = 2)),
                        hovertemplate = paste(
                          "%{xaxis.title.text}: %{x}<br>",
                          "Count: %{y:,.0f}<br><extra></extra>")) %>%
      layout(barmode="overlay",bargap=0.1,
             title = "Distribution of Annual Fuel Cost",hovermode="unified")
    
    ghg_hist
  })
  output$colplot <- renderPlot({
    fuel%>%
      filter(year == input$year)%>%
      filter(fuelType == input$fuel_Type)%>%
      select(make, city08, model,fuelType, ghgScore,fuelCost08)%>%
      unite(make, model, col = "cartype", sep =" ")%>%
      unique() ->a
    if (input$green == TRUE & input$trany == TRUE){
      fuel%>%
        filter(year == input$year)%>%
        filter(fuelType == input$fuel_Type)%>%
        select(make, city08, model,fuelType, ghgScore,trany)%>%
        unite(make, model, trany, col = "cartype", sep =" ")%>%
        unique()%>%
        slice_max(city08,n =input$bins)%>%
        ggplot(aes(x = cartype, y = city08, fill = factor(ghgScore)))+
        geom_col()+
        coord_flip()+
        ylab("miles per gallon")+
        theme_bw()
    }
    else if (input$green == TRUE){
      a%>%
        slice_max(city08,n =input$bins)%>%
        ggplot(aes(x = cartype, y = city08, fill = factor(ghgScore)))+
        geom_col()+
        coord_flip()+
        ylab("miles per gallon")+
        theme_bw()
    }
    else if (input$trany == TRUE){
      fuel%>%
        filter(year == input$year)%>%
        filter(fuelType == input$fuel_Type)%>%
        select(make, city08, model,fuelType, ghgScore,trany)%>%
        unite(make, model, trany, col = "cartype", sep =" ")%>%
        unique()%>%
        slice_max(city08,n =input$bins)%>%
        ggplot(aes(x = cartype, y = city08))+
        geom_col()+
        coord_flip()+
        ylab("miles per gallon")+
        theme_bw()
    }
    else if (input$cost == TRUE){
      a%>%
        slice_max(city08,n =input$bins)%>%
        ggplot(aes(x = cartype, y = city08, fill = as.factor(fuelCost08)))+
        geom_col()+
        coord_flip()+
        ylab("miles per gallon")+
        theme_bw()
    }
    else{
      a%>%
        slice_max(city08,n =input$bins)%>%
        ggplot(aes(x = cartype, y = city08))+
        geom_col()+
        coord_flip()+
        ylab("miles per gallon")+
        theme_bw()
    }
  })
  output$plot_1 <- renderPlot({
    fuel%>%
      filter(year == input$year)%>%
      filter(fuelType == input$fuel_Type_1)%>%
      select(make, city08, model,fuelType, ghgScore, fuelCost08)%>%
      unite(make, model, col = "cartype", sep =" ")%>%
      unique() ->a
    if (input$green == TRUE & input$trany == TRUE){
      fuel%>%
        filter(year == input$year)%>%
        filter(fuelType == input$fuel_Type_1)%>%
        select(make, city08, model,fuelType, ghgScore,trany)%>%
        unite(make, model, trany, col = "cartype", sep =" ")%>%
        unique()%>%
        slice_max(city08,n =input$bins)%>%
        ggplot(aes(x = cartype, y = city08, fill = factor(ghgScore)))+
        geom_col()+
        coord_flip()+
        ylab("miles per gallon")+
        theme_bw()
    }
    else if (input$green == TRUE){
      a%>%
        slice_max(city08,n =input$bins)%>%
        ggplot(aes(x = cartype, y = city08, fill = factor(ghgScore)))+
        geom_col()+
        coord_flip()+
        ylab("miles per gallon")+
        theme_bw()
    }
    else if (input$trany == TRUE){
      fuel%>%
        filter(year == input$year)%>%
        filter(fuelType == input$fuel_Type_1)%>%
        select(make, city08, model,fuelType, ghgScore,trany)%>%
        unite(make, model, trany, col = "cartype", sep =" ")%>%
        unique()%>%
        slice_max(city08,n =input$bins)%>%
        ggplot(aes(x = cartype, y = city08))+
        geom_col()+
        coord_flip()+
        ylab("miles per gallon")+
        theme_bw()
    }
    else if (input$cost == TRUE){
      a%>%
        slice_max(city08,n =input$bins)%>%
        ggplot(aes(x = cartype, y = city08, fill = as.factor(fuelCost08)))+
        geom_col()+
        coord_flip()+
        ylab("miles per gallon")+
        theme_bw()
    }
    else{
      a%>%
        slice_max(city08,n =input$bins)%>%
        ggplot(aes(x = cartype, y = city08))+
        geom_col()+
        coord_flip()+
        ylab("miles per gallon")+
        theme_bw()
    }
  })
  output$plot_2 <- renderPlot({
    if (input$log == TRUE){
      fuel_2%>%
        ggplot(aes( x= !!input$value_1, y= !!input$value_2))+
        geom_point()+
        geom_smooth(method = lm, se = FALSE)+
        scale_x_log10()+
        scale_y_log10()+
        theme_bw()
    }
    else {
      fuel_2%>%
        ggplot(aes( x= !!input$value_1, y= !!input$value_2))+
        geom_point()+
        geom_smooth(method = lm, se = FALSE)+
        theme_bw()}
  })
  output$plot_3 <- renderPrint({
    lm(fuel_2[[input$value_2]]~fuel_2[[input$value_1]])%>%
      summary()
  })
  x1_name <- reactive({
    input$value_1
  })
  x <- reactive({
    lm(fuel_2[[input$value_2]]~fuel_2[[input$value_1]]) -> b
    data.frame(x1 = c(input$xvalue))-> df
    #colnames(df) <- input$value_1
    #df%>%
    #rename(x1_name() = "x1")->df
    #df
  })
  output$plot_4 <- renderPrint({
    lm(fuel_2[[input$value_2]]~fuel_2[[input$value_1]]) ->b
    coef(b)[1] +coef(b)[2]*input$xvalue -> c
    c[[1]]
    
  })
}
# Run the application 
shinyApp(ui = ui, server = server)


