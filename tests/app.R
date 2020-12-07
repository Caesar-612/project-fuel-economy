#' Roxygen style comments
#' This structure supports later package documentation

library(tidyverse)
library(shiny)
library(broom)
library(shinythemes)
library(fresh)
library(stats)
fuel <-  read_csv("../data/vehicles.csv")
fuel%>%
  select(fuelType)%>%
  mutate_all(as.factor)%>%
  mutate_all(~fct_lump(.x, n=5))%>%
  unique()%>%
  filter(fuelType != 'Other')-> fuel_1

fuel%>%
  select(year)%>%
  filter(year > 2000)%>%
  unique()-> fuel_year

fuel%>%
  keep(is.numeric)%>%
  select(city08,ghgScore,displ,fuelCost08,cylinders,co2)-> fuel_2

ui <- fluidPage(
  
  titlePanel("Fuel economy"), theme = shinytheme("superhero"),
  tabsetPanel(
    tabPanel("Cartype recommend",
             sidebarLayout(
               sidebarPanel(
                 selectInput("fuel_Type", "What fuel type are you looking for?", choices =  fuel_1$fuelType),
                 selectInput("fuel_Type_1", "What another fuel type are you looking for?", choices =  fuel_1$fuelType),
                 selectInput("year", "whcih year?", choices = fuel_year$year, selected = "2020"),
                 checkboxInput("green", "Green House Score"),
                 checkboxInput("trany", "transmission of the car"),
                 sliderInput("bins",
                             "TOP ?",
                             min = 5,
                             max = 50,
                             value = 5)
               ),
               mainPanel(plotOutput("barplot"),
                         plotOutput("plot_1")
               ))),
    tabPanel("displ vs fuelcost08",
             sidebarLayout(
               sidebarPanel(
                 
               ),
               mainPanel()
             )),
    tabPanel("Statistical model",
             sidebarLayout(
               sidebarPanel(
                varSelectInput("value_1", "X value?", data =  fuel_2),
                varSelectInput("value_2", "Y value?", data =  fuel_2),
                checkboxInput("log", "better to observe"),
                numericInput("xvalue", "Predict Y base on X value", value = 10, min = 0, max = 100)
               ),
               mainPanel(plotOutput("plot_2"),
                         verbatimTextOutput("plot_3"),
                         verbatimTextOutput("plot_4")
                         
               )
             ))
  ))

server <- function(input, output, session) {
  output$barplot <- renderPlot({
    fuel%>%
      filter(year == input$year)%>%
      filter(fuelType == input$fuel_Type)%>%
      select(make, city08, model,fuelType, ghgScore)%>%
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
      select(make, city08, model,fuelType, ghgScore)%>%
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
    coef(b)[1] +coef(b)[2]*input$xvalue

  })
}

shinyApp(ui, server)
