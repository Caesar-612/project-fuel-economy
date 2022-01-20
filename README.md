### Use case

a) The aim of the application is to find a friendly car type for "customers" by analyzing the GHG Score, Fuel Cost and Energy Cost in terms of manufactures, fuel types, model year, and car types.

b) The application is designed for people who have the interest to buy a car. 

### Required Packages

a) For running this app, we need our users to run the the following packages:
   * shiny - For app
   * shinydashboard - customize UI
   * shinyWidgets - setting screen
   * shinythemes - themes
   * DT - For data table
   * plotly - For plot
   * tidyverse - For tidy
   * reactlog - Reactive Visualizer
   * lazyeval - For plot
   * 
### Data Source and Structure

a) Data source
   * www.fueleconomy.gov
   * There are 43098 rows and 83 columns. The data shows lots of information about the vehicle such as model, make, fuel type, as a categorical variable and city08, ghgscore, fuelCost08 as quantitative variable. In fueltype, the data separate into 16 variables. The unknow value inside the data is displayed as NA.
