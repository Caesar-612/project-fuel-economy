# Packages
library(tidyverse)
library(stringr)

# Data Ingest
vehicles = read_csv("../data/vehicles.csv")
fuel = read_csv("../data/vehicles.csv")

vehicles_select_cols <- c("id", "make", "model", "year", "phevBlended", 
                          "drive","cylinders","displ","fuelType", "fuelType1", "fuelType2", 
                          "city08","cityA08", "cityE", "cityCD", 
                          "highway08", "highwayA08", "highwayE", "highwayCD", 
                          "comb08", "combA08", "combE","combinedCD", 
                          "fuelCost08", "fuelCostA08", 
                          "ghgScore", "ghgScoreA", 
                          "co2TailpipeAGpm", "co2TailpipeGpm", "youSaveSpend")
vehicles <- vehicles %>% select(vehicles_select_cols)

# Data Wrangling
# Premiun, Regular, Diesel, Gasline or E85,Electricity, Hybrid & spicial fuel
vehicles <- vehicles %>% mutate(Fuel_Type= recode(fuelType, "Regular" = "Regular", "Premium" = "Premium",
                                                  "Diesel" = "Diesel", "Electricity" = "Electricity",
                                                  "Gasoline or E85" = "Gasoline or E85", 
                                                  "Midgrade" = "Hybrid & Spicial",
                                                  "CNG" = "Hybrid & Spicial",
                                                  "Premium or E85" = "Hybrid & Spicial",
                                                  "Gasoline or natural gas" = "Hybrid & Spicial",
                                                  "Gasoline or propane" = "Hybrid & Spicial",
                                                  "Regular Gas or Electricity"= "Hybrid & Spicial",
                                                  "Premium and Electricity" = "Hybrid & Spicial",
                                                  "Premium Gas or Electricity" = "Hybrid & Spicial",
                                                  "Regular Gas and Electricity" = "Hybrid & Spicial"))

# Create Car_Type column
vehicles <- vehicles %>% mutate(make_new = make, model_new = model)
vehicles <- unite(vehicles, make_new, model_new, col="Car_Type", sep = "-")

# Round Fuel Cost
vehicles$fuelCost08 <- round(vehicles$fuelCost08, 2)
vehicles$fuelCostA08 <- round(vehicles$fuelCost08, 2)
# Rename Co2
vehicles <- vehicles %>% rename("Manufacture" = "make", 
                                "Model" = "model", 
                                "Model_Year" ="year", 
                                "Drive"= "drive", 
                                "CYL"="cylinders",
                                "DSPL" ="displ",
                                "Annual_Fuel_Cost"="fuelCost08", 
                                "Annual_Fuel_Cost_A"="fuelCostA08", 
                                "GHG" = "ghgScore", 
                                "GHG_A" = "ghgScoreA", 
                                "CO2_Tail_Pipe_A" = "co2TailpipeAGpm", 
                                "CO2_Tail_Pipe" = "co2TailpipeGpm", 
                                "Save_Spend" ="youSaveSpend")

# Manufacture Names
manufacture_list <- vehicles %>% select(Manufacture) %>% unique() %>% pull(Manufacture) %>% sort()
# manufacture_list_positive <- vehicles %>% filter(GHG !=-1) %>% select(Manufacture) %>% unique() %>% pull(Manufacture) %>% sort()
# Year Range
year_list <- vehicles %>% select(Model_Year) %>% unique()%>% pull(Model_Year) %>% sort()

# Fuel Type
fuelType_list <- vehicles %>% select(Fuel_Type) %>% unique() %>% pull(Fuel_Type)
fuelType_list_without_E <- vehicles %>% select(Fuel_Type) %>% unique() %>% filter(Fuel_Type!="Electricity") %>% pull(Fuel_Type)
# fuelType_list <- vehicles %>% select(Fuel_Type) %>% unique() %>% pull(Fuel_Type)

#Road Type
roadType <- c("City", "Highway", "Combine")

# Non-Electricity
vhc_change_nonElec <- vehicles %>% filter(Fuel_Type != "Electricity") %>% select(Manufacture, Model, Car_Type, Model_Year, Fuel_Type, city08, highway08, comb08, DSPL, GHG, CO2_Tail_Pipe, Annual_Fuel_Cost) %>% 
  pivot_longer(cols = c("city08", "highway08", "comb08"), names_to = "roadType", values_to = "MPG") %>%
  distinct() %>% mutate(Road_Type = recode(roadType,"city08" = "City", "highway08" ="Highway", "comb08"="Combine")) %>% select(-roadType)

# Electricity
vhc_change_E <- vehicles %>% filter(Fuel_Type == "Electricity") %>% select(Car_Type, Model_Year, Fuel_Type, cityE, highwayE, combE, GHG, CO2_Tail_Pipe, Annual_Fuel_Cost) %>% 
  pivot_longer(cols = c("cityE", "highwayE", "combE"), names_to = "roadType", values_to = "Electricity_Consumption") %>%
  distinct() %>% mutate(Road_Type = recode(roadType,"cityE" = "City", "highwayE" ="Highway", "combE"="Combine")) %>% select(-roadType)

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
  select(city08,ghgScore,displ,fuelCost08,co2,cylinders)-> fuel_2
fuel%>%
  select(make)%>%
  unique() -> brand
