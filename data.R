# import the data
library(haven)

d2 <- read_dta("R76120L02-Demographic and other particulars of household members.dta",
               col_select = c('HHID', 'Gender', "Age",
                              "Marital_status",
                              "Highes_education", "latrine_code", 
                              "type_latrine_used", 
                              "latrine_used_household"))
d3 <- read_dta("R76120L03-Household characteristics.dta",
               col_select = c('HHID','Total_Monthly_expenditure'))

d5 <- read_dta("R76120L05-Particulars of living facilities- drinking water, bathroom, sanitation, etc.dta",
               col_select = c('HHID','water_sufficient_drink',
                              'water_sufficient',
                              'avg_amoun_month_rs',
                              'Distance_source_water',
                              'source_drinking_water',
                              'source_water_notdrinking',
                              'Access_source_water',
                              'Acces_bathroom',
                              'bathroom_latrine_inpremises',
                              'Access_latrine', 'latrine_type',
                              'avg_amoun_month_rs',
                              'Method_treatment',
                              'is_stagnant_water_around',
                              'availability_water_inlatrine'))
d6 <- read_dta('R76120L06-Housing characteristics and micro environment of the households living in houses.dta',
               col_select = c('HHID', 'Sector', 'District', 'State',
                              'waste_disposal_system',
                              'agency_garbage_collection'))
library(tidyverse)
com_data_76 <- d2 |> 
  left_join(d3, join_by(HHID)) |> 
  left_join(d5, join_by(HHID)) |> 
  left_join(d6, join_by(HHID)) |> 
  relocate(HHID)

write.csv(com_data_76, "com_data_76.csv")

com_data |> names()
table(com_data$Frequency_water_supply[1:50])
