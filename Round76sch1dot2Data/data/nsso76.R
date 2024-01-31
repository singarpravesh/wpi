# nsso 76 
setwd("C:\\Users\\pravesh\\Desktop\\Project2023-24\\Round76sch1dot2Data\\data")
library(haven)

## Read data
read_dta("R76120L01-Identification of sample household.dta",
         col_select = c("Sector","HHID", "District", "State")) -> l1
names(l1)

read_dta("R76120L02-Demographic and other particulars of household members.dta", 
         col_select = c("Sector", "HHID", "District", "State", "Relationship_head", "Gender",
                        "Age", "Marital_status", "Highes_education", 
                        "type_latrine_used")) -> l2

read_dta("R76120L03-Household characteristics.dta",
         col_select = c("Sector", "HHID", "District", "State", "Total_Monthly_expenditure",
                        "HH_size", "Religion", "Social_group" )) -> l3

read_dta("R76120L05-Particulars of living facilities- drinking water, bathroom, sanitation, etc.dta",
         col_select = c("Sector", "HHID", "District", "State","Acces_bathroom","water_sufficient_drink",
                    "Access_source_water", "Distance_source_water", "Method_treatment")) -> l4




## Join data
library(tidyverse)
l1 %>% 
  left_join(l2, join_by(Sector, HHID, State, District)) %>% 
  left_join(l3, join_by(Sector, HHID, State, District)) %>% 
  left_join(l4, join_by(Sector, HHID, State, District)) -> final_data

# number of rural and urban units
l1 %>% 
  group_by(Sector) %>% 
  summarise(n())

# Wrangling
# Change the state codes to state names

fd = final_data %>% 
  mutate(state = case_when(
    State == "35" ~ 'Andaman & Nicobar Islands',
    State == '28' ~ 'Andhra Pradesh',
    State == '12' ~ 'Arunachal Pradesh',
    State == '18' ~ 'Assam',
    State == '10' ~ 'Bihar',
    State == '04' ~ 'Chandigarh',
    State == '22' ~ 'Chhattisgarh',
    State == '26' ~ 'Dadra & Nagar Haveli',
    State == '25' ~ 'Daman & Diu',
    State == '07' ~ 'Delhi',
    State == '30' ~ 'Goa',
    State == '24' ~ 'Gujarat',
    State == '06' ~ 'Haryana',
    State == '02' ~ 'Himachal Pradesh',
    State == '01' ~ 'Jammu & Kashmir',
    State == '20' ~ 'Jharkhand',
    State == '29' ~ 'Karnataka',
    State == '32' ~ 'Kerala',
    State == '31' ~ 'Lakshadweep',
    State == '23' ~ 'Madhya Pradesh',
    State == '27' ~ 'Maharashtra',
    State == '14' ~ 'Manipur',
    State == '17' ~ 'Meghalaya',
    State == '15' ~ 'Mizoram',
    State == '13' ~ 'Nagaland',
    State == '21' ~ 'Odisha',
    State == '34' ~ 'Puducherry',
    State == '03' ~ 'Punjab',
    State == '08' ~ 'Rajasthan',
    State == '11' ~ 'Sikkim',
    State == '33' ~ 'Tamil Nadu',
    State == '36' ~ 'Telengana',
    State == '16' ~ 'Tripura',
    State == '05' ~ 'Uttarakhand',
    State == '09' ~ 'Uttarpradesh',
    State == '19' ~ 'West Bengal'
  ))

# Difference in average monthly expenditures in urban and rural areas 
final_data %>% 
  group_by(Sector) %>% 
  summarise(mean(Total_Monthly_expenditure))

write.csv(fd, "nsso76.csv")

as_tibble(read.csv("rain.csv"))-> rain

table(rain$state)
table(fd$state)
rain %>% 
  group_by(state) %>% 
  summarize(mean(rainfall)) %>% View()

# join
fd %>% 
  left_join(rain[,-1], join_by(state), relationship = "many-to-many") -> combined_final_data

View(combined_final_data)
