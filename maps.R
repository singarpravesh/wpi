library(sf)
districts_shp <- read_sf("DISTRICT_BOUNDARY.shp")
names(districts_shp)

library(ggplot2)
library(tidyverse)

plot(districts_shp$geometry)

districts_names_shp <- districts_shp$District


districts_nsso <- readxl::read_excel("master_dataset.xlsx", sheet = "district_name")

districts_names_nsso <- districts_nsso$`District Name` %>% 
  str_to_upper(locale = "en") %>% sort()

districts_names_shp %>% 
  as_tibble() %>% 
  rename(names = value) %>% 
  arrange(names) -> districts_names_shp_1

# clean the data
for (i in 1:nrow(districts_names_shp_1)){
  districts_names_shp_1$names_cleaned[i] = mgsub::mgsub(string = districts_names_shp_1$names[i], 
                                    pattern = c(">", "\\|", "\\@"), replacement = c("A", "I", "U"))
  }
districts_names_shp_1 

match(districts_names_shp_1$names_cleaned, districts_names_nsso )

for (i in 1:nrow(districts_names_shp_1)){
  for (j in 1:nrow(districts_names_nsso)){
  districts_names_shp_1$final_district[i] <- if_else(districts_names_shp_1[i] %in% districts_names_nsso[j],
                                                                    districts_names_shp_1[i],
                                                                    "No")
  }
}

