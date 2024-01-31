




# get the data from https://www.imdpune.gov.in/lrfindex.php
install.packages("ncdf4") # for manipulating netcdf files
install.packages("tidygeocoder") # for reverse geocoding


library(raster)
library(ncdf4)
library(tidygeocoder)

# Use brick() to convert the netCDF file to RasterBrick object
r_2021 <- brick("RF25_ind2021_rfp25.nc")
image(r_2021$X43831)

r_2021_df <- as.data.frame(rasterToPoints(r_2021, spatial = T))
names(r_2021_df)
View(r_2021_df)

library(tidyverse)
r_2021_df %>% 
  reverse_geocode(lat = y, long = x, address = "address", 
                  method = 'osm', full_results = F) -> r
write.csv(r, "geocoded.csv")

###############
states <-  c('Andaman & Nicobar Islands',
             'Andhra Pradesh',
             'Arunachal Pradesh',
             'Assam',
             'Bihar',
             'Chandigarh',
             'Chhattisgarh',
           'Dadra & Nagar Haveli',
              'Daman & Diu',
            'Delhi',
              'Goa',
              'Gujarat',
             'Haryana',
            'Himachal Pradesh',
             'Jammu & Kashmir',
           'Jharkhand',
              'Karnataka',
              'Kerala',
            'Lakshadweep',
              'Madhya Pradesh',
             'Maharashtra',
             'Manipur',
             'Meghalaya',
              'Mizoram',
            'Nagaland',
            'Odisha',
           'Puducherry',
        'Punjab',
           'Rajasthan',
            'Sikkim',
           'Tamil Nadu',
               'Telengana',
             'Tripura',
            'Uttarakhand',
             'Uttarpradesh',
           'West Bengal' )

result <- list()
readxl::read_xlsx("geocoded.xlsx", sheet = "avg")-> r0
r0 <- r0 %>% relocate(x, y, `Average rainfall`, address)
for (i in 1:length(states)){
  r1 = r0 %>% 
    rowwise() %>% 
    ungroup() %>% 
    filter(stringi::stri_detect_fixed(address, states[i])) 
  result <- append(result, r1)
}


x <- c()
for (i in seq(1,length(result), by = 4)){
  x1 = rbind(unlist(result[i]))
  x = append(x, x1)
}

y <- c()
for (i in seq(2,length(result), by = 4)){
  y1 = rbind(unlist(result[i]))
  y = append(y, y1)
}

rainfall <- c()
for (i in seq(3,length(result), by = 4)){
  rainfall1 = rbind(unlist(result[i]))
  rainfall = append(rainfall, rainfall1)
}

address <- c()
for (i in seq(4,length(result), by = 4)){
  address1 = rbind(unlist(result[i]))
  address = append(address, address1)
}

# Lets create a tibble now
rain <- tibble(x, y, rainfall, address)
#########################


Rain <- rain %>% 
  mutate(state = 
  case_when(
    str_detect(address, fixed(states[1])) ~ states[1],
    str_detect(address, fixed(states[2])) ~ states[2],
    str_detect(address, fixed(states[3])) ~ states[3],
    str_detect(address, fixed(states[4])) ~ states[4],
    str_detect(address, fixed(states[5])) ~ states[5],
    str_detect(address, fixed(states[6])) ~ states[6],
    str_detect(address, fixed(states[7])) ~ states[7],
    str_detect(address, fixed(states[8])) ~ states[8],
    str_detect(address, fixed(states[9])) ~ states[9],
    str_detect(address, fixed(states[10])) ~ states[10],
    str_detect(address, fixed(states[11])) ~ states[11],
    str_detect(address, fixed(states[12])) ~ states[12],
    str_detect(address, fixed(states[13])) ~ states[13],
    str_detect(address, fixed(states[14])) ~ states[14],
    str_detect(address, fixed(states[15])) ~ states[15],
    str_detect(address, fixed(states[16])) ~ states[16],
    str_detect(address, fixed(states[17])) ~ states[17],
    str_detect(address, fixed(states[18])) ~ states[18],
    str_detect(address, fixed(states[19])) ~ states[19],
    str_detect(address, fixed(states[20])) ~ states[20],
    str_detect(address, fixed(states[21])) ~ states[21],
    str_detect(address, fixed(states[22])) ~ states[22],
    str_detect(address, fixed(states[23])) ~ states[23],
    str_detect(address, fixed(states[24])) ~ states[24],
    str_detect(address, fixed(states[25])) ~ states[25],
    str_detect(address, fixed(states[26])) ~ states[26],
    str_detect(address, fixed(states[27])) ~ states[27],
    str_detect(address, fixed(states[28])) ~ states[28],
    str_detect(address, fixed(states[29])) ~ states[29],
    str_detect(address, fixed(states[30])) ~ states[30],
    str_detect(address, fixed(states[31])) ~ states[31],
    str_detect(address, fixed(states[32])) ~ states[32],
    str_detect(address, fixed(states[33])) ~ states[33],
    str_detect(address, fixed(states[34])) ~ states[34],
    str_detect(address, fixed(states[35])) ~ states[35],
    str_detect(address, fixed(states[36])) ~ states[36]
  )) %>% 
  select(-address)
Rain
write.csv(Rain, "Rain.csv")
