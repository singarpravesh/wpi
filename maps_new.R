# read the shape file
library(sf)
state_shp <- read_sf("shapefile_states_india/Admin2.shp")
names(state_shp)

# Join the boundaries of J&K and Ladakh
m1 <- state_shp |> 
  filter(!ST_NM %in% c("Jammu & Kashmir", "Ladakh"))

jk <- state_shp |> 
  filter(ST_NM %in% c("Jammu & Kashmir")) 
  

la <- state_shp |> 
  filter(ST_NM %in% c("Ladakh"))
jk_la <- st_union(jk, la)[, c(1,3)]

state_shp <- rbind(m1, jk_la) 


#--------------------------------
library(tidyverse)
library(ggplot2)
# arrange the states alphabetically
state_shp <- state_shp |> 
  arrange(ST_NM) |>
  rename( state = ST_NM)

# clean the data
#for (i in 1:nrow(districts_shp)){
#  districts_shp$district[i] = 
#    mgsub::mgsub(string = districts_shp$District[i], 
#                 pattern = c(">", "\\|", "\\@"), 
#                 replacement = c("A", "I", "U"))
#}



# read the nsso data# read the nsso dataDISTRICT
state_nsso_raw <- 
  readr::read_csv("data1.csv")

# data cleaning
a <- state_nsso_raw |> 
  rename(state = `Statename`,
         wpi = wpi_add,
         wpi_pca = wpi_pca) |> 
  arrange(state)
# remove #N/A string in district
#filter(!(district %in% "#N/A")) |> 
# change the names to upper case
# mutate(district = str_to_upper(string = district, locale = "en"))

nsso_state <- a |> 
  group_by(state) |> 
  summarise_at(vars(Environment:wpi_pca), mean)

# Join both the UTs together
nsso_state1 <-
  filter(nsso_state, 
         state %in% c("Dadra & Nagar Haveli",
                       "Daman & Diu")) |> 
  select(-state) |> 
  mutate(state = "Dadra and Nagar Haveli and Daman and Diu") |> 
  group_by(state) |> 
  summarize_all(mean)

nsso_state2 <-
  rbind(filter(nsso_state,
               !state %in% c("Dadra & Nagar Haveli",
                           "Daman & Diu")))

# correcting the spelling of Telangana
nsso_state_final <- rbind(nsso_state1, nsso_state2) |> 
  arrange(state) |> 
  mutate(state = ifelse(state == "Telengana", "Telangana", state))

# merging
final_state <- left_join(state_shp, nsso_state_final,  by = "state")
# convert to sf object
sf_final_state <- st_sf(final_state, sf_column_name = "geometry")

# mapping
# wpi map
wpi_map <- ggplot()+
  geom_sf(data = state_shp, fill = "black")+
  geom_sf(aes(fill = wpi), data = sf_final_state)+ 
  scale_fill_distiller(palette = 7, type = "div")
ggsave("wpi_map.png")

# wpi_pca map
wpi_pca_map <- ggplot()+
  geom_sf(data = state_shp, fill = "black")+
  geom_sf(aes(fill = wpi_pca), 
          data = sf_final_state)+
  scale_fill_distiller(palette = 5, type = "div", direction = -1)
ggsave("wpi_pca_map.png")

# resource map
resource_map <- ggplot()+
  geom_sf(data = state_shp, fill = "black")+
  geom_sf(aes(fill = Resource), data = sf_final_state)+
  scale_fill_distiller(palette = 5, type = "div")
ggsave("resource_map.png")


# use map
use_map <- ggplot()+
  geom_sf(data = state_shp, fill = "black")+
  geom_sf(aes(fill = Use), data = sf_final_state)+
  scale_fill_distiller(palette = 5, type = "div")
ggsave("use_map.png")

# environment map
environment_map <- ggplot()+
  geom_sf(data = state_shp, fill = "black")+
  geom_sf(aes(fill = Environment), data = sf_final_state)+
  scale_fill_distiller(palette = 5, type = "div")
ggsave("environment_map.png")

# access map
access_map <- ggplot()+
  geom_sf(data = state_shp, fill = "black")+
  geom_sf(aes(fill = Access), data = sf_final_state)+
  scale_fill_distiller(palette = 5, type = "div")
ggsave("access_map.png")

# capacity map
capacity_map <- ggplot()+
  geom_sf(data = state_shp, fill = "black")+
  geom_sf(aes(fill = Capacity), data = sf_final_state)+
  scale_fill_distiller(palette = 5, type = "div")
ggsave("capacity_map.png")
