
# read the shape file
library(sf)
districts_shp <- read_sf("DISTRICT_BOUNDARY.shp")
names(districts_shp)

library(tidyverse)

# arrange the districts alphabetically
districts_shp <- districts_shp |> 
  arrange(District)
  
# clean the data
for (i in 1:nrow(districts_shp)){
  districts_shp$district[i] = 
    mgsub::mgsub(string = districts_shp$District[i], 
                 pattern = c(">", "\\|", "\\@"), 
                 replacement = c("A", "I", "U"))
}
shp_districts <- districts_shp |> select(district, geometry)

# read the nsso data
districts_nsso_raw <- 
  readr::read_tsv("master_dataset.txt")

# data cleaning
a <- districts_nsso_raw |> 
  rename(district = `District name`,
         wpi = WPI_multiplicative,
         resource = RESOURCE,
         access = Access,
         use = USE,
         capacity = Capacity,
         environment = Environment,
         wpi_pca = WPI_pca) |> 
  mutate(Sector = as.factor(Sector)) |> 
  arrange(district) |> 
  # remove #N/A string in district
  filter(!(district %in% "#N/A")) |> 
  # change the names to upper case
  mutate(district = str_to_upper(string = district, locale = "en"))

nsso_districts <- a |> 
  group_by(district) |> 
  summarise_at(vars(resource:wpi_pca), median)

# merging
final_districts <- left_join(nsso_districts, shp_districts, by = "district")
# convert to sf object
sf_final_districts <- st_sf(final_districts, sf_column_name = "geometry")

# mapping
# wpi map
wpi_map <- ggplot()+
  geom_sf(data = shp_districts, fill = "black")+
  geom_sf(aes(fill = wpi), data = sf_final_districts)+
  scale_fill_distiller(palette = 5, type = "div")
ggsave("wpi_map.png")


# wpi_pca map
wpi_pca_map <- ggplot()+
  geom_sf(data = shp_districts, fill = "black")+
  geom_sf(aes(fill = wpi_pca), 
          data = sf_final_districts)+
  scale_fill_distiller(palette = 5, type = "div", direction = -1)
ggsave("wpi_pca_map.png")

# resource map
resource_map <- ggplot()+
  geom_sf(data = shp_districts, fill = "black")+
  geom_sf(aes(fill = resource), data = sf_final_districts)+
  scale_fill_distiller(palette = 5, type = "div")
ggsave("resource_map.png")


# use map
use_map <- ggplot()+
  geom_sf(data = shp_districts, fill = "black")+
  geom_sf(aes(fill = use), data = sf_final_districts)+
  scale_fill_distiller(palette = 5, type = "div")
ggsave("use_map.png")

# environment map
environment_map <- ggplot()+
  geom_sf(data = shp_districts, fill = "black")+
  geom_sf(aes(fill = environment), data = sf_final_districts)+
  scale_fill_distiller(palette = 5, type = "div")
ggsave("environment_map.png")

# access map
access_map <- ggplot()+
  geom_sf(data = shp_districts, fill = "black")+
  geom_sf(aes(fill = access), data = sf_final_districts)+
  scale_fill_distiller(palette = 5, type = "div")
ggsave("access_map.png")

# capacity map
capacity_map <- ggplot()+
  geom_sf(data = shp_districts, fill = "black")+
  geom_sf(aes(fill = capacity), data = sf_final_districts)+
  scale_fill_distiller(palette = 5, type = "div")
ggsave("capacity_map.png")

#######################

# by sector
nsso_districts_sector <- a |> 
  group_by(district, Sector) |> 
  summarise_at(vars(resource:wpi_pca), median)
