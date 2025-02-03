#Setup --------------
library(tidycensus)
library(tidyverse)
library(cmapgeo)
library(sf)

options(scipen = 999)

# CensusKey <- "[your API key goes here]"
# census_api_key(CensusKey,install=TRUE,overwrite=TRUE)

Wards_geo <- st_read("Reference/Ward Boundaries/Wards.shp") |> 
  st_transform("NAD83") |> 
  rename(Ward = ward)

Alders <- read_csv("Reference/Current Council.csv")

Tracts_geo <- get_acs(year = 2023, geography = "tract", state = "IL", county = "Cook", variables = "B01001_001", geometry = TRUE) 

Wards_Tracts_Intersect <- st_intersects(Wards_geo$geometry, Tracts_geo$geometry) |> 
  data.frame() |> 
  rename(Ward = row.id) |> 
  inner_join(data.frame(GEOID = Tracts_geo$GEOID) |> 
               mutate(col.id = row_number())) 

Tracts_geo_Ward <- Tracts_geo |> 
  inner_join(Wards_Tracts_Intersect)

BG_geo <- get_acs(year = 2023, geography = "block group", state = "IL", county = "Cook", variables = "B01001_001", geometry = TRUE) 

Wards_BG_Intersect <- st_intersects(Wards_geo$geometry, BG_geo$geometry) |> 
  data.frame() |> 
  rename(Ward = row.id) |> 
  inner_join(data.frame(GEOID = BG_geo$GEOID) |> 
               mutate(col.id = row_number()))

BG_geo_Ward <- BG_geo |> 
  inner_join(Wards_BG_Intersect)

Block_geo <- get_decennial(year = 2020, geography = "block", state = "IL", county = "Cook", variables = "H1_001N", geometry = TRUE) 

Wards_Block_Intersect <- st_intersects(Wards_geo$geometry, Block_geo$geometry) |> 
  data.frame() |> 
  rename(Ward = row.id) |> 
  inner_join(data.frame(GEOID = Block_geo$GEOID) |> 
               mutate(col.id = row_number()))

Block_geo_Ward <- Block_geo |> 
  inner_join(Wards_Block_Intersect)

#Visualize disparity between census geometry and Ward boundaries

Ward_Compare <- 2

ggplot() +
  geom_sf(data = Tracts_geo_Ward |> filter(Ward == Ward_Compare) |> pull(geometry)) +
  geom_sf(data = Wards_geo |> filter(Ward == Ward_Compare) |> pull(geometry), fill = "red", alpha = .7)

ggplot() +
  geom_sf(data = BG_geo_Ward |> filter(Ward == Ward_Compare) |> pull(geometry)) +
  geom_sf(data = Wards_geo |> filter(Ward == Ward_Compare) |> pull(geometry), fill = "red", alpha = .7)

ggplot() +
  geom_sf(data = Block_geo_Ward |> filter(Ward == Ward_Compare) |> pull(geometry)) +
  geom_sf(data = Wards_geo |> filter(Ward == Ward_Compare) |> pull(geometry), fill = "red", alpha = .7)
