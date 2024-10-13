
## ACS
library(tidyverse)
library(jsonlite)
library(tidycensus)
library(sf)
library(mapview)
library(tmap)
library(rmapshaper)
library(sp)
library(spdep)
library(rgdal)
library(maptools)
library(spatstat)

source("./syntax/Function.R")

# census_api_key("a55eefb5e7b0cb86b0b29178435bd74f4cb44c39", install=T, overwrite = T)
# readRenviron("~/.Renviron")

# getting two dataset: 2018 and 2014
var_df <- load_variables(2018, "acs5", cache = TRUE)
# View(var_df)

# Home owner rate
# B25003_001    Estimate!!Total Housing units TENURE
# B25003_002    Estimate!!Total!!Owner occupied
# B25003_003    Estimate!!Total!!Renter occupied
# B25003H_003	  Estimate!!Total!!Renter occupied	TENURE (WHITE ALONE)
# B25026_001    Estimate!!Total population in occupied housing units
# B25026_009    Estimate!!Total population in occupied housing units!!Renter occupied
# B25127_001    Estimate!!Total Occupied houing units 
# 
# Single family housing rate
# B11011_004    Estimate!!Total!!households!!Married-couple family!!1-unit structures
# B11011_002	  Estimate!!Total!!Family households	HOUSEHOLD TYPE BY UNITS IN STRUCTURE
# 
# Education 
# B16010_041    Estimate!!Total!!Bachelor's degree or higher Population
# B16010_001	  Estimate!!Total	TOTAL POPULATION
#
# Home value 
# B25077_001	  Estimate!!Median value (dollars)	Owner Occupied MEDIAN VALUE (DOLLARS)
# B25097_001    Estimate!!Median value!!Total MORTGAGE STATUS BY MEDIAN VALUE (DOLLARS)
# B25107_001    Estimate!!Median value!!Total MEDIAN VALUE BY YEAR STRUCTURE BUILT
#
# Income status 
# B19113_001    Estimate!!Median family income in the past 12 months (in 2015 Inflation-adjusted dollars)
# B19013_001    Estimate!!Median household income in the past 12 months (in 2015 Inflation-adjusted dollars)
# B19013A_001	  Estimate!!Median family income in the past 12 months (in 2018 inflation-adjusted dollars)	MEDIAN FAMILY INCOME IN THE PAST 12 MONTHS (White)
# B19013B_001	  Estimate!!Median family income in the past 12 months (in 2018 inflation-adjusted dollars)	MEDIAN FAMILY INCOME IN THE PAST 12 MONTHS (Black)
#
# Race
# B01001A_001	Estimate!!Total	SEX BY AGE (WHITE ALONE)
# B01001D_001	Estimate!!Total	SEX BY AGE (ASIAN ALONE)
# 
#
# Mortgage 
# B25027_001    Estimate!!Total
# B25027_002    Estimate!!Total!!Housing units with a mortgage
# 
# GINI
# B19083_001    Estimate!!Gini Index
#
# Poverty
# B17001_002	  Estimate!!Total!!Income in the past 12 months below poverty level	POVERTY STATUS IN THE PAST 12 MONTHS BY SEX BY AGE
# B17001_001 
# B06012_004    Estimate!!Total!!At or above 150 percent of the poverty level

variables <- c("B25003_002", "B11011_004","B16010_041","B25077_001", "B19013_001","B19083_001","B25003_001", "B11011_002","B16010_001","B06012_004", "B06012_001","B25081_008", "B25003_003","B01001A_001", "B01001D_001","B01001_001", "B25002_002")


VT_acs <- get_acs("county subdivision", state="VT", year = 2015, geometry = TRUE,
                  variables= variables)

ggplot(VT_acs)+
  geom_sf(fill = "transparent") + 
  theme_minimal() 

library(tigris)
options(tigris_use_cache = TRUE, tigris_class = "sf")

# load places 
VT <- places("VT", year = 2018, class = "sf") 

# get the water bodies of  County
state_map <- counties(state = c("Vermont")) %>% sf::st_as_sf()
VT_counties <- state_map %>% filter(STATEFP == 50) %>% distinct(COUNTYFP) %>% pull(COUNTYFP)


# area_water("VT", county = "011", class = "sf")

# water <-  map_dfr(
#   VT_counties,
#   ~ area_water("VT", county = ., class = "sf") %>% filter(AWATER > 10^6)
# ) %>%
#   st_union() %>% st_sf()
  
VT_acs_prop <- VT_acs %>% 
  dplyr::select(-moe) %>%
  spread(variable, estimate) %>%
  # ms_erase(water) %>%
  dplyr::select(GEOID) %>%
  left_join(VT_acs %>% 
              dplyr::select(-moe) %>%
              spread(variable, estimate) %>%
              st_drop_geometry(), by = "GEOID") %>% 
  mutate(NAME = str_extract(NAME, "([^\\s]+\\s+)+(town|city)"),
         City = ifelse(str_detect(NAME, "(Barre|Rutland|Newport|St. Albans)"),
                       str_extract(NAME, "^.+(town|city)") %>% 
                         str_replace_all(c(" town" = " Town", " city" = " City")),
                       str_extract(NAME, "([^\\s]+\\s+)+(town|city)") %>% 
                         str_replace_all(c(" town" = "", " city" = ""))),
         City = ifelse(str_detect(City, "West Rutland"), "West Rutland", City)) %>% 
  filter(!is.na(City))


ggplot(VT) + 
  geom_sf(fill = "transparent") + 
  theme_minimal() 

ggplot(water) + 
  geom_sf(fill = "transparent") + 
  theme_minimal() 


ggplot(VT_acs_prop) + 
  geom_sf(fill = "transparent") + 
  theme_minimal() 



VT_acs_prop <- VT_acs_prop %>% # remove weird geometries 
  mutate(Unit = B25003_001, # total housing units	
         Area = as.numeric(st_area(.)), # area 
         PopDensity = B01001_001/ Area,
         HomeOwn = B25003_002/ B25003_001, # owner/ total housing units 
         SingleFamily = B11011_004/ B11011_002, 
         # 1-unit structure/ family households
         Edu = 	B16010_041/ B16010_001, 
         # bachelor's degree or higher/ total population over 25 years old
         HomeValue = B25077_001,
         Income = B19013_001,
         White = (B01001A_001)/ B01001_001, 
         # white and asisan/ total population   
         Wealth = B06012_004/ B06012_001,
         # above 150 percent poverty level/ total population
         Gini = B19083_001) %>% 
  dplyr::select(GEOID, City, Unit:Gini) 
# The Gini index, or index of income concentration, is a statistical measure of income inequality ranging from 0 to 1. A measure of 1 indicates perfect inequality, i.e., one household having all the income and rest having none. A measure of 0 indicates perfect equality, i.e., all households having an equal share of income.

# Inspect missing values 
test <- VT_acs_prop[!complete.cases(VT_acs_prop %>% 
                             st_drop_geometry()),]

ggplot(test) + 
  geom_sf(fill = "transparent") + 
  theme_minimal() 


ggplot(test %>% filter(Unit == 0)) + 
  geom_sf(fill = "transparent") + 
  theme_minimal() 


# removing missing values 
VT_acs_prop <- VT_acs_prop %>% 
  filter(Unit != 0)
# income and home value are missing for communities with small population 

# Data overview
library(viridis)
VT_acs_prop %>% 
       dplyr::select(HomeOwn:Gini) %>% 
       mutate_at(vars(HomeOwn:Gini), funs(scale(.) %>% as.numeric(.))) %>% 
  gather("Key", "Value", HomeOwn:Gini) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = Value)) +
  scale_fill_viridis(direction = -1) +
  facet_wrap(~Key, nrow = 2) +
  theme_minimal() 

VT_acs_2015 <- VT_acs_prop  

save(VT_acs_2020,VT_acs_2014,VT_acs_2015,VT_acs_2016,VT_acs_2017,VT_acs_2018,VT_acs_2019, file = "./data/derived/ACS.Rdata")

