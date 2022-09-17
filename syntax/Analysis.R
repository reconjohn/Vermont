
# load PV data
# load ACS data - "VT_acs_prop"
load("./data/derived/ACS.Rdata")
load("./data/derived/PV.Rdata")
source("./syntax/Function.R")

Agg_data <- function(PV, census_data){
  
  PV <- st_as_sf(PV, coords = c("Long", "Lat"), crs = 4326,
                 agr = "constant",
                 stringsAsFactors = FALSE,
                 remove = TRUE)
  
  # st_crs(PV)
  
  census <- census_data %>% st_transform(4326)
  
  # PV
  in_tract <- st_join(PV, census, join = st_within)
  # join sf objects based on geometry, point data joined with polygons  
  
  # PV count per census tract
  tract_count <- count(as_tibble(in_tract), GEOID)
  
  # ACS joined with PV count
  tract_sf <- left_join(census, tract_count) %>% 
    rename(PV = n)
  
  
  tract_sf[is.na(tract_sf[["PV"]]),"PV"] <- 0 # fill zero at empty value
  
  N = sum(tract_sf$Unit) # total number of housing units
  ExpectedPV = sum(tract_sf$PV, na.rm = T)/ N # total number of PV/ N, expected pv per one housing unit
  
  regrs <- tract_sf %>%
    mutate(Exp_PV = Unit*ExpectedPV, # expected PV for the census tract
           SIR_PV = PV/ Exp_PV)
  
  return(regrs)
}

regrs <- Agg_data(data, VT_acs_prop) 

library(geojsonsf)
VT_PV <- data %>% 
  group_by(City, Year) %>% 
  summarise(Installation = log(n())) %>% 
  left_join(regrs %>% 
              dplyr::select(City)) %>% 
  st_sf()

st_write(VT_PV, "./data/derived/geo.geojson")


VT_PV %>% 
  ggplot(aes(x = Year, y = Installation)) +
  geom_point()


plot(density(log(VT_PV$Installation)))

