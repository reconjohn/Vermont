load("./data/derived/ACS.Rdata") # load ACS data - "VT_acs_prop" "VT_acs_2014"
load("./data/derived/PV.Rdata") # load PV data
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

# Adoption rate data
library(geojsonsf)
VT_PV <- data %>% 
  group_by(City, Year) %>% 
  summarise(Installation = n()) %>% 
  complete(Year = 1999:2019, fill = list(Installation = 0)) %>% 
  mutate(Sum = cumsum(Installation)) %>% 
  left_join(regrs %>% 
              dplyr::select(City, Unit)) %>% 
  mutate(Sum = Sum/ Unit * 100, # % cumulative penetration 
         Installation = Installation/ Unit * 100) %>% # % annual penetration 
  dplyr::select(-Unit) %>% 
  gather("Key", "Value", 3:4) %>% 
  st_sf()

st_write(VT_PV, "./data/derived/geo.geojson") # for mapbox mapping Vermont polygon PV installation 



# Individual town adoption rate (%)
VT_PV %>% 
  filter(Key == "Sum") %>% 
  ggplot(aes(x = Year, y = Value, group = City)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1999, 2019, by = 2)) +
  xlab("Year") + ylab("Adoption rate (%)") +
  theme_bw() 


# Cumulative installation 
data %>% 
  group_by(Year) %>% 
  summarise(Installation = n()) %>% 
  mutate(Sum = cumsum(Installation)) %>%
  ggplot(aes(x = Year, y = Sum)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1999, 2019, by = 2)) +
  xlab("Year") + ylab("Adoption count") +
  theme_bw() 



#################################################################### Tech analysis

## DER data
PV <- read_csv(file = "./data/raw/PV_final.csv") %>% 
  filter(Year != "2020") %>% 
  complete(Town, Year = 1999:2019, fill = list(Rate = 0, Number = 0)) %>% 
  mutate(Tech = "PV")

PV %>% 
  ggplot(aes(x = Year, y = Number)) +
  geom_line() +
  facet_wrap(~Town)+
  theme_minimal()
  

EV <- read_csv(file = "./data/raw/EV_final.csv") %>% 
  complete(Town, Year = 2012:2020, fill = list(Rate = 0, Number = 0)) %>% 
  mutate(Tech = "EV")

EV %>% 
  ggplot(aes(x = Year, y = Number)) +
  geom_line() +
  facet_wrap(~Town)+
  theme_minimal()

Pump <- read_csv(file = "./data/raw/Pump_final.csv") %>% 
  complete(Town, Year = 2015:2020, fill = list(Rate = 0, Number = 0)) %>% 
  mutate(Tech = "Pump")

Pump %>% 
  ggplot(aes(x = Year, y = Number)) +
  geom_line() +
  facet_wrap(~Town)+
  theme_minimal()

tot_data <- rbind(PV, EV, Pump)

# trend of DER comparison 
tot_data %>% 
  filter(Year > 2014 & Year < 2020) %>% 
  group_by(Year, Tech) %>% 
  summarise(Sum = sum(Number)) %>% 
  ggplot(aes(x = Year, y = Sum, color = Tech, group = Tech)) +
  geom_line() +
  scale_x_continuous(breaks = seq(2015, 2019, by = 1)) +
  xlab("Year") + ylab("Adoption count") +
  theme_bw() 


## energy expenditure data
Exp <- read_csv(file = "./data/raw/Expenditure.csv") %>% 
  mutate(Exp = Cost/ 1000)

# aggregating data and adding St. Albans for city and town 
# final count data
temp <- tot_data %>% 
  filter(Year == 2019) %>% 
  dplyr::select(-Rate) %>% 
  pivot_wider(names_from =  Tech, values_from = Number) %>% 
  complete(Town, Year, fill = list(PV = 0, EV = 0, Pump = 0)) %>% 
  rbind(data.frame(Town = c("St. Albans City", "St. Albans Town"),
                   Year = c(2019, 2019),
                   PV = c(round(219*9/17,0),round(219*8/17,0)),
                   EV = c(round(44*9/17,0),round(44*8/17,0)),
                   Pump = c(round(293*9/17,0),round(293*8/17,0)))) %>% # City:Town = 9:8 for PV, EV, and Pump 219, 44, and 293
  filter(Town != "St. Albans") %>% 
  full_join(VT_acs_prop, by = c("Town" = "City")) %>% # missing Brunswick and Ferdinand
  filter(!Town %in% c("Averill", "Granby", "Brunswick", "Ferdinand")) %>%   # removing Averill and Granby
  inner_join(Exp %>% 
               dplyr::select(-GEOID, -Cost), by = c("Town" = "Town")) %>% 
  mutate(r_PV = PV/ Unit,
         r_EV = EV/ Unit,
         r_Pump = Pump/Unit) %>% 
  st_sf()

# 2015 data 
temp_2015 <- tot_data %>% 
  filter(Year == 2015) %>% 
  dplyr::select(-Rate) %>% 
  pivot_wider(names_from =  Tech, values_from = Number) %>% 
  complete(Town, Year, fill = list(PV = 0, EV = 0, Pump = 0)) %>% 
  rbind(data.frame(Town = c("St. Albans City", "St. Albans Town"),
                   Year = c(2015, 2015),
                   PV = c(round(56*9/17,0),round(56*8/17,0)),
                   EV = c(round(13*9/17,0),round(13*8/17,0)),
                   Pump = c(round(15*9/17,0),round(15*8/17,0)))) %>% # City:Town = 9:8 for PV, EV, and Pump 219, 44, and 293
  filter(Town != "St. Albans") %>% 
  full_join(VT_acs_prop_2014, by = c("Town" = "City")) %>% # missing Brunswick and Ferdinand
  filter(!Town %in% c("Averill", "Granby", "Brunswick", "Ferdinand")) %>%   # removing Averill and Granby
  inner_join(Exp %>% 
               dplyr::select(-GEOID, -Cost), by = c("Town" = "Town")) %>% 
  mutate(r_PV = PV/ Unit,
         r_EV = EV/ Unit,
         r_Pump = Pump/Unit) %>% 
  st_sf()

# 2016 data 
temp_2016 <- tot_data %>% 
  filter(Year == 2016) %>% 
  dplyr::select(-Rate) %>% 
  pivot_wider(names_from =  Tech, values_from = Number) %>% 
  complete(Town, Year, fill = list(PV = 0, EV = 0, Pump = 0)) %>% 
  rbind(data.frame(Town = c("St. Albans City", "St. Albans Town"),
                   Year = c(2016, 2016),
                   PV = c(round(117*9/17,0),round(117*8/17,0)),
                   EV = c(round(21*9/17,0),round(21*8/17,0)),
                   Pump = c(round(70*9/17,0),round(70*8/17,0)))) %>% # City:Town = 9:8 for PV, EV, and Pump 117, 21, and 70
  filter(Town != "St. Albans") 

# 2017 data 
temp_2017 <- tot_data %>% 
  filter(Year == 2017) %>% 
  dplyr::select(-Rate) %>% 
  pivot_wider(names_from =  Tech, values_from = Number) %>% 
  complete(Town, Year, fill = list(PV = 0, EV = 0, Pump = 0)) %>% 
  rbind(data.frame(Town = c("St. Albans City", "St. Albans Town"),
                   Year = c(2017, 2017),
                   PV = c(round(171*9/17,0),round(171*8/17,0)),
                   EV = c(round(31*9/17,0),round(31*8/17,0)),
                   Pump = c(round(126*9/17,0),round(126*8/17,0)))) %>% # City:Town = 9:8 for PV, EV, and Pump 171, 31, and 126
  filter(Town != "St. Albans") 

# 2018 data 
temp_2018 <- tot_data %>% 
  filter(Year == 2018) %>% 
  dplyr::select(-Rate) %>% 
  pivot_wider(names_from =  Tech, values_from = Number) %>% 
  complete(Town, Year, fill = list(PV = 0, EV = 0, Pump = 0)) %>% 
  rbind(data.frame(Town = c("St. Albans City", "St. Albans Town"),
                   Year = c(2018, 2018),
                   PV = c(round(205*9/17,0),round(205*8/17,0)),
                   EV = c(round(35*9/17,0),round(35*8/17,0)),
                   Pump = c(round(216*9/17,0),round(216*8/17,0)))) %>% # City:Town = 9:8 for PV, EV, and Pump 205, 35, and 216
  filter(Town != "St. Albans") 



temp.sp <- as(temp, "Spatial")
temp.nb <- poly2nb(temp.sp, queen=T)
temp.w <- nb2listw(temp.nb, style="W", zero.policy = TRUE)

# 
# plot(temp.sp, border = "grey60")
# plot(temp.w, coords = coordinates(temp.sp), add=T, col=2)

temp$Rho.Exp <- lag.listw(x=temp.w, var=temp$Exp, zero.policy = TRUE)
temp$Rho.PV <- lag.listw(x=temp.w, var=temp$r_PV, zero.policy = TRUE)
temp$Rho.EV <- lag.listw(x=temp.w, var=temp$r_EV, zero.policy = TRUE)
temp$Rho.Pump <- lag.listw(x=temp.w, var=temp$r_Pump, zero.policy = TRUE)

temp <- 
  temp %>% 
  dplyr::select(GEOID, Town, Unit:Gini, Exp:Rho.Pump, PV:Pump)



temp_2015$Rho.Exp <- lag.listw(x=temp.w, var=temp_2015$Exp, zero.policy = TRUE)
temp_2015$Rho.PV <- lag.listw(x=temp.w, var=temp_2015$r_PV, zero.policy = TRUE)
temp_2015$Rho.EV <- lag.listw(x=temp.w, var=temp_2015$r_EV, zero.policy = TRUE)
temp_2015$Rho.Pump <- lag.listw(x=temp.w, var=temp_2015$r_Pump, zero.policy = TRUE)

temp_2015 <- 
  temp_2015 %>% 
  dplyr::select(GEOID, Town, Unit:Gini, Exp:Rho.Pump, PV:Pump)

save(temp, temp_2015,temp_2016,temp_2017,temp_2018, file = "./data/derived/total.Rdata")
