## Data cleaning for PV


library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(forcats)
library(pander)
library(stringr)


data <- read_csv(file = "./data/raw/Rooftop_solar_VT.csv") %>% 
  filter(`Organization Type` == "Residential") %>% 
  mutate(Year = year(`Installation Date`),
         Month = month(`Installation Date`)) %>% 
  dplyr::select(City, `Zip Code`, Lat, Long, Year, Month) %>% 
  mutate(City = ifelse(str_detect(City, "(\\s+Town|\\s+City)"), str_replace_all(City, c(" City" = "", " Town" = "")), City))
           
  
# str(data)
# head(data)
# View(data)

# installation trend 

# by year 
data %>% 
  group_by(Year) %>% 
  summarise(install = n()) %>%
  ggplot(aes(x = Year, y = install))+
  geom_line(size = 1)+
  scale_x_continuous(breaks = seq(1999, 2019, by = 2)) +
  xlab("Year") + ylab("Number of installation") +
  theme_bw() +
  theme(legend.background = element_rect(fill="transparent"))

# by month
data %>% 
  group_by(Month) %>% 
  summarise(install = n()) %>%
  ggplot(aes(x = Month, y = install))+
  geom_line(size = 1)+
  scale_x_continuous(breaks = seq(1, 12, by = 1)) +
  xlab("Month") + ylab("Number of installation") +
  theme_bw() +
  theme(legend.background = element_rect(fill="transparent"))


data %>% 
  group_by(Year, City) %>% 
  summarise(install = n()) %>%
  ggplot(aes(x = Year, y = install, color = City))+
  geom_line(size = 1)+
  scale_x_continuous(breaks = seq(1999, 2019, by = 2)) +
  xlab("Year") + ylab("Number of installation") +
  theme_bw() +
  theme(legend.background = element_rect(fill="transparent"))


save(data, file = "./data/derived/PV.Rdata")
