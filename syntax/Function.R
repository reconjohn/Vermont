
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(forcats)
library(pander)
library(quantmod)
library(stringr)
library(SpatialEpi)
library(maps)
library(shapefiles)
library(maptools)
library(RColorBrewer)
library(spdep)
# library(INLA)
library(rgdal)
library(pander)
library(DCluster)
library(geoR)
library(sp)
library(splancs)
# library(GWmodel)
library(psych)
library (cluster)
library(reshape)
library(reshape2)
library(som)
library(GPArotation)
library(corrplot)
library(GGally)
library(faraway)
library(sjPlot)
library(lattice)
library(png)
library(grid)
library(gridExtra)
library(rgeos)
library(leaflet)
library(tigris)
library(broom)
library("broom.mixed")
library(rmapshaper)
library(ggpubr)
library(lme4)
library(lmerTest)
library(stargazer)
library(arm)
library(pscl)
library(spatialreg)
library(ggstance)
options(tigris_use_cache = TRUE, tigris_class = "sf")

# uploading library 
lapply(c("spatstat","colorRamps","tmap","ggmap","geoR","knitr","kableExtra","data.table","gdata","tigris","sf","scales","tidycensus","plotly", "tidyverse", "lubridate"), require, character.only = TRUE)

## Functions 

## Point clustering and density 
##
Density_plot <- function(Total_Permit, PV, EV, Win){
  ## Total 
  totgeo <- as.data.frame(Total_Permit[,c(6,7)])
  # plot(totgeo,pch = 4, cex = 0.6)
  ppptotgeo <- as.ppp(totgeo, W= Win)
  # plot(ppptotgeo) 
  
  dentot <- as(as(density(ppptotgeo, 0.004), "SpatialGridDataFrame"), "SpatialPixelsDataFrame")
  
  
  ## PV
  ## Density plot
  geo <- as.data.frame(PV[,c(6,7)])
  # plot(geo,pch = 4, cex = 0.6)
  pppgeo <- as.ppp(geo, W= Win)
  # plot(pppgeo)
  
  ## Clustering check
  PV_evn <- envelope(pppgeo, fun= Gest, nrank= 2, nsim= 99)
  
  
  den <- as(as(density(pppgeo, 0.004), "SpatialGridDataFrame"), "SpatialPixelsDataFrame")
  # plot(den, col=blue2red(20))
  # what is the unit of the graph?
  
  
  ## relative density
  scaled_den = (den$v - mean(den$v))/ sd(den$v)
  scaled_dentot = (dentot$v - mean(dentot$v))/ sd(dentot$v)
  den_PV <- den
  den_PV@data$v <- (scaled_den - scaled_dentot) 
  
  ## EV
  ## Density plot
  geo <- as.data.frame(EV[,c(6,7)])
  # plot(geo,pch = 4, cex = 0.6)
  pppgeo <- as.ppp(geo, W= Win)
  # plot(pppgeo)
  
  ## Clustering check
  EV_evn <- envelope(pppgeo, fun= Gest, nrank= 2, nsim= 99)
  
  den <- as(as(density(pppgeo, 0.004), "SpatialGridDataFrame"), "SpatialPixelsDataFrame")
  # plot(den, col=blue2red(20))
  # what is the unit of the graph?
  
  ## relative density
  scaled_den = (den$v - mean(den$v))/ sd(den$v)
  den_EV <- den
  den_EV@data$v <- (scaled_den - scaled_dentot) 
  
  
  result <- list(dentot, PV_evn, den_PV, EV_evn, den_EV)
  return(result) # total housing unit density, PV, and EV density, G estimates 
  
}

# load("./data/derived/Seattle_Data.Rdata")
# Win <- owin(c(-122.45, -122.20), c(47.49, 47.74)) # Seattle
#
# result <- Density_plot(Total_Permit, PV, EV, Win)
# 
# plot(result[[1]], col=blue2red(20))
# title("Residential density in Seattle")
# 
# plot(result[[2]], main="PV G function")
# plot(result[[3]], col=blue2red(20))
# title("Residential PV density in Seattle")
# 
# plot(result[[4]], main="EV G function")
# plot(result[[5]], col=blue2red(20))
# title("Residential EV density in Seattle")
#
# load("./data/derived/Bellevue_Data.Rdata") # Bellevue 
# Win <- owin(c(-122.25, -122.07), c(47.51, 47.68)) # Bellevue 


## Point Mapping 
##
Point_map <- function(PV, EV, dens = F){
  if(dens == F){
    qmplot(data = PV, 
           x = Lon, 
           y = Lat, 
           color = I("#342c5c"), 
           alpha = I(0.3)) +
      geom_point(aes(x = Lon, y = Lat, color = "red"),
                 data = EV, shape = 2)
  } else {
    qmplot(data = PV, geom = "blank",
           x = Lon, y = Lat, 
           maptype = "toner-lite", 
           darken = 0.1) + 
      stat_density_2d(
        aes(fill = stat(level)),
        geom = "polygon", 
        alpha = .2, color = NA) + 
      scale_fill_gradient2(
        "PV\nConcentration", 
        low = "white", 
        mid = "yellow", 
        high = "red") + 
      theme(legend.position = "bottom")
  }
} # point or density map 

# Point_map(PV, EV, F)
# Point_map(PV, EV, T)


## Time trend 
##
Trend_plot <- function(PV, EV){
  
  PEV <- PV %>% 
    group_by(Class, Year) %>% 
    summarise(PV = n()) %>% 
    left_join(EV %>% 
                group_by(Class, Year) %>% 
                summarise(EV = n()), by = c("Class", "Year")) %>% 
    gather(Technology, value, PV, EV) %>% 
    mutate(Technology = parse_factor(Technology, levels = c("PV", "EV")))
  
  PEV$value[is.na(PEV$value)] = 0
  
  PEV_plot <- PEV %>% 
    ggplot(aes(x = Year, y = value, group = Technology, color = Technology))+
    facet_wrap( ~ Class) +
    geom_line(size = 2)+
    xlab("Year") + ylab("Number of installation") +
    theme_bw() +
    theme(legend.position = c(0.90, 0.25),
          legend.background = element_rect(fill="transparent"),
          strip.text = element_text(size = 17),
          legend.text = element_text(size = 12),
          text = element_text(size = 15)) +
    scale_x_continuous(breaks = seq(2000, 2019, by = 5))
  
  return(list(PEV, PEV_plot)) # PV, EV installation trend
}

# result <- Trend_plot(PV, EV)
# result[[2]]
# result[[1]]


## PV, EV relationship 
##
Relationship_plot <- function(PV, EV){
  
  PEV_join <- PV %>% 
    distinct(Lat, Lon,.keep_all=TRUE) %>%
    inner_join(EV %>% 
                 distinct(Lat, Lon,.keep_all=TRUE), 
               by = c("Lat", "Lon"))
  
  # glimpse(PEV_join)
  # View(PEV_join)
  
  ## plot
  PEV_join_plot1 <- PEV_join %>% 
    ggplot(aes(x = Date.x, y = Date.y))+
    geom_point(alpha = 0.5, size = 2) +
    geom_abline(color = "red", size = 1) +
    geom_smooth(method = "lm", se = T) +
    xlab("PV install date") + ylab("EV install date") +
    theme_bw() +
    theme(legend.position = c(0.90, 0.25),
          legend.background = element_rect(fill="transparent"))
  
  ## plot after outliers removed 
  PEV_join_plot2 <- PEV_join %>%
    filter(Date.x > as.Date("2012/01/01", "%Y/%m/%d"),
           Date.y > as.Date("2012/01/01", "%Y/%m/%d")) %>%
    ggplot(aes(x = Date.x, y = Date.y))+
    geom_point(alpha = 0.5, size = 2) +
    geom_abline(color = "red", size = 1) +
    geom_smooth(method = "lm", se = T, aes(color = "correlation")) +
    scale_colour_manual(name="Legend", values=c("blue", "red")) + 
    xlab("PV install date") + ylab("EV install date") +
    theme_bw() +
    theme(legend.position = c(0.90, 0.25),
          legend.background = element_rect(fill="transparent"))
  
  return(list(PEV_join_plot1, PEV_join_plot2))
}

# result <- Relationship_plot(PV, EV)
# result[[1]]
# result[[2]]



## ACS plot
##
# ggplot(bellevue_tracts) + 
#   geom_sf(fill = "white") + 
#   theme_minimal() 

ACS_plot <- function(tracts){
  
  king_water <- area_water("WA", "King", class = "sf")
  
  tracts %>%
    st_difference(st_union(king_water)) %>% 
    gather("var","value", PopDensity:Gini) %>% 
  ggplot() +
  geom_sf(aes(fill = value), size = .25) +
  facet_wrap(vars(var)) +
  scale_fill_viridis_c() + 
  theme_void()

}

# load("./data/derived/ACS.Rdata")
# ACS_plot(WA_tracts)
# ACS_plot(seattle_tracts)
# ACS_plot(bellevue_tracts)

## Aggregation per census tract
## PV and EV counts 
Agg_data <- function(PV, EV, census_data){

  PV <- st_as_sf(PV, coords = c("Lon", "Lat"), crs = 4326,
                       agr = "constant",
                       stringsAsFactors = FALSE,
                       remove = TRUE)
  
  EV <- st_as_sf(EV, coords = c("Lon", "Lat"), crs = 4326,
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
    dplyr::rename(PV = n)
  
  # EV
  in_tract <- st_join(EV, census, join = st_within)
  # join sf objects based on geometry 
  
  # count per census tract
  tract_count <- count(as_tibble(in_tract), GEOID)
  
  tract_sf <- left_join(tract_sf, tract_count) %>% 
    dplyr::rename(EV = n) 
  
  tract_sf[is.na(tract_sf[["PV"]]),"PV"] <- 0 # fill zero at empty value
  tract_sf[is.na(tract_sf[["EV"]]),"EV"] <- 0
  tract_sf$HomeValue[is.na(tract_sf$HomeValue)] <- mean(tract_sf$HomeValue, na.rm = T)
  
  N = sum(tract_sf$Unit) # total number of housing units in Seattle
  ExpectedPV = sum(tract_sf$PV, na.rm = T)/ N # total number of PV/ N, expected pv per one housing unit
  ExpectedEV = sum(tract_sf$EV, na.rm = T)/ N # total number of EV/ N
  
  
  regrs <- tract_sf %>%
    mutate(PV = PV + 1, # ifelse(PV == 0, 0.5, PV)
           EV = EV + 1, # ifelse(EV == 0, 0.5, EV)
           Exp_PV = Unit*ExpectedPV + 1, # expected PV for the census tract
           Exp_EV = Unit*ExpectedEV + 1,
           SIR_PV = PV/ Exp_PV,
           SIR_EV = EV/ Exp_EV,
           SE_SIR_PV = sqrt(SIR_PV/ Exp_PV),
           SE_SIR_EV = sqrt(SIR_EV/ Exp_EV))

  return(regrs)
}

# regrs_seattle <- PVEV_Seattle <- Agg_data(PV, EV, seattle_tracts)
# regrs_bellevue <- Agg_data(PV_Bel, EV_Bel, bellevue_tracts)

## Agg data for portland
Agg_data_port <- function(PV, census_data){
  
  PV <- st_as_sf(PV, coords = c("Lon", "Lat"), crs = 4326,
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
  tract_sf$HomeValue[is.na(tract_sf$HomeValue)] <- mean(tract_sf$HomeValue, na.rm = T)
  
  N = sum(tract_sf$Unit) # total number of housing units in Seattle
  ExpectedPV = sum(tract_sf$PV, na.rm = T)/ N # total number of PV/ N, expected pv per one housing unit
  
  
  regrs <- tract_sf %>%
    mutate(PV = PV+1, # ifelse(PV == 0, 0.5, PV)
           Exp_PV = Unit*ExpectedPV +1, # expected PV for the census tract
           SIR_PV = PV/ Exp_PV,
           SE_SIR_PV = sqrt(SIR_PV/ Exp_PV))
  
  return(regrs)
}


## Plot variables 

fill_plot <- function(regrs, variable, title){
  regrs %>% 
    ggplot(aes(fill=!!sym(variable))) + 
    geom_sf(size=0.1, color="white") + 
    coord_sf(datum=NA) + 
    scale_fill_continuous(name="Value", 
                          type ="viridis") + 
    theme_minimal() + ggtitle(title)
}

# regrs <- Agg_data(PV, EV, seattle_tracts)
# fill_plot(regrs,"SIR_PV","SIR in Seattle")
# fill_plot(regrs,"poisres","Residuals of possison model")

Clustering_plot <- function(regrs_sf, PV = T, city){
  
  regrs_sp <- regrs_sf %>% as("Spatial") # make it to sp
  
  if(PV){
    SIR <- regrs_sp$SIR_PV
    Tech <- "PV"} else {
      SIR <- regrs_sp$SIR_EV
      Tech <- "EV"
    }
  
  nb.map <- poly2nb(regrs_sp, queen=T)
  # summary(nb.map)
  # nb2INLA("seattle.graph", nb.map)
  # nb2INLA("bellevue.graph", nb.map)
  
  W <- nb2listw(nb.map, style= "W", zero.policy= T) 
  # zero.policy removes cases with no neighbors 
  # plot(regrs_sp, border = "grey60")
  # plot(W, coordinates(regrs_sp), add=T, col=2) 
  
  ## Getis-Ord (heatmap)
  nb.map.self <- include.self(nb.map)
  W.self <- nb2listw(nb.map.self, style="W", zero.policy= TRUE)
  # plot(regrs_sp, border = "grey60")
  # plot(W.self, coordinates(regrs_sp), add=T, col=2) 
  
  
    ### PV
    localgstar<-localG(SIR, W.self, zero.policy = TRUE)
    # local cluster of high or low values 
    # summary(localgstar)
    
    regrs_sf <- mutate(regrs_sf, localgstar = as.numeric(localgstar))
    
    # tm_shape(regrs_sf, unit = "km") +
    #   tm_polygons(col = "localgstar", title = "Gi* value", palette = "-RdBu", 
    #               style = "quantile", n=10) +
    #   tm_scale_bar(breaks = c(0, 10, 20), text.size = 1) +
    #   tm_layout(frame = F, main.title = "PV clusters",
    #             legend.outside = T)
    
    breaks <- c(-Inf, -2.58, -1.96, -1.65, 1.65, 1.96, 2.58, Inf)
    # tm_shape(regrs_sf, unit = "km") +
    #   tm_polygons(col = "localgstar", title = "Gi* value", palette = "-RdBu",
    #               breaks = breaks) +
    #   tm_scale_bar(breaks = c(0, 10, 20), text.size = 1) +
    #   tm_layout(frame = F, main.title = "Seattle PV clusters",
    #             legend.outside = T)

    
    regrs_sf <-  mutate(regrs_sf, 
                        gcluster = cut(localgstar, breaks=breaks, include.lowest = TRUE, 
                                       labels=c("Cold spot: 99% confidence", 
                                                "Cold spot: 95% confidence", 
                                                "Cold spot: 90% confidence", 
                                                "Not significant","Hot spot: 90% confidence",
                                                "Hot spot: 95% confidence", 
                                                "Hot spot: 99% confidence"))) 
    
    tm_plot <- tm_shape(regrs_sf, unit = "km") +
      tm_polygons(col = "gcluster", title = "", palette = "-RdBu", 
                  breaks = breaks) +
      tm_scale_bar(breaks = c(0, 10, 20), text.size = 1) +
      tm_layout(frame = F, main.title = paste(city, Tech, "clusters"),
                legend.outside = T)
    
    
    # tmap_mode("view")
    # tm_PV + tm_view(basemaps="OpenStreetMap")
    
    
    ## Local Moran's I - comparison btw neighbor and feature
    locali<-localmoran(SIR, W, p.adjust.method="bonferroni")
    regrs_sf <- mutate(regrs_sf, localmi = locali[,1], localz = locali[,4])
    regrs_sf <- mutate(regrs_sf, mcluster = cut(localz, breaks = c(min(localz),-1.96, 1.96, max(localz)), include.lowest = TRUE, labels = c("Negative Correlation", "Not Significant", "Positive Correlation")))
    
    # tm_shape(regrs_sf, unit = "km") +
    #   tm_polygons(col = "mcluster", title = "", palette = "-RdBu", 
    #               breaks = breaks) +
    #   tm_scale_bar(breaks = c(0, 10, 20), text.size = 1) +
    #   tm_compass(type = "4star", position = c("left", "bottom")) +
    #   tm_layout(frame = F, main.title = "Seattle PV clusters",
    #             legend.outside = T)

  return(list(regrs_sp, W, tm_plot))
  
}

# regrs <- Agg_data(PV_Bel, EV_Bel, bellevue_tracts)
# regrs <- Agg_data(PV, EV, seattle_tracts)
# Clustering <- Clustering_plot(regrs, T, "Seattle")
# plot(Clustering[[1]], border = "grey60")
# plot(Clustering[[2]], coordinates(Clustering[[1]]), add=T, col=2) 
# Clustering[[3]]

# save(Density_plot, Point_map, Trend_plot, Relationship_plot, ACS_plot, Agg_data, fill_plot, Clustering_plot, file = "./data/derived/Fun.Rdata")

