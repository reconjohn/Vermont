
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
library(viridis)
library(ggpubr)
library(ggrepel)
options(tigris_use_cache = TRUE, tigris_class = "sf")

# uploading library 
lapply(c("spatstat","colorRamps","tmap","ggmap","geoR","knitr","kableExtra","data.table","gdata","tigris","sf","scales","tidycensus","plotly", "tidyverse", "lubridate"), require, character.only = TRUE)

source("./syntax/theme_caviz.R")
# load("./data/derived/total.Rdata") # temp (2020 Vermont data raw)
load("./data/derived/vermont.Rdata") # temp (2020 Vermont data after cleaning)
load("./data/derived/RECS.Rdata") # total_data, urban, rural,urbanT, RuralT from RECS_U.R
load("./data/derived/A_comparison.Rdata") # total_data2 
# (RECS model results of the three technology relationship)
load("./data/derived/official_data.Rdata") 
# data (RECS), dataU, dataR, burden, pre (republican), US (state data), us (US border), vt (VT border)

# # data 
# ## RECS
# data <- read_csv(file = "./data/raw/recs2020.csv") %>% 
#   dplyr::select(UATYP10, state_postal, TYPEHUQ, KOWNRENT, EQUIPM, FUELHEAT, SOLAR, ELECVEH, EVCHRGHOME, 
#                 EDUCATION, HOUSEHOLDER_RACE, MONEYPY) %>% 
#   filter(UATYP10 != "C") %>%
#   mutate(urban = UATYP10,
#          state = state_postal,
#          SingleFamily = ifelse(TYPEHUQ %in% c(2,3), 1, 0),
#          HomeOwn = ifelse(KOWNRENT == 1, 1, 0),
#          Pump = ifelse(EQUIPM %in% c(4,13), 1, 0),
#          PV = ifelse(SOLAR == 1, 1, 0),
#          EV = ELECVEH,
#          EVC = ifelse(EVCHRGHOME == 1, 1, 0),
#          Edu = ifelse(EDUCATION %in% c(4,5), 1, 0),
#          White = ifelse(HOUSEHOLDER_RACE == 1, 1, 0),
#          Income = rescale(MONEYPY),
#          Income = ifelse(Income > mean(Income), 1, 0)) %>% 
#   replace_na(list(EVC = 0)) %>% 
#   dplyr::select(state, urban, SingleFamily, HomeOwn, Edu, White, Income, PV, EV, EVC, Pump)
# 
# # dataT <- read_csv(file = "./data/raw/recs2020.csv") %>% 
# #   dplyr::select(UATYP10, state_postal, TYPEHUQ, KOWNRENT, EQUIPM, FUELHEAT, SOLAR, ELECVEH, EVCHRGHOME, 
# #                 EDUCATION, HOUSEHOLDER_RACE, MONEYPY) %>% 
# #   filter(UATYP10 != "C") %>% 
# #   mutate(urban = UATYP10,
# #          state = state_postal,
# #          SingleFamily = ifelse(TYPEHUQ %in% c(2,3), 1, 0),
# #          HomeOwn = ifelse(KOWNRENT == 1, 1, 0),
# #          Pump = ifelse(EQUIPM %in% c(4,13), 1, 0),
# #          PV = ifelse(SOLAR == 1, 1, 0),
# #          EV = ELECVEH,
# #          EVC = ifelse(EVCHRGHOME == 1, 1, 0),
# #          Edu = ifelse(EDUCATION %in% c(4,5), 1, 0),
# #          White = ifelse(HOUSEHOLDER_RACE == 1, 1, 0),
# #          Income = rescale(MONEYPY)) %>% 
# #   replace_na(list(EVC = 0)) %>% 
# #   dplyr::select(state, urban, SingleFamily, HomeOwn, Edu, White, Income, PV, EV, EVC, Pump)
# 
# 
# dataU <- 
#   data %>% 
#   filter(urban == "U")
# 
# dataR <- 
#   data %>% 
#   filter(urban == "R")
# 
# # housing unit
# st <- bind_rows(lapply(state.abb, function(x){
#   get_acs(
#     "state",
#     variables = "B25003_001", # housing unit
#     state = x,
#     year = 2020,
#     survey = "acs5",
#     geometry = TRUE,
#     cache_table = TRUE) %>%
#     mutate(State = x)}))
# 
# 
# # US <- st %>% 
# #   inner_join(data %>% 
# #   group_by(state) %>% 
# #   summarise(pop = n(),
# #             Urban = sum(urban == "U"),
# #             Rural = sum(urban == "R"),
# #             PV = sum(PV),
# #             EV = sum(EV),
# #             Pump = sum(Pump)), by = c("State" = "state")) %>% 
# #   ungroup() %>% 
# #   mutate(Urban = Urban/pop,
# #          Rural = Rural/pop,
# #          `Rooftop solar` = PV/pop,
# #          `Electric vehicle` = EV/pop,
# #          `Heat pump` = Pump/pop) %>% 
# #   mutate_at(vars(`Rooftop solar`:`Heat pump`), funs(scale(.) %>% as.numeric())) %>% 
# #   gather(key, value, `Rooftop solar`:`Heat pump`) %>% 
# #   mutate(key = factor(key, levels = c("Rooftop solar","Electric vehicle","Heat pump")))
# 
# # energy cost
# br <- read_csv(file = "./data/raw/burden.csv")  # LEAD for 2018
# 
# # median income 2018
# inc <- bind_rows(lapply(state.abb, function(x){
#   get_acs(
#     "state",
#     variables = "B19013_001",
#     state = x,
#     year = 2018,
#     survey = "acs5",
#     geometry = F,
#     cache_table = TRUE) %>%
#     mutate(State = x)}))
# 
# 
# burden <- inc %>% 
#   dplyr::select(State, estimate) %>% 
#   rename(state = State,
#          Income = estimate) %>% 
#   left_join(br %>% 
#               rename(state = Name), by = "state") %>% 
#   mutate(Burden = Cost/Income*100)
# 
# # adding political data 
# # republican 
# ## adding political data by state 
# pre <- read_csv(file = "./data/raw/countypres.csv") %>% 
#   filter(party == "REPUBLICAN") %>% 
#   filter(totalvotes > 0) %>% 
#   filter(year > 2012) %>% 
#   group_by(state_po) %>% 
#   summarise(candi = sum(candidatevotes),
#             total = sum(totalvotes),
#             Republican = candi/ total*100) %>% 
#   rename(state = state_po) %>% 
#   ungroup()
# 
# 
# # pre <- read_csv(file = "./data/raw/countypres.csv") %>% 
# #   filter(party == "REPUBLICAN") %>% 
# #   filter(totalvotes > 0) %>% 
# #   mutate(ratio = candidatevotes/ totalvotes) %>% 
# #   pivot_wider(names_from = year, values_from = ratio) %>% 
# #   rename(fips = county_fips)
# # 
# # pre <- pre %>% 
# #   dplyr::select(fips, "2000":"2020") %>% 
# #   group_by(fips) %>% 
# #   dplyr::summarise_all(sum, na.rm = T) %>% 
# #   rename(Politics = `2016`) %>% 
# #   mutate(Politics = Politics*100)
# 
# # get pre and burden from state_analysis.R
# US <- st %>% 
#   rename(total_housing = estimate) %>% 
#   inner_join(data %>% 
#                group_by(state) %>% 
#                summarise(pop = n(),
#                          Urban = sum(urban == "U"),
#                          Rural = sum(urban == "R"),
#                          PV = sum(PV),
#                          EV = sum(EV),
#                          Pump = sum(Pump)), by = c("State" = "state")) %>% 
#   ungroup() %>% 
#   left_join(pre, by = c("State" = "state")) %>% 
#   left_join(burden, by = c("State" = "state")) %>% 
#   mutate(Urban = Urban/pop,
#          Rural = Rural/pop,
#          `Rooftop solar` = 100*PV/pop,
#          `Electric vehicle` = 100*EV/pop,
#          `Heat pump` = 100*Pump/pop) 
# 
# # US border
# us <- st %>% 
#   filter(!State %in% c("HI","AK")) %>% 
#   st_union() %>% 
#   st_as_sf()
# 
# 
# # VT border
# vt <- temp %>% 
#   st_union() %>% 
#   st_as_sf()

# save(data, dataU, dataR, burden, pre, US, us, vt, file = "./data/derived/official_data.Rdata")

################################################################################ function 
### modeling among Vermont technologies  
sim_countf <- function(data, tech){
  
  if(tech == "PV"){
    rho <- "Rho.PV"
    tech1 <- "r_EV"
    tech2 <- "r_Pump"
  }else if(tech == "EV"){
    rho <- "Rho.EV"
    tech1 <- "r_PV"
    tech2 <- "r_Pump"
  }else{
    rho <- "Rho.Pump"
    tech1 <- "r_EV"
    tech2 <- "r_PV"
  }
  
  
  data <- data %>% st_drop_geometry() %>% 
    mutate(SingleFamily = ifelse(SingleFamily > mean(SingleFamily), 1,0),
           HomeOwn = ifelse(HomeOwn > mean(HomeOwn), 1,0),
           Edu = ifelse(Edu > mean(Edu), 1,0),
           White = ifelse(White > mean(White), 1,0),
           Income = ifelse(Income > mean(Income), 1,0)) %>% 
    mutate_at(vars(HomeOwn:White,r_PV:r_Pump,Rho.PV:Rho.Pump), funs(rescale(.) %>% as.numeric(.)))
  
  
  fit <- glm.nb(get(tech) ~ SingleFamily+HomeOwn+Edu+White+Income+get(rho)+get(tech1)+get(tech2)+
                  offset(log(Unit)), data = data)
  
  x <- data %>%                  
    dplyr::select(SingleFamily,HomeOwn,Edu,White,Income,rho,tech1,tech2) %>%
    summarize_all(mean) %>%   # summarize the means for most variables (can't average Species)
    uncount(4) %>%  # repeat 
    mutate(cont = 1) %>% 
    relocate(cont) %>% as.matrix()
  
  x[1,8] <- 1
  x[2,9] <- 1
  
  x[3,8] <- 0
  x[4,9] <- 0
  
  
  
  sims <- 10000
  pe <- fit %>% tidy() %>% dplyr::select(estimate) %>% as.matrix()
  vc <- vcov(fit)
  simbetas <- mvrnorm(sims, pe, vc)
  xbeta <- x %*% t(simbetas) %>% exp()
  xbeta <- xbeta*mean(get("Unit", data))
  
  xbeta <- xbeta[1:2,] - xbeta[3:4,]
  
  pe <- apply(xbeta, 1, mean) 
  upper <- apply(xbeta, 1, quantile, probs= 0.975) 
  lower <- apply(xbeta, 1, quantile, probs= 0.025)
  
  total_pe <- cbind(pe,upper,lower) %>%
    as.data.frame() %>% 
    mutate(group = c(tech1, tech2),
           tech = tech)
  
  return(total_pe)
}

# creating figure
sim_plot <- function(total_pe, tech){
  Var <- tech
  fig <- total_pe %>%
    filter(tech == Var) %>% 
    ggplot(aes(y = reorder(group,pe),
               x = pe,
               xmax = upper,
               xmin = lower,
               color = urban)) +
    geom_pointrangeh(position = position_dodge2v(height = 0.7), fatten = 2, size = 0.8, shape = 15) +
    # scale_color_grey()+
    geom_vline(xintercept = 0, color ="black", linetype = "dashed", size = 0.5)+
    scale_x_continuous(# limits = c(min(total_pe[3]), max(total_pe[2])),
      # breaks = seq(0, max(total_pe), by =0.01),
      # labels = c("0%", "+1%", "+2%"),
      sec.axis =  sec_axis(~ ., name = "")) + # sec.axis = sec_axis(~ . - mean(sp_regime$Adoption), name = "Deviation from Mean %")
    labs(title = "", x = paste(tech ,"\nadoption\ndifference [%]"), y = "", colour = "Area") +
    theme_minimal() + 
    theme(plot.background = element_blank(),
          panel.grid.minor = element_blank())
  
  if(Var == "Rooftop solar"){
    fig <- fig + 
      ggtitle("d") + 
      theme(aspect.ratio = 1.1,
            axis.text.x = element_text(color = "black", size = 10),
            axis.ticks.x = element_blank(),
            legend.position = c(0.99, 0.35),
            axis.line.x = element_line(color="black"),
            plot.title = element_text(hjust = -1.1),
            legend.background = element_rect(size=0.5, linetype="solid"),
            legend.text = element_text(size=7),
            legend.title = element_text(size=8),
            legend.key.height= unit(0.3, 'cm'),
            legend.key.width= unit(0.3, 'cm'))
  }else if(Var == "Heat pump"){
    fig <- fig +
      theme(aspect.ratio = 1.1,
            axis.text.x = element_text(color = "black", size = 10),
            axis.ticks.x = element_blank(),
            legend.position = "none",
            axis.line.x = element_line(color="black"),
            plot.title = element_text(hjust = -1))
  }else{
    fig <- fig +
      theme(aspect.ratio = 1.1,
            axis.text.x = element_text(color = "black", size = 10),
            axis.ticks.x = element_blank(),
            legend.position = "none",
            legend.text = element_text(size=7),
            legend.title = element_text(size=7),
            axis.line.x = element_line(color="black"))
    # legend.background = element_rect(size=0.5, linetype="solid"))
  }
  return(fig)
}


### energy burden modeling 
b_countf <- function(data, tech){
  
  
  if(tech == "PV"){
    rho <- "Rho.PV"
  }else if(tech == "EV"){
    rho <- "Rho.EV"
  }else{
    rho <- "Rho.Pump"
  }
  
  
  data <- data %>% st_drop_geometry() %>% 
    mutate(Bur = Exp*100/ Income) %>% 
    mutate(SingleFamily = ifelse(SingleFamily > mean(SingleFamily), 1,0),
           HomeOwn = ifelse(HomeOwn > mean(HomeOwn), 1,0),
           Edu = ifelse(Edu > mean(Edu), 1,0),
           White = ifelse(White > mean(White), 1,0),
           Income = ifelse(Income > mean(Income), 1,0)) %>% 
    mutate_at(vars(PV:Pump), funs(rescale(., to=c(0,100)) %>% as.numeric(.) %>% 
                                    round(0))) %>% 
    mutate_at(vars(Rho.PV:Rho.Pump), funs(rescale(.) %>% as.numeric(.)))
  
  
  fit <- glm.nb(get(tech) ~ SingleFamily+HomeOwn+Edu+White+Income+get(rho)+Bur+Republican+
                  offset(log(Unit)), data = data)
  
  # to compare with predict function result 
  # new <- data %>% 
  #   mutate_at(vars(Unit:Exp, Rho.PV:Rho.Pump), funs(mean(.)))
  # 
  # new <- new[1:20, ] %>% 
  #   mutate({{rho}} := seq(0,1, length.out = 20))
  # 
  # predict(fit, new, type = "response")
  
  
  x <- data %>%                    
    dplyr::select(SingleFamily,HomeOwn,Edu,White,Income,rho, Bur, Republican) %>% 
    summarize_all(mean) %>%
    uncount(20) %>% 
    mutate(Bur = seq(0, 13, length.out = 20),
           cont = 1
    ) %>% 
    relocate(cont)
  
  x <- x %>% 
    as.matrix()
  
  sims <- 10000
  pe <- fit %>% tidy() %>% dplyr::select(estimate) %>% as.matrix()
  vc <- vcov(fit)
  simbetas <- mvrnorm(sims, pe, vc)
  xbeta <- x %*% t(simbetas) %>% exp()
  xbeta <- xbeta*mean(get("Unit", data))
  
  pe <- apply(xbeta, 1, mean) 
  upper <- apply(xbeta, 1, quantile, probs= 0.975)
  lower <- apply(xbeta, 1, quantile, probs= 0.025)
  
  total_pe <- cbind(pe,upper,lower, x %>% 
                      as.data.frame() %>% 
                      dplyr::select(Bur)) %>%
    as.data.frame() %>% 
    mutate(tech = tech)
  
  return(total_pe)
}

### republican modeling 
r_countf <- function(data, tech){
  
  
  if(tech == "PV"){
    rho <- "Rho.PV"
  }else if(tech == "EV"){
    rho <- "Rho.EV"
  }else{
    rho <- "Rho.Pump"
  }
  
  
  data <- data %>% st_drop_geometry() %>% 
    mutate(Bur = Exp*100/ Income) %>% 
    mutate(SingleFamily = ifelse(SingleFamily > mean(SingleFamily), 1,0),
           HomeOwn = ifelse(HomeOwn > mean(HomeOwn), 1,0),
           Edu = ifelse(Edu > mean(Edu), 1,0),
           White = ifelse(White > mean(White), 1,0),
           Income = ifelse(Income > mean(Income), 1,0)) %>% 
    mutate_at(vars(PV:Pump), funs(rescale(., to=c(0,100)) %>% as.numeric(.) %>% 
                                    round(0))) %>% 
    mutate_at(vars(Rho.PV:Rho.Pump), funs(rescale(.) %>% as.numeric(.)))
  
  
  fit <- glm.nb(get(tech) ~ SingleFamily+HomeOwn+Edu+White+Income+get(rho)+Bur+Republican+
                  offset(log(Unit)), data = data)
  
  # to compare with predict function result 
  # new <- data %>% 
  #   mutate_at(vars(Unit:Exp, Rho.PV:Rho.Pump), funs(mean(.)))
  # 
  # new <- new[1:20, ] %>% 
  #   mutate({{rho}} := seq(0,1, length.out = 20))
  # 
  # predict(fit, new, type = "response")
  
  
  x <- data %>%                    
    dplyr::select(SingleFamily,HomeOwn,Edu,White,Income,rho, Bur, Republican) %>% 
    summarize_all(mean) %>%
    uncount(20) %>% 
    mutate(Republican = seq(0, 13, length.out = 20),
           cont = 1
    ) %>% 
    relocate(cont)
  
  x <- x %>% 
    as.matrix()
  
  sims <- 10000
  pe <- fit %>% tidy() %>% dplyr::select(estimate) %>% as.matrix()
  vc <- vcov(fit)
  simbetas <- mvrnorm(sims, pe, vc)
  xbeta <- x %*% t(simbetas) %>% exp()
  xbeta <- xbeta*mean(get("Unit", data))
  
  pe <- apply(xbeta, 1, mean) 
  upper <- apply(xbeta, 1, quantile, probs= 0.975)
  lower <- apply(xbeta, 1, quantile, probs= 0.025)
  
  total_pe <- cbind(pe,upper,lower, x %>% 
                      as.data.frame() %>% 
                      dplyr::select(Republican)) %>%
    as.data.frame() %>% 
    mutate(tech = tech)
  
  return(total_pe)
}

### technologies vs. predictor modeling 
# general string plot +- sd
s_countf <- function(data, tech){
  
  if(tech == "PV"){
    rho <- "Rho.PV"
    expected <- "E_PV"
  }else if(tech == "EV"){
    rho <- "Rho.EV"
    expected <- "E_EV"
  }else{
    rho <- "Rho.Pump"
    expected <- "E_Pump"
  }
  
  # data <- data %>%
  #   mutate_at(vars(HomeOwn:White, Rho.PV:Rho.Pump), funs(rescale(.) %>% as.numeric(.)))
  
  data <- data %>% 
    mutate(SingleFamily = ifelse(SingleFamily > mean(SingleFamily), 1,0),
           HomeOwn = ifelse(HomeOwn > mean(HomeOwn), 1,0),
           Edu = ifelse(Edu > mean(Edu), 1,0),
           White = ifelse(White > mean(White), 1,0),
           Income = ifelse(Income > mean(Income), 1,0)) %>% 
    mutate_at(vars(r_PV:r_Pump,Rho.PV:Rho.Pump), funs(rescale(.) %>% as.numeric(.)))
  
  
  fit <- glm.nb(get(tech) ~ SingleFamily+HomeOwn+Edu+White+Income+get(rho)+
                  offset(log(get("Unit", data))), data = data)
  
  x <- data %>%     
    st_drop_geometry() %>% 
    dplyr::select(SingleFamily,HomeOwn,Edu,White,Income,rho) %>%
    summarize_all(mean) %>%   # summarize the means for most variables (can't average Species)
    uncount(10) %>%           # repeat the row 14 times
    mutate(cont = 1) %>% 
    relocate(cont) %>% as.matrix()
  
  # x[1,2] <- mean(get("SingleFamily", data)) + 1.645*sd(get("SingleFamily", data))
  # x[2,3] <- mean(get("HomeOwn", data)) + 1.645*sd(get("HomeOwn", data))
  # x[3,4] <- mean(get("Edu", data)) + 1.645*sd(get("Edu", data))
  # x[4,5] <- mean(get("White", data)) + 1.645*sd(get("White", data))
  # x[5,6] <- mean(get("Income", data)) + 1.645*sd(get("Income", data))
  # 
  # x[6,2] <- mean(get("SingleFamily", data)) - 1.645*sd(get("SingleFamily", data))
  # x[7,3] <- mean(get("HomeOwn", data)) - 1.645*sd(get("HomeOwn", data))
  # x[8,4] <- mean(get("Edu", data)) - 1.645*sd(get("Edu", data))
  # x[9,5] <- mean(get("White", data)) - 1.645*sd(get("White", data))
  # x[10,6] <- mean(get("Income", data)) - 1.645*sd(get("Income", data))
  
  
  x[1,2] <- 1
  x[2,3] <- 1
  x[3,4] <- 1
  x[4,5] <- 1
  x[5,6] <- 1
  
  x[6,2] <- 0
  x[7,3] <- 0
  x[8,4] <- 0
  x[9,5] <- 0
  x[10,6] <- 0
  
  
  sims <- 10000
  pe <- fit %>% tidy() %>% dplyr::select(estimate) %>% as.matrix()
  vc <- vcov(fit)
  simbetas <- mvrnorm(sims, pe, vc)
  xbeta <- x %*% t(simbetas) %>% exp()
  xbeta <- xbeta*mean(get("Unit", data))
  
  xbeta <- xbeta[1:5,] - xbeta[6:10,]
  
  pe <- apply(xbeta, 1, mean) 
  upper <- apply(xbeta, 1, quantile, probs= 0.975) 
  lower <- apply(xbeta, 1, quantile, probs= 0.025)
  
  total_pe <- cbind(pe,upper,lower) %>%
    as.data.frame() %>% 
    mutate(tech = tech,
           group = c("SingleFamily","HomeOwn","Edu","White","Income"))
  
  return(total_pe)
}

# plotting for predictor vs. technologies 
s_plot <- function(total_pe, tech){
  Var <- tech
  
  if(tech == "PV"){
    sp <- "Rooftop solar"
  }else if(tech == "EV"){
    sp <- "Electric vehicle"
  }else{
    sp <- "Heat pump"
  }
  
  fig <- total_pe %>%
    filter(tech == Var) %>% 
    ggplot(aes(y = reorder(group,pe),
               x = pe,
               xmax = upper,
               xmin = lower,
               color = urban)) +
    geom_pointrangeh(position = position_dodge2v(height = 0.7), fatten = 2, size = 0.8, shape = 15) +
    # scale_color_grey()+
    geom_vline(xintercept = 0, color ="black", linetype = "dashed", size = 0.5)+
    scale_x_continuous(# limits = c(min(total_pe[3]), max(total_pe[2])),
      # breaks = seq(min(total_pe), max(total_pe), by =2),
      # labels = c("0%", "+1%", "+2%"),
      sec.axis =  sec_axis(~ ., name = "")) + # sec.axis = sec_axis(~ . - mean(sp_regime$Adoption), name = "Deviation from Mean %")
    labs(title = "", x = paste(sp ,"\nadoption\ndifference [%]"), y = "", colour = "Area") +
    theme_minimal() + 
    theme(plot.background = element_blank(),
          panel.grid.minor = element_blank())
  
  if(Var != "Pump"){
    fig <- fig + 
      theme(aspect.ratio = 2,
            axis.text.x = element_text(color = "black", size = 12),
            axis.text.y = element_text(color = "black", size = 12),
            axis.ticks.x = element_blank(),
            legend.position = "none",
            axis.line.x = element_line(color="black"))
  }else{
    fig <- fig +
      ggtitle("") +
      theme(aspect.ratio = 2,
            axis.text.x = element_text(color = "black", size = 12),
            axis.text.y = element_text(color = "black", size = 12),
            axis.ticks.x = element_blank(),
            # legend.position = c(0.9, 0.17),
            legend.position = c(0.9,0.17),
            axis.line.x = element_line(color="black"),
            legend.background = element_rect(size=0.5, linetype="solid"))
  }
  return(fig)
}

### line plots for technologies vs. predictors
countf <- function(data, tech, predictor){
  
  if(tech == "PV"){
    rho <- "Rho.PV"
    expected <- "E_PV"
  }else if(tech == "EV"){
    rho <- "Rho.EV"
    expected <- "E_EV"
  }else{
    rho <- "Rho.Pump"
    expected <- "E_Pump"
  }
  
  # data <- data %>% st_drop_geometry() %>% 
  #   mutate_at(vars(HomeOwn:White, Rho.PV:Rho.Pump), funs(rescale(.) %>% as.numeric(.)))
  
  data <- data %>% st_drop_geometry() %>% 
    mutate(SingleFamily = ifelse(SingleFamily > mean(SingleFamily), 1,0),
           HomeOwn = ifelse(HomeOwn > mean(HomeOwn), 1,0),
           Edu = ifelse(Edu > mean(Edu), 1,0),
           White = ifelse(White > mean(White), 1,0),
           Income = ifelse(Income > mean(Income), 1,0)) %>% 
    mutate_at(vars(r_PV:r_Pump,Rho.PV:Rho.Pump), funs(rescale(.) %>% as.numeric(.)))
  
  
  fit <- glm.nb(get(tech, data) ~ SingleFamily+HomeOwn+Edu+White+Income+get(rho, data)+
                  offset(log(get("Unit", data))), data = data)
  
  # x <-data %>%                    
  #   dplyr::select(SingleFamily,HomeOwn,Edu,White,Income,rho) %>% 
  #   summarize_all(mean) %>%
  #   uncount(20) %>% 
  #   mutate({{predictor}} := seq(mean(get(predictor, data)) - 1.645*sd(get(predictor, data)), 
  #                               mean(get(predictor, data)) + 1.645*sd(get(predictor, data)), length.out = 20),
  #          cont = 1
  #   ) %>% 
  #   relocate(cont)
  
  x <-data %>%                    
    dplyr::select(SingleFamily,HomeOwn,Edu,White,Income,rho) %>% 
    summarize_all(mean) %>%
    uncount(20) %>% 
    mutate({{predictor}} := seq(0,1, length.out = 20),
           cont = 1
    ) %>% 
    relocate(cont)
  
  x <- x %>% 
    as.matrix()
  
  sims <- 10000
  pe <- fit %>% tidy() %>% dplyr::select(estimate) %>% as.matrix()
  vc <- vcov(fit)
  simbetas <- mvrnorm(sims, pe, vc)
  xbeta <- x %*% t(simbetas) %>% exp()
  xbeta <- xbeta*mean(get("Unit", data))
  
  pe <- apply(xbeta, 1, mean) 
  upper <- apply(xbeta, 1, quantile, probs= 0.975)
  lower <- apply(xbeta, 1, quantile, probs= 0.025)
  
  total_pe <- cbind(pe,upper,lower, x %>% 
                      as.data.frame() %>% 
                      mutate({{predictor}} := rescale(get(predictor),to=c(0,1))) %>% 
                      dplyr::select(predictor)) %>%
    as.data.frame() %>% 
    mutate(Tech = tech,
           Var = predictor) %>% 
    rename(Value = predictor)
  
  return(total_pe)
}
