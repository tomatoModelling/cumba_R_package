# Clear environment
rm(list = ls())

# Load required packages
library(tidyverse)
library(data.table)
library(devtools)
#install_github("tomatoModelling/cumba_R_package",force=T)
library(cumba)
library(readxl)


# Set working directory to the script's location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# --- Load and Prepare Input Data ---
load("tomatoFoggia.rda")
exp_data<-tomatoFoggia

weather<-read.csv('weather_foggia.csv') |> 
  mutate(Rad = as.numeric(Rg),
         ET0= as.numeric(ET0),
         Tx = TMAX,
         Tn = TMIN,
         Site= 'Foggia',
         P = 0,
         DATE = MDATE,
         Lat = 41) |> 
  mutate(DATE = as.Date(DATE,format = '%m/%d/%Y'),
         year= year(DATE))

source('../R/Main.R')
#TODO: Change with optimized parameters
cumba_par <- cumba::cumbaParameters

# Perform final simulation
optimizedSimulation <- cumba_scenario(weather |> filter(year>=2021), cumba_par,
                                      estimateRad = T, estimateET0 = T,
                                      transplantingDOY = 100,
                                      waterStressLevel = list(
                                        vegetative = 1,
                                        reproductive = 1,
                                        ripening = 0.2
                                      ),
                                      minimumTurn = list(
                                        vegetative = 1,
                                        reproductive = 1,
                                        ripening = 1
                                      ),
                                      fullOut = T)

cumba_par
options(scipen = 999)

bacche<-optimizedSimulation |> 
  group_by(year) |> 
  filter(floweringRateIde>0.49) |> 
  slice_head() |> 
  select(year,doy)

ggplot(optimizedSimulation, aes(x=doy)) + 
  #geom_area(aes(y=floweringRateAct),fill='pink')+
  #geom_line(aes(y=floweringRateIde),fill='black')+
  geom_line(aes(y=phenoCode*10),fill='black')+
 # geom_line(aes(y=fIntPot))+
  geom_line(aes(y=waterStress*100),col='blue')+
    geom_col(aes(y=irrigation))+
  geom_col(aes(y=p),col='red')+
  geom_line(aes(y=fruitFreshWeightAct/100),col='red')+
  #geom_line(aes(y=brixAct),col='green3')+
  geom_line(aes(y=wc2*1000),col='pink3')+
  facet_wrap(~year)
              

