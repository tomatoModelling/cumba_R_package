###################### RUN THE MODEL  ########################################
# Remove objects from the Global Environment----
rm(list=ls())

# Libraries: ----
library(tidyverse)
library(lubridate)
library(readxl)
library(sirad)
library(tidyr)
library(devtools)
library(remotes)
#install_github("tomatoModelling/cumba_R_package",force=T)
library(cumba)

# Set this directory as the working directory ----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load LAI data
lai <- read_excel('LAI_reference.xlsx') |> 
  filter(!is.na(LAI)) |> 
  janitor::clean_names() |> 
  select(id,year,month,day,lai,lai_sd) |> 
  filter(id%in%c(28,32)) |> 
  mutate(date = as.Date(paste0(year,'-',month,'-',day),format='%Y-%m-%d'),
         doy=yday(date)) |> 
  mutate(fIntRef=1-exp(-.7*lai))

ggplot(lai)+
  geom_point(aes(x=doy,y=lai))+
  geom_errorbar(aes(x=doy,ymin=lai-lai_sd,ymax=lai+lai_sd))+
  geom_point(aes(x=doy,y=fIntRef),col='red')+
  facet_wrap(~year)


excel_file <- paste0("Dataset Carucci et al. new.xlsx")
sheet_names <- excel_sheets(excel_file)  # Get the names of all sheets in the Excel file
all_sheets <- lapply(sheet_names, function(sheet) {
  read_excel(excel_file, sheet = sheet) # Read all sheets into a list of data frames
})
# Read weather data ----
weather<-all_sheets[[4]]
irrigation<-all_sheets[[7]]
ids<-all_sheets[[2]]
irrigation_df <- irrigation |> 
  left_join(ids)
irrigation_df$Site <- 'Foggia'
weather$Date<-as.Date(weather$DATE) #convert Date to Date type
weather$Site <- 'Foggia'
yield  <- as.data.frame(all_sheets[5])

estimateRadiation = T
estimateET0 = T
weather<-weather |> 
  rename(Rad=RAD) |> 
  mutate(Lat = 41) 

options(scipen = 999)

units<-all_sheets[[1]]
source('..//R//Main.R')

par<-cumba::cumbaParameters
par$Tbase$value<-10
par$Topt$value<-25
par$Tmax$value<-35
par$TransplantingLag$value<-9
par$InitialInt$value<-.001
par$HalfIntGrowth$value<-15
par$HalfIntSenescence$value<-100
par$FIntMax$value<-.92
##call the cumba function
outputs<-cumba_experiment(weather |> mutate(year=year(Date)) |>
                            filter(year%in%c(2017,2018)), par,
                          estimateRad = T, estimateET0 = T,
                          irrigation_df |> filter(ID%in%c(28,32))) 

#join with reference data
lai_comp <- outputs |> 
  filter(experiment %in% c(28,32)) |> 
  select(site,year,doy,experiment,gddState,fIntAct,phenoCode) |> 
  left_join(lai,join_by('experiment'=='id','year','doy'))

phenoTransition=outputs |> 
  filter(experiment %in% c(28,32)) |> 
  select(site,year,doy,experiment,phenoCode) |> 
  group_by(phenoCode,experiment) |> 
  slice_tail()

ggplot(lai_comp,aes(x=doy)) + 
  geom_point(aes(y=fIntRef))+
  geom_line(aes(y=fIntAct))+
  geom_vline(data=phenoTransition,aes(xintercept = doy))+
  facet_wrap(~experiment)
  
  
  
  
  
  

