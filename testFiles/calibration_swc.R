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
#devtools::uninstall("cumba")
#devtools::install()
#library(cumba)
#devtools::document()
#devtools::load_all()

# Set this directory as the working directory ----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(cumba)

#load dw data
swc <- read_excel('SoilWaterContent.xlsx') |> 
  janitor::clean_names() |> 
  #filter(id%in%c(28,32)) |> 
  mutate(date = as.Date(paste0(year,'-',month,'-',day),format='%Y-%m-%d'),
         doy=yday(date)) 

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

par$WaterStressSensitivity$value<-5
par$Tbase$value<-10
par$Topt$value<-25
par$Tmax$value<-35
par$TransplantingLag$value<-9
par$InitialInt$value<-.001
par$HalfIntGrowth$value<-15
par$HalfIntSenescence$value<-100
par$FIntMax$value<-.92
par$SoilWaterInitial$value<-100
par$DepletionFraction$value<-80

#TODO: calibrate
par$RootIncrease$value<-0.3


wp <- c(rep(.211,2), rep(.225,3))
fc<-c(rep(.377,2),rep(.47,3))
#fc<-c(rep(.377,2),rep(.45,3))
years<-c(2017,2017,rep(2018,3))
exps<-c(27,28,29,30,32)
swcIni<-c(rep(100,2),rep(40,3))
i<-1

outputs_list<-list()

thisYear<-years[1]
for(i in 1:length(wp))
{
  thisYear <-years[[i]]
  thisFc<-fc[[i]]
  thisWp<-wp[[i]]
  thisExp <- exps[[i]]
  
  par$WiltingPoint$value<- wp[[i]]
  par$FieldCapacity$value<-fc[[i]]
  par$SoilWaterInitial$value<-swcIni[[i]]
  
  param<-par
  ##call the cumba function
  outputs<-cumba_experiment(weather |> mutate(year=year(Date)) |>
                            filter(year%in%c(thisYear)), par,
                          estimateRad = T, estimateET0 = T,
                          irrigation_df |> filter(ID%in%c(thisExp)))
  
  #write.csv(outputs,'testET0.csv')
  outputs_list[[i]]<-outputs
}

library(tidyverse)

out<-do.call(rbind,outputs_list) |> 
  # select(site,year,doy,experiment,wc1,wc2,wc3,wc1mm,wc2mm,wc3mm,rootState,gddState,
  #        waterStress,p,irrigation,fIntAct,phenoCode,et0,radiation) |> 
  mutate(wc3=ifelse(!is.na(wc3),wc3,0)) |> 
  mutate(swc_sim=(wc1*3*10+wc2*(rootState-3)*10+wc3*(60-rootState)*10)/600) |> 
  #mutate(swc_sim=(wc2*(rootState-3)*10+wc3*(60-rootState)*10)/600) |> 
  mutate(swc_sim_2=(wc1mm+wc2mm+wc3mm)/600) |> 
  left_join(swc, by=c('year','doy', 'experiment'='id')) |> 
  mutate(swc=ifelse(gddState/1330<=0.7,swc,NA))  


summarySim<-out |> 
  group_by(experiment) |> 
  summarise(yield=max(fruitFreshWeightAct),
            brix=max(brixAct,na.rm=T),
            irrigation=sum(irrigation),
            prec=sum(p))

ggplot(out ,aes(x=doy)) + 
  geom_col(aes(y=p/100),fill='blue',alpha=.4)+
  geom_col(aes(y=irrigation/100),col='red',alpha=.4)+
  geom_point(aes(y=swc/100))+
  # geom_line(aes(y=et0))+
  # geom_line(aes(y=radiation),col='gold2')+
   geom_line(aes(y=wc1),col='orange')+
   geom_line(aes(y=wc2),col='red')+
   geom_line(aes(y=wc3),col='blue')+
 # geom_line(aes(y=ftsw),col='red',linewidth=1.2)+
  geom_line(aes(y=swc_sim_2),col='gold2',linewidth=1.2)+
  geom_line(aes(y=swc_sim),col='gold4',linewidth=1.2)+
  geom_line(aes(y=rootState/100),col='black',linewidth=1.2)+
  geom_area(aes(y=1-waterStress),fill='red4',alpha=.3)+
  # geom_vline(data=out |> group_by(experiment,phenoCode) |> 
  #              slice_head(),aes(xintercept=doy),col='black',linewidth=1.2)+
  # geom_line(aes(y=swc_sim_2),col='red',linewidth=1.2)+
  geom_line(aes(y=fIntAct),col='pink4',linewidth=1.2)+
 # geom_line(aes(y=rootState),col='black')+
  theme_bw()+
  facet_wrap(~experiment)
  #lemon::facet_rep_wrap(~experiment,repeat.tick.labels = T)

#Kcmid computation
# waterBalance=out |> 
#   filter(experiment==28) |> 
#   filter(fIntAct>=.6&!is.na(swc)) |> 
#   summarise(p=sum(p+irrigation),
#             et0=sum(et0),
#             fintAve=mean(fIntAct))
# 
# swcs=out |> 
#   filter(experiment==28) |> 
#   filter(fIntAct>=.6&!is.na(swc)) 

# swcIni<-swcs[1,]$swc * 600/100
# swcFin <- swcs[nrow(swcs),]$swc * 600/100
# 
# #Kcini computation
# waterBalance_ini=out |> 
#   filter(experiment%in%c(27)) |> 
#   filter(phenoCode==0) |> 
#   summarise(p=sum(p+irrigation),
#             et0=sum(et0))
# 
# swcs_ini=out |> 
#   filter(experiment%in%c(27,28)) |> 
#   filter(phenoCode==0) |> 
#   filter(!is.na(swc)) |> 
#   group_by(doy) |> 
#   summarise(swc=mean(swc))






