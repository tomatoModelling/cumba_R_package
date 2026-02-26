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




wp <- c(.13,.14, .15,.14,.17)
fc<-c(.355,.37,.40,.42,.41)
#fc<-c(rep(.377,2),rep(.45,3))
years<-c(2017,2017,rep(2018,3))
exps<-c(27,28,29,30,32)
swcIni<-c(rep(100,2),rep(100,3))
i<-1

outputs_list<-list()

lastDay<-cumba::tomatoFoggia$weather |> 
  mutate(doy=yday(Date),
         YEAR=year(Date)) |> 
  group_by(YEAR) |> 
  slice_tail() |> 
  left_join(cumba::tomatoFoggia$management) |> 
  dplyr::select(ID,YEAR,doy) 
  
yields<-cumba::tomatoFoggia$production |> 
  filter(ID%in%c(27:32)) |> 
  left_join(lastDay)

par<-cumba::cumbaParameters

par$WaterStressSensitivity$value<-5
par$Tbase$value<-10
par$Topt$value<-25
par$Tmax$value<-35
par$TransplantingLag$value<-9
par$InitialInt$value<-.001
par$HalfIntGrowth$value<-20
par$FIntMax$value<-.96
par$SoilWaterInitial$value<-100
par$DepletionFraction$value<-60
par$FruitWaterContentMax$value<-.91
par$HalfIntSenescence$value<-100
par$RootDepthMax$value<-65
par$FruitWaterContentMin$value<-0.6
par$FloweringLag$value<-30
par$RUE$value<-2.5

#TODO: calibrate
par$RootIncrease$value<-0.5

thisYear<-years[1]
for(i in 1:length(wp))
{
  thisYear <-years[[i]]
  thisFc<-fc[[i]]
  thisWp<-wp[[i]]
  thisExp <- exps[[i]]
  
  par$WiltingPoint$value<- wp[[i]]
  par$FieldCapacity$value<-fc[[i]]
  par$SoilWaterInitial$value<- swcIni[[i]]
  
  param<-par
  ##call the cumba function
  outputs<-cumba_experiment(weather |> mutate(year=year(Date)) |>
                            filter(year%in%c(thisYear)), par,
                          estimateRad = T, estimateET0 = T,
                          irrigation_df |> filter(ID%in%c(thisExp)),
                          fullOut = T)
  
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
  left_join(yields, by=c('experiment'='ID','doy'))  
  #mutate(swc=ifelse(gddState/1330<=0.7,swc,NA))  


summarySim<-out |> 
  group_by(experiment) |> 
  summarise(yield=max(fruitFreshWeightAct),
            brix=max(brixAct,na.rm=T),
            irrigation=sum(irrigation),
            prec=sum(p))

ggplot(out,aes(x=doy)) + 
  geom_area(aes(y=(1-waterStress)*500),fill='red4',alpha=.4)+
   geom_col(aes(y=p*10),fill='blue',alpha=.4)+
  geom_col(aes(y=irrigation*10),col='red',alpha=.4)+
   geom_point(aes(y=yield_ref*2),col='red')+
   geom_point(aes(y=brix_ref*100))+
  geom_line(aes(y=fIntAct*500),col='green3')+
  geom_line(aes(y=fruitFreshWeightAct*0.01*2),col='red')+
  geom_line(aes(y=brixAct*100),col='black')+
  geom_point(aes(y=swc*10),col='blue')+
   geom_line(aes(y=swc_sim_2*1000),col='blue',linewidth=.8)+
  theme_bw()+
  lemon::facet_rep_wrap(~experiment,repeat.tick.labels = T)

rmse_swc <- out |> 
  ungroup() |> 
  filter(!is.na(swc)) |> 
  summarise(rmse = hydroGOF::rmse(swc_sim_2,swc/100),
            r = round(hydroGOF::rPearson(swc_sim_2,swc/100),2)) |> 
  pull(rmse,r)
rmse_swc

rmse_brix<-out |> 
  ungroup() |> 
  filter(!is.na(brix_ref)) |> 
  summarise(rmse = hydroGOF::rmse(brixAct,brix_ref),
            r = round(hydroGOF::rPearson(brixAct,brix_ref),2)) |> 
  pull(rmse,r)
rmse_brix

rmse_yield<-out |> 
  ungroup() |> 
  filter(!is.na(yield_ref)) |> 
  summarise(rmse = hydroGOF::rmse(fruitFreshWeightAct*.01,yield_ref),
            r = round(hydroGOF::rPearson(fruitFreshWeightAct*.01,yield_ref),2)) |> 
  pull(rmse,r)
rmse_yield


#EXECUTE ALL EXPERIMENTS
##call the cumba function
outputs_all<-cumba_experiment(tomatoFoggia$weather, par,
                            estimateRad = T, estimateET0 = T,
                            tomatoFoggia$irrigation,
                            fullOut = T)

yields_all<-cumba::tomatoFoggia$production |> 
  left_join(lastDay)

out<-outputs_all |> 
  # select(site,year,doy,experiment,wc1,wc2,wc3,wc1mm,wc2mm,wc3mm,rootState,gddState,
  #        waterStress,p,irrigation,fIntAct,phenoCode,et0,radiation) |> 
  mutate(wc3=ifelse(!is.na(wc3),wc3,0)) |> 
  mutate(swc_sim=(wc1*3*10+wc2*(rootState-3)*10+wc3*(60-rootState)*10)/600) |> 
  #mutate(swc_sim=(wc2*(rootState-3)*10+wc3*(60-rootState)*10)/600) |> 
  mutate(swc_sim_2=(wc1mm+wc2mm+wc3mm)/600) |> 
  left_join(swc, by=c('year','doy', 'experiment'='id')) |> 
  left_join(yields_all, by=c('experiment'='ID','doy')) |> 
  mutate(swc=ifelse(gddState/1330<=0.7,swc,NA))  

ggplot(out,aes(x=doy)) + 
  geom_point(aes(y=yield_ref),col='red')+
  geom_point(aes(y=brix_ref*100))+
  geom_line(aes(y=fruitFreshWeightAct*0.01),col='red')+
  geom_line(aes(y=brixAct*100),col='black')+
  theme_bw()+
  facet_wrap(~experiment)

brix_data<-out |> 
  filter(!is.na(brix_ref))

ggplot(brix_data)+ 
#         filter(!experiment %in% c(15:17,25))) +
  geom_point(aes(x = brix_ref, y = brixAct)) +
  geom_text(aes(x = brix_ref, y = brixAct,label=experiment)) +
  geom_smooth(aes(x = brix_ref, y = brixAct),method = "lm")

ggplot(brix_data) + 
         #      filter(!experiment %in% c(15:17,25))) +
  geom_point(aes(x = yield_ref, y = fruitFreshWeightAct)) +
  geom_smooth(aes(x = yield_ref, y = fruitFreshWeightAct),method = "lm")

rmse_brix<-out |> 
  # filter(!experiment %in% c(15:17,25)) |> 
  ungroup() |> 
  filter(!is.na(brix_ref)) |> 
  summarise(rmse = hydroGOF::rmse(brixAct,brix_ref),
            r = round(hydroGOF::rPearson(brixAct,brix_ref),2)^2) |> 
  pull(rmse,r)
rmse_brix

rmse_yield<-out |> 
 # filter(!experiment %in% c(15:17,25)) |> 
  ungroup() |> 
  filter(!is.na(yield_ref)) |> 
  summarise(rmse = hydroGOF::rmse(fruitFreshWeightAct*.01,yield_ref),
            r = round(hydroGOF::rPearson(fruitFreshWeightAct*.01,yield_ref),2)^2) |> 
  pull(rmse,r)
rmse_yield




