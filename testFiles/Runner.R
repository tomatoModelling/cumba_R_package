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
#install_github("tomatoModelling/cumba_R_package")
#library(cumba)


# Set this directory as the working directory ----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Read excel sheets with input data ----
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


# Read parameters ----
param<-read.csv('parameters.csv') |> 
  select(-c(unit,min,max)) |> 
  pivot_wider(names_from=parameter, values_from=c(value))

estimateRadiation = T
estimateET0 = T
weather<-weather |> 
  rename(Rad=RAD) |> 
  mutate(Lat = 35) 

#library(data.table)

source("..//R//Main.R")

#call the cumba function
outputs<-cumba_experiment(weather, param, estimateRad = T,estimateET0 = T,
              irrigation_df)   

#call the cumba function
outputs<-cumba_scenario(weather, param, estimateRad = T,estimateET0 = T,
                        waterStressLevel=.2, minimumTurn = 10)  

lastDay<-outputs |> 
  group_by(experiment) |> 
  slice_tail() |> 
  select(experiment,doy)

yieldPlot<-yield |> 
  group_by(ID) |> 
  summarise(yieldMean = mean(Y_TOT *.05*10),
            yieldSD = sd(Y_TOT*.05*10)) |> 
  left_join(lastDay,by=c('ID'='experiment'))

outputs_ref<-outputs |> 
  left_join(yieldPlot,by=c('experiment'='ID','doy'='doy'))

ggplot(outputs_ref  )+
  geom_area(aes(x=doy,y=floweringRateIde*40),col='black',alpha=0.5)+
  geom_area(aes(x=doy,y=floweringRateAct*40),col='red',alpha=0.5)+
  #geom_line(aes(x=doy,y=floweringStatePot*10),col='black',alpha=0.5)+
  #geom_line(aes(x=doy,y=floweringStateAct*10),col='red',alpha=0.5)+
  #geom_line(aes(x=doy,y=fruitSetCoefficient),col='black',alpha=0.5)+
  geom_line(aes(x=doy,y=fruitsStatePot*.01),col='black',alpha=1,linetype=2,size=1)+
  geom_line(aes(x=doy,y=fruitsStateIde*.01),col='blue',alpha=1,size=1)+
  geom_line(aes(x=doy,y=fruitsStateAct*.01),col='red',alpha=1,size=1)+
  #geom_col(aes(x=doy,y=heatStress*10,fill=factor(phenoCode)),fill='red')+
  #geom_col(aes(x=doy,y=waterStress*10,fill=factor(phenoCode)),fill='blue')+
  geom_line(aes(x=doy,y=fIntPot*20),size=2)+
  geom_line(aes(x=doy,y=fIntAct*20),size=2,col='darkgreen')+
  geom_line(aes(x=doy,y=carbonRatePot))+
  geom_line(aes(x=doy,y=carbonRateIde),col='blue')+
  geom_line(aes(x=doy,y=carbonRateAct),col='darkred',size=2)+
  #geom_line(aes(x=doy,y=carbonStatePot/100))+
  #geom_line(aes(x=doy,y=carbonStateAct/100),col='darkred')+
  #ylim(0,40)+
  theme_bw()+
  xlim(120,230)+
  facet_wrap(~experiment,ncol=8,scales='free_y')+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())


  ############## PLOT THE OUTPUT  ###############################################
#ggplot(outputs |> filter(experiment == 1))+
ggplot(outputs_ref)+
  #geom_area(aes(x=doy,y=floweringRatePot*1000),col='yellow',alpha=0.5)+
  #geom_area(aes(x=doy,y=floweringRateAct*1000),col='pink4',alpha=0.5)+
  #geom_line(aes(x=doy,y=floweringStatePot*100),col='yellow4',alpha=0.5)+
  #geom_line(aes(x=doy,y=floweringStateAct*100),col='pink4',alpha=0.5)+
  #geom_line(aes(x=doy,y=fruitSetCoefficient),col='black',alpha=0.5)+
  #geom_line(aes(x=doy,y=fruitsStatePot*0.001),col='tomato',alpha=1,linetype=2,size=1)+
  geom_line(aes(x=doy,y=fruitsStateAct*0.001),col='tomato3',alpha=1,size=1)+
  geom_point(aes(x=doy,y=yieldMean*0.001),col='tomato4',alpha=0.5)+
  geom_errorbar(aes(x=doy,ymin=yieldMean*0.001-yieldSD*0.001,ymax=yieldMean*0.001+yieldSD*0.001),col='tomato4',alpha=0.5)+
  
  #geom_line(aes(x=doy,y=0.4-rootState/250),size=.8)+
  #geom_line(aes(x=doy,y=TRC1),col='red')+
  #geom_line(aes(x=doy,y=EV1),col='blue')+
  #geom_line(aes(x=doy,y=DC1),col='cyan')+
  geom_line(aes(x=doy,y=wc1+.1),col='darkgoldenrod1',size=.8)+
  geom_line(aes(x=doy,y=wc2+.1),col='peru',size=.8)+
  #geom_line(aes(x=doy,y=DC2),col='green3')+
  geom_line(aes(x=doy,y=wc3+.1),col='saddlebrown',size=.8)+
  #geom_line(aes(x=doy,y=waterStress),col='pink4')+
  #geom_line(aes(x=doy,y=waterStress),col='darkblue')+
  #geom_area(aes(x=doy,y=gddS,fill=factor(phenoCode)),alpha=0.2)+
  #geom_col(aes(x=doy,y=heatStress*1000,fill=factor(phenoCode)),fill='red')+
  #geom_col(aes(x=doy,y=coldStress*1000,fill=factor(phenoCode)),fill='blue')+
  #geom_line(aes(x=doy,y=fIntPot,col=factor(phenoCode)))+
  #geom_line(aes(x=doy,y=fIntAct,col=factor(phenoCode)),col='darkgreen')+
  #geom_line(aes(x=doy,y=carbonRatePot))+
  #geom_line(aes(x=doy,y=carbonRateAct),col='darkred')+
  #geom_line(aes(x=doy,y=carbonStatePot/100))+
  #geom_line(aes(x=doy,y=carbonStateAct/100),col='darkred')+
  #geom_line(aes(x=doy,y=kc*10),col='green')+
  #geom_line(aes(x=doy,y=ET0),col='blue')+
  #geom_line(aes(x=doy,y=ETr),col='red')+
  #geom_line(aes(x=doy,y=soilWater))+
  #geom_line(aes(x=doy,y=cropTransp),col='red')+
  #geom_line(aes(x=doy,y=waterStress),col='black')+
  geom_col(aes(x=doy,y=p*0.01),fill='slateblue1',width=0.7)+
  geom_col(aes(x=doy,y=irrigation*0.01),fill='aquamarine4',width=0.7)+
  #ylim(0,40)+
  theme_classic()+
  xlim(120,230)+
  facet_wrap(~experiment,ncol=8,scales='free_y')+
  scale_y_continuous(name='soil water content (m3/m3)',
                     sec.axis = sec_axis( trans=~./0.01, 
                                          name="Precipitation and Irrigation (mm)"))+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())



ggsave('soilWaterAndIrrigation.png',width=14,height=3)

# Filter to keep only rows where doy is an integer and var is 0, 1, 2, or 3
df_filtered <- outputs_ref |> 
  filter(year==2017) |> 
  group_by(experiment,phenoCode) |> 
  slice(1L) |> 
  filter(phenoCode>0)
  

############## PLOT THE OUTPUT  ###############################################
#ggplot(outputs |> filter(experiment == 1))+
ggplot(outputs_ref |> filter(year==2017) )+
  geom_area(aes(x=doy,y=carbonRateAct/20),fill='green',size=0.2,alpha=.4)+
  geom_area(aes(x=doy,y=floweringRatePot),fill='pink',col='pink',alpha=1)+
  geom_area(aes(x=doy,y=floweringRateAct),fill='pink4',col='pink4',alpha=0.9)+
  geom_line(aes(x=doy,y=fIntAct),fill='green3',col='green3',size=1)+
  #geom_line(aes(x=doy,y=floweringStatePot*100),col='yellow4',alpha=0.5)+
  #geom_line(aes(x=doy,y=floweringStateAct*100),col='pink4',alpha=0.5)+
  #geom_line(aes(x=doy,y=fruitSetCoefficient),col='black',alpha=0.5)+
  #geom_line(aes(x=doy,y=fruitsStatePot*0.001),col='tomato',alpha=1,linetype=2,size=1)+
  geom_line(aes(x=doy,y=fruitsStateAct*0.001),col='tomato3',alpha=1,size=1)+
  geom_point(aes(x=doy,y=yieldMean*0.001),fill='black',shape=21,alpha=1,size=2.5)+
  geom_errorbar(aes(x=doy,ymin=yieldMean*0.001-yieldSD*0.001,ymax=yieldMean*0.001+yieldSD*0.001),col='black',alpha=1)+
  geom_vline(data=df_filtered,aes(xintercept=doy,y=1),col='grey23',linetype=2)+
  #geom_line(aes(x=doy,y=-rootS/100))+
  #geom_line(aes(x=doy,y=TRC1),col='red')+
  #geom_line(aes(x=doy,y=EV1),col='blue')+
  #geom_line(aes(x=doy,y=DC1),col='cyan')+
  #geom_line(aes(x=doy,y=wc1+.1),col='darkgoldenrod1',size=.8)+
  #geom_line(aes(x=doy,y=wc2+.1),col='peru',size=.8)+
  #geom_line(aes(x=doy,y=DC2),col='green3')+
  #geom_line(aes(x=doy,y=wc3+.1),col='saddlebrown',size=.8)+
  #geom_line(aes(x=doy,y=waterStress),col='pink4')+
  #geom_line(aes(x=doy,y=waterStress),col='darkblue')+
  #geom_area(aes(x=doy,y=gddS,fill=factor(phenoCode)),alpha=0.2)+
  geom_line(aes(x=doy,y=1-heatStress,fill=factor(phenoCode)),col='black',size=.25)+
  #geom_line(aes(x=doy,y=1-coldStress,fill=factor(phenoCode)),col='cyan',alpha=0.4)+
  #geom_line(aes(x=doy,y=fIntPot,col=factor(phenoCode)))+
 
  #geom_line(aes(x=doy,y=carbonRatePot))+

  #geom_line(aes(x=doy,y=carbonStatePot/100))+
  #geom_line(aes(x=doy,y=carbonStateAct*0.001),col='darkred')+
  #geom_line(aes(x=doy,y=kc*10),col='green')+
  #geom_line(aes(x=doy,y=ET0),col='blue')+
  #geom_line(aes(x=doy,y=ETr),col='red')+
  #geom_line(aes(x=doy,y=soilWater))+
  #geom_line(aes(x=doy,y=cropTransp),col='red')+
  #geom_line(aes(x=doy,y=waterStress),col='black')+
  #geom_col(aes(x=doy,y=p*0.01),fill='slateblue1',width=0.7)+
  #geom_col(aes(x=doy,y=irrigation*0.01),fill='aquamarine4',width=0.7)+
  #ylim(0,40)+
  theme_classic()+
  xlim(120,230)+
  ylim(0,1)+
  lemon::facet_rep_wrap(~experiment,ncol=8, repeat.tick.labels=T)+
  scale_y_continuous(breaks = seq(0,1,by=.2),name='light interception and heat stress (-)',
                     sec.axis = sec_axis( trans=~./0.01, 
                                          name="yield (dw, g/m2)"))+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())+
  xlab('')
ggsave('yield2017.png',width=14,height=3)
