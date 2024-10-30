library(tidyverse)

k0<-3
gammaCarbon<-.44
gammaSugar<-.42
photoPeriod <-14
hourSunrise <-6

out_cumba<-outputs |> 
  filter(experiment == 1) |> 
  select(doy,carbonRateAct,carbonStateAct) |> 
  rename(DMrate = carbonRateAct, DMstate = carbonStateAct)

out_cumba$kt_aux<-0
out_cumba$carbonSugarRate<-0
out_cumba$carbonSugarState<-0
DM_state_1<-0
DM_rate_1<-0
i<-1
for(i in 1:nrow(out_cumba))
{
  doy<-out_cumba[i,1][[1]]
  if(i == 1)
  {
    DMrate_1<-0
    DMstate_1<-0
    DMrate<-out_cumba[i,2][[1]]
    DMstate<-out_cumba[i,3][[1]]
  }
  else
  {
    DMrate<-out_cumba[i,2][[1]]
    DMstate<-out_cumba[i,3][[1]]
    DMrate_1<-out_cumba[i-1,2][[1]]
    DMstate_1<-out_cumba[i-1,3][[1]]
  }
  
  kt_aux <- kt_function(k0,DMrate,DMstate)
  
  out_cumba[i,4]<-kt_aux

  
  if(kt_aux>0.1)
  {
    #TODO: compute photoperiod, sunrise and sunset as integer 
    DM_rate_lightHours = DMrate/photoPeriod
    hourSunrise <- (24- photoPeriod)/2
    hourSunset <- hourSunrise + photoPeriod
    DM_rate_array <- c(rep(0,hourSunrise),
                       rep(DM_rate_lightHours,photoPeriod),
                       rep(0,hourSunrise))
    #hourly loop
    hour<-1
    #create an empty array
    carbonSugarState<-c(rep(0,24))
    #local variable to store the state variable of the previous time step (hour)
    carbonSugarState_hour<-0
    #hourly loop
    for(hour in 1:24)
    {
      #to manage the change of day
      if(hour == 1){#assign to the first hour the value of the last hour of the previous day 
        carbonSugarState_hour = carbonSugarState[[24]]}
      
      else {#otherwise assign the value of the previous hour
        carbonSugarState_hour = carbonSugarState[[hour-1]]}
      
      #define the carbon flow from the phloem - gC m-2 h-1
      carbonSugarRate_in <- DM_rate_array[hour] * gammaCarbon
      #define the carbon used to synthesize other compounds scaled to hourly - gC m-2 h-1
      carbonOtherCompounds_out <- carbonSugarState_hour * gammaCarbon * kt_aux * 1/24 
      #compute the carbon rate in sugar gC d-1
      carbonSugarRate <- carbonSugarRate_in - carbonOtherCompounds_out 
      # integrate the state variable of carbon in sugar - gC m-2 h-1
      carbonSugarState[[hour]] <- carbonSugarState_hour + carbonSugarRate
      if(carbonSugarState[[hour]]<0)
      {
        carbonSugarState[[hour]]=0
      }
    }
    out_cumba[i,5] <- carbonSugarRate
    #assign the variable
    
    out_cumba[i,6] <- carbonSugarState[[24]]
    
  }
  else
  {
    #define the carbon flow from the phloem - gC m-2 d-1
    carbonSugarRate_in <- DMrate_1 * gammaCarbon
    #define the carbon used to synthesize other compounds - gC m-2 d-1
    carbonOtherCompounds_out <- DM_state_1 * gammaCarbon * kt_aux 
    #compute the carbon rate in sugar gC d-1
    carbonSugarRate <- carbonSugarRate_in - carbonOtherCompounds_out 
    out_cumba[i,5] <- carbonSugarRate
    # integrate the state variable of carbon in sugar - gC m-2 d-1
    out_cumba[i,6] <- out_cumba[i-1,6][[1]] + carbonSugarRate
  }
}
  
library(tidyverse)
ggplot(out_cumba,aes(x=doy)) + 
  geom_point(aes(y = carbonSugarState/100))+
  geom_point(aes(y = carbonSugarRate/100),col='blue')+
  geom_line(aes(y = carbonSugarState/100))+
  geom_line(aes(y=kt_aux),col='red')+
  geom_point(aes(y=kt_aux),col='red')+
  ylim(0,0.5)+
  xlim(118,200)

library(ggplotly)  
ggplotly(a)ggplot




df_out <- read.csv()


C_sugRate<-function(dm_rate,gammaCarbon,)
{
  kt<-k0*(dm_rate*1/dm_state)^1.36
  return(kt)
}

kt_function<-function(k0,dm_rate,dm_state)
{
  kt<-k0*(dm_rate*1/dm_state)^1.36
  return(kt)
}
