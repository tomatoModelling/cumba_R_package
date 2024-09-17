#' the CUMBA model
#'
#' It is a daily time step simulation model which computes the yield and brix degree of a tomato crop as a function of weather data and irrigation options.
#' @param weather a dataframe with weather data which must have the following columns......
#' @param param a dataframe with model parameters values
#' @param estimateRad a boolean value to estimate solar radiation based on temperature using Hargreaves model. Default to 'true' (implying that the column Lat is present in weather df) if 'false' the 'weather' df must have the Rad column
#' @param estimateET0 a boolean value to estimate reference evapotranspiration based on temperature using Hargreaves model. Default to 'true'
#' @param deficitIrrigation a boolean value to estimate irrigation requirements. Default to 'false', implying that the irrigation_df is provided.
#' @param waterStressLevel a float corresponding to the threshold of water stress to trigger automatic irrigation. Default to .5, it is needed only if deficitIrrigation is 'true'.
#' @param minimumTurn an integer corresponding to the minimum number of days elapsed from the previous irrigation event. Default to 4, it is needed only if deficitIrrigation is 'true'.
#' @param irrigation_df a dataframe containing the irrigation scheduling for each experiment defined in the weather dataframe.
#' @return a dataframe containing the weatherDf plus the daily outputs of the cumba model
#' @examples 
#' output <- cumba(weather, param, estimateRad=T, estimateET0=T, deficitIrrigation=F, waterStressLevel=.5, minimumTurn = 4,irrigation_df);
#' @import data.table (>= 1.9.8)
#' @export

cumba <- function(weather, param, estimateRad=T, estimateET0=T,
                  deficitIrrigation=F, waterStressLevel=.5, minimumTurn = 4,irrigation_df)
{
  #load dependencies
  library(data.table)
  
  #opening message
  cat(crayon::red("                       _      __   \n"))
  cat(crayon::red("   ___ _   _ _ __ ___ | |__   \\_\\_ \n"))
  cat(crayon::red("  / __| | | | '_ ` _ \\| '_ \\ / _` |\n"))
  cat(crayon::red(" | (__| |_| | | | | | | |_) | (_| |\n"))
  cat(crayon::red("  \\___|\\__,_|_| |_| |_|_.__/ \\__,_|\n"))
  cat(crayon::red("                                   \n"))
  
  if(deficitIrrigation == T)
  {
    cat(crayon::blue(paste("running in deficit irrigation mode, with water stress level = ",
                           waterStressLevel, " and minimum turn equal to", minimumTurn, " days!\n")))
  }
  else{
    cat(crayon::blue(paste("running in scenario mode with the provided irrigation scheduling!\n")))
  }
  
  # Check if required columns exist in weather data frame
  required_columns <- c("Site", "Tx", "Tn", "P", "DATE")
  if (estimateRad) {
    required_columns <- c(required_columns, "Lat")
  } else {
    required_columns <- c(required_columns, "Rad")
  }
  if (!estimateET0) {
    required_columns <- c(required_columns, "ET0")
  }
  missing_columns <- setdiff(required_columns, colnames(weather))
  if (length(missing_columns) > 0) {
    stop(crayon::red(paste("Missing required columns in 'weather':", paste(missing_columns, collapse = ", "))))
  }
  
  #get columns by name
  colNames <- colnames(weather)
  SiteColID<-which(colnames(weather) == "Site")
  TxColID <- which(colnames(weather) == "Tx")
  TnColID<-which(colnames(weather) == "Tn")
  PColID <- which(colnames(weather) == "P")
  RadColID<-0
  Lat <- 0
  if(estimateRad == F)
  {
    RadColID <- which(colnames(weather)=='Rad')
  }else
  {
    LatColID <- which(colnames(weather)=='Lat')
  }
  
  ET0ColID<-0
  if(estimateET0 == F)
  {
    ET0ColID<-which(colnames(weather)=="ET0")
  }

  #check irrigation df
  if(deficitIrrigation == F)
  {
    # Check if required columns exist in irrigation_df data frame
    required_irrigation_columns <- c("ID", "Site", "YEAR", "DATE", "WVOL")
    missing_irrigation_columns <- setdiff(required_irrigation_columns, colnames(irrigation_df))
    if (length(missing_irrigation_columns) > 0) {
      stop(paste("Missing required columns in 'irrigation_df':", 
                 paste(missing_irrigation_columns, collapse = ", ")))
    }
  }
  
  # Initialize a counter for time----
  startTime <- Sys.time()
  
  # Define the list of sites in the weather data ----
  sites<-unlist(as.vector(unique(weather[,SiteColID]))) 
  
  # Check if param list contains all required elements
  required_param_elements <- c("Tbase", "Topt", "Tmax", "Theat", "Tcold", "FIntMax", "CycleLength", "TransplantingLag", "FloweringLag", "HalfIntGrowth", "HalfIntSenescence",  "InitialInt", "RUE", "KcIni", "KcMax", "RootIncrease", "RootDepthMax", "RootDepthInitial", "FieldCapacity", "WiltingPoint", "BulkDensity", "WaterStressSensitivity", "FloweringSlope", "FloweringMax")
  missing_param_elements <- setdiff(required_param_elements, names(param))
  if (length(missing_param_elements) > 0) {
    stop(crayon::red(paste("Missing required elements in 'param':", paste(missing_param_elements, collapse = ", "))))
  }
  
  
  #Check Plant cardinal temperature
  if (param$Tmax < param$Topt || param$Tmax < param$Tbase || param$Topt < param$Tbase) {
    stop(crayon::red(paste("Alert: Please ensure that tMax is higher than tOpt, 
                and/or tMax is higher than tBase, 
                and/or tOpt is higher than tBase!\n"))
  } 
  
  #Check Soil hydrologic properties
  if (param$FieldCapacity < param$WiltingPoint)  {
    stop(crayon::red(paste("Alert: Please ensure that field capacity is higher than wilting point!\n"))
  }
  
  
  
  #Extract parameters from the input parameter list 'param'----
  tBase<-param$Tbase
  tOpt<-param$Topt
  tMax<-param$Tmax
  tHeat<-param$Theat
  tCold<-param$Tcold
  fIntMax<-param$FIntMax
  cycleLength<-param$CycleLength
  transplantingLag<-param$TransplantingLag
  floweringLag<-param$FloweringLag
  halfIntGrowth<-param$HalfIntGrowth
  halfIntSenescence<-param$HalfIntSenescence
  initialInt<-param$InitialInt
  rue<-param$RUE
  kcIni<-param$KcIni
  kcMax<-param$KcMax
  rootIncrease<-param$RootIncrease
  rootDepthMax<-param$RootDepthMax
  rootDepthInitial<-param$RootDepthInitial
  fieldCapacity <-param$FieldCapacity
  wiltingPoint <-param$WiltingPoint
  bulkDensity<-param$BulkDensity
  waterStressSensitivity<- - param$WaterStressSensitivity
  floweringSlope<-param$FloweringSlope
  floweringMax<-param$FloweringMax
 
  
  #compute maximum value of the double logistic for flowering
  x<-seq(0:100)
  floweringPotentialFunction<-sapply(x, floweringDynamics, floweringSlope=floweringSlope,       floweringLag=floweringLag, floweringMax=floweringMax)
  floweringPotentialSum<-sum(floweringPotentialFunction)
  
  
  #Define the output vector with column names for the results dataframe ----
  outputNames<-c('site','year','experiment', 'doy','tMax','tMin','p','irrigation',#8
                 'gddRate','gddState','phenoCode','phenoStage','rootRate','rootState',#14
                 'trc1','ev1','dc1','wc1mm','daysNoRain',#19
                 'trc2','dc2','wc2mm',#22
                 'dc3','wc3mm',#24
                 'wc1',"wc2","wc3",#27
                 'waterStress',#28
                 'heatStress','coldStress','fIntPot','fIntAct',#33
                 'kc','et0','etR',#36
                 'radiation',#37
                 'carbonRatePot','carbonStatePot','carbonRateAct','carbonStateAct',#41
                 'floweringRatePot','floweringStatePot','floweringRateAct','floweringStateAct','cycleCompletion',#46
                 'fruitSetCoefficient','fruitsStatePot','fruitsStateAct')#49
  
 #Initialize lists to store outputs at different levels (years, experiments, sites)----
  outputs<-list()
  outputsYear<-list()
  outputsExperiment<-list()
  outputsAll<-list()


  ## Iterate through each site ---- 
  #TODO: only debug
  site<-1
  ids <- unique(irrigation_df$ID)
  for(site in 1:length(sites))
  {
    # Filter the irrigation data for the current site 
    if(deficitIrrigation == F)
    {
      ids <- irrigation_df |> filter(Site == sites[site])
    }
   
    thisSite<-sites[site][[1]]

    # Subset the weather data for the current site
    dfSite <- weather[weather$Site == thisSite, ]
    
    # Add 'year' and 'doy' columns to the weather data
    dfSite$year<-lubridate::year(dfSite$DATE)
    YearColID<-which(colnames(dfSite) == "year")
    dfSite$doy<-lubridate::yday(dfSite$DATE)
    DOYColID<-which(colnames(dfSite) == "doy")

    ##Iterate through each year  ----    
        
    # List of unique years for the current site
    years<-unlist(as.vector(unique(dfSite[,"year"]))) 

    #TODO: for debug
    year<-1
    for(year in 1:length(years))
    {
      # Filter the irrigation data for the current year
      if(deficitIrrigation == F)
      {
        idsYear <- ids |> filter(YEAR == years[[year]])
      }
      else
      {
        availableYears <- years
        idsYear<- data.frame(ID = rep(0,
                                      length(availableYears)),
                             YEAR = years)
      }
      
      #Iterate through each experiment in the current year ----    
      #TODO: for debug
      experiment<-1
      outputsExperiment <-list() #clean the list each experiment
        
      for(experiment in 1:length(unique(idsYear$ID)))
      {
        # Filter the irrigation data for the current experiment
        thisExperiment <- idsYear |> filter(ID == unique(idsYear$ID)[[experiment]])
        thisId<-unique(thisExperiment$ID)
        
        #Initialize state variables
        gddState <- 0
        carbonStatePot <- 0
        carbonStateAct <- 0
        fruitsStatePot<-0
        fruitsStateAct<-0
        rootState<-rootDepthInitial
        daysNoRain<-0
        wc1mm<-(3)*bulkDensity*fieldCapacity *10
        wc2mm<-(rootDepthInitial)*bulkDensity*fieldCapacity *10
        wc3mm<-(rootDepthMax-rootDepthInitial)*bulkDensity*wiltingPoint*10
        wc1<-0
        wc2<-0
        wc3<-0
        newSoil<-0
        ftsw<-1
        ws<-1
        
        # Current year being processed 
        thisYear <- years[year][[1]]
        
        # Subset the weather data for the current year
        dfYear <- dfSite[dfSite$year == thisYear, ]

        # Iterate through each day in the current year ----       
        #TODO: for debug
        day<-2
        outputs<-list() #clean the list each year
        for(day in 1:nrow(dfYear))
        {
          # Current date being processed
          date<-(dfYear[day,1])[[1]]
          
          #if irrigations are provided
          if(deficitIrrigation==F)
          {
            # Filter the irrigation data (mm) for the current date
            irrigationFilter<- thisExperiment |> 
              filter(DATE==date)
            irrigation<-0
            if(nrow(irrigationFilter)==1)
            {
              irrigation <- irrigationFilter$WVOL
            }
          }else
          {
            if(ws < waterStressLevel & daysNoRain>=minimumTurn-1)
            {
              #compute soil water at field capacity
              pwc1 <- 3 * (fieldCapacity )*bulkDensity*10
              pwc2  <- (rootState-3)* (fieldCapacity )*bulkDensity*10
              soilWFC <- pwc1+pwc2
              
              #actual soil water
              soilWActual <- wc1mm + wc2mm
              
              irrigation <- soilWFC-soilWActual
            }else{
              irrigation<-0
            }
          }
          
          # Extract weather data for the current day
          tX <- round(as.numeric(dfYear[day,TxColID]),2) #Maximum temperature, °C
          tN<-round(as.numeric(dfYear[day,TnColID]),2) #Minimum temperature, °C
          doy <- dfYear[day,DOYColID][[1]] #Day of the year
          tAve<-(tX+tN)*0.5 #Maximum temperature, °C
          p<-as.numeric(dfYear[day,PColID]) #Precipitation, mm
          radSim<-0
          if(estimateRad == T)
          {
            Lat <- round(as.numeric(dfYear[day,LatColID]),2)
            radSim <- radiationCompute(Lat,doy,tX,tN) #Simulated radiation, MJ m-2 d-1
          }else
          {
            radSim <- round(as.numeric(dfYear[day,RadColID]),2) #Radiation, MJ m-2 d-1
          }
          et0<-0
          if(estimateET0 == T)
          {
            et0<-0.008*(tAve+17.78)*radSim #Evapotranspiration-Hargreaves method, mm d-1
          }else
          {
            et0 <- round(as.numeric(dfYear[day,ET0ColID]),2) #Evapotranspiration, mm d-1
          }
          #compute gdd
          gdd<-gdd(tAve,tBase,tOpt,tMax) #Growing degree day
          gddRate <- round(gdd*(tOpt-tBase),2) #Growing degree day rate
          gddState <- gddState + gddRate #Growing degree day state
          cycleCompletion <- gddState/cycleLength*100 # Cycle completion percentage
  
          ## Compute root depth (cm)----     
          rootRate<-rootDepth(rootIncrease,gdd) #Root depth rate (cm/d)
          rootState <- ifelse(rootState+rootRate<=rootDepthMax,
                              rootState+rootRate,rootDepthMax) #Root depth state (cm)
          
          #if root state higher than the max, the root rate is 0
          if(rootState>rootDepthMax)
          {
            rootRate<-0
          }
          
          ## Determine phenological phase ----
          phenoTemp<-phenoPhases(gddState,cycleLength,transplantingLag,floweringLag)
          phenoCode <- phenoTemp[[1]]
          phenoStage<-phenoTemp[[2]]
          
          ## Compute heat and cold stress----
          hs<-heatStressLinear(tX,tMax,tHeat) #heat stress
          cs<-coldStressLinear(tN,tBase,tCold) #cold stress
          
          ## Compute potential (the daily rate) and actual 
          ## (using water stress from the previous day)----
          ## interception function of solar radiation ----
          fIntPot<-fIntCompute(fIntMax,cycleLength,transplantingLag,halfIntGrowth,
                               halfIntSenescence,initialInt,gddState)
          if(length(outputs)>0 & phenoCode>=1)
          { 
            fIntPotRate <- fIntPot-outputs[[as.character(doy-1)]][[31]]
            fIntActRate <- fIntPotRate*outputs[[as.character(doy-1)]][[28]]
            fIntAct<-outputs[[as.character(doy-1)]][[32]] + fIntActRate
          }else
          {
            fIntPotRate = fIntPot
            fIntActRate = fIntPot
            fIntAct = fIntPot
          }
          
          ## Compute crop coefficient (Kc) and real ET ----
          kc<- kcCompute(fIntAct,kcIni, kcMax)
          etR<-etRCompute(et0, fIntAct,kcIni,kcMax)
          
          ## Compute soil water content at 1 layer (3cm)----
          #1. Reinitialize days no rain
          if(p+irrigation<=3)
          {
            daysNoRain<-daysNoRain+1
          }else
          {
            daysNoRain<-0
          }
          
          #2. Compute water stress factor from water stress of previous day
          if(length(outputs)>0) #check if it is the first day
          {
            wc1mm<-outputs[[as.character(doy-1)]][[18]]
            wc2mm<-outputs[[as.character(doy-1)]][[22]]
            wc3mm <-outputs[[as.character(doy-1)]][[24]]
            ftsw <- outputs[[as.character(doy-1)]][[28]]
          }
          waterStressFactor<-waterStressFunction(ftsw,waterStressSensitivity)
          
          #3. Compute soil water content
          wc1Function <- c1Content(irrigation,p,rootState,rootDepthMax,etR*waterStressFactor,
                                   fIntAct,et0,
                           daysNoRain,fieldCapacity ,wiltingPoint,bulkDensity,wc1mm)
          
          #4. Update variables
          trc1<-wc1Function[[1]]
          ev1<-wc1Function[[2]]
          dc1<-wc1Function[[3]]
          wc1mm<-wc1Function[[4]]
          wc1<-wc1Function[[5]]

          
          ## Compute soil water content at layer 2 (rootDepth - 3)----
          #1. Compute soil water content
          wc2Function <- c2Content(dc1,rootRate,rootState,rootDepthMax,etR*waterStressFactor,
                                   fIntAct,et0,fieldCapacity ,wiltingPoint,bulkDensity,wc2mm,wc3mm)
          #2. Update variables
          trc2<-wc2Function[[1]]
          dc2<-wc2Function[[2]]
          wc2mm<-wc2Function[[3]]
          newSoil<-wc2Function[[4]]
          wc2<-wc2Function[[5]]
          
          ## Compute soil water content at layer 3-unrooted zone (rootDepthMax - rootDepth)----
          #1. Compute soil water content
          wc3Function<-c3Content(dc2,rootState,rootDepthMax,
                                 fieldCapacity,wiltingPoint,bulkDensity,newSoil,wc3mm)
          #2. Update variables
          dc3<-wc3Function[[1]]
          wc3mm<-wc3Function[[2]]
          wc3<-wc3Function[[3]]

          
          ## Compute water stress ----
          #1. Potential total water content at layer 1 and 2
          pwc1 <- 3 * (fieldCapacity )*bulkDensity*10
          pwc2  <- (rootState-3)* (fieldCapacity )*bulkDensity*10
          #2. water stress
          wsAve = (wc1 + wc2)*.5
          ws <- (wsAve - wiltingPoint) / (fieldCapacity - wiltingPoint ) 
          if(ws>1) {ws<-1}
        
            

          ## Compute carbon rates and update the carbon states (potential and actual)----
          carbonRatePot <- carbonRate(rue,radSim,gdd,fIntPot,0,0,1) #no stress
          carbonRateAct <- carbonRate(rue,radSim,gdd,fIntAct,hs,cs,ws)
          carbonStatePot<-carbonStatePot + carbonRatePot
          carbonStateAct<-carbonStateAct + carbonRateAct
         
          ## Compute flowering rates and update the flowering states (potential and actual) ----
          if(cycleCompletion>=floweringLag)
          {
            floweringRatePot<-floweringDynamics(cycleCompletion,floweringLag,
                                                floweringSlope,floweringMax)
            floweringStatePot <- (floweringRatePot/floweringPotentialSum + 
                                    outputs[[as.character(doy-1)]][[42]]) 
            floweringRateAct <-floweringRatePot*(1-hs)
            floweringStateAct <-  (floweringRateAct/floweringPotentialSum +  
                                     outputs[[as.character(doy-1)]][[44]])  
          }else
          {
            floweringRatePot<-0
            floweringStatePot<-0
            floweringRateAct<-0
            floweringStateAct<-0
          }
          
          ## compute fruit set coefficient
          fruitSetCoefficient <- 1-(floweringStatePot-floweringStateAct)
          
          #compute partitioning to fruits
          fruitsRatePot<-0
          fruitsRateAct<-0
          if(cycleCompletion>=floweringLag)
          {
            fruitsRatePot <- carbonRatePot
            fruitsRateAct<-carbonRateAct*fruitSetCoefficient
          }
          
          if(length(outputs)>0) #check if it is the first day
          {
            fruitsStatePot<-fruitsRatePot+ outputs[[as.character(doy-1)]][[47]]
            fruitsStateAct<-fruitsRateAct+ outputs[[as.character(doy-1)]][[48]]
          }
          
          #Combine all the results into the outputs vector ----
          outputs[[as.character(doy)]] <- list(thisSite,thisYear,thisId,#3
                                      doy,tX,tN,p,irrigation,#8
                                      gddRate, gddState,phenoCode,phenoStage,#12
                                      rootRate,rootState,#14
                                      trc1,ev1,dc1,wc1mm,daysNoRain,#19
                                      trc2,dc2,wc2mm,#22
                                      dc3,wc3mm,#24
                                      wc1,wc2,wc3,#27
                                      ws,hs,cs,#30
                                      fIntPot,fIntAct,#32
                                      kc,et0,etR,radSim,#36
                                      carbonRatePot,carbonStatePot,carbonRateAct,carbonStateAct,#40
                                      floweringRatePot,floweringStatePot,
                                      floweringRateAct,floweringStateAct,#44
                                      cycleCompletion,fruitSetCoefficient,
                                      fruitsStatePot,fruitsStateAct)#48
        
        }
        ## Store the result in the outputs list ----     
        tempOutputs<-as.data.frame(t((matrix(unlist(outputs), 
                                             nrow=length(unlist(outputs[1]))))))
        names(tempOutputs) <- outputNames # Rename columns
        outputsExperiment[[as.character(thisId)]]<-tempOutputs
        
        cat(crayon::green(paste("experiment ", thisId, " in site ", 
                                   thisSite, " and year ", thisYear, " executed\n")))#message to console
      }
      tempOutputsExperiment<-data.table::rbindlist(outputsExperiment)
      outputsYear[[as.character(thisYear)]]<-tempOutputsExperiment
    }
    outputsAll[[as.character(thisSite)]]<-outputsYear
  }
  
  #flatten the output list
  flattenList <- unlist(outputsAll, recursive = F) # Flatten the list
       dfOut <- data.table::rbindlist(flattenList, fill=T)
       colsToConvert <- names(dfOut)[-c(1,12)]
  
  #create the output dataframe
  dfOut<-dfOut[, (colsToConvert) := lapply(.SD, as.numeric), 
              .SDcols = colsToConvert]
  
  
  ## Print the execution time ---- 
  endTime <- Sys.time()
  elapsedTime <- endTime - startTime
  print(paste("Elapsed time:", elapsedTime))
  
  #return the output dataframe
  return(dfOut)
}

### MODEL FUNCTION   ###########################################################
  ## Abiotic stresses ----
        #2.1.1 heat stress (https://www.sciencedirect.com/science/article/abs/pii/S1161030118304234)----
#' @keywords internal      
heatStressLinear<-function(tX,tMax,tHeat)
{
  heatStress<-case_when(
    tX<tMax ~ 0,
    tX>tHeat ~ 1,
    .default = (tX-tMax)/(tHeat-tMax)
  )
  return(heatStress)
}
        #2.1.2 cold stress (https://academic.oup.com/insilicoplants/article/6/1/diad023/7470451)----
#' @keywords internal   
coldStressLinear<-function(tN,tBase,tCold)
{
  coldStress<-case_when(
    tN>tBase~0,
    tN<tCold~1,
    .default=1-((tN-tCold)/(tBase-tCold))
  )
  return(coldStress)
}

  ## Real evapotranspiration ----
        # kc (https://pismin.com/10.1007/s00271-011-0312-2) ----
#' @keywords internal         
kcCompute<-function(fInt,kcIni,kcMax)
{
  kc<- kcIni + (kcMax - kcIni) * fInt
  return(kc)
}
        # Real evapotranspiration ----
#' @keywords internal         
etRCompute<-function(et0,fInt,kcIni,kcMax)
{
  kc <- kcCompute(fInt,kcIni,kcMax)
  etR<-et0*kc
  return(etR)
}

## Phenology (Yan and Hunt, 1999 https://www.ggebiplot.com/Yan-Hunt1999a.pdf)----# GDD ----
#' @keywords internal 
gdd<-function(tAve,tBase,tOpt,tMax)
{
  firstTerm<-(tMax-tAve)/(tMax-tOpt)
  secondTerm<-(tAve-tBase)/(tOpt-tBase)
  exponent<-(tOpt-tBase)/(tMax-tOpt)
  maxGdd<-tOpt-tBase
  
  rateGdd<-case_when(
    tAve<=tBase | tAve >=tMax ~ 0,
    .default = firstTerm*secondTerm^exponent
  )
  
  return(rateGdd)
  
}

# Phenological phase ----
#' @keywords internal 
phenoPhases<-function(gddState,cycleLength,transplantingLag,floweringLag)
{
  if(gddState < cycleLength*transplantingLag/100)
  {
    phenoCode<-0
    phenoStage<-'post-transplanting'
  }else if (gddState < cycleLength*floweringLag/100)
  {
    phenoCode<-1
    phenoStage<-'vegetative'
  }else
  {
    phenoCode<-2
    phenoStage<-'reproductive'
  }
  return(list(phenoCode,phenoStage))
}

## Photosynthesis ----
## # Interception fraction of solar radiation----
#' @keywords internal 
fIntCompute<-function(fIntMax,cycleLength,
               transplantingLag,halfIntGrowth,
               halfIntSenescence,initialInt,gdd)
{
  
  #percentage to thermal time
  transplantingGdd = cycleLength*transplantingLag/100
  halfIntGrowthGdd=cycleLength*halfIntGrowth/100
  halfIntSenescenceGdd=cycleLength*halfIntSenescence/100
  
  #light interception - growth phase 
  fIntGrowth<-fIntMax/
    (1+exp(-0.01*(gdd-transplantingGdd- halfIntGrowthGdd)))
  
  #light interception - senescence phase
  fIntSenescence<-fIntMax/
    (1+exp(0.01*(gdd-halfIntSenescenceGdd)))
  
  #light interception - end transplanting
  fIntGrowthEndTransplanting<-1/
    (1+exp(-0.01*(-halfIntGrowthGdd)))
  
  #gap between light interception transplanting end and the initial interception
  gapTransplanting = fIntGrowthEndTransplanting-
    initialInt
  
  #light interception 
  fIntSim <- ifelse(gdd < transplantingGdd, 
                    initialInt+(gapTransplanting * 
                                  gdd/transplantingGdd), 
                    pmin(fIntGrowth, fIntSenescence))
  
  return(fIntSim)
}
## Carbon rate ----
#' @keywords internal 
carbonRate<-function(rue, radiation, fTemp, fInt, hs,cs,ws)
{
  #TODO: the factor is multiplicative now!
  carbonRate<-rue * radiation * .5 * fTemp * (1-hs)*(1-cs)*(ws)
  
  return(carbonRate)
}
## Root growth ----
#' @keywords internal 
rootDepth<-function(rootIncrease,fTemp)
{
  rootDepth <- fTemp*rootIncrease
}

## Water availability ----
# Water content at layer 1----
#' @keywords internal 
c1Content <- function(irrigation, p, rootState,rootDepthMax,
                      etR, fInt,et0,daysNoRain,fieldCapacity ,wiltingPoint,bulkDensity, wc1mm)
{
  trc1<-0
  #transpiration component
  if(rootState <=3)
  {
    trc1 <- etR
  }else
  {
    trc1<-etR*3/rootState
  }
  
  #evaporation component
  dRE<-(1+daysNoRain)^.5-(daysNoRain)^.5
  ev1<-(1-fInt)*et0*dRE
  
  #soil water content max
  swcMax <- 3*bulkDensity*fieldCapacity *10 #in mm 
  swcMin <- 3*bulkDensity*wiltingPoint*10 #in mm
  wc1mm <- wc1mm + irrigation+ p-trc1-ev1
  dc1<-0
  if (wc1mm >= swcMax)
  {
    #compute drainage
    dc1 <- wc1mm - swcMax
    #set wc1mm to max
    wc1mm <- swcMax
  }
  
  if(wc1mm<swcMin)
  {
    wc1mm <- swcMin
  }
  #the actual water content
  wc1fraction = (wc1mm-swcMin)/(swcMax-swcMin)
  wc1 <- wiltingPoint + wc1fraction * (fieldCapacity - wiltingPoint)
  return(list(trc1,ev1,dc1,wc1mm,wc1))
}

# Water content at layer 2----
#' @keywords internal 
c2Content <- function(dc1, rootRate, rootState,rootDepthMax, 
                      etR, fInt,et0,fieldCapacity ,wiltingPoint,bulkDensity, wc2mm, wc3mm)
{
  trc2<-0
  
  trc2<-etR* (1-(3/rootState))
  
  newSoil<-rootRate*wc3mm/rootDepthMax
  

  swcMax <- (rootState-3)*bulkDensity*fieldCapacity *10 #in mm 
  swcMin <- (rootState-3)*bulkDensity*wiltingPoint*10 #in mm
  wc2mm<-wc2mm + dc1 -trc2+newSoil
  #Drainage
  dc2<-0
  if (wc2mm >= swcMax)
  {
    #compute drainage
    dc2 <- wc2mm - swcMax
    #set wc1 to max
    wc2mm <- swcMax
  }
  if(wc2mm<swcMin)
  {
    wc2mm <- swcMin
  }
  #the actual water content
  wc2Fraction =(wc2mm-swcMin)/(swcMax-swcMin)
  wc2 <- wiltingPoint + wc2Fraction * (fieldCapacity -wiltingPoint)
  
  return(list(trc2,dc2,wc2mm,newSoil,wc2))
}

    # Water content at layer 3----
#' @keywords internal 
c3Content <- function(dc2, rootState,rootDepthMax, fieldCapacity ,wiltingPoint,bulkDensity, newSoil, wc3mm)
{
  
  #SWC max
  swcMax <- (rootDepthMax-rootState)*bulkDensity*fieldCapacity *10 #in mm 
  swcMin <- (rootDepthMax-rootState)*bulkDensity*wiltingPoint*10 #in mm
  wc3mm<-wc3mm+ dc2-newSoil
  #Drainage
  dc3<-0
  if (wc3mm >= swcMax)
  {
    #compute drainage
    dc3 <- wc3mm - swcMax
    #set wc3 to max
    wc3mm <- swcMax
  }
  if(wc3mm<swcMin)
  {
    wc3mm <- swcMin
  }
  #the actual water content
  wc3fraction = (wc3mm-swcMin)/(swcMax-swcMin)
  wc3 <- wiltingPoint + wc3fraction * (fieldCapacity -wiltingPoint)
  return(list(dc3,wc3mm,wc3))
}

## Water stress factor
#' @keywords internal 
waterStressFunction<- function(ftsw, waterStressSensitivity)
{
  WSfactor <- -1+2/(1+exp(waterStressSensitivity*ftsw))
  return(WSfactor)
}

## Estimate solar radiation ----
#' @keywords internal 
radiationCompute<-function(latitude, doy, tMax, tMin)
{
  # Constants
  solarConstant <- 0.0820 # Solar constant [MJ m^-2 min^-1]
  
  # Convert latitude from degrees to radians
  latitudeRad <- latitude * pi / 180
  
  # Calculate the solar declination
  solarDeclination <- 0.409 * sin((2 * pi / 365) * doy - 1.39)
  
  # Calculate the sunset hour angle
  sunsetHourAngle <- acos(-tan(latitudeRad) * tan(solarDeclination))
  
  # Calculate the extraterrestrial radiation (Ra) in MJ m^-2 day^-1
  ra <- (24 * 60 / pi) * solarConstant * 
    (sunsetHourAngle * sin(latitudeRad) * sin(solarDeclination) + 
       cos(latitudeRad) * cos(solarDeclination) * sin(sunsetHourAngle))
  
  # Calculate the mean daily temperature
  tMean <- (tMax + tMin) / 2
  
  # Calculate the Hargreaves coefficient
  hargreavesCoefficient <- 0.0023
  
  # Calculate the radiation (rs) in MJ m^-2 day^-1
  rs <- hargreavesCoefficient * ra * sqrt(tMax - tMin) * (tMean + 17.8)
  
  return(rs)
}

#Flowering dynamics 
#' @keywords internal 
floweringDynamics<-function(cycleCompletion, floweringLag, floweringSlope,floweringMax)
{
  if(cycleCompletion < floweringLag)
  {
    flowering <- 0
  }else
  {
    logGrowth <- .5 / (1 + exp(-floweringSlope*((cycleCompletion-floweringLag) - 0.5 * (floweringMax-floweringLag))))
    logDecline <- .5 / (1 + exp(floweringSlope*((cycleCompletion- (floweringLag + (floweringMax-floweringLag)) - .5 * floweringLag))))
    flowering <- min(logGrowth, logDecline)
  }
  
  
  
  return(flowering)
}


