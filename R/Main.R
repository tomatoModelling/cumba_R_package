#' the CUMBA model
#'
#' It is a daily time step simulation model which computes the yield and brix degree of a tomato crop as a function of weather data and irrigation options.
#' @param weather a dataframe with weather data which must have the following columns......
#' @param param a dataframe with model parameters values
#' @param estimateRad a boolean value to estimate solar radiation based on temperature using Hargreaves model. Default to 'true' (implying that the column Lat is present in weather df) if 'false' the 'weather' df must have the Rad column
#' @param estimateET0 a boolean value to estimate reference evapotranspiration based on temperature using Hargreaves model. Default to 'true'
#' @param irrigation_df a dataframe containing the irrigation scheduling for each experiment defined in the weather dataframe.
#' @return a dataframe containing the weatherDf plus the daily outputs of the cumba model
#' @examples 
#' @export
cumba_experiment <- function(weather, param, estimateRad=T, estimateET0=T, irrigation_df)
{
  #opening message
  cat(crayon::red("                       _      __   \n"))
  cat(crayon::red("   ___ _   _ _ __ ___ | |__   \\_\\_ \n"))
  cat(crayon::red("  / __| | | | '_ ` _ \\| '_ \\ / _` |\n"))
  cat(crayon::red(" | (__| |_| | | | | | | |_) | (_| |\n"))
  cat(crayon::red("  \\___|\\__,_|_| |_| |_|_.__/ \\__,_|\n"))
  cat(crayon::red("                                   \n"))
  
  #message on runner modality
  cat(crayon::blue(paste("running in experiment mode")))
  
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

 
    # Check if required columns exist in irrigation_df data frame
    required_irrigation_columns <- c("ID", "Site", "YEAR", "DATE", "WVOL")
    missing_irrigation_columns <- setdiff(required_irrigation_columns, colnames(irrigation_df))
    if (length(missing_irrigation_columns) > 0) {
      stop(paste("Missing required columns in 'irrigation_df':", 
                 paste(missing_irrigation_columns, collapse = ", ")))}
  
  
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
                and/or tOpt is higher than tBase!\n"))) 
    } 
  
  #Check Soil hydrologic properties
  if (param$FieldCapacity < param$WiltingPoint)  {
    stop(crayon::red(paste("Alert: Please ensure that field capacity is higher than wilting point!\n")))
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
                'carbonRateIde','carbonStateIde','carbonRatePot',
                'carbonStatePot','carbonRateAct','carbonStateAct',#41
                'floweringRateIde','floweringStateIde',
                'floweringRateAct','floweringStateAct','cycleCompletion',#46
                'fruitSetCoefficient','fruitsStateIde','fruitsStatePot','fruitsStateAct')#49
  
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
    
    #the ids
      ids <- irrigation_df |> filter(Site == sites[site])
    
   
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
     
      idsYear <- ids |> filter(YEAR == years[[year]])
     
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
        carbonStateIde <- 0
        carbonStatePot <- 0
        carbonStateAct <- 0
        fruitsStatePot<-0
        fruitsStateAct<-0
        fruitsStateIde<-0
        rootState<-rootDepthInitial
        daysNoRain<-0
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
         
            # Filter the irrigation data (mm) for the current date
            irrigationFilter<- thisExperiment |> 
              filter(DATE==date)
            irrigation<-0
            if(nrow(irrigationFilter)==1)
            {
              irrigation <- irrigationFilter$WVOL
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
            fIntPotRate <- fIntPot-outputs[[as.character(doy-1)]][['fIntPot']]
            fIntActRate <- fIntPotRate*outputs[[as.character(doy-1)]][['waterStress']]
            fIntAct<-outputs[[as.character(doy-1)]][['fIntAct']] + fIntActRate
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
          
          #compute soil water dynamics
          soilModel <- soilWaterModel(doy, outputs, waterStressSensitivity, irrigation, p,
                         rootState, rootRate, rootDepthInitial, rootDepthMax, etR, waterStressFactor, fIntAct,
                         et0,daysNoRain,fieldCapacity, wiltingPoint, bulkDensity)
          
          trc1 <- soilModel[[1]]
          ev1 <- soilModel[[2]]
          dc1 <- soilModel[[3]]
          wc1mm <- soilModel[[4]]
          wc1 <- soilModel[[5]]
          trc2 <- soilModel[[6]]
          dc2 <- soilModel[[7]]
          wc2mm <- soilModel[[8]]
          newSoil <- soilModel[[9]]
          wc2 <- soilModel[[10]]
          dc3 <- soilModel[[11]]
          wc3mm <- soilModel[[12]]
          wc3 <- soilModel[[13]]
          
          
          #2. water stress TODO CHECK THE WEIGHTED AVERAGE
          wsAve = (wc1 + wc2)*.5
          
          if (rootState>3)
          {
            wsAve = (wc1*3 + wc2 * (rootState-3))/(rootState)
          }
          else
          {
            wsAve = wc1
          }
            
          ws <- (wsAve - wiltingPoint) / (fieldCapacity - wiltingPoint ) 
          if(ws>1) {ws<-1}
        
            

          ## Compute carbon rates and update the carbon states (potential and actual)----
          carbonRateIde <- carbonRate(rue,radSim,gdd,fIntPot,0,0,1) #no stress
          carbonRatePot <- carbonRate(rue,radSim,gdd,fIntPot,hs,cs,1) #only heat and cold stress
          carbonRateAct <- carbonRate(rue,radSim,gdd,fIntAct,hs,cs,ws)
          # daily integration
          carbonStateIde <- carbonStateIde + carbonRateIde
          carbonStatePot<-carbonStatePot + carbonRatePot
          carbonStateAct<-carbonStateAct + carbonRateAct
          
         
          ## Compute flowering rates and update the flowering states (potential and actual) ----
          if(cycleCompletion>=floweringLag)
          {
            floweringRateIde<-floweringDynamics(cycleCompletion,floweringLag,
                                                floweringSlope,floweringMax)
            floweringStateIde <- (floweringRateIde/floweringPotentialSum + 
                                    outputs[[as.character(doy-1)]][['floweringStateIde']]) 
            
            floweringRateAct <-floweringRateIde*(1-hs)
            floweringStateAct <-  (floweringRateAct/floweringPotentialSum +  
                                     outputs[[as.character(doy-1)]][['floweringStateAct']])  
          }else
          {
            floweringRateIde<-0
            floweringStateIde<-0
            floweringRateAct<-0
            floweringStateAct<-0
          }
          
          ## compute fruit set coefficient
          fruitSetCoefficient <- 1-(floweringStateIde-floweringStateAct)
          
          #compute partitioning to fruits
          fruitsRateIde<-0
          fruitsRatePot<-0
          fruitsRateAct<-0
          if(cycleCompletion>=floweringLag)
          {
            fruitsRateIde <- carbonRateIde
            fruitsRatePot<-carbonRatePot * fruitSetCoefficient
            fruitsRateAct<-carbonRateAct * fruitSetCoefficient
          }
          
          if(length(outputs)>0) #check if it is the first day
          {
            fruitsStateIde<-fruitsRateIde+ outputs[[as.character(doy-1)]][['fruitsStateIde']] #47
            fruitsStatePot<-fruitsRatePot+ outputs[[as.character(doy-1)]][['fruitsStatePot']] #47
            fruitsStateAct<-fruitsRateAct+ outputs[[as.character(doy-1)]][['fruitsStateAct']]
          }
          
          outputs[[as.character(doy)]]<-setNames(
            list(
              thisSite, thisYear, thisId, doy, tX, tN, p, irrigation, # 8
              gddRate, gddState, phenoCode, phenoStage, rootRate, rootState, # 14
              trc1, ev1, dc1, wc1mm, daysNoRain, # 19
              trc2, dc2, wc2mm, # 22
              dc3, wc3mm, # 24
              wc1, wc2, wc3, # 27
              ws, # Assuming ws maps to 'waterStress', # 28
              hs, cs, fIntPot, fIntAct, # 33
              kc, et0, etR, # 36
              radSim, # 37
              carbonRateIde,carbonStateIde,carbonRatePot, carbonStatePot, carbonRateAct, carbonStateAct, # 41
              floweringRateIde, floweringStateIde, floweringRateAct, floweringStateAct, # 45
              cycleCompletion, # 46
              fruitSetCoefficient, fruitsStateIde, fruitsStatePot, fruitsStateAct # 49
            ), 
            outputNames)
        }
        ## Store the result in the outputs list ----     
        tempOutputs<-as.data.frame(t((matrix(unlist(outputs), 
                                             nrow=length(unlist(outputs[1]))))))
        names(tempOutputs) <- outputNames # Rename columns
        outputsExperiment[[as.character(thisId)]]<-tempOutputs
        
        cat(crayon::green(paste("experiment ", thisId, " in site ", 
                                   thisSite, " and year ", thisYear, " executed\n")))#message to console
      }
      
      # Use do.call and rbind to bind rows into a single data frame
      tempOutputsExperiment <- do.call(rbind, outputsExperiment)
      # Convert to data frame in case it's still a list
      tempOutputsExperiment <- as.data.frame(tempOutputsExperiment, stringsAsFactors = FALSE)
      
      outputsYear[[as.character(thisYear)]]<-tempOutputsExperiment
    }
    outputsAll[[as.character(thisSite)]]<-outputsYear
  }

  # Flatten the output list
  flattenList <- unlist(outputsAll, recursive = FALSE) # Flatten the list
  
  # Bind rows into a dataframe using do.call and rbind
  dfOut <- do.call(rbind, flattenList)
  
  # Convert to data.frame in case the result is still a list
  dfOut <- as.data.frame(dfOut)
  
  # Specify the columns to convert (all columns except the first and the 12th)
  colsToConvert <- setdiff(names(dfOut), names(dfOut)[c(1, 12)])
  
  # Convert specified columns to numeric using lapply and handle NA coercion
  dfOut[colsToConvert] <- lapply(dfOut[colsToConvert], function(x) as.numeric(as.character(x)))
  
  ## Print the execution time ---- 
  endTime <- Sys.time()
  elapsedTime <- endTime - startTime
  print(paste("Elapsed time:", elapsedTime))
  rownames(dfOut) <- NULL
  #return the output dataframe
  return(dfOut)
}

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
#' @return a dataframe containing the weatherDf plus the daily outputs of the cumba model
#' @examples 
#' @export
cumba_scenario <- function(weather, param, estimateRad=T, 
                           estimateET0=T,waterStressLevel=.5, minimumTurn = 4)
{
  #opening message
  cat(crayon::red("                       _      __   \n"))
  cat(crayon::red("   ___ _   _ _ __ ___ | |__   \\_\\_ \n"))
  cat(crayon::red("  / __| | | | '_ ` _ \\| '_ \\ / _` |\n"))
  cat(crayon::red(" | (__| |_| | | | | | | |_) | (_| |\n"))
  cat(crayon::red("  \\___|\\__,_|_| |_| |_|_.__/ \\__,_|\n"))
  cat(crayon::red("                                   \n"))
  
  
  cat(crayon::blue(paste("running in deficit irrigation mode, with water stress level = ",
                           waterStressLevel, " and minimum turn equal to", 
                       minimumTurn, " days!\n")))
  
  
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
                and/or tOpt is higher than tBase!\n"))) 
  } 
  
  #Check Soil hydrologic properties
  if (param$FieldCapacity < param$WiltingPoint)  {
    stop(crayon::red(paste("Alert: Please ensure that field capacity is higher than wilting point!\n")))
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
                 'carbonRateIde','carbonStateIde','carbonRatePot','carbonStatePot','carbonRateAct','carbonStateAct',#41
                 'floweringRateIde','floweringStateIde','floweringRateAct','floweringStateAct','cycleCompletion',#46
                 'fruitSetCoefficient','fruitsStateIde','fruitsStatePot','fruitsStateAct')#49
  
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
      #run in scenario mode, ids are set to 0
      availableYears <- years
      idsYear<- data.frame(ID = rep(year,  length(availableYears)),
                             YEAR = years)
      
      
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
        carbonStateIde <- 0
        carbonStatePot <- 0
        carbonStateAct <- 0
        fruitsStatePot<-0
        fruitsStateAct<-0
        fruitsStateIde<-0
        rootState<-rootDepthInitial
        daysNoRain<-0
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
          
          #run in scenario mode --> trigger irrigation based on conditions
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
            fIntPotRate <- fIntPot-outputs[[as.character(doy-1)]][['fIntPot']]
            fIntActRate <- fIntPotRate*outputs[[as.character(doy-1)]][['waterStress']]
            fIntAct<-outputs[[as.character(doy-1)]][['fIntAct']] + fIntActRate
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
          
          #compute soil water dynamics
          soilModel <- soilWaterModel(doy, outputs, waterStressSensitivity, irrigation, p,
                                      rootState, rootRate, rootDepthInitial, rootDepthMax, etR, waterStressFactor, fIntAct, et0,daysNoRain,fieldCapacity, wiltingPoint, bulkDensity)
          
          trc1 <- soilModel[[1]]
          ev1 <- soilModel[[2]]
          dc1 <- soilModel[[3]]
          wc1mm <- soilModel[[4]]
          wc1 <- soilModel[[5]]
          trc2 <- soilModel[[6]]
          dc2 <- soilModel[[7]]
          wc2mm <- soilModel[[8]]
          newSoil <- soilModel[[9]]
          wc2 <- soilModel[[10]]
          dc3 <- soilModel[[11]]
          wc3mm <- soilModel[[12]]
          wc3 <- soilModel[[13]]
          
          
          #2. water stress TODO CHECK THE WEIGHTED AVERAGE
          wsAve = (wc1 + wc2)*.5
          
          if (rootState>3)
          {
            wsAve = (wc1*3 + wc2 * (rootState-3))/(rootState)
          }
          else
          {
            wsAve = wc1
          }
          
          ws <- (wsAve - wiltingPoint) / (fieldCapacity - wiltingPoint ) 
          if(ws>1) {ws<-1}
          
          ## Compute carbon rates and update the carbon states (potential and actual)----
          carbonRateIde <- carbonRate(rue,radSim,gdd,fIntPot,0,0,1) #no stress
          carbonRatePot <- carbonRate(rue,radSim,gdd,fIntPot,hs,cs,1) #only heat and cold stress
          carbonRateAct <- carbonRate(rue,radSim,gdd,fIntAct,hs,cs,ws)
          # daily integration
          carbonStateIde <- carbonStateIde + carbonRateIde
          carbonStatePot<-carbonStatePot + carbonRatePot
          carbonStateAct<-carbonStateAct + carbonRateAct
          
          ## Compute flowering rates and update the flowering states (potential and actual) ----
          if(cycleCompletion>=floweringLag)
          {
            floweringRateIde<-floweringDynamics(cycleCompletion,floweringLag,
                                                floweringSlope,floweringMax)
            floweringStateIde <- (floweringRateIde/floweringPotentialSum + 
                                    outputs[[as.character(doy-1)]][['floweringStateIde']]) 
            
            floweringRateAct <-floweringRateIde*(1-hs)
            floweringStateAct <-  (floweringRateAct/floweringPotentialSum +  
                                     outputs[[as.character(doy-1)]][['floweringStateAct']])  
          }else
          {
            floweringRateIde<-0
            floweringStateIde<-0
            floweringRateAct<-0
            floweringStateAct<-0
          }
          
          ## compute fruit set coefficient
          fruitSetCoefficient <- 1-(floweringStateIde-floweringStateAct)
          
          #compute partitioning to fruits
          fruitsRateIde<-0
          fruitsRatePot<-0
          fruitsRateAct<-0
          if(cycleCompletion>=floweringLag)
          {
            fruitsRateIde <- carbonRateIde
            fruitsRatePot<-carbonRatePot * fruitSetCoefficient
            fruitsRateAct<-carbonRateAct * fruitSetCoefficient
          }
          
          if(length(outputs)>0) #check if it is the first day
          {
            fruitsStateIde<-fruitsRateIde+ outputs[[as.character(doy-1)]][['fruitsStateIde']] #47
            fruitsStatePot<-fruitsRatePot+ outputs[[as.character(doy-1)]][['fruitsStatePot']] #47
            fruitsStateAct<-fruitsRateAct+ outputs[[as.character(doy-1)]][['fruitsStateAct']]
          }
          
          outputs[[as.character(doy)]]<-setNames(
            list(
              thisSite, thisYear, thisId, doy, tX, tN, p, irrigation, # 8
              gddRate, gddState, phenoCode, phenoStage, rootRate, rootState, # 14
              trc1, ev1, dc1, wc1mm, daysNoRain, # 19
              trc2, dc2, wc2mm, # 22
              dc3, wc3mm, # 24
              wc1, wc2, wc3, # 27
              ws, # Assuming ws maps to 'waterStress', # 28
              hs, cs, fIntPot, fIntAct, # 33
              kc, et0, etR, # 36
              radSim, # 37
              carbonRateIde,carbonStateIde,carbonRatePot, carbonStatePot, carbonRateAct, carbonStateAct, # 41
              floweringRateIde, floweringStateIde, floweringRateAct, floweringStateAct, # 45
              cycleCompletion, # 46
              fruitSetCoefficient, fruitsStateIde, fruitsStatePot, fruitsStateAct # 49
            ), 
            outputNames)
        }
        ## Store the result in the outputs list ----     
        tempOutputs<-as.data.frame(t((matrix(unlist(outputs), 
                                             nrow=length(unlist(outputs[1]))))))
        names(tempOutputs) <- outputNames # Rename columns
        outputsExperiment[[as.character(thisId)]]<-tempOutputs
        
        cat(crayon::green(paste("experiment ", thisId, " in site ", 
                                thisSite, " and year ", thisYear, " executed\n")))#message to console
      }
      
      # Use do.call and rbind to bind rows into a single data frame
      tempOutputsExperiment <- do.call(rbind, outputsExperiment)
      # Convert to data frame in case it's still a list
      tempOutputsExperiment <- as.data.frame(tempOutputsExperiment, stringsAsFactors = FALSE)
      
      outputsYear[[as.character(thisYear)]]<-tempOutputsExperiment
    }
    outputsAll[[as.character(thisSite)]]<-outputsYear
  }
  
  # Flatten the output list
  flattenList <- unlist(outputsAll, recursive = FALSE) # Flatten the list
  
  # Bind rows into a dataframe using do.call and rbind
  dfOut <- do.call(rbind, flattenList)
  
  # Convert to data.frame in case the result is still a list
  dfOut <- as.data.frame(dfOut)
  
  # Specify the columns to convert (all columns except the first and the 12th)
  colsToConvert <- setdiff(names(dfOut), names(dfOut)[c(1, 12)])
  
  # Convert specified columns to numeric using lapply and handle NA coercion
  dfOut[colsToConvert] <- lapply(dfOut[colsToConvert], function(x) as.numeric(as.character(x)))
  
  ## Print the execution time ---- 
  endTime <- Sys.time()
  elapsedTime <- endTime - startTime
  print(paste("Elapsed time:", elapsedTime))
  rownames(dfOut) <- NULL
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
#main function for soil water content
soilWaterModel <- function(doy,outputs, waterStressSensitivity,irrigation,p,rootState,rootRate,
                           rootDepthInitial,rootDepthMax,etR, waterStressFactor,
                           fIntAct,et0,
                           daysNoRain,fieldCapacity,
                           wiltingPoint,bulkDensity)
{
  #2. Compute water stress factor from water stress of previous day
  if(length(outputs)>0) #check if it is the first day
  {
    wc1mm<-outputs[[as.character(doy-1)]][['wc1mm']]
    wc2mm<-outputs[[as.character(doy-1)]][['wc2mm']]
    wc3mm <-outputs[[as.character(doy-1)]][['wc3mm']]
    ftsw <- outputs[[as.character(doy-1)]][['waterStress']]
  }
  else
  {
    aveSWC = (fieldCapacity+wiltingPoint)*.5
    ftsw<-1
    wc1mm<-(3)*bulkDensity*aveSWC *10
    wc2mm<-(rootDepthInitial)*bulkDensity*aveSWC *10
    wc3mm<-(rootDepthMax-rootDepthInitial)*bulkDensity*aveSWC*10
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
  
  
  return(list(trc1,ev1,dc1,wc1mm,wc1,
              trc2,dc2,wc2mm,newSoil,wc2,
              dc3,wc3mm,wc3))
  
}


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
  trc2 <- 0
  
  trc2 <- etR * (1-(3/rootState))
  
  newSoil <- rootRate*wc3mm/rootDepthMax
  

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


