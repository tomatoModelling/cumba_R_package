#' CUMBA model — experiment runner
#'
#' Runs the daily time-step CUMBA simulation for one or more sites/experiments,
#' computing tomato yield and Brix from weather inputs and irrigation options.
#' The function validates inputs, converts a list-of-lists parameter object to a
#' tibble of numeric values when needed, and prints a blue message indicating
#' it is running in *experiment* mode.
#'
#' @section Required inputs:
#' \strong{weather}: must include columns \code{Site}, \code{Tx}, \code{Tn},
#' \code{P}, \code{DATE}. Depending on the flags:
#' \itemize{
#'   \item if \code{estimateRad = TRUE} (default), column \code{Lat} is required;
#'   \item if \code{estimateRad = FALSE}, column \code{Rad} is required;
#'   \item if \code{estimateET0 = FALSE}, column \code{ET0} is required.
#' }
#'
#' \strong{irrigation\_df}: must include columns \code{ID}, \code{Site},
#' \code{YEAR}, \code{DATE}, \code{WVOL}.
#'
#' @param weather A \code{data.frame} (or tibble) of daily weather by site with
#'   at least \code{Site}, \code{Tx}, \code{Tn}, \code{P}, \code{DATE} and, based
#'   on flags, either \code{Lat} (to estimate radiation) or \code{Rad}
#'   (provided radiation). If \code{estimateET0 = FALSE}, an \code{ET0} column
#'   must be present.
#' @param param Model parameters. Either:
#'   \itemize{
#'     \item a \emph{named list} where each element is itself a list with fields
#'       \code{value}, \code{min}, \code{max}, \code{description}; this form is
#'       automatically converted to a tibble of parameter \emph{values}; or
#'     \item a tibble/data.frame already containing the required parameter values.
#'   }
#'   The object must contain (as names/columns) all of:
#'   \code{Tbase}, \code{Topt}, \code{Tmax}, \code{Theat}, \code{Tcold},
#'   \code{FIntMax}, \code{CycleLength}, \code{TransplantingLag},
#'   \code{FloweringLag}, \code{HalfIntGrowth}, \code{HalfIntSenescence},
#'   \code{InitialInt}, \code{RUE}, \code{KcIni}, \code{KcMax}, \code{RootIncrease},
#'   \code{RootDepthMax}, \code{RootDepthInitial}, \code{FieldCapacity},
#'   \code{WiltingPoint}, \code{DepletionFraction}, \code{FloweringSlope},
#'   \code{FloweringMax}, \code{k0}, \code{FruitWaterContentMin},
#'   \code{FruitWaterContentMax}, \code{FruitWaterContentInc},
#'   \code{FruitWaterContentDecreaseMax}.
#'   Cardinal temperatures are validated so that \code{Tmax > Topt > Tbase}.
#' @param estimateRad Logical. If \code{TRUE} (default), solar radiation is
#'   estimated from temperature (Hargreaves) and \code{Lat} must be in
#'   \code{weather}. If \code{FALSE}, \code{weather} must include \code{Rad}.
#' @param estimateET0 Logical. If \code{TRUE} (default), reference
#'   evapotranspiration (ET0) is estimated from temperature (Hargreaves). If
#'   \code{FALSE}, \code{weather} must include an \code{ET0} column.
#' @param irrigation_df A \code{data.frame} with the irrigation schedule for each
#'   experiment/site, containing columns \code{ID}, \code{Site}, \code{YEAR},
#'   \code{DATE}, \code{WVOL}.
#' @param fullOut Logical. If \code{FALSE} (default) returns key outputs; if
#'   \code{TRUE} returns all internal variables.
#'
#' @details
#' On start, the function prints \code{"CUMBA running in experiment mode"} in
#' blue (via \pkg{crayon}). It checks that all required columns/elements are
#' present and throws informative errors if anything is missing. Sites are
#' inferred from the unique values of \code{weather$Site}.
#'
#' If \code{param} is supplied as a list-of-lists with a \code{value} field,
#' it is converted internally with \code{tibble::as_tibble(lapply(param, `[[`, "value"))}.
#'
#' @return A \code{data.frame} containing the input \code{weather} plus the
#'   daily CUMBA outputs per site and date; the set of columns depends on
#'   \code{fullOut}.
#'
#' @examples
#' # Minimal reproducible inputs (toy data)
#' weather <- data.frame(
#'   Site = "TestSite",
#'   Tx   = c(30, 32),
#'   Tn   = c(20, 21),
#'   P    = c(0, 5),
#'   DATE = as.Date(c("2025-06-01", "2025-06-02")),
#'   Lat  = 40
#' )
#'
#' irrigation_df <- data.frame(
#'   ID = 1L,
#'   Site = "TestSite",
#'   YEAR = 2025L,
#'   DATE = as.Date(c("2025-06-01", "2025-06-02")),
#'   WVOL = c(5, 10)
#' )
#'
#' # Parameters as list-of-lists (auto-converted):
#' param <- list(
#'   Tbase = list(value = 8, min = 0, max = 15, description = "Base T"),
#'   Topt  = list(value = 26, min = 15, max = 35, description = "Opt T"),
#'   Tmax  = list(value = 40, min = 30, max = 50, description = "Max T"),
#'   Theat = list(value = 35), Tcold = list(value = 5),
#'   FIntMax = list(value = 1), CycleLength = list(value = 120),
#'   TransplantingLag = list(value = 5), FloweringLag = list(value = 40),
#'   HalfIntGrowth = list(value = 0.5), HalfIntSenescence = list(value = 0.8),
#'   InitialInt = list(value = 0.05), RUE = list(value = 2.5),
#'   KcIni = list(value = 0.6), KcMax = list(value = 1.15),
#'   RootIncrease = list(value = 0.01), RootDepthMax = list(value = 1.0),
#'   RootDepthInitial = list(value = 0.1), FieldCapacity = list(value = 0.30),
#'   WiltingPoint = list(value = 0.12), DepletionFraction = list(value = 0.5),
#'   FloweringSlope = list(value = 1), FloweringMax = list(value = 1),
#'   k0 = list(value = 0.01),
#'   FruitWaterContentMin = list(value = 0.90),
#'   FruitWaterContentMax = list(value = 0.95),
#'   FruitWaterContentInc = list(value = 0.001),
#'   FruitWaterContentDecreaseMax = list(value = 0.002)
#' )
#'
#' # Run (uncomment when the function body is implemented):
#' # result <- cumba_experiment(weather, param, irrigation_df = irrigation_df)
#'
#' @seealso \code{\link[tibble]{as_tibble}}, \code{\link[crayon]{blue}}, \code{\link[crayon]{red}}
#' @importFrom tibble as_tibble
#' @importFrom crayon blue red
#' @export
cumba_experiment <- function(weather, 
                             param, 
                             estimateRad=T, 
                             estimateET0=T, 
                             irrigation_df,
                             fullOut = F)
{
  # convert param list
  if (is.list(param) && all(sapply(param, function(p) is.list(p) && "value" %in% names(p)))) {
    param <- tibble::as_tibble(lapply(param, function(p) p$value))
  }
  
  #message on runner modality
  cat(crayon::blue(paste("CUMBA running in experiment mode","\n")))
  
  # Check if required columns exist in weather data frame
  required_columns <- c("Site", "Tx", "Tn", "P", "DATE")
  #Radiation
  if (estimateRad) {required_columns <- c(required_columns, "Lat")
  } else {required_columns <- c(required_columns, "Rad")}
  #ET0
  if (!estimateET0) {required_columns <- c(required_columns, "ET0")}
  
  missing_columns <- setdiff(required_columns, colnames(weather))
  
  if (length(missing_columns) > 0) {stop(crayon::red(paste("Missing required columns in 'weather':", paste(missing_columns, collapse = ", "))))}
  
  #get columns by name
  colNames <- colnames(weather)
  SiteColID<-which(colnames(weather) == "Site")
  TxColID <- which(colnames(weather) == "Tx")
  TnColID<-which(colnames(weather) == "Tn")
  PColID <- which(colnames(weather) == "P")
  RadColID<-0
  Lat <- 0
  
  if(estimateRad == F){
    RadColID <- which(colnames(weather)=='Rad')
  }
  else{
    LatColID <- which(colnames(weather)=='Lat')
  }
  
  ET0ColID<-0
  if(estimateET0 == F){ET0ColID<-which(colnames(weather)=="ET0")}

 
    # Check if required columns exist in irrigation_df data frame
    required_irrigation_columns <- c("ID", "Site", "YEAR", "DATE", "WVOL")
    missing_irrigation_columns <- setdiff(required_irrigation_columns, colnames(irrigation_df))
  if(length(missing_irrigation_columns) > 0) {stop(paste("Missing required columns in 'irrigation_df':", paste(missing_irrigation_columns, collapse = ", ")))}
  
  # Initialize a counter for time----
  startTime <- Sys.time()
  
  # Define the list of sites in the weather data ----
  sites<-unlist(as.vector(unique(weather[,SiteColID]))) 
  
  # Check if param list contains all required elements
  required_param_elements <- c("Tbase", "Topt", "Tmax", "Theat", "Tcold", "FIntMax", "CycleLength", "TransplantingLag", "FloweringLag", "HalfIntGrowth", "HalfIntSenescence",  "InitialInt", "RUE", "KcIni", "KcMax", "RootIncrease", "RootDepthMax", "RootDepthInitial", "FieldCapacity", "WiltingPoint", "DepletionFraction", "FloweringSlope", "FloweringMax","k0","FruitWaterContentMin","FruitWaterContentMax","FruitWaterContentInc","FruitWaterContentDecreaseMax")
  
  missing_param_elements <- setdiff(required_param_elements, names(param))
  if (length(missing_param_elements) > 0) {stop(crayon::red(paste("Missing required elements in 'param':", paste(missing_param_elements, collapse = ", "))))}
  
  #Check Plant cardinal temperature
  if (param$Tmax < param$Topt || param$Tmax < param$Tbase || param$Topt < param$Tbase) {
    stop(crayon::red(paste("Alert: Please ensure that tMax is higher than tOpt, 
                and/or tMax is higher than tBase, 
                and/or tOpt is higher than tBase!\n")))} 
  
  #Check Soil hydrologic properties
  if (param$FieldCapacity < param$WiltingPoint)  {stop(crayon::red(paste("Alert: Please ensure that field capacity is higher than wilting point!\n")))}
  
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
  depletionFraction<- param$DepletionFraction/100
  waterStressSensitivity<- -param$WaterStressSensitivity
  floweringSlope<-param$FloweringSlope
  floweringMax<-param$FloweringMax
  k0<-param$k0
  fruitWaterContentMin <- param$FruitWaterContentMin
  fruitWaterContentMax <- param$FruitWaterContentMax
  fruitWaterContentInc <- param$FruitWaterContentInc
  fruitWaterContentDecreaseMax<-param$FruitWaterContentDecreaseMax
  soilWaterInitial <- param$SoilWaterInitial
  
  
  #compute maximum value of the double logistic for flowering
  x<-seq(0:100)
  floweringPotentialFunction<-sapply(x, floweringDynamics, floweringSlope=floweringSlope,       floweringLag=floweringLag, floweringMax=floweringMax)
  floweringPotentialSum<-sum(floweringPotentialFunction)
  
  #Define the output vector with column names for the results dataframe ----
    outputNames<-c('site','year','experiment', 'doy','tMax','tMin','p','irrigation',
                'gddRate','gddState','phenoCode','phenoStage','rootRate','rootState','ftsw',
                'trc1','ev1','dc1','wc1mm','daysNoRain',
                'trc2','dc2','wc2mm','dc3','wc3mm','wc1',"wc2","wc3",
                'waterStress','heatStress','coldStress','fIntPot','fIntAct',
                'kc','et0','etR','radiation',
                'carbonRateIde','carbonStateIde','carbonRatePot',
                'carbonStatePot','carbonRateAct','carbonStateAct',
                'floweringRateIde','floweringStateIde',
                'floweringRateAct','floweringStateAct','cycleCompletion',
                'fruitSetCoefficient','fruitsStateIde',
                'fruitsStatePot','fruitsStateAct',
                'kt',"carbonSugarRate","carbonSugarState",
                "fruitWaterContentPot","fruitWaterContentPotRate",
                "fruitWaterContentSensitivity","fruitWaterContentAct",
                "fruitFreshWeightPot","fruitFreshWeightAct",
                "brixPot","brixAct")
 
  
 #Initialize lists to store outputs at different levels (years, experiments, sites)----
  outputs<-list()
  outputsYear<-list()
  outputsExperiment<-list()
  outputsAll<-list()

  ## Iterate through each site ---- 
  #TODO: only debug
  site<-1
  ids <- unique(irrigation_df$ID)
  
  # Progress bar setup
  total_iterations <- length(unique(irrigation_df$ID)) # Estimated total
  
  # constant for BRIX model
  gammaCarbon<-.44
  gammaSugar<-.42
  
  
  for(site in 1:length(sites))
  {
    #the ids
    ids <- irrigation_df |> filter(Site == sites[site])
    #this site
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
    year<-9
 
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
        carbonSugarRate<-0
        carbonSugarState<-0
        rootState<-rootDepthInitial
        daysNoRain<-0
        ws<-1
        wc1_y<-wiltingPoint+((fieldCapacity-wiltingPoint)*(soilWaterInitial/100))
        wc2_y<-wc1_y
        
        # Current year being processed 
        thisYear <- years[year][[1]]
        
        # Subset the weather data for the current year
        dfYear <- dfSite[dfSite$year == thisYear, ]

        # Iterate through each day in the current year ----       
        #TODO: for debug
        day<-1
        outputs<-list() #clean the list each year
        for(day in 1:nrow(dfYear))
        {
          # Current date being processed
          date<-(dfYear[day,1])[[1]]
          
          # Filter the irrigation data (mm) for the current date
          irrigationFilter<- thisExperiment |> filter(DATE==date)
          irrigation<-0
          if(nrow(irrigationFilter)==1){irrigation <- irrigationFilter$WVOL}
          
          # Extract weather data for the current day
          tX <- round(as.numeric(dfYear[day,TxColID]),2) #Maximum temperature, °C
          tN<-round(as.numeric(dfYear[day,TnColID]),2) #Minimum temperature, °C
          doy <- dfYear[day,DOYColID][[1]] #Day of the year
          tAve<-(tX+tN)*0.5 #Maximum temperature, °C
          p<-as.numeric(dfYear[day,PColID]) #Precipitation, mm
          #radiation
          radSim<-0
          Lat <- round(as.numeric(dfYear[day,LatColID]),2)
          if(estimateRad == T){
            radSim <- radiationCompute(Lat,doy,tX,tN) #Simulated radiation, MJ m-2 d-1
          }
          else{
            radSim <- round(as.numeric(dfYear[day,RadColID]),2) #Radiation, MJ m-2 d-1
          }
          
          #ET0
          et0<-0
          if(estimateET0 == T){
            et0<-et0Compute(Lat,doy,tX,tN)
          }else{
            et0 <- round(as.numeric(dfYear[day,ET0ColID]),2) #ET0, mm d-1
          }
          
          #compute gdd
          gdd<-gdd_compute(tAve,tBase,tOpt,tMax) #Growing degree day
          gddRate <- round(gdd*(tOpt-tBase),2) #Growing degree day rate
          gddState <- gddState + gddRate #Growing degree day state
          cycleCompletion <- gddState/cycleLength*100 # Cycle completion percentage
  
          ## Compute root depth (cm)----     
          rootRate<-rootDepth(rootIncrease,gdd) #Root depth rate (cm/d)
          rootState <- ifelse(rootState+rootRate<=rootDepthMax,
                              rootState+rootRate,rootDepthMax) #Root depth state (cm)
          
          #if root state higher than the max, the root rate is 0
          if(rootState>rootDepthMax){rootRate<-0}
          
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
          
          if(length(outputs)>0 & phenoCode>=1){ 
            fIntPotRate <- fIntPot-outputs[[as.character(doy-1)]][['fIntPot']]
            fIntActRate <- fIntPotRate*outputs[[as.character(doy-1)]][['waterStress']]
            
            fIntModifier<-0
            if(fIntPotRate>0){
              fIntModifier<-fIntPotRate*outputs[[as.character(doy-1)]][['waterStress']]
            }
            else
            {
              fIntModifier<-fIntPotRate*(1+(1-outputs[[as.character(doy-1)]][['waterStress']]))
            }
            
            fIntAct<-outputs[[as.character(doy-1)]][['fIntAct']] + fIntModifier
            
            
            if(fIntAct<0)
            {
              fIntAct<-0
            }
            if(fIntPot<0)
            {
              fIntPot<-0
            }
          }else{
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
          
          
          #ftsw calculation
          if(length(outputs)>0)
          {
            wc1_y <- outputs[[as.character(doy-1)]][['wc1']]
            wc2_y <- outputs[[as.character(doy-1)]][['wc2']]
          }
          
          if (rootState>3){
             wsAve = (wc1_y*3 + wc2_y * (rootState-3))/(rootState)
           }else
           {
             wsAve = wc1_y
           }

          #water stress
          ftsw <- (wsAve - wiltingPoint) / (fieldCapacity - wiltingPoint )
          if(ftsw>1) {ftsw <- 1}
          
          
          #compute soil water dynamics
          soilModel <- soilWaterModel(doy, outputs, ftsw,
                                      depletionFraction, irrigation, p,
                         rootState, rootRate, rootDepthInitial, rootDepthMax, etR,
                         waterStressFactor, fIntAct,
                         et0,daysNoRain,fieldCapacity, wiltingPoint,
                         soilWaterInitial, waterStressSensitivity)
          #assign local variables
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
          ws <- soilModel[[14]]
          
          ## Compute carbon rates and update carbon states (potential and actual)----
          carbonRateIde <- carbonRate(rue,radSim,gdd,fIntPot,0,0,1) #no stress
          carbonRatePot <- carbonRate(rue,radSim,gdd,fIntPot,hs,cs,1) #heat/cold stress
          carbonRateAct <- carbonRate(rue,radSim,gdd,fIntAct,hs,cs,ws)
          # daily integration
          carbonStateIde <- carbonStateIde + carbonRateIde
          carbonStatePot<-carbonStatePot + carbonRatePot
          carbonStateAct<-carbonStateAct + carbonRateAct
          
          ## Compute flowering rates and update flowering states (potential and actual)---- 
          if(cycleCompletion>=floweringLag){
            floweringRateIde<-floweringDynamics(cycleCompletion,floweringLag,
                                                floweringSlope,floweringMax)
            floweringStateIde <- (floweringRateIde/floweringPotentialSum + 
                                    outputs[[as.character(doy-1)]][['floweringStateIde']]) 
            floweringRateAct <-floweringRateIde*(1-hs)*(1-cs)
            floweringStateAct <-  (floweringRateAct/floweringPotentialSum +  
                                     outputs[[as.character(doy-1)]][['floweringStateAct']])  
          }else{
            floweringRateIde<-0
            floweringStateIde<-0
            floweringRateAct<-0
            floweringStateAct<-0
          }
          
          fruitSetCoefficient <- 1-(floweringStateIde-floweringStateAct)
          
          ### reinitialize states----
          fruitsRateIde<-0
          fruitsRatePot<-0
          fruitsRateAct<-0
          
          ### check the phenological stage (only after flowering)----
          if(cycleCompletion>=floweringLag){
            fruitsRateIde <- carbonRateIde
            fruitsRatePot<-carbonRatePot * fruitSetCoefficient
            fruitsRateAct<-carbonRateAct * fruitSetCoefficient
          }
          
          ## Compute BRIX ----
          
          ### check if it is the first day----
          if(length(outputs)>0) #
          {
            fruitsStateIde<-fruitsRateIde+ outputs[[as.character(doy-1)]][['fruitsStateIde']]
            fruitsStatePot<-fruitsRatePot+ outputs[[as.character(doy-1)]][['fruitsStatePot']]
            fruitsStateAct<-fruitsRateAct+ outputs[[as.character(doy-1)]][['fruitsStateAct']]
            #for BRIX
            carbonSugarState_1<-outputs[[as.character(doy-1)]][['carbonSugarState']]
            fruitWaterContentPot_y<-outputs[[as.character(doy-1)]][['fruitWaterContentPot']]
            fruitWaterContentAct_y<-outputs[[as.character(doy-1)]][['fruitWaterContentAct']]
          }
          else
          {
            carbonSugarState_1<-0
            fruitsStateAct<-0
            fruitWaterContentPot_y<-NA
            fruitWaterContentAct_y<-NA
          }
          
          Brix <- BRIX_model(k0,fruitsRateAct,fruitsStateAct,
                             Lat,doy,carbonSugarState_1,
                             gammaCarbon,gammaSugar,
                             fruitWaterContentMin,
                             fruitWaterContentMax,fruitWaterContentInc,
                             cycleCompletion,floweringLag,
                             fruitWaterContentPot_y,fruitWaterContentAct_y,
                             fruitWaterContentDecreaseMax,ws)
          
          ### pass function variables to local variables----  
          kt_aux<-Brix[[1]]
          carbonSugarRate<-Brix[[2]]
          carbonSugarState<-Brix[[3]]
          fruitWaterContentPot<-Brix[[4]]
          fruitWaterContentPotRate<-Brix[[5]]
          fruitWaterContentSensitivity<-Brix[[6]]
          fruitWaterContentAct<-Brix[[7]]
          fruitFreshWeightPot<-Brix[[8]]
          fruitFreshWeightAct<-Brix[[9]]
          brixPot<-Brix[[10]]
          brixAct<-Brix[[11]]
          
          
          ## Populate output variables----
            outputs[[as.character(doy)]]<-setNames(list(
              thisSite, thisYear, thisId, doy, tX, tN, p, irrigation,
              gddRate, gddState, phenoCode, phenoStage, rootRate, rootState, ftsw,
              trc1, ev1, dc1, wc1mm, daysNoRain, 
              trc2, dc2, wc2mm, 
              dc3, wc3mm,
              wc1, wc2, wc3, 
              ws, # Assuming ws maps to 'waterStress',
              hs, cs, fIntPot, fIntAct, 
              kc, et0, etR, radSim,
              carbonRateIde,carbonStateIde,carbonRatePot, 
              carbonStatePot, carbonRateAct, carbonStateAct,
              floweringRateIde, floweringStateIde, floweringRateAct, floweringStateAct,
              cycleCompletion,
              fruitSetCoefficient, fruitsStateIde, fruitsStatePot, fruitsStateAct,
              kt_aux,carbonSugarRate,carbonSugarState,
              fruitWaterContentPot,fruitWaterContentPotRate,
              fruitWaterContentSensitivity,fruitWaterContentAct,
              fruitFreshWeightPot,fruitFreshWeightAct,
              brixPot,brixAct), 
              outputNames)

        }
        ## Store the result in the outputs list ----     
        tempOutputs<-as.data.frame(t((matrix(unlist(outputs), 
                                             nrow=length(unlist(outputs[1]))))))
        names(tempOutputs) <- outputNames # Rename columns
        outputsExperiment[[as.character(thisId)]]<-tempOutputs
      }
      
      #message to console
      cat(crayon::green(paste("\r","🍅 running experiment ", thisId, " in site ", 
                              thisSite, " and year ", thisYear)))
      
      
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
  elapsedTime <- round(endTime - startTime,1)
  cat("\nElapsed time:", elapsedTime, "\n")
  
  rownames(dfOut) <- NULL
  
  
  #select a subset of variables if fullOut == F
  if(fullOut==F){
    dfOut<-dfOut |> 
      mutate(swc = (wc1mm+wc2mm+wc3mm)/rootDepthMax*10,
             fruitFreshWeightAct = fruitFreshWeightAct*.01) |> 
      select(site:doy, p, irrigation,
             phenoStage,swc,fruitFreshWeightAct,brixAct) |> 
      rename(stage=phenoStage,
             yield=fruitFreshWeightAct,
             brix = brixAct)
  }
  
  #return the output dataframe
  return(dfOut)
}

#' the CUMBA model
#'
#' It is a daily time step simulation model which computes the yield and brix degree of a tomato crop as a function of weather data and irrigation options.
#' @param weather a dataframe with weather data which must have the following columns......
#' @param param A named list of model parameters, where each element is a list with fields 'value', 'min', 'max', and 'description'
#' @param estimateRad a boolean value to estimate solar radiation based on temperature using Hargreaves model. Default to 'true' (implying that the column Lat is present in weather df) if 'false' the 'weather' df must have the Rad column
#' @param estimateET0 a boolean value to estimate reference evapotranspiration based on temperature using Hargreaves model. Default to 'true'
#' @param deficitIrrigation a boolean value to estimate irrigation requirements. Default to 'false', implying that the irrigation_df is provided.
#' @param waterStressLevel a float corresponding to the threshold of water stress to trigger automatic irrigation. Default to .5, it is needed only if deficitIrrigation is 'true'.
#' @param minimumTurn an integer corresponding to the minimum number of days elapsed from the previous irrigation event. Default to 4, it is needed only if deficitIrrigation is 'true'.
#' @param fullOut boolean, if FALSE main output variables are saved (default), if TRUE all variables are saved
#' @return a dataframe containing the weatherDf plus the daily outputs of the cumba model
#' @examples 
#' #' # Example weather dataframe
#' weather <- data.frame(
#'   Site = "TestSite",
#'   Tx = c(30, 32, 31),
#'   Tn = c(20, 21, 19),
#'   P = c(0, 5, 2),
#'   DATE = as.Date(c("2025-06-01", "2025-06-02", "2025-06-03")),
#'   Lat = 40
#' )
#'
#' # Minimal parameters: use default package data
#' params <- lapply(cumbaParameters, function(p) p)  # copy default parameters
#'
#' # Run the model in automatic irrigation (scenario) mode
#' # result <- cumba_scenario(
#' #   weather,
#' #   params,
#' #   estimateRad = TRUE,
#' #   estimateET0 = TRUE,
#' #   waterStressLevel = 0.5,
#' #   minimumTurn = 4
#' # )
#'
#' @export
cumba_scenario <- function(weather, param, 
                           estimateRad=T, 
                           estimateET0=T,
                           transplantingDOY=120,
                           waterStressLevel=.5, 
                           minimumTurn = 4,
                           fullOut=F)
{
  # convert param list
  if (is.list(param) && all(sapply(param, function(p) is.list(p) && "value" %in% names(p)))) {
    param <- tibble::as_tibble(lapply(param, function(p) p$value))
  }
  
  cat(crayon::blue(paste("CUMBA running in deficit irrigation mode, with water stress level = ",
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
  required_param_elements <- c("Tbase", "Topt", "Tmax", "Theat", "Tcold", "FIntMax", "CycleLength", "TransplantingLag", "FloweringLag", "HalfIntGrowth", "HalfIntSenescence",  "InitialInt", "RUE", "KcIni", "KcMax", "RootIncrease", "RootDepthMax", "RootDepthInitial", "FieldCapacity", "WiltingPoint", "DepletionFraction", "FloweringSlope", "FloweringMax","k0","FruitWaterContentMin","FruitWaterContentMax","FruitWaterContentInc","FruitWaterContentDecreaseMax")
  
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
  depletionFraction<- param$DepletionFraction/100
  waterStressSensitivity<- -param$WaterStressSensitivity
  floweringSlope<-param$FloweringSlope
  floweringMax<-param$FloweringMax
  k0<-param$k0
  fruitWaterContentMin <- param$FruitWaterContentMin
  fruitWaterContentMax <- param$FruitWaterContentMax
  fruitWaterContentInc <- param$FruitWaterContentInc
  fruitWaterContentDecreaseMax<-param$FruitWaterContentDecreaseMax
  soilWaterInitial <- param$SoilWaterInitial
  
  #compute maximum value of the double logistic for flowering
  x<-seq(0:100)
  floweringPotentialFunction<-sapply(x, floweringDynamics, floweringSlope=floweringSlope,       floweringLag=floweringLag, floweringMax=floweringMax)
  floweringPotentialSum<-sum(floweringPotentialFunction)
  
  
  #Define the output vector with column names for the results dataframe ----
  outputNames<-c('site','year','experiment', 'doy','tMax','tMin','p','irrigation',
                 'gddRate','gddState','phenoCode','phenoStage','rootRate','rootState','ftsw',
                 'trc1','ev1','dc1','wc1mm','daysNoRain',
                 'trc2','dc2','wc2mm','dc3','wc3mm','wc1',"wc2","wc3",
                 'waterStress','heatStress','coldStress','fIntPot','fIntAct',
                 'kc','et0','etR','radiation',
                 'carbonRateIde','carbonStateIde','carbonRatePot',
                 'carbonStatePot','carbonRateAct','carbonStateAct',
                 'floweringRateIde','floweringStateIde',
                 'floweringRateAct','floweringStateAct','cycleCompletion',
                 'fruitSetCoefficient','fruitsStateIde',
                 'fruitsStatePot','fruitsStateAct',
                 'kt',"carbonSugarRate","carbonSugarState",
                 "fruitWaterContentPot","fruitWaterContentPotRate",
                 "fruitWaterContentSensitivity","fruitWaterContentAct",
                 "fruitFreshWeightPot","fruitFreshWeightAct",
                 "brixPot","brixAct")
  
  
  
  
  #Initialize lists to store outputs at different levels (years, experiments, sites)----
  outputs<-list()
  outputsYear<-list()
  outputsExperiment<-list()
  outputsAll<-list()
  
  ## Iterate through each site ---- 
  #TODO: only debug
  site<-1
  
  # constant for BRIX model
  gammaCarbon<-.44
  gammaSugar<-.42
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
        wc1_y<-wiltingPoint+((fieldCapacity-wiltingPoint)*(soilWaterInitial/100))
        wc2_y<-wc1_y
        
        # Current year being processed 
        thisYear <- years[year][[1]]
        
        # Subset the weather data for the current year
        dfYear <- dfSite[dfSite$year == thisYear, ]
        
        # Iterate through each day in the current year ----       
        #TODO: for debug
        day<-120
        outputs<-list() #clean the list each year
        for(day in 1:nrow(dfYear))
        {
          doy <- dfYear[day,DOYColID][[1]] #Day of the year
          # Current date being processed
          date<-(dfYear[day,1])[[1]]
          #150 days is the maximum duration of the tomato cycle (5 months)
          if(doy>=transplantingDOY && doy <= transplantingDOY+150)
          {
            #run in scenario mode --> trigger irrigation based on conditions
            if(ws < waterStressLevel & daysNoRain>=minimumTurn-1)
            {
              #compute soil water at field capacity
              pwc1 <- 3 * (fieldCapacity )*10
              pwc2  <- (rootState-3)* (fieldCapacity )*10
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
          #ET0
          et0<-0
          if(estimateET0 == T){
            et0<-et0Compute(Lat,doy,tX,tN)
          }else{
            et0 <- round(as.numeric(dfYear[day,ET0ColID]),2) #ET0, mm d-1
          }
          
          #compute gdd
          gdd<-gdd_compute(tAve,tBase,tOpt,tMax) #Growing degree day
          gddRate <- round(gdd*(tOpt-tBase),2) #Growing degree day rate
          gddState <- gddState + gddRate #Growing degree day state
          cycleCompletion <- gddState/cycleLength*100 # Cycle completion percentage
          
          ## Compute root depth (cm)----     
          rootRate<-rootDepth(rootIncrease,gdd) #Root depth rate (cm/d)
          rootState <- ifelse(rootState+rootRate<=rootDepthMax, rootState+rootRate,rootDepthMax) #Root depth state (cm)
          
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
          
          if(length(outputs)>0 & phenoCode>=1){ 
            fIntPotRate <- fIntPot-outputs[[as.character(doy-1)]][['fIntPot']]
            fIntActRate <- fIntPotRate*outputs[[as.character(doy-1)]][['waterStress']]
            
            fIntModifier<-0
            if(fIntPotRate>0){
              fIntModifier<-fIntPotRate*outputs[[as.character(doy-1)]][['waterStress']]
            }
            else
            {
              fIntModifier<-fIntPotRate*(1+(1-outputs[[as.character(doy-1)]][['waterStress']]))
            }
            
            fIntAct<-outputs[[as.character(doy-1)]][['fIntAct']] + fIntModifier
            
            if(fIntAct<0){
              fIntAct=0
            }
            if(fIntPot<0){
              fIntPot=0
            }
          }else{
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
          
          
          #ftsw calculation
          if(length(outputs)>0)
          {
            wc1_y <- outputs[[as.character(doy-1)]][['wc1']]
            wc2_y <- outputs[[as.character(doy-1)]][['wc2']]
          }
          
          if (rootState>3){
            wsAve = (wc1_y*3 + wc2_y * (rootState-3))/(rootState)
          }else
          {
            wsAve = wc1_y
          }
          
          #water stress
          ftsw <- (wsAve - wiltingPoint) / (fieldCapacity - wiltingPoint )
          if(ftsw>1) {ftsw = 1}
          
          
          #compute soil water dynamics
          soilModel <- soilWaterModel(doy, outputs, ftsw,
                                      depletionFraction, irrigation, p,
                                      rootState, rootRate, rootDepthInitial, rootDepthMax, etR,
                                      waterStressFactor, fIntAct,
                                      et0,daysNoRain,fieldCapacity, wiltingPoint,
                                      soilWaterInitial, waterStressSensitivity)
          #assign local variables
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
          ws <- soilModel[[14]]
          
          
          ## Compute carbon rates and update the carbon states (potential and actual)----
          carbonRateIde <- carbonRate(rue,radSim,gdd,fIntPot,0,0,1) #no stress
          carbonRatePot <- carbonRate(rue,radSim,gdd,fIntPot,hs,cs,1) #only heat and cold stress
          carbonRateAct <- carbonRate(rue,radSim,gdd,fIntAct,hs,cs,ws)
          # daily integration
          carbonStateIde <- carbonStateIde + carbonRateIde
          carbonStatePot<-carbonStatePot + carbonRatePot
          carbonStateAct<-carbonStateAct + carbonRateAct
          
           ## Compute flowering rates and update flowering states (potential and actual)---- 
          if(cycleCompletion>=floweringLag){
            floweringRateIde<-floweringDynamics(cycleCompletion,floweringLag,
                                                floweringSlope,floweringMax)
            floweringStateIde <- (floweringRateIde/floweringPotentialSum + 
                                    outputs[[as.character(doy-1)]][['floweringStateIde']]) 
            floweringRateAct <-floweringRateIde*(1-hs)*(1-cs)
            floweringStateAct <-  (floweringRateAct/floweringPotentialSum +  
                                     outputs[[as.character(doy-1)]][['floweringStateAct']])  
          }else{
            floweringRateIde<-0
            floweringStateIde<-0
            floweringRateAct<-0
            floweringStateAct<-0
          }
          
          fruitSetCoefficient <- 1-(floweringStateIde-floweringStateAct)
          
          ### reinitialize states----
          fruitsRateIde<-0
          fruitsRatePot<-0
          fruitsRateAct<-0
          
          ### check the phenological stage (only after flowering)----
          if(cycleCompletion>=floweringLag){
            fruitsRateIde <- carbonRateIde
            fruitsRatePot<-carbonRatePot * fruitSetCoefficient
            fruitsRateAct<-carbonRateAct * fruitSetCoefficient
          }
          ## Compute BRIX ----
          
          ### check if it is the first day----
          if(length(outputs)>0) #
          {
            fruitsStateIde<-fruitsRateIde+ outputs[[as.character(doy-1)]][['fruitsStateIde']]
            fruitsStatePot<-fruitsRatePot+ outputs[[as.character(doy-1)]][['fruitsStatePot']]
            fruitsStateAct<-fruitsRateAct+ outputs[[as.character(doy-1)]][['fruitsStateAct']]
            #for BRIX
            carbonSugarState_1<-outputs[[as.character(doy-1)]][['carbonSugarState']]
            fruitWaterContentPot_y<-outputs[[as.character(doy-1)]][['fruitWaterContentPot']]
            fruitWaterContentAct_y<-outputs[[as.character(doy-1)]][['fruitWaterContentAct']]
          }
          else
          {
            carbonSugarState_1<-0
            fruitsStateAct<-0
            fruitWaterContentPot_y<-NA
            fruitWaterContentAct_y<-NA
          }
          
          
          Brix <- BRIX_model(k0,fruitsRateAct,fruitsStateAct,
                             Lat,doy,carbonSugarState_1,
                             gammaCarbon,gammaSugar,
                             fruitWaterContentMin,
                             fruitWaterContentMax,fruitWaterContentInc,
                             cycleCompletion,floweringLag,
                             fruitWaterContentPot_y,fruitWaterContentAct_y,
                             fruitWaterContentDecreaseMax,ws)
          
          ### pass function variables to local variables----  
          kt_aux<-Brix[[1]]
          carbonSugarRate<-Brix[[2]]
          carbonSugarState<-Brix[[3]]
          fruitWaterContentPot<-Brix[[4]]
          fruitWaterContentPotRate<-Brix[[5]]
          fruitWaterContentSensitivity<-Brix[[6]]
          fruitWaterContentAct<-Brix[[7]]
          fruitFreshWeightPot<-Brix[[8]]
          fruitFreshWeightAct<-Brix[[9]]
          brixPot<-Brix[[10]]
          brixAct<-Brix[[11]]
          
          
          ## Populate output variables----
          outputs[[as.character(doy)]]<-setNames(list(
            thisSite, thisYear, thisId, doy, tX, tN, p, irrigation,
            gddRate, gddState, phenoCode, phenoStage, rootRate, rootState, ftsw,
            trc1, ev1, dc1, wc1mm, daysNoRain, 
            trc2, dc2, wc2mm, 
            dc3, wc3mm,
            wc1, wc2, wc3, 
            ws, # Assuming ws maps to 'waterStress',
            hs, cs, fIntPot, fIntAct, 
            kc, et0, etR, radSim,
            carbonRateIde,carbonStateIde,carbonRatePot, 
            carbonStatePot, carbonRateAct, carbonStateAct,
            floweringRateIde, floweringStateIde, floweringRateAct, floweringStateAct,
            cycleCompletion,
            fruitSetCoefficient, fruitsStateIde, fruitsStatePot, fruitsStateAct,
            kt_aux,carbonSugarRate,carbonSugarState,
            fruitWaterContentPot,fruitWaterContentPotRate,
            fruitWaterContentSensitivity,fruitWaterContentAct,
            fruitFreshWeightPot,fruitFreshWeightAct,
            brixPot,brixAct), 
            outputNames)
          }
        }
        ## Store the result in the outputs list ----     
        tempOutputs<-as.data.frame(t((matrix(unlist(outputs), 
                                             nrow=length(unlist(outputs[1]))))))
        names(tempOutputs) <- outputNames # Rename columns
        outputsExperiment[[as.character(thisId)]]<-tempOutputs
        
        cat(crayon::green(paste("🍅 running experiment ", thisId, " in site ", 
                                thisSite, " and year ", thisYear, "\n")))#message to console
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
  
  #select a subset of variables if fullOut == F
  if(fullOut==F){
    dfOut<-dfOut |> 
      mutate(swc = (wc1mm+wc2mm+wc3mm)/rootDepthMax*10,
             fruitFreshWeightAct = fruitFreshWeightAct*.01) |> 
      select(site:doy, p, irrigation,
             phenoStage,swc,fruitFreshWeightAct,brixAct) |> 
      rename(stage=phenoStage,
             yield=fruitFreshWeightAct,
             brix = brixAct)
  }
  
  
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
gdd_compute<-function(tAve,tBase,tOpt,tMax)
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
  
  steepGrowth<-.015
  steepSenescence<-.007
  #light interception - growth phase 
  fIntGrowth<-fIntMax/
    (1+exp(-steepGrowth*(gdd-transplantingGdd- halfIntGrowthGdd)))
  
  #light interception - senescence phase
  fIntSenescence<-fIntMax/
    (1+exp(steepSenescence*(gdd-halfIntSenescenceGdd)))
  
  #light interception - end transplanting
  fIntGrowthEndTransplanting<-fIntMax/
    (1+exp(-steepGrowth*(-halfIntGrowthGdd)))
  
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
  carbonRate<-rue * radiation * .5 * fInt * fTemp * (1-hs)*(1-cs)*(ws)
  
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
soilWaterModel <- function(doy,outputs, ftsw, depletionFraction,irrigation,p,rootState,
                           rootRate,rootDepthInitial,rootDepthMax,etR, waterStressFactor,
                           fIntAct,et0,daysNoRain,fieldCapacity,
                           wiltingPoint,soilWaterInitial, waterStressSensitivity)
{
  #2. Compute water stress factor from water stress of previous day
  if(length(outputs)>0) #check if it is the first day
  {
    wc1mm<-outputs[[as.character(doy-1)]][['wc1mm']]
    wc2mm<-outputs[[as.character(doy-1)]][['wc2mm']]
    wc3mm <-outputs[[as.character(doy-1)]][['wc3mm']]
    # ftsw <- outputs[[as.character(doy-1)]][['waterStress']]
  }
  else
  {
    iniSWC = wiltingPoint + (fieldCapacity-wiltingPoint)*soilWaterInitial/100
    #iniSWC=(wiltingPoint+fieldCapacity)*.5
    # ftsw<-1
    wc1mm<-(3)*iniSWC *10
    wc2mm<-(rootDepthInitial)*iniSWC *10
    wc3mm<-(rootDepthMax-rootDepthInitial)*iniSWC*10
  }
  
  waterStressFactor<- 1 - (waterStressFunction(ftsw,depletionFraction,waterStressSensitivity))
  
  #3. Compute soil water content
  wc1Function <- c1Content(irrigation,p,rootState,rootDepthMax,etR*waterStressFactor,
                           fIntAct,et0,daysNoRain,fieldCapacity ,wiltingPoint,wc1mm)
  
  #4. Update variables
  trc1<-wc1Function[[1]]
  ev1<-wc1Function[[2]]
  dc1<-wc1Function[[3]]
  wc1mm<-wc1Function[[4]]
  wc1<-wc1Function[[5]]
  
  
  ## Compute soil water content at layer 2 (rootDepth - 3)----
  #1. Compute soil water content
  wc2Function <- c2Content(dc1,rootRate,rootState,rootDepthMax,etR*waterStressFactor,
                           fIntAct,et0,fieldCapacity ,wiltingPoint,wc2mm,wc3mm)
  #2. Update variables
  trc2<-wc2Function[[1]]
  dc2<-wc2Function[[2]]
  wc2mm<-wc2Function[[3]]
  newSoil<-wc2Function[[4]]
  wc2<-wc2Function[[5]]
  
  ## Compute soil water content at layer 3-unrooted zone (rootDepthMax - rootDepth)----
  #1. Compute soil water content
  wc3Function<-c3Content(dc2,rootState,rootDepthMax,
                         fieldCapacity,wiltingPoint,newSoil,wc3mm)
  #2. Update variables
  dc3<-wc3Function[[1]]
  wc3mm<-wc3Function[[2]]
  wc3<-wc3Function[[3]]
  
  
  ## Compute water stress ----
  #1. Potential total water content at layer 1 and 2
  pwc1 <- 3 * (fieldCapacity )*10
  pwc2  <- (rootState-3)* (fieldCapacity )*10
  
  
  return(list(trc1,ev1,dc1,wc1mm,wc1,
              trc2,dc2,wc2mm,newSoil,wc2,
              dc3,wc3mm,wc3,waterStressFactor))
  
}


# Water content at layer 1----
#' @keywords internal 
c1Content <- function(irrigation, p, rootState,rootDepthMax,
                      etR, fInt,et0,daysNoRain,fieldCapacity ,wiltingPoint,wc1mm)
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
  
  #evaporation component TODO - this does not consider soil texture 
  dRE<-(1+daysNoRain)^.5-(daysNoRain)^.5
  ev1<-(1-fInt)*et0*dRE
  
  #soil water content max
  swcMax <- 3*fieldCapacity *10 #in mm 
  swcMin <- 3*wiltingPoint*10 #in mm
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
                      etR, fInt,et0,fieldCapacity ,wiltingPoint,wc2mm, wc3mm)
{
  trc2 <- 0
  
  trc2 <- etR * (1-(3/rootState))
  
  newSoil <- rootRate*wc3mm/rootDepthMax
  

  swcMax <- (rootState-3)*fieldCapacity *10 #in mm 
  swcMin <- (rootState-3)*wiltingPoint*10 #in mm
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
c3Content <- function(dc2, rootState,rootDepthMax, fieldCapacity ,wiltingPoint,newSoil, wc3mm)
{
  
  #SWC max
  swcMax <- (rootDepthMax-rootState)*fieldCapacity *10 #in mm 
  swcMin <- (rootDepthMax-rootState)*wiltingPoint*10 #in mm
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
waterStressFunction<- function(ftsw, depletionFraction,waterStressSensitivity)
{
  if(ftsw >= depletionFraction)
  {
    WSfactor <- 0
  }
  else
  {
    #WSfactor <- 1-(1/(1+exp(-20*(ftsw-depletionFraction/2)) ))
    #(ftsw / depletionFraction)
    WSfactor <- 1-((exp(ftsw*waterStressSensitivity)-1)/(exp(waterStressSensitivity)-1))
    
  }
  #WSfactor <- -1+2/(1+exp(waterStressSensitivity*ftsw))
  return(WSfactor)
}

## Estimate solar radiation ----
#' @keywords internal 
radiationCompute<-function(latitude, doy, tMax, tMin)
{
  # Constants
  solarConstant <- 0.0820 # MJ m^-2 min^-1
  
  # Convert latitude from degrees to radians
  latitudeRad <- latitude * pi / 180
  
  # Solar declination (rad)
  solarDeclination <- 0.409 * sin((2 * pi / 365) * doy - 1.39)
  
  # Inverse relative distance Earth-Sun (dr)
  dr <- 1 + 0.033 * cos((2 * pi / 365) * doy)
  
  # Sunset hour angle (rad)
  sunsetHourAngle <- acos(-tan(latitudeRad) * tan(solarDeclination))
  
  # Extraterrestrial radiation (Ra) [MJ m^-2 day^-1]
  ra <- (24 * 60 / pi) * solarConstant * dr *
    (sunsetHourAngle * sin(latitudeRad) * sin(solarDeclination) +
       cos(latitudeRad) * cos(solarDeclination) * sin(sunsetHourAngle))
  
  # Calculate the mean daily temperature
  tMean <- (tMax + tMin) / 2
  
  # Calculate the Hargreaves coefficient
  hargreavesCoefficient <- 0.155
  
  # Calculate the radiation (rs) in MJ m^-2 day^-1
  rs <- hargreavesCoefficient * ra * sqrt(tMax - tMin)
  
  return(rs)
}

et0Compute<-function(latitude, doy, tMax, tMin)
{
  solarConstant <- 0.0820 # MJ m^-2 min^-1
  lambda <- 2.45          # MJ/kg (MJ/mm for water)
  
  latitudeRad <- latitude * pi / 180
  solarDeclination <- 0.409 * sin((2 * pi / 365) * doy - 1.39)
  dr <- 1 + 0.033 * cos((2 * pi / 365) * doy)
  sunsetHourAngle <- acos(-tan(latitudeRad) * tan(solarDeclination))
  
  ra <- (24 * 60 / pi) * solarConstant * dr *
    (sunsetHourAngle * sin(latitudeRad) * sin(solarDeclination) +
       cos(latitudeRad) * cos(solarDeclination) * sin(sunsetHourAngle))
  
  ra_mm <- ra / lambda   # convert MJ/m²/day → mm/day
  tMean <- (tMax + tMin) / 2
  
  ET0 <- 0.0023 * (tMean + 17.8) * sqrt(tMax - tMin) * ra_mm
  return(ET0)
}

#Flowering dynamics ----
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

#BRIX ----
#' @keywords internal
BRIX_model<-function(k0,dm_rate,dm_state,latitude,doy,carbonSugar_y,
                     gammaCarbon,gammaSugar,fruitWaterContentMin,fruitWaterContentMax,
                     fruitWaterContentInc,cycleCompletion,floweringLag,
                     fruitWaterContentPot_y,fruitWaterContentAct_y,
                     fruitWaterContentSensitivity,waterStress)
{
  if(cycleCompletion>=floweringLag)
  {
    kt_aux <- kt_function(k0,dm_rate,dm_state)
    dayLength<-as.integer(photoperiod(doy,latitude))
  
    carbonSugarFunction<-carbonSugar(kt_aux,dm_rate,dm_state,dayLength,carbonSugar_y,
                                     gammaCarbon,gammaSugar)
    #output of the function
    carbonSugarRate<-carbonSugarFunction[[1]]
    carbonSugarState<-carbonSugarFunction[[2]]
    
    #call the fruit water content
    fruitWaterContentFunction <- fruitWaterContent(fruitWaterContentMin,fruitWaterContentMax,
                                              fruitWaterContentInc,cycleCompletion,floweringLag,
                                              fruitWaterContentSensitivity,waterStress,
                                              fruitWaterContentPot_y,fruitWaterContentAct_y)
    fruitWaterContentPot<-fruitWaterContentFunction[[1]]
    fruitWaterContentPotRate<-fruitWaterContentFunction[[2]]
    fruitWaterContentSensitivity <- fruitWaterContentFunction[[3]]
    fruitWaterContentAct<-fruitWaterContentFunction[[4]]
    freshWeightFunction <- freshWeight(dm_state,fruitWaterContentPot,fruitWaterContentAct)
    fruitFreshWeightPot<-freshWeightFunction[[1]]
    fruitFreshWeightAct<-freshWeightFunction[[2]]
    
    if(fruitWaterContentPot < fruitWaterContentMax*.99)
    {
      brixPot<-NA
      brixAct<-NA
    }
    else
    {
      brixPot<-(100*carbonSugarState)/(gammaSugar*fruitFreshWeightPot)
      brixAct<-(100*carbonSugarState)/(gammaSugar*fruitFreshWeightAct)
    }
  }
  else
  {
    kt_aux<-0
    carbonSugarRate<-0
    carbonSugarState<-0
    fruitWaterContentPot<-0
    fruitWaterContentSensitivity<-0
    fruitWaterContentAct<-0
    fruitWaterContentPotRate<-0
    fruitFreshWeightPot<-0
    fruitFreshWeightAct<-0
    brixPot<-0
    brixAct<-0
  }
  return(list(kt_aux,carbonSugarRate,carbonSugarState,
              fruitWaterContentPot,fruitWaterContentPotRate,
              fruitWaterContentSensitivity,fruitWaterContentAct,
              fruitFreshWeightPot,fruitFreshWeightAct,
              brixPot,brixAct))
}

#' @keywords internal
kt_function<-function(k0,dm_rate,dm_state)
{
    if(dm_state>0)
    {
      kt<-k0*(dm_rate*1/dm_state)^1.36
    }
    else
    {
      kt<-0
    }
      
 
  return(kt)
}

#' @keywords internal
carbonSugar<-function(kt,dm_rate,dm_state,dayLength,carbonSugar_1,
                      gammaCarbon,gammaSugar)
{
  if((is.na(kt)))
  {
    kt<-0
  }
  
  if(kt>0.025) #HOURLY LOOP
  {
    DM_rate_lightHours = dm_rate/as.integer(dayLength)
   
    hourSunrise <- as.integer((24- dayLength)/2)
    hourSunset <- hourSunrise + dayLength  # End point of light
    # Create DMrate_h with exactly 24 hours
    DMrate_h <- rep(0, 24)  # Initialize DMrate_h with zeros
    # Fill in the light hours in the correct indices
    DMrate_h[(hourSunrise + 1):hourSunset] <- DM_rate_lightHours
    #hourly loop
    hour<-1
    #create an empty array
    carbonSugarState_h<-c(rep(0,24))
    #local variable to store the state variable of the previous time step (hour)
    carbonSugarState_hour<-0
    carbonSugarRate_in_hour<-0
    carbonSugarRate_out_hour<-0
    hour<-1
    
    #BEGIN HOURLY LOOP
    for(hour in 1:24)
    {
      #to manage the change of day
      if(hour == 1){#assign to the first hour the value of the last hour of the previous day 
        # TODO: check if it is 0 at the first hour, possibly use 'carbonSugar_1'
        #carbonSugarState_hour = carbonSugarState_h[[24]]
        carbonSugarState_hour = carbonSugar_1
      }else {#otherwise assign the value of the previous hour
        carbonSugarState_hour = carbonSugarState_h[[hour-1]]}
      
      #define the carbon flow from the phloem - gC m-2 h-1
      carbonSugarRate_in <- DMrate_h[hour] * gammaCarbon
      #define the carbon used to synthesize other compounds scaled to hourly - gC m-2 h-1
      carbonSugarRate_out <- carbonSugarState_hour * kt * 1/24 
      #compute the carbon rate in sugar gC d-1
      carbonSugarRate <- carbonSugarRate_in - carbonSugarRate_out 
      # integrate the state variable of carbon in sugar - gC m-2 h-1
      carbonSugarState_h[[hour]] <- carbonSugarState_hour + carbonSugarRate

      if(carbonSugarState_h[[hour]]<0){
        carbonSugarState_h[[hour]]=0}
      carbonSugarRate_in_hour<-carbonSugarRate_in_hour+carbonSugarRate_in
      carbonSugarRate_out_hour<-carbonSugarRate_out_hour+carbonSugarRate_out
    }
    
    #END HOURLY LOOP
    #TODO: do not consider it when there is a hourly loop
    carbonSugarRateDay <- carbonSugarRate_in_hour-carbonSugarRate_out_hour
    #assign the variable
    #carbonSugarStateDay <- carbonSugar_1 + carbonSugarState_h[[24]]  
    #TODO: changed 21/07
    carbonSugarStateDay <- carbonSugarState_h[[24]]  
    
  }
  else
  {
    #define the carbon flow from the phloem - gC m-2 d-1
    carbonSugarRate_in <- dm_rate * gammaCarbon
    #define the carbon used to synthesize other compounds - gC m-2 d-1
    carbonSugarState_d <- carbonSugar_1
    carbonSugarRate_out <- carbonSugarState_d * kt
    #compute the carbon rate in sugar gC d-1
    carbonSugarRateDay <- carbonSugarRate_in - carbonSugarRate_out
    # integrate the state variable of carbon in sugar - gC m-2 d-1
    carbonSugarStateDay <- carbonSugar_1 + carbonSugarRateDay
  }
  return(list(carbonSugarRateDay,carbonSugarStateDay))
}

#' @keywords internal
# Function to compute fruit water content
fruitWaterContent<-function(fruitWaterContentMin,fruitWaterContentMax,
                            fruitWaterContentInc,cycleCompletion,floweringLag,
                            fruitWaterContentDecreaseMax,waterStress,
                            fruitWaterContentPot_y,fruitWaterContentAct_y)
{
 
  #potential fruit water content state
  fruitWaterStatePot<-fruitWaterContentMin + 
    (fruitWaterContentMax - fruitWaterContentMin)*((1-exp(-(cycleCompletion-floweringLag)*fruitWaterContentInc)))
  
  #potential fruit water content rate
  fruitWaterRatePot <- fruitWaterStatePot - fruitWaterContentPot_y
  
  #percentage ripening completion
  ripeningCompletion <- (cycleCompletion-floweringLag) / (100-floweringLag)*100
  #sensitivity to water stress
  fruitSensitivityWS <- 1/(1+exp(-.1*(ripeningCompletion-50)))
  #actual fruit water content rate
  fruitWaterRateAct <- fruitWaterRatePot-fruitWaterContentDecreaseMax*
                                            (fruitSensitivityWS*(1-waterStress))
  #actual fruit water content state
  fruitWaterStateAct <- fruitWaterContentAct_y + fruitWaterRateAct
  
  if(fruitWaterStateAct<fruitWaterContentMin)
  {
    fruitWaterStateAct<-fruitWaterContentMin
  }
  
  return(list(fruitWaterStatePot,fruitWaterRatePot,
              fruitSensitivityWS,fruitWaterStateAct))
}

freshWeight<-function(dm_state,fruitWaterContentPot,fruitWaterContentAct)
{
  freshWeightPot <- dm_state/(1-fruitWaterContentPot)
  freshWeightAct <- dm_state/(1-fruitWaterContentAct)
  return(list(freshWeightPot,freshWeightAct))
}

#' @keywords internal
# Function to compute photoperiod
photoperiod <- function(doy, latitude) {
  # Ensure latitude is within valid range
  if (latitude < -90 || latitude > 90) {
    stop("Latitude must be between -90 and 90 degrees.")
  }
  
  # Convert latitude to radians
  lat_rad <- latitude * (pi / 180)
  # Calculate declination angle (in radians)
  declination <- 23.44 * sin((360 / 365) * (doy - 81) * (pi / 180)) * (pi / 180)
  
  # Calculate the photoperiod in hours
  # Formula derived from the spherical law of cosines
  cos_omega <- -tan(lat_rad) * tan(declination)
  
  # Check if cos_omega is valid
  if (cos_omega < -1 || cos_omega > 1) {
    # Polar day or night
    if (latitude > 0) {
      return(ifelse(doy < 172, 24, 0))  # Northern Hemisphere
    } else {
      return(ifelse(doy < 172, 0, 24))  # Southern Hemisphere
    }
  }
  # Calculate the hour angle
  omega <- acos(cos_omega)
  # Convert to photoperiod in hours
  photoperiod_hours <- (2 * omega) * (12 / pi)  # converting radians to hours
  
  return(photoperiod_hours)
}


walk.through <- function() {
  tb <- unlist(.Traceback)
  if(is.null(tb)) stop("no traceback to use for debugging")
  assign("debug.fun.list", matrix(unlist(strsplit(tb, "\\(")), nrow=2)[1,], envir=.GlobalEnv)
  lapply(debug.fun.list, function(x) debug(get(x)))
  print(paste("Now debugging functions:", paste(debug.fun.list, collapse=",")))
}

unwalk.through <- function() {
  lapply(debug.fun.list, function(x) undebug(get(as.character(x))))
  print(paste("Now undebugging functions:", paste(debug.fun.list, collapse=",")))
  rm(list="debug.fun.list", envir=.GlobalEnv)
}
