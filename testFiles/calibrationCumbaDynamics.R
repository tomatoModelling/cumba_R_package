# Clear environment
rm(list = ls())

#remove.packages('cumba')
#devtools::install_github('https://github.com/tomatoModelling/cumba_R_package.git')
# Load required packages
library(devtools)
library(tidyverse)
library(data.table)
library(cumba)
library(readxl)
library(GA)  # Genetic Algorithm package

# Set working directory to the script's location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# --- Load and Prepare Input Data ---

# Load all sheets from the Excel input file
excel_file <- "Dataset Carucci et al. dynamics.xlsx"
sheet_names <- excel_sheets(excel_file)
all_sheets <- lapply(sheet_names, function(sheet) read_excel(excel_file, sheet = sheet))

# Extract relevant data frames
weather     <- all_sheets[[4]]
irrigation  <- all_sheets[[7]]
ids         <- all_sheets[[2]]
yield       <- as.data.frame(all_sheets[[5]]) |>
  janitor::clean_names() |> 
  rename(year=date) |> 
  group_by(id,year,month,day) |> 
  summarise(y_tot=mean(y_tot),
            fint=mean(fint_8),
            dry_weight=mean(dry_weight),
            brix=mean(brix))

# Merge irrigation with IDs and assign site name
irrigation_df <- irrigation |> left_join(ids)
irrigation_df$Site <- "Foggia"

# Process weather data
weather <- weather |> 
  mutate(Date = as.Date(DATE), Site = "Foggia") |> 
  rename(Rad = RAD) |> 
  mutate(Lat = 41)

swc <-read_excel('SoilWaterContent.xlsx') |> 
  janitor::clean_names() |> 
  left_join(yield) |> 
  mutate(date=as.Date(paste(year, month, day, sep = "-")),
         DOY=yday(date))


estimateRadiation <- TRUE
estimateET0 <- TRUE

# Function to calculate normalized RMSE
nrmse <- function(sim, obs) {
  idx <- complete.cases(sim, obs)
  sqrt(mean((sim[idx] - obs[idx])^2)) / mean(obs[idx])
}

# Function to calculate composite objective
composite_obj <- function(df) {
  # Individual normalized RMSEs
  nrmse_y <- nrmse(df$fruitsStateAct, df$y_tot)
  nrmse_fint <- nrmse(df$fIntAct, df$fint)
  nrmse_dry <- nrmse(df$carbonStateAct, df$dry_weight)
  nrmse_swc <- nrmse(df$wc2, df$swc/10)
  
  # Composite score (example: average nRMSE, penalize low r)
  composite_score <- mean(c(nrmse_y, nrmse_fint, nrmse_dry, nrmse_swc)) 
  
  return(composite_score)
}



# --- Define Genetic Algorithm Loss Function ---
cumbaParameters<-cumba::cumbaParameters
cumbaParameters$FieldCapacity$value=.45
cumbaParameters$WiltingPoint$value=.23
lossFunctionGA <- function(params, weather, sowing_date) {
  # Assign parameter values to model
  cumbaParameters$Topt$value                <- params[1]
  cumbaParameters$RUE$value                 <- params[2]
  cumbaParameters$FloweringLag$value        <- params[3]
  cumbaParameters$CycleLength$value         <- params[4]
  cumbaParameters$WaterStressSensitivity$value <- params[5]
  cumbaParameters$Theat$value               <- params[6]
  cumbaParameters$RootIncrease$value        <- params[7]
  cumbaParameters$FloweringMax$value        <- params[8]
  cumbaParameters$RootDepthMax$value        <- params[9]
  cumbaParameters$Tbase$value        <- params[10]
  cumbaParameters$Tmax$value        <- params[11]
  cumbaParameters$HalfIntGrowth$value <- params[12]
  cumbaParameters$HalfIntSenescence$value <- params[13]
  cumbaParameters$FIntMax$value <- params[14]
  
  # Run crop simulation
  CropSim <- cumba_experiment(weather, cumbaParameters, 
                              estimateRad = TRUE, estimateET0 = TRUE,
                              irrigation_df)
  
  # Join with yield data and filter for last row per experiment
  CropSim <- as.data.frame(CropSim) |> 
    left_join(swc, by = c("experiment" = "id",
                          'year'='year','doy'='DOY')) |>
    group_by(experiment) 
  
  # Calculate normalized RMSE and Pearson correlation

  # Objective function to minimize
  objFun <-composite_obj(CropSim)
  
  cat(" Obj fun: ", round(objFun, 4), "\n")
  
  return(objFun)
}

# --- Prepare Parameters for Optimization ---

parametersCumba <- read.csv("parameters.csv")

cumbaParameters<-cumba::cumbaParameters
# Define bounds
lowerPar <- c(cumbaParameters$Topt$min, cumbaParameters$RUE$min, cumbaParameters$FloweringLag$min,
              cumbaParameters$CycleLength$min, cumbaParameters$WaterStressSensitivity$min,
              cumbaParameters$Theat$min, cumbaParameters$RootIncrease$min,
              cumbaParameters$FloweringMax$min, cumbaParameters$RootDepthMax$min,
              cumbaParameters$Tbase$min,cumbaParameters$Tmax$min,
              cumbaParameters$HalfIntGrowth$min,cumbaParameters$HalfIntSenescence$min,
              cumbaParameters$FIntMax$min)

upperPar <- c(cumbaParameters$Topt$max, cumbaParameters$RUE$max, cumbaParameters$FloweringLag$max,
              cumbaParameters$CycleLength$max, cumbaParameters$WaterStressSensitivity$max,
              cumbaParameters$Theat$max, cumbaParameters$RootIncrease$max,
              cumbaParameters$FloweringMax$max, cumbaParameters$RootDepthMax$max,
              cumbaParameters$Tbase$max,cumbaParameters$Tmax$max,
              cumbaParameters$HalfIntGrowth$max,cumbaParameters$HalfIntSenescence$max,
              cumbaParameters$FIntMax$max)

# Run GA Optimization
ga_result <- ga(
  type = "real-valued",
  fitness = function(params) -lossFunctionGA(params, weather, sowing_date),
  lower = lowerPar,
  upper = upperPar,
  popSize = 50,
  maxiter = 100,
  run = 30,
  pmutation = 0.1,
  pcrossover = 0.8,
  optimArgs = list(method = "L-BFGS-B")
)

# Store best result
best_params <- ga_result@solution
results_list <- list(
  best_params = best_params,
  ga_result_summary = summary(ga_result)
)

# Save results to file
saveRDS(results_list, file = "ga_result.rds")

# --- Run Simulation with Optimized Parameters ---

paramCalibrated <- readRDS("ga_result.rds")[[1]]

# Update model parameters

cumbaParameters<-cumba::cumbaParameters
cumbaParameters$Topt$value                <- paramCalibrated[1]
cumbaParameters$RUE$value                 <- paramCalibrated[2]
cumbaParameters$FloweringLag$value        <- paramCalibrated[3]
cumbaParameters$CycleLength$value         <- paramCalibrated[4]
cumbaParameters$WaterStressSensitivity$value <- paramCalibrated[5]
cumbaParameters$Theat$value               <- paramCalibrated[6]
cumbaParameters$RootIncrease$value        <- paramCalibrated[7]
cumbaParameters$FloweringMax$value        <- paramCalibrated[8]
cumbaParameters$RootDepthMax$value        <- paramCalibrated[9]
cumbaParameters$Tbase$value        <- paramCalibrated[10]
cumbaParameters$Tmax$value        <- paramCalibrated[11]
cumbaParameters$HalfIntGrowth$value        <- paramCalibrated[12]
cumbaParameters$HalfIntSenescence$value        <- paramCalibrated[13]
cumbaParameters$FIntMax$value        <- paramCalibrated[14]

# Perform final simulation
optimizedSimulation <- cumba_experiment(weather, cumbaParameters,
                                        estimateRad = TRUE, estimateET0 = TRUE,
                                        irrigation_df)

# Attach yield only to final row per experiment
optimizedSimulation <- optimizedSimulation %>%
  as.data.frame() %>%
  left_join(swc, by = c("experiment" = "id",
                        'year'='year','doy'='DOY'))  %>%
  group_by(experiment)

# --- Visualization ---

unique(optimizedSimulation$experiment)

ggplot(optimizedSimulation, aes(x = daysAfterSowing)) +
  geom_line(aes(x = doy, y = fruitsStateAct), 
            color = "tomato3", alpha = 1, size = 1) +
  stat_summary(
    aes(x = doy, y = y_tot),
    fun.data = mean_sdl,
    fun.args = list(mult = 1),
    geom = "pointrange",
    color = "tomato4",
    alpha = 0.5,
    size = 0.7
  ) +
  geom_line(aes(x = doy, y = wc2*1000), color = "peru", size = 0.8) +
  geom_point(aes(x = doy, y = swc/100*1000), color = "peru", size = 2) +
  geom_line(aes(x = doy, y = carbonStateAct), color = "blue", size = 0.8) +
  geom_point(aes(x = doy, y = dry_weight), color = "blue", size = 2) +
  geom_line(aes(x = doy, y = fIntAct*1000), color = "pink3", size = 0.8) +
  geom_point(aes(x = doy, y = fint*1000), color = "pink3", size = 2) +
  
  geom_col(aes(x = doy, y = p * 0.01), fill = "slateblue1", width = 0.7) +
  geom_col(aes(x = doy, y = irrigation * 0.01), fill = "aquamarine4", width = 0.7) +
  theme_classic() +
  xlim(120, 230) +
  facet_wrap(~experiment, ncol = 4, scales = "free_y") +
  scale_y_continuous(name = "Soil water content (m³/m³)",
                     sec.axis = sec_axis(trans = ~ ./0.01,
                                         name = "Precipitation and Irrigation (mm)")) 

# --- Output Results ---

# Save calibrated parameters
write.csv(paramCalibrated, "paramCalibrated.csv", row.names = FALSE)

# Analyze and visualize correlation
optimizedSimulation <- optimizedSimulation |> filter(!is.na(Y_TOT))

lmD <- lm(fruitsStateAct ~ y_tot, data = optimizedSimulation)
summary(lmD)

# Scatter plot with regression
scatter <- optimizedSimulation |>
  ggplot(aes(x = y_tot, y = fruitsStateAct)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x)
scatter
