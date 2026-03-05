# Clear environment
rm(list = ls())

# Load required packages
library(tidyverse)
library(data.table)
library(devtools)
install_github("tomatoModelling/cumba_R_package",force=T)
library(cumba)
library(readxl)
library(GA)  # Genetic Algorithm package


# Set working directory to the script's location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# --- Load and Prepare Input Data ---
load("tomatoFoggia.rda")
exp_data<-tomatoFoggia

#load parameters
load("cumbaParameters.rda")
cumba_par <- cumbaParameters
cumba_par$Topt$value <- 25
cumba_par$Tcold$value<-5
cumba_par$FIntMax$value<-.92
cumba_par$TransplantingLag$value<-9
cumba_par$InitialInt$value<-.001
cumba_par$RootDepthInitial$value<-5
cumba_par$SoilWaterInitial$value<-100
cumba_par$DepletionFraction$value<-60
cumba_par$FloweringMax$value<-41
cumba_par$WaterStressSensitivity$min<-3
cumba_par$RootIncrease$min<-.1


# Define parameters in calibration
pars <- c("RUE", "FloweringLag", "CycleLength",
          "HalfIntSenescence","HalfIntGrowth",
          "KcMax", "FruitWaterContentMax",  
          "WaterStressSensitivity",
          "RootIncrease", "RootDepthMax",
          "Topt")

# Extract lower and upper bounds
# Plain numeric vectors (exactly like manual c(...))
lowerPar <- vapply(pars, function(p) cumba_par[[p]]$min, numeric(1))
upperPar <- vapply(pars, function(p) cumba_par[[p]]$max, numeric(1))
lowerPar <- unname(lowerPar)
upperPar <- unname(upperPar)

lowerPar<-c(2.4, 15, 800, 80, 15, .9, 0.85,  6, .9,  90, 22)
upperPar<-c(2.9, 25, 1200, 88, 25, 1, 0.92,  9, 1.2,  120, 28)

# --- Define Genetic Algorithm Loss Function ---
lossFunctionGA <- function(params, weather) {
  
  # Assign parameter values to model
  cumba_par$RUE$value                    <- params[1]
  cumba_par$FloweringLag$value           <- params[2]
  cumba_par$CycleLength$value            <- params[3]
  cumba_par$HalfIntSenescence$value      <- params[4]
  cumba_par$HalfIntGrowth$value          <- params[5]
  cumba_par$KcMax$value                  <- params[6]
  cumba_par$FruitWaterContentMax$value   <- params[7]
  cumba_par$WaterStressSensitivity$value <- params[8]
  cumba_par$RootIncrease$value           <- params[9]
  cumba_par$RootDepthMax$value           <- params[10]
  cumba_par$Topt$value                   <- params[11]

  #source("..//R//Main.R")
  # Run crop simulation
  out_calib <- cumba_experiment(exp_data$weather, cumba_par, 
                              estimateRad = T, estimateET0 = T,
                              exp_data$irrigation,fullOut=F)
  
  # Join with yield data and filter for last row per experiment
  out_calib <- as.data.frame(out_calib) |> 
    left_join(exp_data$production, by = c("experiment" = "ID")) |>
    group_by(experiment) |>
    slice_tail()
  
  # objective function definition
  # Calculate normalized RMSE and Pearson correlation
  nrmse_yield <- sqrt(mean((out_calib$yield - out_calib$yield_ref)^2)) / 
    mean(out_calib$yield_ref)
  
  nrmse_brix <- sqrt(mean((out_calib$brix - out_calib$brix_ref)^2)) / 
    mean(out_calib$brix_ref)
  
  
  r_yield <- cor(out_calib$yield, out_calib$yield_ref, method = "pearson")
  r_brix <- cor(out_calib$brix, out_calib$brix_ref, method = "pearson")
  
  # Objective function to minimize
  objFun_yield <- (nrmse_yield * 0.5) + ((1 - r_yield) * 0.5)
  objFun_brix <- (nrmse_brix * 0.5) + ((1 - r_brix) * 0.5)
  
  if(!is.na(objFun_brix))
  {
    objFun <- (objFun_yield+objFun_brix)/2
  }
  else
  {
    objFun<-9999
  }
    
  cat(paste0("RMSE yield: ", round(nrmse_yield, 2), "r yield: ", round(r_yield, 2)),"\n")
  cat(paste0("RMSE brix: ", round(nrmse_brix, 2), "r brix: ", round(r_brix, 2)),"\n")
  cat(paste0("objFun: ", round(objFun, 2)),"\n")
  return(objFun)
}

# Run GA Optimization (see ?ga for advanced settings of the genetic algorithm)
ga_result <- ga(
  type = "real-valued",
  fitness = function(params) -lossFunctionGA(params, exp_data$weather),
  lower = lowerPar,
  upper = upperPar,
  optimArgs = list(method = "L-BFGS-B")
)

# Store best result
best_params <- ga_result@solution
results_list <- list(
  best_params = best_params,
  ga_result_summary = summary(ga_result)
)

# Save results to file
saveRDS(results_list, file = "ga_result_fc.rds")

# --- Run Simulation with Optimized Parameters ---

paramCalibrated <- readRDS("ga_result_fc.rds")[[1]]
#paramCalibrated<-cumba::cumbaParameters
# Update model parameters
cumba_par$RUE$value                 <- paramCalibrated[1]
cumba_par$FloweringLag$value        <- paramCalibrated[2]
cumba_par$CycleLength$value         <- paramCalibrated[3]
cumba_par$HalfIntSenescence$value         <- paramCalibrated[4]
cumba_par$HalfIntGrowth$value         <- paramCalibrated[5]
cumba_par$KcMax$value         <- paramCalibrated[6]
cumba_par$FruitWaterContentMax$value         <- paramCalibrated[7]
cumba_par$WaterStressSensitivity$value <- paramCalibrated[8]
cumba_par$RootIncrease$value        <- paramCalibrated[9]
cumba_par$RootDepthMax$value        <- paramCalibrated[10]



# Perform final simulation
optimizedSimulation <- cumba_experiment(exp_data$weather, cumba_par,
                                        estimateRad = TRUE, estimateET0 = TRUE,
                                        exp_data$irrigation,
                                        fullOut = T)


# Analyze and visualize correlation
out_calib <- as.data.frame(optimizedSimulation) |> 
  left_join(exp_data$production, by = c("experiment" = "ID")) |>
  group_by(experiment) |>
  slice_tail()


lmYield <- lm(fruitFreshWeightAct ~ yield_ref, data = out_calib)
summary(lmYield)




# Scatter plot with regression
scatterYield <- out_calib |>
  ggplot(aes(x = yield_ref, y = fruitFreshWeightAct*0.01)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x)
scatterYield


lmBrix<- lm(brixAct ~ brix_ref, data = out_calib)
summary(lmBrix)

# Scatter plot with regression
scatterBrix <- out_calib |>
  ggplot(aes(x = brix_ref, y = brixAct)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x)
scatterBrix







# Attach yield only to final row per experiment
optimizedSimulation <- optimizedSimulation %>%
  as.data.frame() %>%
  left_join(exp_data[[4]], by = c("experiment" = "ID")) %>%
  group_by(experiment) %>%
  mutate(
    row_number_within_group = row_number(),
    last_row = row_number_within_group == n(),
    across(starts_with("Y_TOT"), ~ if_else(last_row, ., NA))
  ) %>%
  select(-row_number_within_group, -last_row) %>%
  ungroup()

# --- Visualization ---

ggplot(optimizedSimulation, aes(x = daysAfterSowing)) +
  #geom_line(aes(x = doy, y = yield), color = "tomato3", alpha = 1, size = 1) +
  geom_line(aes(x = doy, y = fIntAct), color = "tomato3", alpha = 1, size = 1) +
  geom_line(aes(x = doy, y = fIntPot), color = "tomato3", alpha = 1, size = 1) +
  #geom_line(aes(x = doy, y = yield), color = "tomato3", alpha = 1, size = 1) +
  #geom_line(aes(x = doy, y = yield), color = "tomato3", alpha = 1, size = 1) +
  
  # stat_summary(
  #   aes(x = doy, y = yield_ref),
  #   fun.data = mean_sdl,
  #   fun.args = list(mult = 1),
  #   geom = "pointrange",
  #   color = "tomato4",
  #   alpha = 0.5,
  #   size = 0.7
  # ) +
  #geom_line(aes(x = doy, y = swc + 0.1), color = "darkgoldenrod1", size = 0.8)+
  geom_col(aes(x = doy, y = p * 0.01), fill = "slateblue1", width = 0.7) +
  geom_col(aes(x = doy, y = irrigation * 0.01), fill = "aquamarine4", width = 0.7) +
  theme_classic() +
  xlim(120, 230) +
  facet_wrap(~experiment, ncol = 8, scales = "free_y") +
  scale_y_continuous(name = "Soil water content (m³/m³)",
                     sec.axis = sec_axis(trans = ~ ./0.01,
                                         name = "Precipitation and Irrigation (mm)")) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())

# --- Output Results ---

# Save calibrated parameters
write.csv(paramCalibrated, "paramCalibrated.csv", row.names = FALSE)

# Analyze and visualize correlation
out_calib <- as.data.frame(optimizedSimulation) |> 
  left_join(exp_data$production, by = c("experiment" = "ID")) |>
  group_by(experiment) |>
  slice_tail()



