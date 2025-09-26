# Clear environment
rm(list = ls())

# Load required packages
library(tidyverse)
library(data.table)
library(cumba)
library(readxl)
library(GA)  # Genetic Algorithm package

# Set working directory to the script's location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# --- Load and Prepare Input Data ---
exp_data<-tomatoFoggia

#load parameters
cumba_par <- cumbaParameters

# Define parameters in calibration
pars <- c("Topt", "RUE", "FloweringLag", "CycleLength", "WaterStressSensitivity",
          "Theat", "RootIncrease", "FloweringMax", "RootDepthMax", "Tbase", "Tmax")

# Extract lower and upper bounds
# Plain numeric vectors (exactly like manual c(...))
lowerPar <- vapply(pars, function(p) cumba_par[[p]]$min, numeric(1))
upperPar <- vapply(pars, function(p) cumba_par[[p]]$max, numeric(1))
lowerPar <- unname(lowerPar)
upperPar <- unname(upperPar)

# --- Define Genetic Algorithm Loss Function ---
lossFunctionGA <- function(params, weather) {
  
  # Assign parameter values to model
  cumba_par$Topt$value                   <- params[1]
  cumba_par$RUE$value                    <- params[2]
  cumba_par$FloweringLag$value           <- params[3]
  cumba_par$CycleLength$value            <- params[4]
  cumba_par$WaterStressSensitivity$value <- params[5]
  cumba_par$Theat$value                  <- params[6]
  cumba_par$RootIncrease$value           <- params[7]
  cumba_par$FloweringMax$value           <- params[8]
  cumba_par$RootDepthMax$value           <- params[9]
  cumba_par$Tbase$value                  <- params[10]
  cumba_par$Tmax$value                   <- params[11]

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
  rmse <- sqrt(mean((out_calib$yield - out_calib$yield_ref)^2)) / 
    mean(out_calib$yield_ref)
  
  r <- cor(out_calib$yield, out_calib$yield_ref, method = "pearson")
  
  # Objective function to minimize
  objFun <- (rmse * 0.5) + ((1 - r) * 0.5)
  
  cat(paste0("RMSE: ", round(rmse, 3)), " Pearson r: ", round(r, 3), " Obj fun: ", round(objFun, 4), "\n")
  
  return(objFun)
}

# --- Prepare Parameters for Optimization ---

# parametersCumba <- read.csv("parameters.csv")
# 
# cumbaParameters<-cumba::cumbaParameters
# # Define bounds
# lowerPar <- c(cumbaParameters$Topt$min, cumbaParameters$RUE$min, cumbaParameters$FloweringLag$min,
#               cumbaParameters$CycleLength$min, cumbaParameters$WaterStressSensitivity$min,
#               cumbaParameters$Theat$min, cumbaParameters$RootIncrease$min,
#               cumbaParameters$FloweringMax$min, cumbaParameters$RootDepthMax$min,
#               cumbaParameters$Tbase$min,cumbaParameters$Tmax$min)
# 
# upperPar <- c(cumbaParameters$Topt$max, cumbaParameters$RUE$max, cumbaParameters$FloweringLag$max,
#               cumbaParameters$CycleLength$max, cumbaParameters$WaterStressSensitivity$max,
#               cumbaParameters$Theat$max, cumbaParameters$RootIncrease$max,
#               cumbaParameters$FloweringMax$max, cumbaParameters$RootDepthMax$max,
#               cumbaParameters$Tbase$max,cumbaParameters$Tmax$max)


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
saveRDS(results_list, file = "ga_result.rds")

# --- Run Simulation with Optimized Parameters ---

paramCalibrated <- readRDS("ga_result.rds")[[1]]
paramCalibrated<-cumba::cumbaParameters
# Update model parameters
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

# Perform final simulation
optimizedSimulation <- cumba_experiment(weather, cumbaParameters,
                                        estimateRad = TRUE, estimateET0 = TRUE,
                                        irrigation_df)

# Attach yield only to final row per experiment
optimizedSimulation <- optimizedSimulation %>%
  as.data.frame() %>%
  left_join(yield, by = c("experiment" = "ID")) %>%
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
  geom_line(aes(x = doy, y = fruitsStateAct * 0.001), color = "tomato3", alpha = 1, size = 1) +
  stat_summary(
    aes(x = doy, y = Y_TOT * 0.05 * 10 * 0.001),
    fun.data = mean_sdl,
    fun.args = list(mult = 1),
    geom = "pointrange",
    color = "tomato4",
    alpha = 0.5,
    size = 0.7
  ) +
  geom_line(aes(x = doy, y = wc1 + 0.1), color = "darkgoldenrod1", size = 0.8) +
  geom_line(aes(x = doy, y = wc2 + 0.1), color = "peru", size = 0.8) +
  geom_line(aes(x = doy, y = wc3 + 0.1), color = "saddlebrown", size = 0.8) +
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
optimizedSimulation <- optimizedSimulation |> filter(!is.na(Y_TOT))

lmD <- lm(fruitsStateAct ~ Y_TOT, data = optimizedSimulation)
summary(lmD)

# Scatter plot with regression
scatter <- optimizedSimulation |>
  ggplot(aes(x = Y_TOT * 0.05 * 10, y = fruitsStateAct)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x)
scatter
