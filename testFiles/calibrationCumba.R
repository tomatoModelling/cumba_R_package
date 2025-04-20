# Clear environment
rm(list = ls())

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

# List output files
filesOuputs <- list.files("AgMIP", pattern = ".AgMIP", full.names = TRUE)

# Load all sheets from the Excel input file
excel_file <- "Dataset Carucci et al. new.xlsx"
sheet_names <- excel_sheets(excel_file)
all_sheets <- lapply(sheet_names, function(sheet) read_excel(excel_file, sheet = sheet))

# Extract relevant data frames
weather     <- all_sheets[[4]]
irrigation  <- all_sheets[[7]]
ids         <- all_sheets[[2]]
yield       <- as.data.frame(all_sheets[[5]])

# Merge irrigation with IDs and assign site name
irrigation_df <- irrigation |> left_join(ids)
irrigation_df$Site <- "Foggia"

# Process weather data
weather <- weather |> 
  mutate(Date = as.Date(DATE), Site = "Foggia") |> 
  rename(Rad = RAD) |> 
  mutate(Lat = 41)

estimateRadiation <- TRUE
estimateET0 <- TRUE

# --- Define Genetic Algorithm Loss Function ---

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
  
  # Run crop simulation
  CropSim <- cumba_experiment(weather, cumbaParameters, 
                              estimateRad = TRUE, estimateET0 = TRUE,
                              irrigation_df)
  
  # Join with yield data and filter for last row per experiment
  CropSim <- as.data.frame(CropSim) |> 
    left_join(yield, by = c("experiment" = "ID")) |>
    group_by(experiment) |>
    slice_tail()
  
  # Calculate normalized RMSE and Pearson correlation
  mse <- sqrt(mean((CropSim$fruitsStateAct - CropSim$Y_TOT * 0.05 * 10)^2)) / 
    mean(CropSim$Y_TOT * 0.05 * 10)
  r <- cor(CropSim$fruitsStateAct, CropSim$Y_TOT * 0.05 * 10, method = "pearson")
  
  # Objective function to minimize
  objFun <- (mse * 0.5) / 100 + ((1 - r) * 0.5)
  
  cat(paste0("RMSE: ", round(mse, 3)), " Pearson r: ", round(r, 3), " Obj fun: ", round(objFun, 4), "\n")
  
  return(objFun)
}

# --- Prepare Parameters for Optimization ---

parametersCumba <- read.csv("parameters.csv")

# Define bounds
lowerPar <- c(cumbaParameters$Topt$min, cumbaParameters$RUE$min, cumbaParameters$FloweringLag$min,
              cumbaParameters$CycleLength$min, cumbaParameters$WaterStressSensitivity$min,
              cumbaParameters$Theat$min, cumbaParameters$RootIncrease$min,
              cumbaParameters$FloweringMax$min, cumbaParameters$RootDepthMax$min,
              cumbaParameters$Tbase$min,cumbaParameters$Tmax$min)

upperPar <- c(cumbaParameters$Topt$max, cumbaParameters$RUE$max, cumbaParameters$FloweringLag$max,
              cumbaParameters$CycleLength$max, cumbaParameters$WaterStressSensitivity$max,
              cumbaParameters$Theat$max, cumbaParameters$RootIncrease$max,
              cumbaParameters$FloweringMax$max, cumbaParameters$RootDepthMax$max,
              cumbaParameters$Tbase$max,cumbaParameters$Tmax$max)

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
cumbaParameters$Topt$value                <- paramCalibrated[1]
cumbaParameters$RUE$value                 <- paramCalibrated[2]*0.6
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
