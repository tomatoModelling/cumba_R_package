# Clear environment
rm(list = ls())

# Load required packages
library(tidyverse)
library(data.table)
library(devtools)
#install_github("tomatoModelling/cumba_R_package",force=T)
library(cumba)
library(readxl)


# Set working directory to the script's location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# --- Load and Prepare Input Data ---
load("tomatoFoggia.rda")
exp_data<-tomatoFoggia

weather<-read.csv('weather_foggia.csv') |> 
  mutate(Rad = as.numeric(Rg),
         ET0= as.numeric(ET0),
         Tx = TMAX,
         Tn = TMIN,
         Site= 'Foggia',
         P = 0,
         DATE = MDATE,
         Lat = 41) |> 
  mutate(DATE = as.Date(DATE,format = '%m/%d/%Y'),
         year= year(DATE))

source('../R/Main.R')
#TODO: Change with optimized parameters
cumba_par <- cumba::cumbaParameters


vegetativeWS <- c(0.6,0.8)
vegetativeTurn <- c(1,2)

reproductiveWS <- c(0.6,0.8)
reproductiveTurn <- c(1,2)

ripeningWS <- c(0.6,0.8)
ripeningTurn <- c(1,2)

transplantingDOY <- c(100,120)

results_list <- list()
i <- 1

for (vws in vegetativeWS) {
  for (vt in vegetativeTurn) {
    for (rws in reproductiveWS) {
      for (rt in reproductiveTurn) {
        for (riws in ripeningWS) {
          for (rit in ripeningTurn) {
            for (td in transplantingDOY) {
              
              sim <- cumba_scenario(
                weather |> dplyr::filter(year >= 2019),
                cumba_par,
                estimateRad = TRUE,
                estimateET0 = TRUE,
                irrigationStrategy = list(
                  vegetative = list(wsLevel = vws, turnMin = vt),
                  reproductive = list(wsLevel = rws, turnMin = rt),
                  ripening = list(wsLevel = riws, turnMin = rit)
                ),
                transplantingDOY = td,
                fullOut = FALSE
              )
              
              outSynth <- sim |>
                dplyr::summarise(
                  yield = dplyr::last(yield),
                  brix = dplyr::last(brix),
                  n_irrigation = sum(irrigation > 0, na.rm = TRUE),
                  irrigation = sum(irrigation, na.rm = TRUE)
                )
              
              # aggiungi parametri
              outSynth$vegetativeWS <- vws
              outSynth$vegetativeTurn <- vt
              outSynth$reproductiveWS <- rws
              outSynth$reproductiveTurn <- rt
              outSynth$ripeningWS <- riws
              outSynth$ripeningTurn <- rit
              outSynth$transplantingDOY <- td
              
              results_list[[i]] <- outSynth
              i <- i + 1
            }
          }
        }
      }
    }
  }
}

results_df <- dplyr::bind_rows(results_list)

ggplot(results_df, aes(x=factor(transplantingDOY), y= n_irrigation, fill=factor(vegetativeWS))) + 
  geom_boxplot()+
  facet_wrap(~vegetativeTurn)
              

