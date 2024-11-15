library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(nasapower)
library(plotly)
library(devtools)
library(remotes)
library(leaflet.extras)



navbarPage("",
           #tabPanel Parameters####          
           tabPanel("Parameters", 
                    # Custom CSS
                    tags$head(
                      tags$style(HTML("
      /* Style for Growth Parameters Header */
      h4.growth-parameters {
       font-size: 15px;
        font-weight: bold;
        color: #4CAF50;         /* Dark green text color */
        background-color: #e8f5e9; /* Light green background */
        padding: 5px;
        border-radius: 5px;
        margin-bottom: 0px;
        text-align: center;
      }
      
     

      /* Growth Parameters Slider Styling */
      .growth-slider .irs--shiny .irs-bar {
        background-color: #4CAF50; /* Green bar color */
        height: 2px;               /* Adjust thickness */
        font-size: 2px;
      }
      
      .growth-slider .irs--shiny .irs-handle {
        border: 1px solid darkblue; /* Dark green border for handle */
        background-color: blue; /* Light green handle color */
      }
      
      /* Style for Phenology Parameters Header */
      h4.phenology-parameters {
        padding: 0px 0;
        font-size: 15px;
        font-weight: bold;
        color: #0288d1;          /* Dark blue text color */
        background-color: lightblue; /* Light blue background */
        text-align: center;
      }
      
      /* Phenology Parameters Slider Styling */
      .phenology-slider .irs--shiny .irs-bar {
        background-color: green; /* Blue bar color */
        height: 4px;               /* Adjust thickness */
        font-size: 2px;
      }
      
      .phenology-slider .irs--shiny .irs-handle {
        border: 1px solid #01579b; /* Dark blue border for handle */
        background-color: #81d4fa; /* Light blue handle color */
      }
      
       /* Style for Stress Parameters Header */
      h4.stress-parameters {
        padding: 5px;
        font-size: 15px;
        font-weight: bold;
        color: #0288d1;          /* Dark blue text color */
        background-color: lightblue; /* Light blue background */
        margin-bottom: 0px;
        text-align: center;
      }
      
      /* Stress Parameters Slider Styling */
      .stress-slider .irs--shiny .irs-bar {
        background-color: red; /* Blue bar color */
        height: 4px;               /* Adjust thickness */
        font-size: 2px;
      }
      
      .stress-slider .irs--shiny .irs-handle {
        border: 1px solid #01579b; /* Dark blue border for handle */
        background-color: red; /* Light blue handle color */
      }
      
        /* Style for Stress Parameters Header */
      h4.brix-parameters {
        padding: 5px;
        font-size: 15px;
        font-weight: bold;
        color: #0288d1;          /* Dark blue text color */
        background-color: orange; /* Light blue background */
        margin-bottom: 0px;
        text-align: center;
      }
      
      /* Stress Parameters Slider Styling */
      .brix-slider .irs--shiny .irs-bar {
        background-color: orange; /* Blue bar color */
        height: 4px;               /* Adjust thickness */
        font-size: 2px;
      }
      
      .brix-slider .irs--shiny .irs-handle {
        border: 1px solid #01579b; /* Dark blue border for handle */
        background-color: orange; /* Light blue handle color */
      }
      
      /* General spacing for sliders */
      .slider-container {
        padding: 0px 0;
        margin-bottom: 0px;
        font-size: 13px;
        label-size: 10px;
        
      }
    "))
                    ),
                    
                    # application title ####
                    # slider parameters #### 
                    # 
                    sidebarLayout(
                      sidebarPanel(
                        h4("Growth Parameters", class = "growth-parameters"),  # Use h4 or h5 for your label
                        fluidRow(
                          column(3, div(class = "slider-container growth-slider", sliderInput("TGro", "T growth (°C)", 
                                                                                min = 6, max = 35, value = c(10,33)))),
                          column(3, div(class = "slider-container growth-slider", sliderInput("Topt", "T opt (°C)", 
                                                                                min = 20, max = 27, value = 26))),
                          column(3, div(class = "slider-container growth-slider", sliderInput("TStress", "T stress (°C)", 
                                                                                min = 2, max = 50, value = c(0,40)))),
                          column(3, div(class = "slider-container growth-slider", sliderInput("RUE", "RUE (g MJ-1)", 
                                                                                min = 1, max = 3, value = 2.3))),
                        ),
                        h4("Phenology Parameters", class = "phenology-parameters"),  # Use h4 or h5 for your label
                        fluidRow(
                          column(3,div(class = "slider-container phenology-slider",
                                       sliderInput("CycleLength", "Cycle (°C day)", min = 1300, max = 1700, value = 1500))),
                          column(3,div(class = "slider-container phenology-slider",
                                       sliderInput("LightInterception", "Light Int (-)", min = 0.01, max = 1.5, 
                                                   value = c(0.05,1)))),
                          column(3,div(class = "slider-container phenology-slider",
                                       sliderInput("FloweringSlope", "Flower Slope (-)", min = 0.2, max = 0.6, value = 0.4))),
                          column(3,div(class = "slider-container phenology-slider",
                                       sliderInput("FloweringMax", "Flower Max (-)", min = 40, max = 90, value = 80))),
                          column(6,div(class = "slider-container phenology-slider",
                                       sliderInput("TransFloLag", "Transplanting and flowering Lag (%)", min = 0, max = 100, value = c(10,30)))),
                          column(6,div(class = "slider-container phenology-slider",
                                       sliderInput("GrowthSenescenceCanopy", "Canopy dynamics (-)", min = 0, max = 100, value = c(20,95)))),
                        ),
                        h4("Water Stress Parameters", class = "stress-parameters"),  # Use h4 or h5 for your label
                        fluidRow(
                          column(3,div(class = "slider-container stress-slider",
                                       sliderInput("Kc", "Kc (-)", min = 0.05, max = 1.5, value = c(0.1,1.5)))),
                          column(3,div(class = "slider-container stress-slider",
                                       sliderInput("RootIncrease", "Root Inc (-)", min = 0.5, max = 1.5, value = 1))),
                          column(3,div(class = "slider-container stress-slider",
                                       sliderInput("RootDepth", "Root Depth (cm)", min = 1, max = 100, value = c(4,60)))),
                          column(3,div(class = "slider-container stress-slider",
                                       sliderInput("WaterStressSensitivity", "WS Sensitivity (-)", min = 0.5, max = 4.5, value = 3))),
                        ),
                        h4("Fruit Quality Parameters", class = "brix-parameters"),  # Use h4 or h5 for your label
                        fluidRow(
                          column(3,div(class = "slider-container brix-slider",
                                       sliderInput("k0", "k0 sugar (-)", min = 2, max = 5, value = 4))),
                          column(3,div(class = "slider-container brix-slider",
                                       sliderInput("FruitWaterContent", "Water Cont (%)", min = 0.8, max = 1,
                                                   value = c(0.8,0.95)))),
                          column(3,div(class = "slider-container brix-slider",
                                       sliderInput("FruitWaterContentInc", "Water Inc (-)", min = 0.005, max = 0.015, 
                                                   value = 0.1))),
                          column(3,div(class = "slider-container brix-slider",
                                       sliderInput("FruitWaterContentDecreaseMax", "Dec Max (%)", 
                                                   min = 0.0005, max = 0.015, value = 0.001))),
                        ),
                        
                        ###action button ----
                        actionButton("runModel", "Run Model"),
                      ),
                      ## main panel----
                      mainPanel(
                        fluidRow(
                          column(12),
                          sliderInput("year_range", "Select Year Range:",
                                                 min = 1990, 
                                                 max = 2024, 
                                                 value = c(2000, 2024),
                                                 step = 1, 
                                                 sep = "")
                        ),
                        # Render the same map in the Parameters tab
                        leafletOutput("growthMap"),
                        plotlyOutput("hectare_hist"),  # Example plot
                        verbatimTextOutput("layer_click"),
                        
                      )
                    )
           ),
)