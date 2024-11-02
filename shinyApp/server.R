library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(nasapower)
library(plotly)
library(devtools)
library(remotes)
#remove.packages("cumba")
#devtools::install_github("tomatoModelling/cumba_R_package")
#library(cumba)

source("C://Users//simoneugomaria.brega//Documents//gitProjects//cumba_R_package//R//Main.R")


function(input, output, session) {
  
  # Method to update weatherData based on some logic
  updateWeatherData <- function(new_data) {
    weatherData(new_data)
  }
  
  # Initialize weatherData as a reactive value
  weatherData <- reactiveVal(NULL)
  
  # Reactive expression to calculate the bbox and grid
  grid_data <- reactive({
    # Extracting the bbox from the inputs
    bbox <- bbox()
    
    # Create a grid based on the bounding box
    longitudes <- seq(bbox[1], bbox[3], by = .5)
    latitudes <- seq(bbox[2], bbox[4], by = .5)
    
    # Grid points
    grid_points <- expand.grid(Longitude = longitudes, Latitude = latitudes)
    
    return(grid_points)})
  
  # Reactive expressions----
  start_date <- reactive({
    today <-  as.Date(Sys.Date(), format="%Y-%m-%d %H:%M:%S")
    
  })
  
  end_date <- reactive({
    today <-  as.Date(Sys.Date(), format="%Y-%m-%d %H:%M:%S")
    if(input$year_range[[2]]==lubridate::year(today))
    {
      as.character(today-5)
    }
    else
    {
      paste(input$year_range[[2]], "-12-31", sep = "")
    }
  })
  
  bbox <- reactive({
    req(input$growthMap_draw_new_feature)  # Ensure the feature is drawn
    
    # Extract coordinates from the drawn feature
    point_result <- input$growthMap_draw_new_feature$geometry$coordinates
    
    # Process to get bbox (as in your example)
    if (length(point_result) == 2) {
      # For a point
      c(as.numeric(point_result))
    } else if (is.list(point_result) && length(point_result) > 0 && is.list(point_result[[1]])) {
      # For a rectangle or polygon
      longitudes <- sapply(point_result[[1]], function(coord) coord[1])
      latitudes <- sapply(point_result[[1]], function(coord) coord[2])
      c(min(unlist(longitudes)), min(unlist(latitudes)), max(unlist(longitudes)), max(unlist(latitudes)))
    } else {
      stop("Invalid input type. Input must be a point, rectangle, or polygon.")
    }
  })
  
  date_range <- reactive({
    req(input$year_range)  # Ensure year_range is defined
    
    start_date <- as.Date(paste(input$year_range[[1]], "01-01", sep = "-"))
    end_date <- as.Date(paste(input$year_range[[2]], "12-31", sep = "-"))
    c(start_date, end_date)
  })
  
  output$growthMap  <- renderLeaflet({
    leaflet() %>%
      clearControls() %>%
      clearMarkers() %>%
      clearShapes() %>%   # Clears all circles, rectangles, and polygons
      addProviderTiles("Esri.WorldTopoMap") %>%
      fitBounds(8, 48, 16.5, 38) %>%
      addDrawToolbar(
        targetGroup = "draw",
        editOptions = editToolbarOptions(edit = FALSE)
      ) 
  })
  
  # Observe events----
  observeEvent(input$growthMap_draw_new_feature, {
    req(input$growthMap_draw_new_feature)
    
    point_result <- input$growthMap_draw_new_feature$geometry$coordinates
    # Fetch the bounding box and date range
    bounds <- bbox()
    dates <- date_range()
    
    length(bounds)
    if(length(bounds)>2)
    {
      dates[1]<-paste0(substring(dates[2],0,4),'-01-01')
      latRange<- bounds[4]-bounds[2]     
      if(latRange<2){bounds[4]<-bounds[2]+2}
    
      lonRange<-bounds[3]-bounds[1]     
      if(lonRange<2){bounds[3]<-bounds[1]+2}
    }
    
    today<-Sys.Date()
    if(substring(dates[2],0,4)==lubridate::year(today))
    {
      dates[2]<-as.character(today-5)
    }
  
    # Assuming start_date and end_date are already defined earlier
    new_data <- nasapower::get_power(
      community = "ag",
      lonlat = bounds,
      dates =dates,  # Use reactive expressions
      temporal_api = "daily",
      pars = c("T2M_MAX","T2M_MIN", "PRECTOTCORR")
    )
    
    
    
    updateWeatherData(new_data)  # Call the update function with new data
  })
  
  observeEvent(input$growthMap_draw_new_feature, {
    
    thisWeather <-as.data.frame(weatherData())
    thisWeather$grid <- paste0(thisWeather$LAT, '_', thisWeather$LON)
    thisWeather<-thisWeather |> 
      group_by(grid,LON,LAT) |> 
      summarise(T2M_MAX=mean(T2M_MAX,na.rm = T))
    
    
    
    # Add markers for each grid point to the Leaflet map
    leafletProxy("growthMap") %>%
      clearControls() %>%
      clearMarkers() %>%
      clearShapes() %>%   # Clears all circles, rectangles, and polygons
      addCircles(data = thisWeather, 
                 lng = ~LON, lat = ~LAT,radius=~T2M_MAX,color="black")
  })
  
  # Update charts----
  output$hectare_hist <- renderPlotly({
    req(weatherData())
    
    thisWeather_data <- as.data.frame(weatherData())
    thisWeather_data$grid <- paste0(thisWeather_data$LAT, thisWeather_data$LON)
    
    dates <- date_range()
    yearStart <- lubridate::year(dates[[1]])
    yearEnd<-lubridate::year(dates[[2]])
    thisWeather_data <- thisWeather_data |> 
      filter(YEAR>=yearStart & YEAR<=yearEnd)
      
    
    max_year <- lubridate::year(max(thisWeather_data$YYYYMMDD))
    
    if(max_year!=lubridate::year(Sys.Date()))
    {
      bounds <- bbox()
      length(bounds)
      if(length(bounds)>2)
      {
        dates[1]<-paste0(substring(dates[2],0,4),'-01-01')
        latRange<- bounds[4]-bounds[2]     
        if(latRange<2){bounds[4]<-bounds[2]+2}
        
        lonRange<-bounds[3]-bounds[1]     
        if(lonRange<2){bounds[3]<-bounds[1]+2}
      }
      new_data <- nasapower::get_power(
        community = "ag",
        lonlat = bounds,
        dates =dates,  # Use reactive expressions
        temporal_api = "daily",
        pars = c("T2M_MAX","T2M_MIN", "PRECTOTCORR")
      )
      
      thisWeather_data <- as.data.frame(new_data) # Call the update function with new data
    }
    
    # Create a data frame with the current slider values----
    param<-data.frame(
      Parameter =  c("Tbase", "Topt", "Tmax", "Theat", "Tcold", 
                     "FIntMax", "CycleLength", "TransplantingLag", "FloweringLag", "HalfIntGrowth", "HalfIntSenescence",  
                     "InitialInt", "RUE", "KcIni", "KcMax", "RootIncrease", "RootDepthMax", "RootDepthInitial", 
                     "FieldCapacity", "WiltingPoint", "BulkDensity", "WaterStressSensitivity", "FloweringSlope", 
                     "FloweringMax","k0","FruitWaterContentMin","FruitWaterContentMax","FruitWaterContentInc",
                     "FruitWaterContentDecreaseMax"),
      Value = c(input$TGro[[1]],input$Topt,input$TGro[[2]],input$TStress[[2]], input$TStress[[1]],
                input$LightInterception[[2]], input$CycleLength,input$TransFloLag[[1]],input$TransFloLag[[2]],
                input$GrowthSenescenceCanopy[[1]],input$GrowthSenescenceCanopy[[2]],
                input$LightInterception[[1]],input$RUE,input$Kc[[1]],input$Kc[[2]],input$RootIncrease,
                input$RootDepth[[2]],input$RootDepth[[1]],0.3,0.1,1.2, input$WaterStressSensitivity,input$FloweringSlope,
                input$FloweringMax,input$k0,input$FruitWaterContent[[1]], input$FruitWaterContent[[2]],
                input$FruitWaterContentInc,input$FruitWaterContentDecreaseMax)
    )
    
    cumbaInput<-thisWeather_data |> 
      mutate(grid = paste0(LAT, '_',LON)) |> 
      rename(Site = grid,Tx = T2M_MAX,Tn=T2M_MIN,P=PRECTOTCORR,DATE=YYYYMMDD,Lat = LAT ) |> 
      select(Site,Tx,Tn,P,DATE,Lat)
    
    # call cumbà ----
    outputs<- cumba_scenario(cumbaInput,param |> pivot_wider(names_from=Parameter, values_from=c(Value)),
                   estimateRad = T,estimateET0 = T, 120,waterStressLevel=0.5,minimumTurn = 3)
    outputs$DATE <- as.Date(paste(outputs$year, outputs$doy, sep = "-"), format = "%Y-%j")
    outputs$wc1<--100+((outputs$wc1 - 0.1) / (0.3 - 0.1)) * (100)
    outputs$wc2<--100+((outputs$wc2 - 0.1) / (0.3 - 0.1)) * (100)
    outputs$wc3<--100+((outputs$wc3 - 0.1) / (0.3 - 0.1)) * (100)
    
    
    # Get the unique years
    years <- unique(outputs$year)
    
    # Create plots for each year
    plots <- lapply(years, function(yr) {
      # Filter data for the specific year and DOY range
      year_data <- outputs %>%
        filter(year == yr, doy > 120, doy < 300)
      
      # Check if year_data is not empty
      if (nrow(year_data) == 0) {
        return(NULL)  # Return NULL if there's no data for this year
      }
      
      # Initialize the plot
      p <- plot_ly()
      
    
      # Add an area for daily precipitation
      p <- p %>%
        add_trace(data = year_data, x = ~doy, y = ~fIntAct * 100, type = 'scatter', mode = 'none',
                  fill = 'tozeroy', fillcolor = I("green"), name = "Light interception") %>%
        add_lines(data = year_data, x = ~doy, y = ~floweringStateAct*100, name = paste("floweringStateAct  -", yr), 
                  yaxis = "y2",
                  color = I("yellow"), line = list(width = 1), showlegend = FALSE) %>%
        add_trace(data = year_data, x = ~doy, y = ~floweringStateIde * 100, type = 'scatter', mode = 'none',
                  fill = 'tozeroy', fillcolor = I("yellow4"), name = "FloweringPot") %>%
        add_trace(data = year_data, x = ~doy, y = ~-rootState, type = 'scatter', mode = 'none',
                  fill = 'tozeroy', fillcolor = 'rgba(0, 0, 255, 0.5)', name = "Daily Precipitation") %>%
        add_bars(data = year_data, x = ~doy, y = ~p, name = paste("Daily Precipitation -", yr), 
                 color = I("blue")) %>%
        add_bars(data = year_data, x = ~doy, y = ~irrigation, name = paste("irrigation -", yr), 
                 color = I("cyan")) %>%
        add_lines(data = year_data, x = ~doy, y = ~100-(heatStress*100), name = paste("Heat stress  -", yr), yaxis = "y2",
                  color = I("pink"), line = list(width = 1), showlegend = FALSE) %>%
        add_lines(data = year_data, x = ~doy, y = ~100-(coldStress*100), name = paste("Cold stress  -", yr), yaxis = "y2",
                  color = I("black"), line = list(width = 1), showlegend = FALSE) %>%
        add_lines(data = year_data, x = ~doy, y = ~waterStress*100, name = paste("Water stress  -", yr), yaxis = "y2",
                  color = I("gold"), line = list(width = 1), showlegend = FALSE) %>%
        add_lines(data = year_data, x = ~doy, y = ~tMax, name = paste("Max Temperature -", yr), yaxis = "y2",
                  color = I("orange"), line = list(width = 1), showlegend = FALSE) %>%
        add_lines(data = year_data, x = ~doy, y = ~tMin, name = paste("Min Temperature -", yr), yaxis = "y2",
                  color = I("lightblue2"), line = list(width = 1), showlegend = FALSE) %>%
        add_lines(data = year_data, x = ~doy, y = ~fruitsStateAct/5, name = paste("Fruits -", yr), yaxis = "y1",
                  color = I("red"), line = list(width = 2)) |> 
        add_lines(data = year_data, x = ~doy, y = ~brixAct*10, name = paste("Brix -", yr), yaxis = "y2",
                color = I("blue"), line = list(width = 2)) |> 
        add_lines(data = year_data, x = ~doy, y = ~wc1, name = paste("WC1 -", yr), yaxis = "y2",
                  color = I("magenta"), line = list(width = 2)) |> 
        add_lines(data = year_data, x = ~doy, y =  ~wc2, name = paste("WC2 -", yr), yaxis = "y2",
                  color = I("cyan4"), line = list(width = 2)) |> 
        add_lines(data = year_data, x = ~doy, y = ~wc3, name = paste("WC3 -", yr), yaxis = "y2",
                  color = I("slateblue4"), line = list(width = 2))
      
      # Define the layout with dual y-axes and title
      p <- p %>%
        layout(
          title = paste("Some Outputs for Year", yr),
          yaxis = list(title = "Precipitation (mm)"),
          yaxis2 = list(title = "Temperature (°C)", overlaying = "y", side = "right"),
          xaxis = list(title = "Day of Year (DOY)"),
          showlegend = FALSE  # Remove legend from the entire layout as well
        )
      
      # Return the plot for this year
      return(p)
    })
    
    # Remove NULL elements from the plots list (if any)
    plots <- Filter(Negate(is.null), plots)
    
    # Check if plots list is empty before creating subplot
    if (length(plots) > 0) {
      # Combine the plots into a single column layout
      final_plot <- subplot(plots, nrows = 1, shareY = T, titleY = TRUE)
      
      # Display the final plot
      final_plot
    } else {
      # Display a message if no data is available
      plot_ly() %>% add_text(text = "No data available for the specified DOY range across years.")
    }
  })
}

