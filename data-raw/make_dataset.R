# data-raw/make_cumbaParameters.R
excel_file<-"..//data-raw/Dataset Carucci et al. new.xlsx"
sheet_names <- excel_sheets("..//data-raw/Dataset Carucci et al. new.xlsx")
all_sheets <- lapply(sheet_names, function(sheet) read_excel(excel_file, sheet = sheet))

# Extract relevant data frames
weather     <- all_sheets[[4]]
irrigation  <- all_sheets[[7]]
ids         <- all_sheets[[2]]
yield       <- as.data.frame(all_sheets[[5]]) |> 
  group_by(ID) |> 
  summarise(yield_ref = mean(Y_TOT/10),
            yield_ref_sd = sd(Y_TOT/10),
            brix_ref = mean(BRIX),
            brix_ref_sd = sd(BRIX))

# Merge irrigation with IDs and assign site name
irrigation_df <- irrigation |> left_join(ids)
irrigation_df$Site <- "Foggia"

# Process weather data
weather <- weather |> 
  mutate(Date = as.Date(DATE), Site = "Foggia") |> 
  rename(Rad = RAD) |> 
  mutate(Lat = 41)

tomatoFoggia<-list()
tomatoFoggia$weather <- weather
tomatoFoggia$irrigation <- irrigation_df
tomatoFoggia$management <- ids
tomatoFoggia$production <- yield


head(weather,3)
head(irrigation_df,3)
head(ids,3)
head(yield,3)
# Save to data/ folder (this is where R packages store datasets)
usethis::use_data(tomatoFoggia, overwrite = TRUE)
