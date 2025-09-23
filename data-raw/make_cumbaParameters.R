# data-raw/make_cumbaParameters.R
df <- read.csv("data-raw/parameters.csv", stringsAsFactors = FALSE)

cumbaParameters <- setNames(
  lapply(seq_len(nrow(df)), function(i) {
    list(
      description = df$description[i],
      value = df$value[i],
      min = df$min[i],
      max = df$max[i]
    )
  }),
  df$parameter
)

# Save to data/ folder (this is where R packages store datasets)
usethis::use_data(cumbaParameters, overwrite = TRUE)
