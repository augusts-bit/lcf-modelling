# Link to data folder
linkData <- "Data/"

# Read files from specific directory and add date column to each data frame
dir <- paste0(linkData, "Output/RF/Dense/")
dense_preds <- list.files(dir, pattern = "RFpredict.*\\.csv") 

dense_preds <- lapply(paste0(dir, dense_preds), function(filename) {
  df <- read.csv(filename)
  date_str <- gsub("RFpredict|\\.csv", "", basename(filename))
  date_str <- gsub("RFpredict", "", date_str)
  df$date <- as.Date(date_str, format = "%Y-%m-%d")
  return(df)
})

# Combine data frames into one and sort by location and date
combined_df <- do.call(rbind, dense_preds)
combined_df <- combined_df[order(combined_df$location_id, combined_df$date), ]

# Write to file
write.csv(combined_df, paste0(linkData, "Output/RF/DenseRFPredict.csv"))
