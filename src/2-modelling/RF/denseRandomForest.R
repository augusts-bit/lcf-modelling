## Set libraries ##
install.packages("fame")
library(fame)
library(sf)
library(pbapply)
library(ranger)
library(data.table)

## Source utils ##
source("utils/loadData.R")
source("utils/extractDates.R")

# Link to data folder
linkData <- "Data/"

## Read training and validation data ##
train2015 = read.csv(paste0(linkData, "Processed/Training/train2015.csv"))
dense_valis <- list.files(path=paste0(linkData, "Processed/Validation/Dense_Window/"), pattern="*.csv", full.names=TRUE, recursive=FALSE)

# Change some column names
setnames(train2015, old = c('nbr2015median','ndmi2015median','ndvi2015median', 'nbr2015iqr', 'ndmi2015iqr', 'ndvi2015iqr'), 
         new = c('nbr_median','ndmi_median','ndvi_median', 'nbr_IQR', 'ndmi_IQR', 'ndvi_IQR'))

# If wanted, oversample urban samples
# All urban samples, times 5
# All urban samples where urban is 100%, 5 times (seems to not help)
df_urban <- train2015[train2015$urban_built_up > 0, ]
df_urban_100 <- train2015[train2015$urban_built_up == 100, ]
df_urban <- df_urban[rep(seq_len(nrow(df_urban)), each = 5), ]
df_urban_100 <- df_urban_100[rep(seq_len(nrow(df_urban_100)), each = 5), ]

# bind the original data frame with the repeated rows
train2015_oversampled <- rbind(train2015, df_urban)

## Random Forest. With median voting (important) ##

# Set median voting settings

# Type = quantiles
PredictTypeQuantiles="quantiles"

PredictQuantiles = 0.5

# Load class names
classes = loadClassNames()

# Create initial formula string
subformula <- "~x+y+min+max+intercept+co+si+co2+si2+trend+phase1+amplitude1+phase2+amplitude2+nbr_median+ndmi_median+ndvi_median+nbr_IQR+ndmi_IQR+ndvi_IQR"

## Adjust formulas ##
formula_tree = paste0("tree", subformula)
formula_shrub = paste0("shrub", subformula)
formula_grassland = paste0("grassland", subformula)
formula_crops = paste0("crops", subformula)
formula_urban_built_up = paste0("urban_built_up", subformula)
formula_bare = paste0("bare", subformula)
formula_water = paste0("water", subformula)

## Make RF for every class ##
## Apply RF ##

# type = quantiles (quantreg=TRUE)
train_input = train2015

rfmodel_tree_quant = ranger(formula_tree, train_input, seed = 0xbadcafe, quantreg=TRUE)
rfmodel_shrub_quant = ranger(formula_shrub, train_input, seed = 0xbadcafe, quantreg=TRUE)
rfmodel_grassland_quant = ranger(formula_grassland, train_input, seed = 0xbadcafe, quantreg=TRUE)
rfmodel_crops_quant = ranger(formula_crops, train_input, seed = 0xbadcafe, quantreg=TRUE)
rfmodel_urban_built_up_quant = ranger(formula_urban_built_up, train_input, seed = 0xbadcafe, quantreg=TRUE)
rfmodel_bare_quant = ranger(formula_bare, train_input, seed = 0xbadcafe, quantreg=TRUE)
rfmodel_water_quant = ranger(formula_water, train_input, seed = 0xbadcafe, quantreg=TRUE)

# Make predictions for every validation dataset (for loop)

n = 1

dates = extractDates()
acqdates = dates[grepl("2015|2016|2017|2018",dates)]

for (date in acqdates) {
  
  # Read vali data as csv
  dense_vali_csv <- read.csv(dense_valis[n])
  
  # Get predictions per class
  output1 = predict(rfmodel_tree_quant, dense_vali_csv, type=PredictTypeQuantiles, quantiles=PredictQuantiles)$predictions
  output2 = predict(rfmodel_shrub_quant, dense_vali_csv, type=PredictTypeQuantiles, quantiles=PredictQuantiles)$predictions
  output3 = predict(rfmodel_grassland_quant, dense_vali_csv, type=PredictTypeQuantiles, quantiles=PredictQuantiles)$predictions
  output4 = predict(rfmodel_crops_quant, dense_vali_csv, type=PredictTypeQuantiles, quantiles=PredictQuantiles)$predictions
  output5 = predict(rfmodel_urban_built_up_quant, dense_vali_csv, type=PredictTypeQuantiles, quantiles=PredictQuantiles)$predictions
  output6 = predict(rfmodel_bare_quant, dense_vali_csv, type=PredictTypeQuantiles, quantiles=PredictQuantiles)$predictions
  output7 = predict(rfmodel_water_quant, dense_vali_csv, type=PredictTypeQuantiles, quantiles=PredictQuantiles)$predictions
  
  # Calculate total to rescale results
  total <- output1 + output2 + output3 + output4 + output5 + output6 + output7
  
  # Add predictions to dataset
  df <- dense_vali_csv[, c("location_id", "x", "y")]
  df$tree <- (output1/total) * 100
  df$shrub <- (output2/total) * 100
  df$grassland <- (output3/total) * 100
  df$crops <- (output4/total) * 100
  df$urban_built_up <- (output5/total) * 100
  df$bare <- (output6/total) * 100
  df$water <- (output7/total) * 100
  df$total <- df$tree + df$shrub + df$grassland + df$crops + df$urban_built_up + df$bare + df$water
  
  # Write result to file
  write.csv(df, paste0(linkData, "Output/RF/Dense/RFPredict",acqdates[n],".csv"))
  
  n = n + 1
}
