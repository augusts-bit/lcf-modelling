## Set libraries ##
install.packages("fame")
library(fame)
library(sf)
library(pbapply)
library(ranger)
library(data.table)

## Source utils ##
source("C:/Users/augus/Documents/Studie/MGI/Thesis/Rob's/lcfMapping-main/lcfMapping-main/utils/loadData.R")
source("C:/Users/augus/Documents/Studie/MGI/Thesis/Rob's/lcfMapping-main/lcfMapping-main/utils/extractDates.R")

## Read training and validation data ##
dense_train2015 = read.csv("C:/Users/augus/Documents/Studie/MGI/Thesis/DataAugust/RobMethod_entireTrain_3yearVali/Dense/dense_train2015.csv")
dense_valis <- list.files(path="C:/Users/augus/Documents/Studie/MGI/Thesis/DataAugust/RobMethod_entireTrain_3yearVali/Dense/Validation", pattern="*.csv", full.names=TRUE, recursive=FALSE)

# Change some column names
setnames(dense_train2015, old = c('nbr2015median','ndmi2015median','ndvi2015median', 'nbr2015iqr', 'ndmi2015iqr', 'ndvi2015iqr'), 
         new = c('nbr_median','ndmi_median','ndvi_median', 'nbr_IQR', 'ndmi_IQR', 'ndvi_IQR'))

# Dense train contains some NA values in the growing season columns. Remove these, but only for Growing season RF
dense_train2015_GS <- dense_train2015
dense_train2015_GS <- dense_train2015_GS[!is.na(dense_train2015_GS$ndvi_growingseason),]
dense_train2015_GS <- dense_train2015_GS[!is.na(dense_train2015_GS$ndmi_growingseason),]
dense_train2015_GS <- dense_train2015_GS[!is.na(dense_train2015_GS$nbr_growingseason),]

## Random Forest. With median voting (important) ##

# Set median voting settings

# Type = response
PredictTypeResponse="response"

# Type = quantiles
PredictTypeQuantiles="quantiles"

# quantreg = ifelse(PredictType=="response", FALSE, TRUE)
# in this case, if "response" = FALSE, and "quantiles" = TRUE

PredictQuantiles = 0.5

# Load class names
classes = loadClassNames()

# Create initial formula string
subformula <- "~x+y+min+max+intercept+co+si+co2+si2+trend+phase1+amplitude1+phase2+amplitude2+nbr_median+ndmi_median+ndvi_median+nbr_IQR+ndmi_IQR+ndvi_IQR"
subformula_inclGS <- "~x+y+min+max+intercept+co+si+co2+si2+trend+phase1+amplitude1+phase2+amplitude2+nbr_median+ndmi_median+ndvi_median+nbr_IQR+ndmi_IQR+ndvi_IQR+ndvi_growingseason+ndmi_growingseason+nbr_growingseason"

## Adjust formulas ##

# without growing season
formula_tree = paste0("tree", subformula)
formula_shrub = paste0("shrub", subformula)
formula_grassland = paste0("grassland", subformula)
formula_crops = paste0("crops", subformula)
formula_urban_built_up = paste0("urban_built_up", subformula)
formula_bare = paste0("bare", subformula)
formula_water = paste0("water", subformula)

# with growing season
formula_tree_inclGS = paste0("tree", subformula_inclGS)
formula_shrub_inclGS = paste0("shrub", subformula_inclGS)
formula_grassland_inclGS = paste0("grassland", subformula_inclGS)
formula_crops_inclGS = paste0("crops", subformula_inclGS)
formula_urban_built_up_inclGS = paste0("urban_built_up", subformula_inclGS)
formula_bare_inclGS = paste0("bare", subformula_inclGS)
formula_water_inclGS = paste0("water", subformula_inclGS)

## Make RF for every class ##
## Apply RF ##
## Remove RF to save memory##

# type = response (quantreg=FALSE)
# without growing season
rfmodel_tree_resp = ranger(formula_tree, dense_train2015, seed = 0xbadcafe, quantreg=FALSE)
rfmodel_shrub_resp = ranger(formula_shrub, dense_train2015, seed = 0xbadcafe, quantreg=FALSE)
rfmodel_grassland_resp = ranger(formula_grassland, dense_train2015, seed = 0xbadcafe, quantreg=FALSE)
rfmodel_crops_resp = ranger(formula_crops, dense_train2015, seed = 0xbadcafe, quantreg=FALSE)
rfmodel_urban_built_up_resp = ranger(formula_urban_built_up, dense_train2015, seed = 0xbadcafe, quantreg=FALSE)
rfmodel_bare_resp = ranger(formula_bare, dense_train2015, seed = 0xbadcafe, quantreg=FALSE)
rfmodel_water_resp = ranger(formula_water, dense_train2015, seed = 0xbadcafe, quantreg=FALSE)

# Make predictions for every validation dataset (for loop)

n = 1

dates = extractDates()
acqdates = dates[grepl("2015|2016|2017|2018",dates)]

for (date in acqdates) {
  
  # Read vali data as csv
  dense_vali_csv <- read.csv(dense_valis[n])
  # assign(paste0("dense_vali", acqdates[n]), dense_vali_csv)
  
  ## 1) type = response, Without growing season ##
  
  # Get predictions per class
  output1 = predict(rfmodel_tree_resp, dense_vali_csv, type=PredictTypeResponse, quantiles=PredictQuantiles)$predictions
  output2 = predict(rfmodel_shrub_resp, dense_vali_csv, type=PredictTypeResponse, quantiles=PredictQuantiles)$predictions
  output3 = predict(rfmodel_grassland_resp, dense_vali_csv, type=PredictTypeResponse, quantiles=PredictQuantiles)$predictions
  output4 = predict(rfmodel_crops_resp, dense_vali_csv, type=PredictTypeResponse, quantiles=PredictQuantiles)$predictions
  output5 = predict(rfmodel_urban_built_up_resp, dense_vali_csv, type=PredictTypeResponse, quantiles=PredictQuantiles)$predictions
  output6 = predict(rfmodel_bare_resp, dense_vali_csv, type=PredictTypeResponse, quantiles=PredictQuantiles)$predictions
  output7 = predict(rfmodel_water_resp, dense_vali_csv, type=PredictTypeResponse, quantiles=PredictQuantiles)$predictions
  
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
  write.csv(df, paste0("C:/Users/augus/Documents/Studie/MGI/Thesis/DataAugust/RobMethod_entireTrain_3yearVali/Output/Dense/Response/WithoutGrowingSeason/resp_predict",acqdates[n],".csv"))
  
  n = n + 1
}

# Remove memory
rm(rfmodel_tree_resp, rfmodel_shrub_resp, rfmodel_grassland_resp, rfmodel_crops_resp, rfmodel_urban_built_up_resp, rfmodel_bare_resp, rfmodel_water_resp)
gc()

# with growing season 
rfmodel_tree_inclGS_resp = ranger(formula_tree_inclGS, dense_train2015_GS, seed = 0xbadcafe, quantreg=FALSE)
rfmodel_shrub_inclGS_resp = ranger(formula_shrub_inclGS, dense_train2015_GS, seed = 0xbadcafe, quantreg=FALSE)
rfmodel_grassland_inclGS_resp = ranger(formula_grassland_inclGS, dense_train2015_GS, seed = 0xbadcafe, quantreg=FALSE)
rfmodel_crops_inclGS_resp = ranger(formula_crops_inclGS, dense_train2015_GS, seed = 0xbadcafe, quantreg=FALSE)
rfmodel_urban_built_up_inclGS_resp = ranger(formula_urban_built_up_inclGS, dense_train2015_GS, seed = 0xbadcafe, quantreg=FALSE)
rfmodel_bare_inclGS_resp = ranger(formula_bare_inclGS, dense_train2015_GS, seed = 0xbadcafe, quantreg=FALSE)
rfmodel_water_inclGS_resp = ranger(formula_water_inclGS, dense_train2015_GS, seed = 0xbadcafe, quantreg=FALSE)

# Make predictions for every validation dataset (for loop)

n = 1

dates = extractDates()
acqdates = dates[grepl("2015|2016|2017|2018",dates)]

for (date in acqdates) {
  
  # Read vali data as csv
  dense_vali_csv <- read.csv(dense_valis[n])
  # assign(paste0("dense_vali", acqdates[n]), dense_vali_csv)
  
  ## 2) type = response, With growing season ##
  
  # Get predictions per class
  output1 = predict(rfmodel_tree_inclGS_resp, dense_vali_csv, type=PredictTypeResponse, quantiles=PredictQuantiles)$predictions
  output2 = predict(rfmodel_shrub_inclGS_resp, dense_vali_csv, type=PredictTypeResponse, quantiles=PredictQuantiles)$predictions
  output3 = predict(rfmodel_grassland_inclGS_resp, dense_vali_csv, type=PredictTypeResponse, quantiles=PredictQuantiles)$predictions
  output4 = predict(rfmodel_crops_inclGS_resp, dense_vali_csv, type=PredictTypeResponse, quantiles=PredictQuantiles)$predictions
  output5 = predict(rfmodel_urban_built_up_inclGS_resp, dense_vali_csv, type=PredictTypeResponse, quantiles=PredictQuantiles)$predictions
  output6 = predict(rfmodel_bare_inclGS_resp, dense_vali_csv, type=PredictTypeResponse, quantiles=PredictQuantiles)$predictions
  output7 = predict(rfmodel_water_inclGS_resp, dense_vali_csv, type=PredictTypeResponse, quantiles=PredictQuantiles)$predictions
  
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
  write.csv(df, paste0("C:/Users/augus/Documents/Studie/MGI/Thesis/DataAugust/RobMethod_entireTrain_3yearVali/Output/Dense/Response/WithGrowingSeason/resp_withGS_predict",acqdates[n],".csv"))
  
  n = n + 1
}

# Remove memory
rm(rfmodel_tree_inclGS_resp, rfmodel_shrub_inclGS_resp, rfmodel_grassland_inclGS_resp, rfmodel_crops_inclGS_resp, rfmodel_urban_built_up_inclGS_resp, rfmodel_bare_inclGS_resp, rfmodel_water_inclGS_resp)
gc()

# type = quantiles (quantreg=TRUE)
# without growing season
rfmodel_tree_quant = ranger(formula_tree, dense_train2015, seed = 0xbadcafe, quantreg=TRUE)
rfmodel_shrub_quant = ranger(formula_shrub, dense_train2015, seed = 0xbadcafe, quantreg=TRUE)
rfmodel_grassland_quant = ranger(formula_grassland, dense_train2015, seed = 0xbadcafe, quantreg=TRUE)
rfmodel_crops_quant = ranger(formula_crops, dense_train2015, seed = 0xbadcafe, quantreg=TRUE)
rfmodel_urban_built_up_quant = ranger(formula_urban_built_up, dense_train2015, seed = 0xbadcafe, quantreg=TRUE)
rfmodel_bare_quant = ranger(formula_bare, dense_train2015, seed = 0xbadcafe, quantreg=TRUE)
rfmodel_water_quant = ranger(formula_water, dense_train2015, seed = 0xbadcafe, quantreg=TRUE)

# Make predictions for every validation dataset (for loop)

n = 1

dates = extractDates()
acqdates = dates[grepl("2015|2016|2017|2018",dates)]

for (date in acqdates) {
  
  # Read vali data as csv
  dense_vali_csv <- read.csv(dense_valis[n])
  # assign(paste0("dense_vali", acqdates[n]), dense_vali_csv)
  
  ## 3) type = quantiles, Without growing season ##
  
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
  write.csv(df, paste0("C:/Users/augus/Documents/Studie/MGI/Thesis/DataAugust/RobMethod_entireTrain_3yearVali/Output/Dense/Quantiles/WithoutGrowingSeason/quant_predict",acqdates[n],".csv"))
  
  n = n + 1
}

# Remove memory
rm(rfmodel_tree_quant, rfmodel_shrub_quant, rfmodel_grassland_quant, rfmodel_crops_quant, rfmodel_urban_built_up_quant, rfmodel_bare_quant, rfmodel_water_quant)
gc()

# with growing season
rfmodel_tree_inclGS_quant = ranger(formula_tree_inclGS, dense_train2015_GS, seed = 0xbadcafe, quantreg=TRUE)
rfmodel_shrub_inclGS_quant = ranger(formula_shrub_inclGS, dense_train2015_GS, seed = 0xbadcafe, quantreg=TRUE)
rfmodel_grassland_inclGS_quant = ranger(formula_grassland_inclGS, dense_train2015_GS, seed = 0xbadcafe, quantreg=TRUE)
rfmodel_crops_inclGS_quant = ranger(formula_crops_inclGS, dense_train2015_GS, seed = 0xbadcafe, quantreg=TRUE)
rfmodel_urban_built_up_inclGS_quant = ranger(formula_urban_built_up_inclGS, dense_train2015_GS, seed = 0xbadcafe, quantreg=TRUE)
rfmodel_bare_inclGS_quant = ranger(formula_bare_inclGS, dense_train2015_GS, seed = 0xbadcafe, quantreg=TRUE)
rfmodel_water_inclGS_quant = ranger(formula_water_inclGS, dense_train2015_GS, seed = 0xbadcafe, quantreg=TRUE)

# Make predictions for every validation dataset (for loop)

n = 1

dates = extractDates()
acqdates = dates[grepl("2015|2016|2017|2018",dates)]

for (date in acqdates) {
  
  # Read vali data as csv
  dense_vali_csv <- read.csv(dense_valis[n])
  # assign(paste0("dense_vali", acqdates[n]), dense_vali_csv)
  
  ## 4) type = quantiles, With growing season ##
  
  # Get predictions per class
  output1 = predict(rfmodel_tree_inclGS_quant, dense_vali_csv, type=PredictTypeQuantiles, quantiles=PredictQuantiles)$predictions
  output2 = predict(rfmodel_shrub_inclGS_quant, dense_vali_csv, type=PredictTypeQuantiles, quantiles=PredictQuantiles)$predictions
  output3 = predict(rfmodel_grassland_inclGS_quant, dense_vali_csv, type=PredictTypeQuantiles, quantiles=PredictQuantiles)$predictions
  output4 = predict(rfmodel_crops_inclGS_quant, dense_vali_csv, type=PredictTypeQuantiles, quantiles=PredictQuantiles)$predictions
  output5 = predict(rfmodel_urban_built_up_inclGS_quant, dense_vali_csv, type=PredictTypeQuantiles, quantiles=PredictQuantiles)$predictions
  output6 = predict(rfmodel_bare_inclGS_quant, dense_vali_csv, type=PredictTypeQuantiles, quantiles=PredictQuantiles)$predictions
  output7 = predict(rfmodel_water_inclGS_quant, dense_vali_csv, type=PredictTypeQuantiles, quantiles=PredictQuantiles)$predictions
  
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
  write.csv(df, paste0("C:/Users/augus/Documents/Studie/MGI/Thesis/DataAugust/RobMethod_entireTrain_3yearVali/Output/Dense/Quantiles/WithGrowingSeason/quant_withGS_predict",acqdates[n],".csv"))
  
  n = n + 1
}

# Remove memory
rm(rfmodel_tree_inclGS_quant, rfmodel_shrub_inclGS_quant, rfmodel_grassland_inclGS_quant, rfmodel_crops_inclGS_quant, rfmodel_urban_built_up_inclGS_quant, rfmodel_bare_inclGS_quant, rfmodel_water_inclGS_quant)
gc()
