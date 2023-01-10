## Download packages ##
install.packages("Metrics")

## Set libraries ##
library(sf)
library(pbapply)
library(Metrics)
library(data.table)

## Source utils ##
source("C:/Users/augus/Documents/Studie/MGI/Thesis/Rob's/lcfMapping-main/lcfMapping-main/utils/loadData.R")

## Read validation and prediction data ##

# Validation
vali <- read.csv("C:/Users/augus/Documents/Studie/MGI/Thesis/DataAugust/LSTM/Input/vali20152018_noXY.csv")

# Prediction
pred <- read.csv("C:/Users/augus/Documents/Studie/MGI/Thesis/DataAugust/LSTM/Output/LSTM_predict_noXY_SmoothL1.csv")

## Rescaling predictions (a lot of fractions don't add up to 100)  ##
# Rescaling was also done after RF, so is it fair?
# If not, comment/remove these lines

pred$total <- pred$bare + pred$crops + pred$grassland + pred$shrub + pred$tree + pred$urban_built_up + pred$water
pred$bare <- (pred$bare / pred$total)*100
pred$crops <- (pred$crops / pred$total)*100 
pred$grassland <- (pred$grassland / pred$total)*100 
pred$shrub <- (pred$shrub / pred$total)*100
pred$tree <- (pred$tree / pred$total)*100 
pred$urban_built_up <- (pred$urban_built_up / pred$total)*100 
pred$water <- (pred$water / pred$total)*100 
pred$total_rescaled <- pred$bare + pred$crops + pred$grassland + pred$shrub + pred$tree + pred$urban_built_up + pred$water

# Remove rows (and IDs) where total = 0 (basically where every class was predicted 0)

badsamples <- list()
for (i in 1:nrow(pred)) {
  if (any(is.na(pred[i, ]))) {
    sample <- pred$sample_id[i]
    badsamples <- c(badsamples, sample)
  }
}
pred <- subset(pred, !(sample_id %in% badsamples))

# Remove those IDs then as well for the validation
vali <- subset(vali, sample_id %in% pred$sample_id)

## Classes ##
classes = loadClassNames()
years = c("2015", "2016", "2017", "2018")

## RMSE ##
# sqrt(mean((data$actual - data$predicted)^2))

# Create dataframe to store results
tree <- c(0,0,0,0,0)
shrub <- c(0,0,0,0,0)
grassland <- c(0,0,0,0,0)
crops <- c(0,0,0,0,0)
urban_built_up <- c(0,0,0,0,0)
bare <- c(0,0,0,0,0)
water <- c(0,0,0,0,0)
avg <- c(0,0,0,0,0)
RMSEdf <- data.frame(tree, shrub, grassland, crops, urban_built_up, bare, water, avg)
rownames(RMSEdf) <- c("2015", "2016", "2017", "2018", "avg")

# RMSE per class

RMSEtree <- sqrt(mean((vali$tree - pred$tree)^2))
RMSEshrub <- sqrt(mean((vali$shrub - pred$shrub)^2))
RMSEgrassland <- sqrt(mean((vali$grassland - pred$grassland)^2))
RMSEcrops <- sqrt(mean((vali$crops - pred$crops)^2))
RMSEurban <- sqrt(mean((vali$urban_built_up - pred$urban_built_up)^2))
RMSEbare <- sqrt(mean((vali$bare - pred$bare)^2))
RMSEwater <- sqrt(mean((vali$water - pred$water)^2))

RMSEdf[5,]$tree <- RMSEtree
RMSEdf[5,]$shrub <- RMSEshrub
RMSEdf[5,]$grassland <- RMSEgrassland
RMSEdf[5,]$crops <- RMSEcrops
RMSEdf[5,]$urban_built_up <- RMSEurban
RMSEdf[5,]$bare <- RMSEbare
RMSEdf[5,]$water <- RMSEwater
RMSEdf[5,]$avg <- (RMSEtree+RMSEshrub+RMSEgrassland+RMSEcrops+RMSEurban+RMSEbare+RMSEwater)/7

# RMSE per year

for (year in years) {
  
  vali_year = vali[vali$reference_year==year,]
  pred_year = pred[pred$reference_year==year,]
  
  RMSEtree <- sqrt(mean((vali_year$tree - pred_year$tree)^2))
  RMSEshrub <- sqrt(mean((vali_year$shrub - pred_year$shrub)^2))
  RMSEgrassland <- sqrt(mean((vali_year$grassland - pred_year$grassland)^2))
  RMSEcrops <- sqrt(mean((vali_year$crops - pred_year$crops)^2))
  RMSEurban <- sqrt(mean((vali_year$urban_built_up - pred_year$urban_built_up)^2))
  RMSEbare <- sqrt(mean((vali_year$bare - pred_year$bare)^2))
  RMSEwater <- sqrt(mean((vali_year$water - pred_year$water)^2))
  
  RMSEdf[year,]$tree <- RMSEtree
  RMSEdf[year,]$shrub <- RMSEshrub
  RMSEdf[year,]$grassland <- RMSEgrassland
  RMSEdf[year,]$crops <- RMSEcrops
  RMSEdf[year,]$urban_built_up <- RMSEurban
  RMSEdf[year,]$bare <- RMSEbare
  RMSEdf[year,]$water <- RMSEwater
  RMSEdf[year,]$avg <- (RMSEtree+RMSEshrub+RMSEgrassland+RMSEcrops+RMSEurban+RMSEbare+RMSEwater)/7
}

## Mean Absolute Error (MAE) ##

# Create data frame to store results
MAEdf <- RMSEdf

# MAE per class

MAEtree <- mae(vali$tree, pred$tree)
MAEshrub <- mae(vali$shrub, pred$shrub)
MAEgrassland <- mae(vali$grassland, pred$grassland)
MAEcrops <- mae(vali$crops, pred$crops)
MAEurban <- mae(vali$urban_built_up, pred$urban_built_up)
MAEbare <- mae(vali$bare, pred$bare)
MAEwater <- mae(vali$water, pred$water)

MAEdf[5,]$tree <- MAEtree
MAEdf[5,]$shrub <- MAEshrub
MAEdf[5,]$grassland <- MAEgrassland
MAEdf[5,]$crops <- MAEcrops
MAEdf[5,]$urban_built_up <- MAEurban
MAEdf[5,]$bare <- MAEbare
MAEdf[5,]$water <- MAEwater
MAEdf[5,]$avg <- (MAEtree+MAEshrub+MAEgrassland+MAEcrops+MAEurban+MAEbare+MAEwater)/7

# MAE per year

for (year in years) {
  
  vali_year = vali[vali$reference_year==year,]
  pred_year = pred[pred$reference_year==year,]
  
  MAEtree <- mae(vali_year$tree, pred_year$tree)
  MAEshrub <- mae(vali_year$shrub, pred_year$shrub)
  MAEgrassland <- mae(vali_year$grassland, pred_year$grassland)
  MAEcrops <- mae(vali_year$crops, pred_year$crops)
  MAEurban <- mae(vali_year$urban_built_up, pred_year$urban_built_up)
  MAEbare <- mae(vali_year$bare, pred_year$bare)
  MAEwater <- mae(vali_year$water, pred_year$water)
  
  MAEdf[year,]$tree <- MAEtree
  MAEdf[year,]$shrub <- MAEshrub
  MAEdf[year,]$grassland <- MAEgrassland
  MAEdf[year,]$crops <- MAEcrops
  MAEdf[year,]$urban_built_up <- MAEurban
  MAEdf[year,]$bare <- MAEbare
  MAEdf[year,]$water <- MAEwater
  MAEdf[year,]$avg <- (MAEtree+MAEshrub+MAEgrassland+MAEcrops+MAEurban+MAEbare+MAEwater)/7
}

## Write RMSE and MAE results to file
write.csv(RMSEdf, "C:/Users/augus/Documents/Studie/MGI/Thesis/DataAugust/LSTM/Output/AccuracyAssessment/LSTM_SmoothL1_RMSE.csv")
write.csv(MAEdf, "C:/Users/augus/Documents/Studie/MGI/Thesis/DataAugust/LSTM/Output/AccuracyAssessment/LSTM_SmoothL1_MAE.csv")
