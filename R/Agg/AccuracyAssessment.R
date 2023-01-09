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
# Train
train2015 = read.csv("C:/Users/augus/Documents/Studie/MGI/Thesis/DataAugust/entireTrain_3yearVali/Training/2015/train2015.csv")

# Validation
vali2015 = read.csv("C:/Users/augus/Documents/Studie/MGI/Thesis/DataAugust/entireTrain_3yearVali/Validation/2015/vali2015.csv")
vali2016 = read.csv("C:/Users/augus/Documents/Studie/MGI/Thesis/DataAugust/entireTrain_3yearVali/Validation/2016/vali2016.csv")
vali2017 = read.csv("C:/Users/augus/Documents/Studie/MGI/Thesis/DataAugust/entireTrain_3yearVali/Validation/2017/vali2017.csv")
vali2018 = read.csv("C:/Users/augus/Documents/Studie/MGI/Thesis/DataAugust/entireTrain_3yearVali/Validation/2018/vali2018.csv")

# Change column names 
colnames(vali2015)[3] <- "x"
colnames(vali2015)[4] <- "y"
colnames(vali2016)[3] <- "x"
colnames(vali2016)[4] <- "y"
colnames(vali2017)[3] <- "x"
colnames(vali2017)[4] <- "y"
colnames(vali2018)[3] <- "x"
colnames(vali2018)[4] <- "y"

setnames(vali2015, old = c('nbr2015median','ndmi2015median','ndvi2015median', 'nbr2015iqr', 'ndmi2015iqr', 'ndvi2015iqr'), 
         new = c('nbr_median','ndmi_median','ndvi_median', 'nbr_iqr', 'ndmi_iqr', 'ndvi_iqr'))
setnames(vali2016, old = c('nbr2016median','ndmi2016median','ndvi2016median', 'nbr2016iqr', 'ndmi2016iqr', 'ndvi2016iqr'), 
         new = c('nbr_median','ndmi_median','ndvi_median', 'nbr_iqr', 'ndmi_iqr', 'ndvi_iqr'))
setnames(vali2017, old = c('nbr2017median','ndmi2017median','ndvi2017median', 'nbr2017iqr', 'ndmi2017iqr', 'ndvi2017iqr'), 
         new = c('nbr_median','ndmi_median','ndvi_median', 'nbr_iqr', 'ndmi_iqr', 'ndvi_iqr'))
setnames(vali2018, old = c('nbr2018median','ndmi2018median','ndvi2018median', 'nbr2018iqr', 'ndmi2018iqr', 'ndvi2018iqr'), 
         new = c('nbr_median','ndmi_median','ndvi_median', 'nbr_iqr', 'ndmi_iqr', 'ndvi_iqr'))

# Use only similar IDs 
similarIDs <- vali2016
similarIDs <- subset(similarIDs, sample_id %in% vali2015$sample_id)
similarIDs <- subset(similarIDs, sample_id %in% vali2017$sample_id)
similarIDs <- subset(similarIDs, sample_id %in% vali2018$sample_id)

vali2015 <- subset(vali2015, sample_id %in% similarIDs$sample_id)
vali2016 <- subset(vali2016, sample_id %in% similarIDs$sample_id)
vali2017 <- subset(vali2017, sample_id %in% similarIDs$sample_id)
vali2018 <- subset(vali2018, sample_id %in% similarIDs$sample_id)

# Predictions
predict2015 = read.csv("C:/Users/augus/Documents/Studie/MGI/Thesis/DataAugust/entireTrain_3yearVali/Output/Rob_RFpredict2015_quantiles.csv")
predict2016 = read.csv("C:/Users/augus/Documents/Studie/MGI/Thesis/DataAugust/entireTrain_3yearVali/Output/Rob_RFpredict2016_quantiles.csv")
predict2017 = read.csv("C:/Users/augus/Documents/Studie/MGI/Thesis/DataAugust/entireTrain_3yearVali/Output/Rob_RFpredict2017_quantiles.csv")
predict2018 = read.csv("C:/Users/augus/Documents/Studie/MGI/Thesis/DataAugust/entireTrain_3yearVali/Output/Rob_RFpredict2018_quantiles.csv")

# predict2015 = read.csv("C:/Users/augus/Documents/Studie/MGI/Thesis/DataAugust/entireTrain_3yearVali/Output/Rob_RFpredict2015.csv")
# predict2016 = read.csv("C:/Users/augus/Documents/Studie/MGI/Thesis/DataAugust/entireTrain_3yearVali/Output/Rob_RFpredict2016.csv")
# predict2017 = read.csv("C:/Users/augus/Documents/Studie/MGI/Thesis/DataAugust/entireTrain_3yearVali/Output/Rob_RFpredict2017.csv")
# predict2018 = read.csv("C:/Users/augus/Documents/Studie/MGI/Thesis/DataAugust/entireTrain_3yearVali/Output/Rob_RFpredict2018.csv")

# Remove IDs not similar with validation data
predict2015 <- subset(predict2015, sample_id %in% similarIDs$sample_id)
predict2016 <- subset(predict2016, sample_id %in% similarIDs$sample_id)
predict2017 <- subset(predict2017, sample_id %in% similarIDs$sample_id)
predict2018 <- subset(predict2018, sample_id %in% similarIDs$sample_id)

# Drop NA (some predicted were NA)
predict2015 <- predict2015[!is.na(predict2015$total),]
predict2016 <- predict2016[!is.na(predict2016$total),]
predict2017 <- predict2017[!is.na(predict2017$total),]
predict2018 <- predict2018[!is.na(predict2018$total),]

# Drop the same rows for the vali data
vali2015 <- subset(vali2015, sample_id %in% predict2015$sample_id)
vali2016 <- subset(vali2016, sample_id %in% predict2016$sample_id)
vali2017 <- subset(vali2017, sample_id %in% predict2017$sample_id)
vali2018 <- subset(vali2018, sample_id %in% predict2018$sample_id)

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

# Calculate RMSE
n = 1
  
for (year in years) { 
  
  if (year=="2015"){
    vali = vali2015
    pred = predict2015
  }
  
  if (year=="2016"){
    vali = vali2016
    pred = predict2016
  }
  
  if (year=="2017"){
    vali = vali2017
    pred = predict2017
  }
  
  if (year=="2018"){
    vali = vali2018
    pred = predict2018
  }
  
  RMSEtree <- sqrt(mean((vali$tree - pred$tree)^2))
  RMSEshrub <- sqrt(mean((vali$shrub - pred$shrub)^2))
  RMSEgrassland <- sqrt(mean((vali$grassland - pred$grassland)^2))
  RMSEcrops <- sqrt(mean((vali$crops - pred$crops)^2))
  RMSEurban <- sqrt(mean((vali$urban_built_up - pred$urban_built_up)^2))
  RMSEbare <- sqrt(mean((vali$bare - pred$bare)^2))
  RMSEwater <- sqrt(mean((vali$water - pred$water)^2))
  
  RMSEdf[n,]$tree <- RMSEtree
  RMSEdf[n,]$shrub <- RMSEshrub
  RMSEdf[n,]$grassland <- RMSEgrassland
  RMSEdf[n,]$crops <- RMSEcrops
  RMSEdf[n,]$urban_built_up <- RMSEurban
  RMSEdf[n,]$bare <- RMSEbare
  RMSEdf[n,]$water <- RMSEwater
  RMSEdf[n,]$avg <- (RMSEtree+RMSEshrub+RMSEgrassland+RMSEcrops+RMSEurban+RMSEbare+RMSEwater)/7
  
  n = n + 1
  
}

rm(vali)
rm(pred)

# Average RMSE per class
RMSEdf[5,]$tree <- (RMSEdf[1,]$tree + RMSEdf[2,]$tree + RMSEdf[3,]$tree + RMSEdf[4,]$tree)/4 
RMSEdf[5,]$shrub <- (RMSEdf[1,]$shrub + RMSEdf[2,]$shrub + RMSEdf[3,]$shrub + RMSEdf[4,]$shrub)/4 
RMSEdf[5,]$grassland <- (RMSEdf[1,]$grassland + RMSEdf[2,]$grassland + RMSEdf[3,]$grassland + RMSEdf[4,]$grassland)/4
RMSEdf[5,]$crops <- (RMSEdf[1,]$crops + RMSEdf[2,]$crops + RMSEdf[3,]$crops + RMSEdf[4,]$crops)/4
RMSEdf[5,]$urban_built_up <- (RMSEdf[1,]$urban_built_up + RMSEdf[2,]$urban_built_up + RMSEdf[3,]$urban_built_up + RMSEdf[4,]$urban_built_up)/4
RMSEdf[5,]$bare <- (RMSEdf[1,]$bare + RMSEdf[2,]$bare + RMSEdf[3,]$bare + RMSEdf[4,]$bare)/4
RMSEdf[5,]$water <- (RMSEdf[1,]$water + RMSEdf[2,]$water + RMSEdf[3,]$water + RMSEdf[4,]$water)/4
RMSEdf[5,]$avg <- (RMSEdf[1,]$avg + RMSEdf[2,]$avg + RMSEdf[3,]$avg + RMSEdf[4,]$avg)/4

## Mean Absolute Error (MAE) ##

# Create dataframe to store results
MAEdf <- RMSEdf

# Calculate MAE
n = 1

for (year in years) { 
  
  if (year=="2015"){
    vali = vali2015
    pred = predict2015
  }
  
  if (year=="2016"){
    vali = vali2016
    pred = predict2016
  }
  
  if (year=="2017"){
    vali = vali2017
    pred = predict2017
  }
  
  if (year=="2018"){
    vali = vali2018
    pred = predict2018
  }
  
  MAEtree <- mae(vali$tree, pred$tree)
  MAEshrub <- mae(vali$shrub, pred$shrub)
  MAEgrassland <- mae(vali$grassland, pred$grassland)
  MAEcrops <- mae(vali$crops, pred$crops)
  MAEurban <- mae(vali$urban_built_up, pred$urban_built_up)
  MAEbare <- mae(vali$bare, pred$bare)
  MAEwater <- mae(vali$water, pred$water)
  
  MAEdf[n,]$tree <- MAEtree
  MAEdf[n,]$shrub <- MAEshrub
  MAEdf[n,]$grassland <- MAEgrassland
  MAEdf[n,]$crops <- MAEcrops
  MAEdf[n,]$urban_built_up <- MAEurban
  MAEdf[n,]$bare <- MAEbare
  MAEdf[n,]$water <- MAEwater
  MAEdf[n,]$avg <- (MAEtree+MAEshrub+MAEgrassland+MAEcrops+MAEurban+MAEbare+MAEwater)/7
  
  n = n + 1
  
}

rm(vali)
rm(pred)

# Average MAE per class
MAEdf[5,]$tree <- (MAEdf[1,]$tree + MAEdf[2,]$tree + MAEdf[3,]$tree + MAEdf[4,]$tree)/4 
MAEdf[5,]$shrub <- (MAEdf[1,]$shrub + MAEdf[2,]$shrub + MAEdf[3,]$shrub + MAEdf[4,]$shrub)/4 
MAEdf[5,]$grassland <- (MAEdf[1,]$grassland + MAEdf[2,]$grassland + MAEdf[3,]$grassland + MAEdf[4,]$grassland)/4
MAEdf[5,]$crops <- (MAEdf[1,]$crops + MAEdf[2,]$crops + MAEdf[3,]$crops + MAEdf[4,]$crops)/4
MAEdf[5,]$urban_built_up <- (MAEdf[1,]$urban_built_up + MAEdf[2,]$urban_built_up + MAEdf[3,]$urban_built_up + MAEdf[4,]$urban_built_up)/4
MAEdf[5,]$bare <- (MAEdf[1,]$bare + MAEdf[2,]$bare + MAEdf[3,]$bare + MAEdf[4,]$bare)/4
MAEdf[5,]$water <- (MAEdf[1,]$water + MAEdf[2,]$water + MAEdf[3,]$water + MAEdf[4,]$water)/4
MAEdf[5,]$avg <- (MAEdf[1,]$avg + MAEdf[2,]$avg + MAEdf[3,]$avg + MAEdf[4,]$avg)/4

### Takeaways: RMSE is ~21.9, whereas MAE is ~9.2. Very similar to Rob's: RMSE is ~23.2 and MAE is ~9.2!
# The RMSE is a little bit different because Rob only used similar IDs among the years (I think)

## Write RMSE and MAE results to file
write.csv(RMSEdf, "C:/Users/augus/Documents/Studie/MGI/Thesis/DataAugust/entireTrain_3yearVali/Output/AccuracyAssessment/Rob_RF_RMSE_quantiles.csv")
write.csv(MAEdf, "C:/Users/augus/Documents/Studie/MGI/Thesis/DataAugust/entireTrain_3yearVali/Output/AccuracyAssessment/Rob_RF_MAE_quantiles.csv")
# write.csv(RMSEdf, "C:/Users/augus/Documents/Studie/MGI/Thesis/DataAugust/entireTrain_3yearVali/Output/AccuracyAssessment/Rob_RF_RMSE.csv")
# write.csv(MAEdf, "C:/Users/augus/Documents/Studie/MGI/Thesis/DataAugust/entireTrain_3yearVali/Output/AccuracyAssessment/Rob_RF_MAE.csv")
