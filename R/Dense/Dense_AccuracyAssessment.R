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
vali2015 = read.csv("C:/Users/augus/Documents/Studie/MGI/Thesis/DataAugust/RobMethod_entireTrain_3yearVali/Validation/2015/vali2015.csv")
vali2016 = read.csv("C:/Users/augus/Documents/Studie/MGI/Thesis/DataAugust/RobMethod_entireTrain_3yearVali/Validation/2016/vali2016.csv")
vali2017 = read.csv("C:/Users/augus/Documents/Studie/MGI/Thesis/DataAugust/RobMethod_entireTrain_3yearVali/Validation/2017/vali2017.csv")
vali2018 = read.csv("C:/Users/augus/Documents/Studie/MGI/Thesis/DataAugust/RobMethod_entireTrain_3yearVali/Validation/2018/vali2018.csv")

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

## Loop through predictions and calculate RMSE / MAE ##

# As function
calcErrors <- function(dir) {
  
  # Give vali new name so you don't overwite them
  valida2015 <- vali2015
  valida2016 <- vali2016
  valida2017 <- vali2017
  valida2018 <- vali2018
  
  # Read csv
  pred2015 <- read.csv(paste0(dir, "predict2015.csv"))
  pred2016 <- read.csv(paste0(dir, "predict2016.csv"))
  pred2017 <- read.csv(paste0(dir, "predict2017.csv"))
  pred2018 <- read.csv(paste0(dir, "predict2018.csv"))
  
  # Remove NA predictions
  pred2015 <- pred2015[complete.cases(pred2015),]
  pred2016 <- pred2016[complete.cases(pred2016),]
  pred2017 <- pred2017[complete.cases(pred2017),]
  pred2018 <- pred2018[complete.cases(pred2018),]
  
  # Only use similar IDs
  valida2015 <- subset(valida2015, location_id %in% pred2015$location_id)
  valida2016 <- subset(valida2016, location_id %in% pred2016$location_id)
  valida2017 <- subset(valida2017, location_id %in% pred2017$location_id)
  valida2018 <- subset(valida2018, location_id %in% pred2018$location_id)
  
  pred2015 <- subset(pred2015, location_id %in% valida2015$location_id)
  pred2016 <- subset(pred2016, location_id %in% valida2016$location_id)
  pred2017 <- subset(pred2017, location_id %in% valida2017$location_id)
  pred2018 <- subset(pred2018, location_id %in% valida2018$location_id)
  
  # Sort similarly
  pred2015 <- pred2015[match(valida2015$location_id, pred2015$location_id),]
  pred2016 <- pred2016[match(valida2016$location_id, pred2016$location_id),]
  pred2017 <- pred2017[match(valida2017$location_id, pred2017$location_id),]
  pred2018 <- pred2018[match(valida2018$location_id, pred2018$location_id),]
  
  ## RMSE / MAE ##
  classes = loadClassNames()
  
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
  years = c("2015", "2016", "2017", "2018")
  
  for (year in years) { 
    
    if (year=="2015"){
      vali = valida2015
      pred = pred2015
    }
    
    if (year=="2016"){
      vali = valida2016
      pred = pred2016
    }
    
    if (year=="2017"){
      vali = valida2017
      pred = pred2017
    }
    
    if (year=="2018"){
      vali = valida2018
      pred = pred2018
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
  years = c("2015", "2016", "2017", "2018")
  
  for (year in years) { 
    
    if (year=="2015"){
      vali = valida2015
      pred = pred2015
    }
    
    if (year=="2016"){
      vali = valida2016
      pred = pred2016
    }
    
    if (year=="2017"){
      vali = valida2017
      pred = pred2017
    }
    
    if (year=="2018"){
      vali = valida2018
      pred = pred2018
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
  
  ## Return RMSEdf and MAEdf ##
  return(list(RMSEdf, MAEdf))
  
  } 

## Get RMSE / MAE dfs by applying function ##
commondir <- "C:/Users/augus/Documents/Studie/MGI/Thesis/DataAugust/RobMethod_entireTrain_3yearVali/Output/Dense/"

resp_Es <- calcErrors(paste0(commondir, "Response/WithoutGrowingSeason/"))
resp_withGS_Es <- calcErrors(paste0(commondir, "Response/WithGrowingSeason/"))
quant_Es <- calcErrors(paste0(commondir, "Quantiles/WithoutGrowingSeason/"))
quant_withGS_Es <- calcErrors(paste0(commondir, "Quantiles/WithGrowingSeason/"))

## Write results to file ##
write.csv(resp_Es[1], paste0(commondir, "Response/WithoutGrowingSeason/AccuracyAssessment/resp_RMSE.csv"))
write.csv(resp_Es[2], paste0(commondir, "Response/WithoutGrowingSeason/AccuracyAssessment/resp_MAE.csv"))

write.csv(resp_withGS_Es[1], paste0(commondir, "Response/WithGrowingSeason/AccuracyAssessment/resp_withGS_RMSE.csv"))
write.csv(resp_withGS_Es[2], paste0(commondir, "Response/WithGrowingSeason/AccuracyAssessment/resp_withGS_MAE.csv"))

write.csv(quant_Es[1], paste0(commondir, "Quantiles/WithoutGrowingSeason/AccuracyAssessment/quant_RMSE.csv"))
write.csv(quant_Es[2], paste0(commondir, "Quantiles/WithoutGrowingSeason/AccuracyAssessment/quant_MAE.csv"))

write.csv(quant_withGS_Es[1], paste0(commondir, "Quantiles/WithGrowingSeason/AccuracyAssessment/quant_withGS_RMSE.csv"))
write.csv(quant_withGS_Es[2], paste0(commondir, "Quantiles/WithGrowingSeason/AccuracyAssessment/quant_withGS_MAE.csv"))
