## Download packages ##
install.packages("Metrics")
library(Metrics)

source("C:/Users/augus/Documents/Studie/MGI/Thesis/Rob's/lcfMapping-main/lcfMapping-main/utils/loadData.R")

getRMSE = function(listpredicts, listvalis) {
  
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
      vali = listvalis[1]
      vali = as.data.frame(vali)
      pred = listpredicts[1]
      pred = as.data.frame(pred)
    }
    
    if (year=="2016"){
      vali = listvalis[2]
      vali = as.data.frame(vali)
      pred = listpredicts[2]
      pred = as.data.frame(pred)
    }
    
    if (year=="2017"){
      vali = listvalis[3]
      vali = as.data.frame(vali)
      pred = listpredicts[3]
      pred = as.data.frame(pred)
    }
    
    if (year=="2018"){
      vali = listvalis[4]
      vali = as.data.frame(vali)
      pred = listpredicts[4]
      pred = as.data.frame(pred)
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
  
  return(RMSEdf)
  
}


getMAE <- function(listpredicts, listvalis) {
  
  ## Mean Absolute Error (MAE) ##
  
  # Create dataframe to store results
  tree <- c(0,0,0,0,0)
  shrub <- c(0,0,0,0,0)
  grassland <- c(0,0,0,0,0)
  crops <- c(0,0,0,0,0)
  urban_built_up <- c(0,0,0,0,0)
  bare <- c(0,0,0,0,0)
  water <- c(0,0,0,0,0)
  avg <- c(0,0,0,0,0)
  MAEdf <- data.frame(tree, shrub, grassland, crops, urban_built_up, bare, water, avg)
  rownames(MAEdf) <- c("2015", "2016", "2017", "2018", "avg")
  
  # Calculate MAE
  classes = loadClassNames()
  years = c("2015", "2016", "2017", "2018")
  n = 1
  
  for (year in years) { 
    
    if (year=="2015"){
      vali = listvalis[1]
      vali = as.data.frame(vali)
      pred = listpredicts[1]
      pred = as.data.frame(pred)
    }
    
    if (year=="2016"){
      vali = listvalis[2]
      vali = as.data.frame(vali)
      pred = listpredicts[2]
      pred = as.data.frame(pred)
    }
    
    if (year=="2017"){
      vali = listvalis[3]
      vali = as.data.frame(vali)
      pred = listpredicts[3]
      pred = as.data.frame(pred)
    }
    
    if (year=="2018"){
      vali = listvalis[4]
      vali = as.data.frame(vali)
      pred = listpredicts[4]
      pred = as.data.frame(pred)
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
  
  return(MAEdf)
  
}

