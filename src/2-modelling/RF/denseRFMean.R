## Set libraries ##
install.packages("fame")
library(fame)
library(sf)
library(pbapply)
library(ranger)
library(data.table)
library(matrixStats)

## Source utils ##
source("utils/loadData.R")
source("utils/extractDates.R")

# Link to data folder
linkData <- "Data/"

## Read validation to sort predictions ##
vali2015 <- read.csv(paste0(linkData, "Processed/Validation/vali2015.csv"))
vali2016 <- read.csv(paste0(linkData, "Processed/Validation/vali2016.csv"))
vali2017 <- read.csv(paste0(linkData, "Processed/Validation/vali2017.csv"))
vali2018 <- read.csv(paste0(linkData, "Processed/Validation/vali2018.csv"))

## Report growing season value of each year to assess results ##

# Make it a function so that it can easily be applied 

MergePreds <- function(directory) {

  ## List files ##

  files2015_south <- list.files(path=directory, pattern="2015-01|2015-02|2015-03", full.names=TRUE, recursive=FALSE)
  files2015_north <- list.files(path=directory, pattern="2015-07|2015-08|2015-09", full.names=TRUE, recursive=FALSE)
  
  files2016_south <- list.files(path=directory, pattern="2016-01|2016-02|2016-03", full.names=TRUE, recursive=FALSE)
  files2016_north <- list.files(path=directory, pattern="2016-07|2016-08|2016-09", full.names=TRUE, recursive=FALSE)
  
  files2017_south <- list.files(path=directory, pattern="2017-01|2017-02|2017-03", full.names=TRUE, recursive=FALSE)
  files2017_north <- list.files(path=directory, pattern="2017-07|2017-08|2017-09", full.names=TRUE, recursive=FALSE)
  
  files2018_south <- list.files(path=directory, pattern="2018-01|2018-02|2018-03", full.names=TRUE, recursive=FALSE)
  files2018_north <- list.files(path=directory, pattern="2018-07|2018-08|2018-09", full.names=TRUE, recursive=FALSE)
  
  
  files2015_Jan <- list.files(path="directory", pattern="2015-01", full.names=TRUE, recursive=FALSE) 
  files2015_Jul <- list.files(path="directory", pattern="2015-07", full.names=TRUE, recursive=FALSE) 
  files2016_Jan <- list.files(path="directory", pattern="2016-01", full.names=TRUE, recursive=FALSE) 
  files2016_Jul <- list.files(path="directory", pattern="2016-07", full.names=TRUE, recursive=FALSE) 
  files2017_Jan <- list.files(path="directory", pattern="2017-01", full.names=TRUE, recursive=FALSE) 
  files2017_Jul <- list.files(path="directory", pattern="2017-07", full.names=TRUE, recursive=FALSE) 
  files2018_Jan <- list.files(path="directory", pattern="2018-01", full.names=TRUE, recursive=FALSE) 
  files2018_Jul <- list.files(path="directory", pattern="2018-07", full.names=TRUE, recursive=FALSE) 
  
  ## 2015 ##
  
  # south 
  
  # Create base data frame
  south2015_large <- read.csv(files2015_south[1])
  south2015_large <- south2015_large[, c("location_id", "x", "y")]
  
  # Loop through files and add to base data frame
  n = 1
  
  for (file in files2015_south) {
    
    acq_pred <- read.csv(file)
    acq_pred$X <- NULL
    colnames(acq_pred) <- c("location_id", "x", "y", paste0("tree",n), paste0("shrub", n), paste0("grassland", n), paste0("crops", n), 
                            paste0("urban_built_up", n), paste0("bare", n), paste0("water", n))
    
    
    south2015_large <- merge(south2015_large, acq_pred, by = c("location_id", "x", "y"), all.y = TRUE)
    n = n + 1
    
  }
  
  # Take the mean of every class
  south2015_tree <- rowMeans(south2015_large[, grep("tree", colnames(south2015_large))])
  south2015_shrub <- rowMeans(south2015_large[, grep("shrub", colnames(south2015_large))])
  south2015_grassland <- rowMeans(south2015_large[, grep("grassland", colnames(south2015_large))])
  south2015_crops <- rowMeans(south2015_large[, grep("crops", colnames(south2015_large))])
  south2015_urban_built_up <- rowMeans(south2015_large[, grep("urban_built_up", colnames(south2015_large))])
  south2015_bare <- rowMeans(south2015_large[, grep("bare", colnames(south2015_large))])
  south2015_water <- rowMeans(south2015_large[, grep("water", colnames(south2015_large))])
  
  # Make new data frame that contains every new class (total adds up to 100) 
  south2015_df <- cbind(south2015_large[, c("location_id", "x", "y")], data.frame(south2015_tree, south2015_shrub, south2015_grassland, south2015_crops, south2015_urban_built_up, 
                                   south2015_bare, south2015_water))
  names(south2015_df) <- c("location_id", "x", "y", "tree", "shrub", "grassland", "crops", "urban_built_up", "bare", "water")
  rm(south2015_large)
  
  # north
  
  # Create base data frame
  north2015_large <- read.csv(files2015_north[1])
  north2015_large <- north2015_large[, c("location_id", "x", "y")]
  
  # Loop through files and add to base data frame
  n = 1
  
  for (file in files2015_north) {
    
    acq_pred <- read.csv(file)
    acq_pred$X <- NULL
    colnames(acq_pred) <- c("location_id", "x", "y", paste0("tree",n), paste0("shrub", n), paste0("grassland", n), paste0("crops", n), 
                            paste0("urban_built_up", n), paste0("bare", n), paste0("water", n))
    
    
    north2015_large <- merge(north2015_large, acq_pred, by = c("location_id", "x", "y"), all.y = TRUE)
    n = n + 1
    
  }
  
  # Take the mean of every class
  north2015_tree <- rowMeans(north2015_large[, grep("tree", colnames(north2015_large))])
  north2015_shrub <- rowMeans(north2015_large[, grep("shrub", colnames(north2015_large))])
  north2015_grassland <- rowMeans(north2015_large[, grep("grassland", colnames(north2015_large))])
  north2015_crops <- rowMeans(north2015_large[, grep("crops", colnames(north2015_large))])
  north2015_urban_built_up <- rowMeans(north2015_large[, grep("urban_built_up", colnames(north2015_large))])
  north2015_bare <- rowMeans(north2015_large[, grep("bare", colnames(north2015_large))])
  north2015_water <- rowMeans(north2015_large[, grep("water", colnames(north2015_large))])
  
  # Make new data frame that contains every new class (total adds up to 100) 
  north2015_df <- cbind(north2015_large[, c("location_id", "x", "y")], data.frame(north2015_tree, north2015_shrub, north2015_grassland, north2015_crops, north2015_urban_built_up, 
                                                                                  north2015_bare, north2015_water))
  names(north2015_df) <- c("location_id", "x", "y", "tree", "shrub", "grassland", "crops", "urban_built_up", "bare", "water")
  rm(north2015_large)
  
  # Take y < 0 for southern hemisphere, y > 0 for northern hemisphere
  south2015_df <- subset(south2015_df, y<0)
  north2015_df <- subset(north2015_df, y>0)
  
  # Row bind north and south data frames into one
  pred2015 <- rbind(south2015_df, north2015_df)
  
  ## 2016 ##
  
  # south 
  
  # Create base data frame
  south2016_large <- read.csv(files2016_south[1])
  south2016_large <- south2016_large[, c("location_id", "x", "y")]
  
  # Loop through files and add to base data frame
  n = 1
  
  for (file in files2016_south) {
    
    acq_pred <- read.csv(file)
    acq_pred$X <- NULL
    colnames(acq_pred) <- c("location_id", "x", "y", paste0("tree",n), paste0("shrub", n), paste0("grassland", n), paste0("crops", n), 
                            paste0("urban_built_up", n), paste0("bare", n), paste0("water", n))
    
    
    south2016_large <- merge(south2016_large, acq_pred, by = c("location_id", "x", "y"), all.y = TRUE)
    n = n + 1
    
  }
  
  # Take the mean of every class
  south2016_tree <- rowMeans(south2016_large[, grep("tree", colnames(south2016_large))])
  south2016_shrub <- rowMeans(south2016_large[, grep("shrub", colnames(south2016_large))])
  south2016_grassland <- rowMeans(south2016_large[, grep("grassland", colnames(south2016_large))])
  south2016_crops <- rowMeans(south2016_large[, grep("crops", colnames(south2016_large))])
  south2016_urban_built_up <- rowMeans(south2016_large[, grep("urban_built_up", colnames(south2016_large))])
  south2016_bare <- rowMeans(south2016_large[, grep("bare", colnames(south2016_large))])
  south2016_water <- rowMeans(south2016_large[, grep("water", colnames(south2016_large))])
  
  # Make new data frame that contains every new class (total adds up to 100) 
  south2016_df <- cbind(south2016_large[, c("location_id", "x", "y")], data.frame(south2016_tree, south2016_shrub, south2016_grassland, south2016_crops, south2016_urban_built_up, 
                                                                                  south2016_bare, south2016_water))
  names(south2016_df) <- c("location_id", "x", "y", "tree", "shrub", "grassland", "crops", "urban_built_up", "bare", "water")
  rm(south2016_large)
  
  # north
  
  # Create base data frame
  north2016_large <- read.csv(files2016_north[1])
  north2016_large <- north2016_large[, c("location_id", "x", "y")]
  
  # Loop through files and add to base data frame
  n = 1
  
  for (file in files2016_north) {
    
    acq_pred <- read.csv(file)
    acq_pred$X <- NULL
    colnames(acq_pred) <- c("location_id", "x", "y", paste0("tree",n), paste0("shrub", n), paste0("grassland", n), paste0("crops", n), 
                            paste0("urban_built_up", n), paste0("bare", n), paste0("water", n))
    
    
    north2016_large <- merge(north2016_large, acq_pred, by = c("location_id", "x", "y"), all.y = TRUE)
    n = n + 1
    
  }
  
  # Take the mean of every class
  north2016_tree <- rowMeans(north2016_large[, grep("tree", colnames(north2016_large))])
  north2016_shrub <- rowMeans(north2016_large[, grep("shrub", colnames(north2016_large))])
  north2016_grassland <- rowMeans(north2016_large[, grep("grassland", colnames(north2016_large))])
  north2016_crops <- rowMeans(north2016_large[, grep("crops", colnames(north2016_large))])
  north2016_urban_built_up <- rowMeans(north2016_large[, grep("urban_built_up", colnames(north2016_large))])
  north2016_bare <- rowMeans(north2016_large[, grep("bare", colnames(north2016_large))])
  north2016_water <- rowMeans(north2016_large[, grep("water", colnames(north2016_large))])
  
  # Make new data frame that contains every new class (total adds up to 100) 
  north2016_df <- cbind(north2016_large[, c("location_id", "x", "y")], data.frame(north2016_tree, north2016_shrub, north2016_grassland, north2016_crops, north2016_urban_built_up, 
                                                                                  north2016_bare, north2016_water))
  names(north2016_df) <- c("location_id", "x", "y", "tree", "shrub", "grassland", "crops", "urban_built_up", "bare", "water")
  rm(north2016_large)
  
  # Take y < 0 for southern hemisphere, y > 0 for northern hemisphere
  south2016_df <- subset(south2016_df, y<0)
  north2016_df <- subset(north2016_df, y>0)
  
  # Row bind north and south data frames into one
  pred2016 <- rbind(south2016_df, north2016_df)
  
  ## 2017 ##
  
  # south 
  
  # Create base data frame
  south2017_large <- read.csv(files2017_south[1])
  south2017_large <- south2017_large[, c("location_id", "x", "y")]
  
  # Loop through files and add to base data frame
  n = 1
  
  for (file in files2017_south) {
    
    acq_pred <- read.csv(file)
    acq_pred$X <- NULL
    colnames(acq_pred) <- c("location_id", "x", "y", paste0("tree",n), paste0("shrub", n), paste0("grassland", n), paste0("crops", n), 
                            paste0("urban_built_up", n), paste0("bare", n), paste0("water", n))
    
    
    south2017_large <- merge(south2017_large, acq_pred, by = c("location_id", "x", "y"), all.y = TRUE)
    n = n + 1
    
  }
  
  # Take the mean of every class
  south2017_tree <- rowMeans(south2017_large[, grep("tree", colnames(south2017_large))])
  south2017_shrub <- rowMeans(south2017_large[, grep("shrub", colnames(south2017_large))])
  south2017_grassland <- rowMeans(south2017_large[, grep("grassland", colnames(south2017_large))])
  south2017_crops <- rowMeans(south2017_large[, grep("crops", colnames(south2017_large))])
  south2017_urban_built_up <- rowMeans(south2017_large[, grep("urban_built_up", colnames(south2017_large))])
  south2017_bare <- rowMeans(south2017_large[, grep("bare", colnames(south2017_large))])
  south2017_water <- rowMeans(south2017_large[, grep("water", colnames(south2017_large))])
  
  # Make new data frame that contains every new class (total adds up to 100) 
  south2017_df <- cbind(south2017_large[, c("location_id", "x", "y")], data.frame(south2017_tree, south2017_shrub, south2017_grassland, south2017_crops, south2017_urban_built_up, 
                                                                                  south2017_bare, south2017_water))
  names(south2017_df) <- c("location_id", "x", "y", "tree", "shrub", "grassland", "crops", "urban_built_up", "bare", "water")
  rm(south2017_large)
  
  # north
  
  # Create base data frame
  north2017_large <- read.csv(files2017_north[1])
  north2017_large <- north2017_large[, c("location_id", "x", "y")]
  
  # Loop through files and add to base data frame
  n = 1
  
  for (file in files2017_north) {
    
    acq_pred <- read.csv(file)
    acq_pred$X <- NULL
    colnames(acq_pred) <- c("location_id", "x", "y", paste0("tree",n), paste0("shrub", n), paste0("grassland", n), paste0("crops", n), 
                            paste0("urban_built_up", n), paste0("bare", n), paste0("water", n))
    
    
    north2017_large <- merge(north2017_large, acq_pred, by = c("location_id", "x", "y"), all.y = TRUE)
    n = n + 1
    
  }
  
  # Take the mean of every class
  north2017_tree <- rowMeans(north2017_large[, grep("tree", colnames(north2017_large))])
  north2017_shrub <- rowMeans(north2017_large[, grep("shrub", colnames(north2017_large))])
  north2017_grassland <- rowMeans(north2017_large[, grep("grassland", colnames(north2017_large))])
  north2017_crops <- rowMeans(north2017_large[, grep("crops", colnames(north2017_large))])
  north2017_urban_built_up <- rowMeans(north2017_large[, grep("urban_built_up", colnames(north2017_large))])
  north2017_bare <- rowMeans(north2017_large[, grep("bare", colnames(north2017_large))])
  north2017_water <- rowMeans(north2017_large[, grep("water", colnames(north2017_large))])
  
  # Make new data frame that contains every new class (total adds up to 100) 
  north2017_df <- cbind(north2017_large[, c("location_id", "x", "y")], data.frame(north2017_tree, north2017_shrub, north2017_grassland, north2017_crops, north2017_urban_built_up, 
                                                                                  north2017_bare, north2017_water))
  names(north2017_df) <- c("location_id", "x", "y", "tree", "shrub", "grassland", "crops", "urban_built_up", "bare", "water")
  rm(north2017_large)
  
  # Take y < 0 for southern hemisphere, y > 0 for northern hemisphere
  south2017_df <- subset(south2017_df, y<0)
  north2017_df <- subset(north2017_df, y>0)
  
  # Row bind north and south data frames into one
  pred2017 <- rbind(south2017_df, north2017_df)
  
  ## 2018 ##
  
  # south 
  
  # Create base data frame
  south2018_large <- read.csv(files2018_south[1])
  south2018_large <- south2018_large[, c("location_id", "x", "y")]
  
  # Loop through files and add to base data frame
  n = 1
  
  for (file in files2018_south) {
    
    acq_pred <- read.csv(file)
    acq_pred$X <- NULL
    colnames(acq_pred) <- c("location_id", "x", "y", paste0("tree",n), paste0("shrub", n), paste0("grassland", n), paste0("crops", n), 
                            paste0("urban_built_up", n), paste0("bare", n), paste0("water", n))
    
    
    south2018_large <- merge(south2018_large, acq_pred, by = c("location_id", "x", "y"), all.y = TRUE)
    n = n + 1
    
  }
  
  # Take the mean of every class
  south2018_tree <- rowMeans(south2018_large[, grep("tree", colnames(south2018_large))])
  south2018_shrub <- rowMeans(south2018_large[, grep("shrub", colnames(south2018_large))])
  south2018_grassland <- rowMeans(south2018_large[, grep("grassland", colnames(south2018_large))])
  south2018_crops <- rowMeans(south2018_large[, grep("crops", colnames(south2018_large))])
  south2018_urban_built_up <- rowMeans(south2018_large[, grep("urban_built_up", colnames(south2018_large))])
  south2018_bare <- rowMeans(south2018_large[, grep("bare", colnames(south2018_large))])
  south2018_water <- rowMeans(south2018_large[, grep("water", colnames(south2018_large))])
  
  # Make new data frame that contains every new class (total adds up to 100) 
  south2018_df <- cbind(south2018_large[, c("location_id", "x", "y")], data.frame(south2018_tree, south2018_shrub, south2018_grassland, south2018_crops, south2018_urban_built_up, 
                                                                                  south2018_bare, south2018_water))
  names(south2018_df) <- c("location_id", "x", "y", "tree", "shrub", "grassland", "crops", "urban_built_up", "bare", "water")
  rm(south2018_large)
  
  # north
  
  # Create base data frame
  north2018_large <- read.csv(files2018_north[1])
  north2018_large <- north2018_large[, c("location_id", "x", "y")]
  
  # Loop through files and add to base data frame
  n = 1
  
  for (file in files2018_north) {
    
    acq_pred <- read.csv(file)
    acq_pred$X <- NULL
    colnames(acq_pred) <- c("location_id", "x", "y", paste0("tree",n), paste0("shrub", n), paste0("grassland", n), paste0("crops", n), 
                            paste0("urban_built_up", n), paste0("bare", n), paste0("water", n))
    
    
    north2018_large <- merge(north2018_large, acq_pred, by = c("location_id", "x", "y"), all.y = TRUE)
    n = n + 1
    
  }
  
  # Take the mean of every class
  north2018_tree <- rowMeans(north2018_large[, grep("tree", colnames(north2018_large))])
  north2018_shrub <- rowMeans(north2018_large[, grep("shrub", colnames(north2018_large))])
  north2018_grassland <- rowMeans(north2018_large[, grep("grassland", colnames(north2018_large))])
  north2018_crops <- rowMeans(north2018_large[, grep("crops", colnames(north2018_large))])
  north2018_urban_built_up <- rowMeans(north2018_large[, grep("urban_built_up", colnames(north2018_large))])
  north2018_bare <- rowMeans(north2018_large[, grep("bare", colnames(north2018_large))])
  north2018_water <- rowMeans(north2018_large[, grep("water", colnames(north2018_large))])
  
  # Make new data frame that contains every new class (total adds up to 100) 
  north2018_df <- cbind(north2018_large[, c("location_id", "x", "y")], data.frame(north2018_tree, north2018_shrub, north2018_grassland, north2018_crops, north2018_urban_built_up, 
                                                                                  north2018_bare, north2018_water))
  names(north2018_df) <- c("location_id", "x", "y", "tree", "shrub", "grassland", "crops", "urban_built_up", "bare", "water")
  rm(north2018_large)
  
  # Take y < 0 for southern hemisphere, y > 0 for northern hemisphere
  south2018_df <- subset(south2018_df, y<0)
  north2018_df <- subset(north2018_df, y>0)
  
  # Row bind north and south data frames into one
  pred2018 <- rbind(south2018_df, north2018_df)
  
  ## Return predictions ##
  
  return(list(pred2015, pred2016, pred2017, pred2018))
  
}

## Apply function ##

predictlist_resp = MergePreds(paste0(linkData, "Output/RF/Dense/"))

## Write predictions to file ##

# Function to write to csv

# index 1 is 2015 and so on

year = 2015

predictlist_to_csv <- function(inputlist, outputdir) {
  for (df in inputlist) {
    write.csv(df, paste0(outputdir, "Agg_DenseRFPredict", year, ".csv"))
    year = year + 1
  }
}

## Write final  predictions to file ##

predictlist_to_csv(predictlist_resp, 
                   paste0(linkData, "Output/RF/"))
