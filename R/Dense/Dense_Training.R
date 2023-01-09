# Access to probaV github library (uncomment lines below to get probaV package)
install.packages("devtools")
library(devtools)
options(unzip = "internal")
install_github("JornDallinga/probaV")

install.packages("matrixStats")

# Set libraries
library(probaV)
library(sf)
library(pbapply)
library(matrixStats)
library(readxl)
library(stats)

# source utils
source("C:/Users/augus/Documents/Studie/MGI/Thesis/Rob's/lcfMapping-main/lcfMapping-main/utils/dataManagement.R")
source("C:/Users/augus/Documents/Studie/MGI/Thesis/Rob's/lcfMapping-main/lcfMapping-main/utils/extractDates.R")
source("C:/Users/augus/Documents/Studie/MGI/Thesis/Rob's/lcfMapping-main/lcfMapping-main/utils/harmonicsFunctions.R")

## For the dense predictions, we can use the same 2015 training dataset ##

train2015 <- read.csv("C:/Users/augus/Documents/Studie/MGI/Thesis/DataAugust/RobMethod_entireTrain_3yearVali/Training/2015/train2015.csv")

##  Add NDVI/NBR/NDMI of acquisition dates (different for northern and southern hemisphere) ##

InputLink = "C:/Users/augus/Documents/Studie/MGI/Thesis/Rob's/Data/Processed/IIASAtrainingFiltered.gpkg"

# Loop over the GPKG layers to extract the bands timeseries
for (band in c(paste0("b", 1:7))){
  
  # Read in band
  InputSF = st_read(InputLink, band)
  
  if (band != "b2") {
    # For some reason band 2 does not have IDs
    # Only use location_id's that are in the training data
    InputSF = subset(InputSF, location_id %in% train2015$location_id)
  }
  
  # Convert SF to DF
  st_geometry(InputSF) = NULL
  
  # Assign new name
  assign(paste0(band, "_TS"), InputSF)
}

# Get Dates of 2015
dates = extractDates()
dates2015 = dates[grepl("2014|2015|2016",dates)] 
NewColDates = paste0("X", gsub("-", ".", dates2015))

## Calculate Vegetation Indices Training ##

# NDVI #

# Try to convert values to numeric
sapply(b5_TS, class)
b4Temp = as.data.frame(sapply(b4_TS[,NewColDates], as.numeric))
b5Temp = as.data.frame(sapply(b5_TS[,NewColDates], as.numeric))
sapply(b5Temp, class)

# Calculate NDVI 
ndvi_training = (b5Temp - b4Temp) / (b5Temp + b4Temp)

# NDMI #

# Try to convert values to numeric
sapply(b6_TS, class)
b5Temp = as.data.frame(sapply(b5_TS[,NewColDates], as.numeric))
b6Temp = as.data.frame(sapply(b6_TS[,NewColDates], as.numeric))
sapply(b6Temp, class)

# Calculate NDMI
ndmi_training = (b5Temp - b6Temp) / (b5Temp + b6Temp)

# NBR #

# Try to convert values to numeric
sapply(b7_TS, class)
b5Temp = as.data.frame(sapply(b5_TS[,NewColDates], as.numeric))
b7Temp = as.data.frame(sapply(b7_TS[,NewColDates], as.numeric))
sapply(b7Temp, class)

# Calculate NBR
nbr_training = (b5Temp - b7Temp) / (b5Temp + b7Temp)

# Add location ID to VIs (useful for later)
locationID <- b1_TS[, c("location_id")]
ndvi_training <- cbind(ndvi_training, locationID)
ndmi_training <- cbind(ndmi_training, locationID)
nbr_training <- cbind(nbr_training, locationID)

## Split training into North and South subsets ##

trainNorth <- subset(train2015, train2015$y > 0)
trainSouth <- subset(train2015, train2015$y < 0)

ndviNorth <- subset(ndvi_training, locationID %in% trainNorth$location_id)
ndviSouth <- subset(ndvi_training, locationID %in% trainSouth$location_id)

ndmiNorth <- subset(ndmi_training, locationID %in% trainNorth$location_id)
ndmiSouth <- subset(ndmi_training, locationID %in% trainSouth$location_id)

nbrNorth <- subset(nbr_training, locationID %in% trainNorth$location_id)
nbrSouth <- subset(nbr_training, locationID %in% trainSouth$location_id)

# Take full growing season (~4/5 months)
# For north, take mean VIs of acq. dates "2015-04-25" to "2015-10-02"
# For south, take mean VIs of acq. dates "2014-10-15" to "2015-03-24"

# Respective column indices in the *vi*_training data frames:
# 30 to 40
# 18 to 28 

ndviNorth_acqValues <- rowMeans(ndviNorth[,30:40], na.rm = TRUE)
ndviSouth_acqValues <- rowMeans(ndviSouth[,18:28], na.rm = TRUE)

ndmiNorth_acqValues <- rowMeans(ndmiNorth[,30:40], na.rm = TRUE)
ndmiSouth_acqValues <- rowMeans(ndmiSouth[,18:28], na.rm = TRUE)

nbrNorth_acqValues <- rowMeans(nbrNorth[,30:40], na.rm = TRUE)
nbrSouth_acqValues <- rowMeans(nbrSouth[,18:28], na.rm = TRUE)

# Add to data frames
trainNorth$ndvi_growingseason <- ndviNorth_acqValues
trainNorth$ndmi_growingseason <- ndmiNorth_acqValues
trainNorth$nbr_growingseason <- nbrNorth_acqValues

trainSouth$ndvi_growingseason <- ndviSouth_acqValues
trainSouth$ndmi_growingseason <- ndmiSouth_acqValues
trainSouth$nbr_growingseason <- nbrSouth_acqValues

# Rowbind into new train data frame
train2015_inclGS <- rbind(trainNorth,trainSouth)

# Write to file
write.csv(train2015_inclGS, "C:/Users/augus/Documents/Studie/MGI/Thesis/DataAugust/RobMethod_entireTrain_3yearVali/Dense/dense_train2015.csv")
