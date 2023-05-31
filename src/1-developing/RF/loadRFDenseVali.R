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
source("utils/dataManagement.R")
source("utils/extractDates.R")
source("utils/harmonicsFunctions.R")

## Validation ##

# Link to data folder
linkData <- "Data/"

# Link to data 
InputLink = paste0(linkData, "Processed/WURChangeFiltered.gpkg")

# Get Dates
dates = extractDates()
NewColDates = paste0("X", gsub("-", ".", dates))

# Loop over the GPKG layers to extract the bands timeseries
for (band in c(paste0("b", 1:7))){
  
  # Read in band
  InputSF = st_read(InputLink, band)
  
  # Convert SF to DF
  st_geometry(InputSF) = NULL
  #InputSF = InputSF[,NewColDates]
  
  # Assign new name
  assign(paste0(band, "_TS"), InputSF)
}

## Calculate Vegetation Indices Validation ##

# NDVI #

# Try to convert values to numeric
sapply(b5_TS, class)
b4Temp = as.data.frame(sapply(b4_TS[,NewColDates], as.numeric))
b5Temp = as.data.frame(sapply(b5_TS[,NewColDates], as.numeric))
sapply(b5Temp, class)

# Calculate NDVI 
ndvi = (b5Temp - b4Temp) / (b5Temp + b4Temp)

# NDMI #

# Try to convert values to numeric
sapply(b6_TS, class)
b5Temp = as.data.frame(sapply(b5_TS[,NewColDates], as.numeric))
b6Temp = as.data.frame(sapply(b6_TS[,NewColDates], as.numeric))
sapply(b6Temp, class)

# Calculate NDMI
ndmi = (b5Temp - b6Temp) / (b5Temp + b6Temp)

# NBR #

# Try to convert values to numeric
sapply(b7_TS, class)
b5Temp = as.data.frame(sapply(b5_TS[,NewColDates], as.numeric))
b7Temp = as.data.frame(sapply(b7_TS[,NewColDates], as.numeric))
sapply(b7Temp, class)

# Calculate NBR
nbr = (b5Temp - b7Temp) / (b5Temp + b7Temp)

## Harmonics, medians and IQRs ##
# Each validation data is based on 3 adjacent years

dates = extractDates() # important to run before getHarmonics below
dates20132020 = dates[grepl("2013|2014|2015|2016|2017|2018|2019|2020",dates)] 
acqdates = dates[grepl("2015|2016|2017|2018",dates)]

medianWithoutNA<-function(x) {
  median(x[which(!is.na(x))])
}

n = 1
i = 42 # index first acq. date in 2015 (2015-01-03) in the years 2013-2020

# One window = 1.5 years before and 1.5 years after acq. date 
# For the first acq. date (2015-01-03), the window is ~ June/July 2013 - July 2016
# Index of window dates = ~7 & ~77 first window (so +- 35 is one window)

# In case of one year window = 0.5 years before and 0.5 years after
# Index of window dates = ~30 & ~54 first window (so +- 12 is one window)

for (date in acqdates){
  
  dates <- dates20132020[(i-35):(i+35)]
  NewColDates = paste0("X", gsub("-", ".", dates))
  
  # st_geometry(ndvi) = NULL
  ndvi_window = ndvi[,NewColDates]
  ndmi_window = ndmi[,NewColDates]
  nbr_window = nbr[,NewColDates]
  
  HarmMetrics = t(pbapply(as.matrix(ndvi_window), 1, getHarmonics))
  
  colnames(HarmMetrics)= c("min", "max", "intercept", "co",
                           "si", "co2", "si2", "trend", "phase1",
                           "amplitude1", "phase2", "amplitude2")
  
  HarmMetrics = as.data.frame(HarmMetrics)
  
  # IDs
  sampleIDs <- b5_TS[,c("location_id","sample_x","sample_y")]
  valiDF <- sampleIDs
  valiDF <- cbind(valiDF,HarmMetrics)
  
  # Rename x and y
  colnames(valiDF)[colnames(valiDF) == "sample_x"] ="x"
  colnames(valiDF)[colnames(valiDF) == "sample_y"] ="y"
  
  # Get median and IQRs
  valiDF$ndvi_median = apply(ndvi_window[,-1], 1, medianWithoutNA)
  valiDF$ndmi_median = apply(ndmi_window[,-1], 1, medianWithoutNA)
  valiDF$nbr_median = apply(nbr_window[,-1], 1, medianWithoutNA)
  
  ndvimatrix = as.matrix(ndvi_window)
  ndmimatrix = as.matrix(ndmi_window)
  nbrmatrix = as.matrix(nbr_window)
  
  valiDF$ndvi_IQR = rowIQRs(ndvimatrix, na.rm = TRUE)
  valiDF$ndmi_IQR = rowIQRs(ndmimatrix, na.rm = TRUE)
  valiDF$nbr_IQR = rowIQRs(nbrmatrix, na.rm = TRUE)
  
  # Also add NDVI, NDMI and NBR of growing season (or purely acq. date??)
  # Taken of 'growing season' (is not necessarily growing season here)
  # Growing season window is 10 acq. dates, so i-5:i+5
  
  valiDF$ndvi_growingseason <- rowMeans(ndvi[,i-5:i+5], na.rm = TRUE)
  valiDF$ndmi_growingseason <- rowMeans(ndmi[,i-5:i+5], na.rm = TRUE)
  valiDF$nbr_growingseason <- rowMeans(nbr[,i-5:i+5], na.rm = TRUE)
  
  # valiDF$ndvi_acqdate <- ndvi[(n+41)]
  # valiDF$ndmi_acqdate <- ndmi[(n+41)]
  # valiDF$nbr_acqdate <- nbr[(n+41)]
  
  # Remove NA rows
  valiDF <- valiDF[!is.na(valiDF$ndvi_median),]
  valiDF <- valiDF[!is.na(valiDF$ndmi_median),]
  valiDF <- valiDF[!is.na(valiDF$nbr_median),]
  valiDF <- valiDF[!is.na(valiDF$trend),]
  valiDF <- valiDF[!is.na(valiDF$ndvi_growingseason),]
  valiDF <- valiDF[!is.na(valiDF$ndmi_growingseason),]
  valiDF <- valiDF[!is.na(valiDF$nbr_growingseason),]
  
  # Write the final validation dataset to file with unique name (as matrix because dataframe is too large somehow)
  vali <- as.matrix(valiDF)
  write.csv(vali, paste0(paste0(linkData, "Processed/Validation/Dense_Window/vali"),acqdates[n],".csv"))
  
  i = i + 1
  n = n + 1
  
}

