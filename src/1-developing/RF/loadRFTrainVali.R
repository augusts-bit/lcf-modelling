# Access libraries
library(sf)
library(pbapply)
library(probaV)

source("utils/loadData.R")
source("utils/extractDates.R")
source("utils/dataManagement.R")

# Link to data folder
linkData <- "Data/"

# Get column dates
dates = extractDates()
NewColDates = paste0("X", gsub("-", ".", dates))

## IIASA.TRAIN
# Read in b4 and b5 (for ndvi stats)
linkIIASAtrain = linkIIASAtrain = paste0(linkData, "Processed/Training/LSTimeSeries/IIASAtrainingFiltered.gpkg")
nameBands <- st_layers(linkIIASAtrain)
coords = read.csv(paste0(linkData, "Processed/Training/LSTimeSeries/IIASAtrainingCoords.csv"))
coords = coords[,c("x", "y")]
ID = read.csv(paste0(linkData, "Processed/Training/LSTimeSeries/IIASAtrainingCoords.csv"))

b4 <- st_read(linkIIASAtrain, nameBands$name[4])
b5 <- st_read(linkIIASAtrain, nameBands$name[5])
b6 <- st_read(linkIIASAtrain, nameBands$name[6])
b7 <- st_read(linkIIASAtrain, nameBands$name[7])
st_geometry(b4)=NULL
st_geometry(b5)=NULL
st_geometry(b6)=NULL
st_geometry(b7)=NULL

# Convert to numeric
b4 = as.data.frame(sapply(b4[,NewColDates], as.numeric))
b5 = as.data.frame(sapply(b5[,NewColDates], as.numeric))
b6 = as.data.frame(sapply(b6[,NewColDates], as.numeric))
b7 = as.data.frame(sapply(b7[,NewColDates], as.numeric))

# Do on whole time series first
nbr = (b5 - b7) / (b5 + b7)
ndmi = (b5 - b6) / (b5 + b6)
ndvi = (b5 - b4) / (b5 + b4)

nbrmedian = as.numeric(apply(nbr, 1, function(x){median(x, na.rm = TRUE)}))
nbriqr = as.numeric(apply(nbr, 1, function(x){IQR(x, na.rm = TRUE)}))

ndmimedian = as.numeric(apply(ndmi, 1, function(x){median(x, na.rm = TRUE)}))
ndmiiqr = as.numeric(apply(ndmi, 1, function(x){IQR(x, na.rm = TRUE)}))

ndvimedian = as.numeric(apply(ndvi, 1, function(x){median(x, na.rm = TRUE)}))
ndviiqr = as.numeric(apply(ndvi, 1, function(x){IQR(x, na.rm = TRUE)}))

# save median and iqr in one table
VIs2015 = data.frame(ID, coords,
                     nbrmedian, ndmimedian, ndvimedian,
                     nbriqr, ndmiiqr, ndviiqr)
# write median and iqr VIs
VIs2015SF = DFtoSF(VIs2015)
st_write(VIs2015SF, paste0(linkData, "Processed/Training/entire_timeseries_IIASAtrainingVIs.gpkg"), delete_layer = TRUE)


# Split data into 2015, 2016, 2017 and 2018

# 2015
b42015 = b4 
b52015 = b5
b62015 = b6
b72015 = b7

nbr2015 = (b52015 - b72015) / (b52015 + b72015)
plot(as.numeric(apply(nbr2015, 2, function(x){mean(x, na.rm = TRUE)})), ylab="mean nbr")
ndmi2015 = (b52015 - b62015) / (b52015 + b62015)
plot(as.numeric(apply(ndmi2015, 2, function(x){mean(x, na.rm = TRUE)})), ylab="mean ndmi")
ndvi2015 = (b52015 - b42015) / (b52015 + b42015)

nbr2015median = as.numeric(apply(nbr2015, 1, function(x){median(x, na.rm = TRUE)}))
nbr2015iqr = as.numeric(apply(nbr2015, 1, function(x){IQR(x, na.rm = TRUE)}))

ndmi2015median = as.numeric(apply(ndmi2015, 1, function(x){median(x, na.rm = TRUE)}))
ndmi2015iqr = as.numeric(apply(ndmi2015, 1, function(x){IQR(x, na.rm = TRUE)}))

ndvi2015median = as.numeric(apply(ndvi2015, 1, function(x){median(x, na.rm = TRUE)}))
ndvi2015iqr = as.numeric(apply(ndvi2015, 1, function(x){IQR(x, na.rm = TRUE)}))

# save median and iqr in one table
VIs2015 = data.frame(ID, 
                     nbr2015median, ndmi2015median, ndvi2015median,
                     nbr2015iqr, ndmi2015iqr, ndvi2015iqr)
# write median and iqr VIs
VIs2015SF = DFtoSF(VIs2015)
st_write(VIs2015SF, paste0(linkData, "Processed/Training/IIASAtrainingVIs.gpkg"), delete_layer = TRUE)

## WUR change
linkWURchange = paste0(linkData, "Processed/Validation/LSTimeSeries/WURChangeFiltered.gpkg")
nameBands <- st_layers(linkWURchange)

b4 <- st_read(linkWURchange, nameBands$name[4])
b5 <- st_read(linkWURchange, nameBands$name[5])
b6 <- st_read(linkWURchange, nameBands$name[6])
b7 <- st_read(linkWURchange, nameBands$name[7])
st_geometry(b4)=NULL
st_geometry(b5)=NULL
st_geometry(b6)=NULL
st_geometry(b7)=NULL

coordsID = b4[,colnames(b4)[grepl("id|x|y", colnames(b4))]]

# Convert to numeric
b4 = as.data.frame(sapply(b4[,NewColDates], as.numeric))
b5 = as.data.frame(sapply(b5[,NewColDates], as.numeric))
b6 = as.data.frame(sapply(b6[,NewColDates], as.numeric))
b7 = as.data.frame(sapply(b7[,NewColDates], as.numeric))

# Split data into 2015, 2016, 2017 and 2018

# 2015
b42015 = b4[,colnames(b4)[grepl("2014|2015|2016", colnames(b4))]] # For the validation the years are taken
b52015 = b5[,colnames(b5)[grepl("2014|2015|2016", colnames(b5))]]
b62015 = b6[,colnames(b6)[grepl("2014|2015|2016", colnames(b6))]]
b72015 = b7[,colnames(b7)[grepl("2014|2015|2016", colnames(b7))]]

# NBR, NDMI, NDVI
nbr2015 = (b52015 - b72015) / (b52015 + b72015)
ndmi2015 = (b52015 - b62015) / (b52015 + b62015)
ndvi2015 = (b52015 - b42015) / (b52015 + b42015)

nbr2015median = as.numeric(apply(nbr2015, 1, function(x){median(x, na.rm = TRUE)}))
nbr2015iqr = as.numeric(apply(nbr2015, 1, function(x){IQR(x, na.rm = TRUE)}))

ndmi2015median = as.numeric(apply(ndmi2015, 1, function(x){median(x, na.rm = TRUE)}))
ndmi2015iqr = as.numeric(apply(ndmi2015, 1, function(x){IQR(x, na.rm = TRUE)}))

ndvi2015median = as.numeric(apply(ndvi2015, 1, function(x){median(x, na.rm = TRUE)}))
ndvi2015iqr = as.numeric(apply(ndvi2015, 1, function(x){IQR(x, na.rm = TRUE)}))

# save median and iqr in one table
VIs2015 = data.frame(coordsID, 
                     nbr2015median, ndmi2015median, ndvi2015median,
                     nbr2015iqr, ndmi2015iqr, ndvi2015iqr)
# write median and iqr VIs
VIs2015SF = DFtoSF(VIs2015, validation = TRUE, coords = c("sample_x","sample_y"))
st_write(VIs2015SF, paste0(linkData, "Processed/Validation/2015/WURchangeVIs.gpkg"), delete_layer = TRUE)

# 2016
b42016 = b4[,colnames(b4)[grepl("2015|2016|2017", colnames(b4))]]
b52016 = b5[,colnames(b5)[grepl("2015|2016|2017", colnames(b5))]]
b62016 = b6[,colnames(b6)[grepl("2015|2016|2017", colnames(b6))]]
b72016 = b7[,colnames(b7)[grepl("2015|2016|2017", colnames(b7))]]

# NBR, NDMI, NDVI
nbr2016 = (b52016 - b72016) / (b52016 + b72016)
ndmi2016 = (b52016 - b62016) / (b52016 + b62016)
ndvi2016 = (b52016 - b42016) / (b52016 + b42016)

# Median + IQR
nbr2016median = as.numeric(apply(nbr2016, 1, function(x){median(x, na.rm = TRUE)}))
nbr2016iqr = as.numeric(apply(nbr2016, 1, function(x){IQR(x, na.rm = TRUE)}))

ndmi2016median = as.numeric(apply(ndmi2016, 1, function(x){median(x, na.rm = TRUE)}))
ndmi2016iqr = as.numeric(apply(ndmi2016, 1, function(x){IQR(x, na.rm = TRUE)}))

ndvi2016median = as.numeric(apply(ndvi2016, 1, function(x){median(x, na.rm = TRUE)}))
ndvi2016iqr = as.numeric(apply(ndvi2016, 1, function(x){IQR(x, na.rm = TRUE)}))

# Combine in one table
VIs2016 = data.frame(coordsID,
                     nbr2016median, ndmi2016median, ndvi2016median,
                     nbr2016iqr, ndmi2016iqr, ndvi2016iqr)
# write median and iqr VIs
VIs2016SF = DFtoSF(VIs2016, validation = TRUE, coords = c("sample_x","sample_y"))
st_write(VIs2016SF, paste0(linkData, "Processed/Validation/2016/WURchangeVIs.gpkg"), delete_layer = TRUE)

# 2017
b42017 = b4[,colnames(b4)[grepl("2016|2017|2018", colnames(b4))]]
b52017 = b5[,colnames(b5)[grepl("2016|2017|2018", colnames(b5))]]
b62017 = b6[,colnames(b6)[grepl("2016|2017|2018", colnames(b6))]]
b72017 = b7[,colnames(b7)[grepl("2016|2017|2018", colnames(b7))]]

# NBR, NDMI, NDVI
nbr2017 = (b52017 - b72017) / (b52017 + b72017)
ndmi2017 = (b52017 - b62017) / (b52017 + b62017)
ndvi2017 = (b52017 - b42017) / (b52017 + b42017)

# Median + IQR
nbr2017median = as.numeric(apply(nbr2017, 1, function(x){median(x, na.rm = TRUE)}))
nbr2017iqr = as.numeric(apply(nbr2017, 1, function(x){IQR(x, na.rm = TRUE)}))

ndmi2017median = as.numeric(apply(ndmi2017, 1, function(x){median(x, na.rm = TRUE)}))
ndmi2017iqr = as.numeric(apply(ndmi2017, 1, function(x){IQR(x, na.rm = TRUE)}))

ndvi2017median = as.numeric(apply(ndvi2017, 1, function(x){median(x, na.rm = TRUE)}))
ndvi2017iqr = as.numeric(apply(ndvi2017, 1, function(x){IQR(x, na.rm = TRUE)}))

# Combine in one table
VIs2017 = data.frame(coordsID,
                     nbr2017median, ndmi2017median, ndvi2017median,
                     nbr2017iqr, ndmi2017iqr, ndvi2017iqr)
# write median and iqr VIs
VIs2017SF = DFtoSF(VIs2017, validation = TRUE, coords = c("sample_x","sample_y"))
st_write(VIs2017SF, paste0(linkData, "Processed/Validation/2017/WURchangeVIs.gpkg"), delete_layer = TRUE)

# 2018
b42018 = b4[,colnames(b4)[grepl("2017|2018|2019", colnames(b4))]]
b52018 = b5[,colnames(b5)[grepl("2017|2018|2019", colnames(b5))]]
b62018 = b6[,colnames(b6)[grepl("2017|2018|2019", colnames(b6))]]
b72018 = b7[,colnames(b7)[grepl("2017|2018|2019", colnames(b7))]]

# NBR, NDMI, NDVI
nbr2018 = (b52018 - b72018) / (b52018 + b72018)
ndmi2018 = (b52018 - b62018) / (b52018 + b62018)
ndvi2018 = (b52018 - b42018) / (b52018 + b42018)

# Median + IQR
nbr2018median = as.numeric(apply(nbr2018, 1, function(x){median(x, na.rm = TRUE)}))
nbr2018iqr = as.numeric(apply(nbr2018, 1, function(x){IQR(x, na.rm = TRUE)}))

ndmi2018median = as.numeric(apply(ndmi2018, 1, function(x){median(x, na.rm = TRUE)}))
ndmi2018iqr = as.numeric(apply(ndmi2018, 1, function(x){IQR(x, na.rm = TRUE)}))

ndvi2018median = as.numeric(apply(ndvi2018, 1, function(x){median(x, na.rm = TRUE)}))
ndvi2018iqr = as.numeric(apply(ndvi2018, 1, function(x){IQR(x, na.rm = TRUE)}))

# Combine in one table
VIs2018 = data.frame(coordsID,
                     nbr2018median, ndmi2018median, ndvi2018median,
                     nbr2018iqr, ndmi2018iqr, ndvi2018iqr)
# write median and iqr VIs
VIs2018SF = DFtoSF(VIs2018, validation = TRUE, coords = c("sample_x","sample_y"))
st_write(VIs2018SF, paste0(linkData, "Processed/Validation/2018/WURchangeVIs.gpkg"), delete_layer = TRUE)

## Now add Harmonics

# Access to probaV github library (uncomment lines below to get probaV package)
install.packages("devtools")
library(devtools)
options(unzip = "internal")
install_github("JornDallinga/probaV")
library(probaV)

source("utils/harmonicsFunctions.R")

## Training harmonics

InputLink = paste0(linkData, "Processed/Training/LSTimeSeries/IIASAtrainingFiltered.gpkg")

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

# NDVI #

# Try to convert values to numeric
sapply(b5_TS, class)
b4Temp = as.data.frame(sapply(b4_TS[,NewColDates], as.numeric))
b5Temp = as.data.frame(sapply(b5_TS[,NewColDates], as.numeric))
sapply(b5Temp, class)

# Calculate NDVI 
ndvi = (b5Temp - b4Temp) / (b5Temp + b4Temp)

dates = extractDates() # important to run before getHarmonics below
NewColDates = paste0("X", gsub("-", ".", dates))

# st_geometry(ndvi) = NULL
ndvi = ndvi[,NewColDates]

HarmMetrics = t(pbapply(as.matrix(ndvi), 1, getHarmonics))

colnames(HarmMetrics)= c("min", "max", "intercept", "co",
                         "si", "co2", "si2", "trend", "phase1",
                         "amplitude1", "phase2", "amplitude2")

HarmMetrics = as.data.frame(HarmMetrics)

# Merge data
VIS = st_read(paste0(linkData, "Processed/Training/IIASAtrainingVIs.gpkg"))

train2015 <- cbind(VIS, HarmMetrics)

# Now add fractions
filename = paste0(linkData, "raw/Processed_data_2015_100m_20190402_V4_August.xlsx") #use this csv!!!
samplePoints = read_excel(filename, col_names = TRUE)
samplePoints$ï..rowid = NULL #remove duplicate ID column
rm(filename)

# merge fractions based on location_id
train2015 <- merge(train2015,samplePoints, by = c("location_id", "x", "y"))

# Drop NA rows
train2015 <- train2015[!is.na(train2015$trend),] 

# Function to update classes and most dominant class

# classes = loadClassNames()
classes = c("tree", "shrub", "grassland", "crops", "urban_built_up", "bare", "water",
            "burnt", "fallow_shifting_cultivation", "wetland_herbaceous", "lichen_and_moss",
            "snow_and_ice", "not_sure")

updateClasses <- function(df) {
  
  # Comments pertain to training dataset, ignore if other dataset is inputted
  
  # First rename colnames to common colnames
  names(df)[names(df) == "trees"] = "tree"
  names(df)[names(df) == "grass"] = "grassland"
  names(df)[names(df) == "urban"] = "urban_built_up"
  
  # Now calculate most dominant class + add as column
  classFractions = df[,classes[classes %in% names(df)]]
  classFractions$geometry <- NULL
  DominantClasses = apply(classFractions, 1, which.max)
  classes[DominantClasses]
  df$dominant_lc = factor(classes[DominantClasses]) # new column with dominant land cover
  
  # Drop rows dominated by not_sure
  df = df[!df$dominant_lc == "not_sure",]
  
  # Now Reclassify and merge classes
  # ClassMap = c(lichen="grassland", snow="bare", fl.grass="grassland", fl.lichen="grassland")
  ClassMap = c(burnt="grassland", fallow_shifting_cultivation="crops", 
               wetland_herbaceous="grassland", lichen_and_moss="grassland", snow_and_ice="bare")
  
  for (class in 1:length(ClassMap)){
    if (names(ClassMap[class]) %in% names(df))
      df[[ClassMap[class]]] = df[[ClassMap[class]]] + df[[names(ClassMap[class])]]
  }
  
  df$geometry <- NULL
  
  # Now load the new classes
  classes <- loadClassNames()
  
  sum(rowSums(df[, classes]) != 100) # This is the number of rows of which the sum of all classes is not 100 (4,661, correct!)
  sum(rowSums(df[, classes]) == 0) # This is the number of rows of which the sum of all classes is 0 (4,440, correct!)
  
  # Remove rows with Zero fractions
  RelevantClasses = df[, classes]
  ClassSums = rowSums(RelevantClasses)
  ZeroRows = ClassSums == 0
  
  RelevantClasses = RelevantClasses[!ZeroRows,]
  df = df[!ZeroRows,]
  ClassSums = ClassSums[!ZeroRows]
  
  sum(rowSums(df[, classes]) != 100) # 221 that do not total 100, correct!
  sum(rowSums(df[, classes]) == 0) # -> 0 that total 0, correct!
  
  # Rescale classes to 100%
  df[,classes] = RelevantClasses / (ClassSums / 100)
  sum(rowSums(df[, classes]) != 100) # 35, correct!
  sum(round(rowSums(df[, classes])) != 100) # 0, correct!
  
  # Update dominant land cover class
  table(df$dominant_lc)
  classFractions = df[,classes[classes %in% names(df)]]
  DominantClasses = apply(classFractions, 1, which.max)
  #classes[DominantClasses]
  df$dominant_lc = factor(classes[DominantClasses])
  table(df$dominant_lc)
  rm(classFractions)
  
  # Remove old columns
  df$burnt=NULL
  df$fallow_shifting_cultivation=NULL
  df$lichen_and_moss=NULL
  df$snow_and_ice=NULL
  df$wetland_herbaceous=NULL
  df$not_sure=NULL
  
  # Check to see if adds up to 100
  df$total <- df$bare + df$shrub + df$grassland + df$crops + df$urban_built_up + df$water + df$tree
  
  return(df)
  
}

train2015 <- updateClasses(train2015) 

write.csv(train2015, paste0(linkData, "Processed/Training/train2015.csv"))

## Validation
vali2015_VIS = st_read(paste0(linkData, "Processed/Validation/2015/WURchangeVIs.gpkg"))
vali2016_VIS = st_read(paste0(linkData, "Processed/Validation/2016/WURchangeVIs.gpkg"))
vali2017_VIS = st_read(paste0(linkData, "Processed/Validation/2017/WURchangeVIs.gpkg"))
vali2018_VIS = st_read(paste0(linkData, "Processed/Validation/2018/WURchangeVIs.gpkg"))

# Harmonics
InputLink = paste0(linkData, "Processed/Validation/LSTimeSeries/WURChangeFiltered.gpkg")

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

# NDVI #

# Try to convert values to numeric
sapply(b5_TS, class)
b4Temp = as.data.frame(sapply(b4_TS[,NewColDates], as.numeric))
b5Temp = as.data.frame(sapply(b5_TS[,NewColDates], as.numeric))
sapply(b5Temp, class)

# Calculate NDVI 
ndvi = (b5Temp - b4Temp) / (b5Temp + b4Temp)

# 2015
dates = extractDates() # important to run before getHarmonics below
dates = dates[grepl("2014|2015|2016",dates)] # So, now do take years
NewColDates = paste0("X", gsub("-", ".", dates))

# st_geometry(ndvi) = NULL
years_ndvi = ndvi[,NewColDates]

HarmMetrics = t(pbapply(as.matrix(years_ndvi), 1, getHarmonics))
HarmMetricsBackup = HarmMetrics

colnames(HarmMetrics)= c("min", "max", "intercept", "co",
                         "si", "co2", "si2", "trend", "phase1",
                         "amplitude1", "phase2", "amplitude2")

HarmMetrics2015 = as.data.frame(HarmMetrics)

# 2016
dates = extractDates() # important to run before getHarmonics below
dates = dates[grepl("2015|2016|2017",dates)] # So, now do take years
NewColDates = paste0("X", gsub("-", ".", dates))

# st_geometry(ndvi) = NULL
years_ndvi = ndvi[,NewColDates]

HarmMetrics = t(pbapply(as.matrix(years_ndvi), 1, getHarmonics))

colnames(HarmMetrics)= c("min", "max", "intercept", "co",
                         "si", "co2", "si2", "trend", "phase1",
                         "amplitude1", "phase2", "amplitude2")

HarmMetrics2016 = as.data.frame(HarmMetrics)

# 2017
dates = extractDates() # important to run before getHarmonics below
dates = dates[grepl("2016|2017|2018",dates)] # So, now do take years
NewColDates = paste0("X", gsub("-", ".", dates))

# st_geometry(ndvi) = NULL
years_ndvi = ndvi[,NewColDates]

HarmMetrics = t(pbapply(as.matrix(years_ndvi), 1, getHarmonics))

colnames(HarmMetrics)= c("min", "max", "intercept", "co",
                         "si", "co2", "si2", "trend", "phase1",
                         "amplitude1", "phase2", "amplitude2")

HarmMetrics2017 = as.data.frame(HarmMetrics)

# 2018
dates = extractDates() # important to run before getHarmonics below
dates = dates[grepl("2017|2018|2019",dates)] # So, now do take years
NewColDates = paste0("X", gsub("-", ".", dates))

# st_geometry(ndvi) = NULL
years_ndvi = ndvi[,NewColDates]

HarmMetrics = t(pbapply(as.matrix(years_ndvi), 1, getHarmonics))

colnames(HarmMetrics)= c("min", "max", "intercept", "co",
                         "si", "co2", "si2", "trend", "phase1",
                         "amplitude1", "phase2", "amplitude2")

HarmMetrics2018 = as.data.frame(HarmMetrics)

# Bind columns
vali2015 <- cbind(vali2015_VIS, HarmMetrics2015)
vali2016 <- cbind(vali2016_VIS, HarmMetrics2016)
vali2017 <- cbind(vali2017_VIS, HarmMetrics2017)
vali2018 <- cbind(vali2018_VIS, HarmMetrics2018)

# Add fractions
# Now add fractions
filename = paste0(linkData, "raw/reference_global_100m_orig&change_year2015-2019_20210407.xlsx")
samplePoints = read_excel(filename, col_names = TRUE)
samplePoints$ï..rowid = NULL #remove duplicate ID column
rm(filename)

validationRaw2015 = samplePoints[samplePoints$dataYear == "2015",]
validationRaw2016 = samplePoints[samplePoints$dataYear == "2016",]
validationRaw2017 = samplePoints[samplePoints$dataYear == "2017",]
validationRaw2018 = samplePoints[samplePoints$dataYear == "2018",]

# merge data 

vali2015 <- merge(vali2015,validationRaw2015, by = c("location_id"))
vali2016 <- merge(vali2016,validationRaw2016, by = c("location_id"))
vali2017 <- merge(vali2017,validationRaw2017, by = c("location_id"))
vali2018 <- merge(vali2018,validationRaw2018, by = c("location_id"))

# drop rows and add columns compatible with training data #

vali2015 <- vali2015[!is.na(vali2015$trend),]  
vali2016 <- vali2016[!is.na(vali2016$trend),] 
vali2017 <- vali2017[!is.na(vali2017$trend),] 
vali2018 <- vali2018[!is.na(vali2018$trend),]

# Apply (new) updateClasses function

updateClasses_vali <- function(df) {
  
  # Comments pertain to training dataset, ignore if other dataset is inputted
  
  # First rename colnames to common colnames
  names(df)[names(df) == "trees"] = "tree"
  names(df)[names(df) == "grass"] = "grassland"
  names(df)[names(df) == "urban"] = "urban_built_up"
  
  # Now calculate most dominant class + add as column
  classFractions = df[,classes[classes %in% names(df)]]
  classFractions$geometry <- NULL
  DominantClasses = apply(classFractions, 1, which.max)
  classes[DominantClasses]
  df$dominant_lc = factor(classes[DominantClasses]) # new column with dominant land cover
  
  # Drop rows dominated by not_sure
  df = df[!df$dominant_lc == "not_sure",]
  
  # Now Reclassify and merge classes
  ClassMap = c(lichen="grassland", snow="bare", fl.grass="grassland", fl.lichen="grassland")
  #ClassMap = c(burnt="grassland", fallow_shifting_cultivation="crops", 
  #             wetland_herbaceous="grassland", lichen_and_moss="grassland", snow_and_ice="bare")
  
  for (class in 1:length(ClassMap)){
    if (names(ClassMap[class]) %in% names(df))
      df[[ClassMap[class]]] = df[[ClassMap[class]]] + df[[names(ClassMap[class])]]
  }
  
  df$geometry <- NULL
  
  # Now load the new classes
  classes <- loadClassNames()
  
  sum(rowSums(df[, classes]) != 100) # This is the number of rows of which the sum of all classes is not 100 (4,661, correct!)
  sum(rowSums(df[, classes]) == 0) # This is the number of rows of which the sum of all classes is 0 (4,440, correct!)
  
  # Remove rows with Zero fractions
  RelevantClasses = df[, classes]
  ClassSums = rowSums(RelevantClasses)
  ZeroRows = ClassSums == 0
  
  RelevantClasses = RelevantClasses[!ZeroRows,]
  df = df[!ZeroRows,]
  ClassSums = ClassSums[!ZeroRows]
  
  sum(rowSums(df[, classes]) != 100) # 221 that do not total 100, correct!
  sum(rowSums(df[, classes]) == 0) # -> 0 that total 0, correct!
  
  # Rescale classes to 100%
  df[,classes] = RelevantClasses / (ClassSums / 100)
  sum(rowSums(df[, classes]) != 100) # 35, correct!
  sum(round(rowSums(df[, classes])) != 100) # 0, correct!
  
  # Update dominant land cover class
  table(df$dominant_lc)
  classFractions = df[,classes[classes %in% names(df)]]
  DominantClasses = apply(classFractions, 1, which.max)
  #classes[DominantClasses]
  df$dominant_lc = factor(classes[DominantClasses])
  table(df$dominant_lc)
  rm(classFractions)
  
  # Remove old columns
  df$burnt=NULL
  df$fallow_shifting_cultivation=NULL
  df$lichen_and_moss=NULL
  df$snow_and_ice=NULL
  df$wetland_herbaceous=NULL
  df$not_sure=NULL
  df$fl.lichen=NULL
  df$fl.grass=NULL
  
  # Check to see if adds up to 100
  df$total <- df$bare + df$shrub + df$grassland + df$crops + df$urban_built_up + df$water + df$tree
  
  return(df)
  
}

# classes = loadClassNames()
classes = c("tree", "shrub", "grassland", "crops", "urban_built_up", "bare", "water",
            "burnt", "fallow_shifting_cultivation", "wetland_herbaceous", "lichen_and_moss",
            "snow_and_ice", "not_sure")

vali2015 <- updateClasses_vali(vali2015)

# classes = loadClassNames()
classes = c("tree", "shrub", "grassland", "crops", "urban_built_up", "bare", "water",
            "burnt", "fallow_shifting_cultivation", "wetland_herbaceous", "lichen_and_moss",
            "snow_and_ice", "not_sure")
vali2016 <- updateClasses_vali(vali2016)

# classes = loadClassNames()
classes = c("tree", "shrub", "grassland", "crops", "urban_built_up", "bare", "water",
            "burnt", "fallow_shifting_cultivation", "wetland_herbaceous", "lichen_and_moss",
            "snow_and_ice", "not_sure")
vali2017 <- updateClasses_vali(vali2017)

# classes = loadClassNames()
classes = c("tree", "shrub", "grassland", "crops", "urban_built_up", "bare", "water",
            "burnt", "fallow_shifting_cultivation", "wetland_herbaceous", "lichen_and_moss",
            "snow_and_ice", "not_sure")

vali2018 <- updateClasses_vali(vali2018)

# Now vali2015 has 30,574 IDs, vali2016 30,580, vali2017 30,571 and vali2018 30,564 (looks good!)

# Write to csv
write.csv(vali2015, paste0(linkData, "Processed/Validation/vali2015.csv"))
write.csv(vali2016, paste0(linkData, "Processed/Validation/vali2016.csv"))
write.csv(vali2017, paste0(linkData, "Processed/Validation/vali2017.csv"))
write.csv(vali2018, paste0(linkData, "Processed/Validation/vali2018.csv"))

