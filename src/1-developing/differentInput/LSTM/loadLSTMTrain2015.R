# Access libraries
library(devtools)
library(sf)
library(pbapply)
library(probaV)
options(unzip = "internal")
install_github("JornDallinga/probaV")
library(probaV)

# Source utils
source("utils/harmonicsFunctions.R")
source("utils/loadData.R")
source("utils/extractDates.R")
source("utils/dataManagement.R")

# Link to data folder
linkData <- "Data/"

# Get column dates
dates = extractDates()
NewColDates = paste0("X", gsub("-", ".", dates))

## IIASA.TRAIN
linkIIASAtrain = paste0(linkData, "Processed/Training/LSTimeSeries/IIASAtrainingFiltered.gpkg")
nameBands <- st_layers(linkIIASAtrain)
coords = read.csv(paste0(linkData, "Processed/Training/LSTimeSeries/trainingLocationID2015.csv"))
ID = coords[,c("location_id")]

# Training bands data
b1 <- st_read(linkIIASAtrain, nameBands$name[1])
b2 <- st_read(linkIIASAtrain, nameBands$name[2])
b3 <- st_read(linkIIASAtrain, nameBands$name[3])
b4 <- st_read(linkIIASAtrain, nameBands$name[4])
b5 <- st_read(linkIIASAtrain, nameBands$name[5])
b6 <- st_read(linkIIASAtrain, nameBands$name[6])
b7 <- st_read(linkIIASAtrain, nameBands$name[7])
st_geometry(b1)=NULL
st_geometry(b2)=NULL
st_geometry(b3)=NULL
st_geometry(b4)=NULL
st_geometry(b5)=NULL
st_geometry(b6)=NULL
st_geometry(b7)=NULL

# Convert to numeric
b1 = as.data.frame(sapply(b1[,NewColDates], as.numeric))
b2 = as.data.frame(sapply(b2[,NewColDates], as.numeric))
b3 = as.data.frame(sapply(b3[,NewColDates], as.numeric))
b4 = as.data.frame(sapply(b4[,NewColDates], as.numeric))
b5 = as.data.frame(sapply(b5[,NewColDates], as.numeric))
b6 = as.data.frame(sapply(b6[,NewColDates], as.numeric))
b7 = as.data.frame(sapply(b7[,NewColDates], as.numeric))

## Function to retrieve data for years 2015-2018 ## 

getDataDF <- function(years) {
 
  # Bands 
  years_b1 = b1[,colnames(b1)[grepl(years, colnames(b1))]]
  years_b2 = b2[,colnames(b2)[grepl(years, colnames(b2))]]
  years_b3 = b3[,colnames(b3)[grepl(years, colnames(b3))]]
  years_b4 = b4[,colnames(b4)[grepl(years, colnames(b4))]]
  years_b5 = b5[,colnames(b5)[grepl(years, colnames(b5))]]
  years_b6 = b6[,colnames(b6)[grepl(years, colnames(b6))]]
  years_b7 = b7[,colnames(b7)[grepl(years, colnames(b7))]]
  
  # VIs
  nbr = (years_b5 - years_b7) / (years_b5 + years_b7)
  ndmi = (years_b5 - years_b6) / (years_b5 + years_b6)
  ndvi = (years_b5 - years_b4) / (years_b5 + years_b4)
  
  # Medians (and IQRs for VIs)
  b1_median = as.numeric(apply(years_b1, 1, function(x){median(x, na.rm = TRUE)}))
  b2_median = as.numeric(apply(years_b2, 1, function(x){median(x, na.rm = TRUE)}))
  b3_median = as.numeric(apply(years_b3, 1, function(x){median(x, na.rm = TRUE)}))
  b4_median = as.numeric(apply(years_b4, 1, function(x){median(x, na.rm = TRUE)}))
  b5_median = as.numeric(apply(years_b5, 1, function(x){median(x, na.rm = TRUE)}))
  b6_median = as.numeric(apply(years_b6, 1, function(x){median(x, na.rm = TRUE)}))
  b7_median = as.numeric(apply(years_b7, 1, function(x){median(x, na.rm = TRUE)}))
  
  nbr_median = as.numeric(apply(nbr, 1, function(x){median(x, na.rm = TRUE)}))
  nbr_iqr = as.numeric(apply(nbr, 1, function(x){IQR(x, na.rm = TRUE)}))
  
  ndmi_median = as.numeric(apply(ndmi, 1, function(x){median(x, na.rm = TRUE)}))
  ndmi_iqr = as.numeric(apply(ndmi, 1, function(x){IQR(x, na.rm = TRUE)}))
  
  ndvi_median = as.numeric(apply(ndvi, 1, function(x){median(x, na.rm = TRUE)}))
  ndvi_iqr = as.numeric(apply(ndvi, 1, function(x){IQR(x, na.rm = TRUE)}))
  
  # save median and iqr in one table
  stats = data.frame(ID,coords, b1_median, b2_median, b3_median, b4_median,
                     b5_median, b6_median, b7_median,
                       nbr_median, ndmi_median, ndvi_median,
                       nbr_iqr, ndmi_iqr, ndvi_iqr)

  return(stats)
}

## Harmonics ##

dates = extractDates() # important to run before getHarmonics below
dates = dates[grepl("2014|2015|2016",dates)] # 2015
NewColDates = paste0("X", gsub("-", ".", dates))

# Calculate NDVI
ndvi = (b5 - b4) / (b5 + b4)

ndvi = ndvi[,NewColDates]

HarmMetrics2015 = t(pbapply(as.matrix(ndvi), 1, getHarmonics))

colnames(HarmMetrics2015)= c("min", "max", "intercept", "co",
                         "si", "co2", "si2", "trend", "phase1",
                         "amplitude1", "phase2", "amplitude2")

HarmMetrics2015 = as.data.frame(HarmMetrics2015)

dates = extractDates() # important to run before getHarmonics below
dates = dates[grepl("2015|2016|2017",dates)] # 2016
NewColDates = paste0("X", gsub("-", ".", dates))

# Calculate NDVI
ndvi = (b5 - b4) / (b5 + b4)

ndvi = ndvi[,NewColDates]

HarmMetrics2016 = t(pbapply(as.matrix(ndvi), 1, getHarmonics))

colnames(HarmMetrics2016)= c("min", "max", "intercept", "co",
                             "si", "co2", "si2", "trend", "phase1",
                             "amplitude1", "phase2", "amplitude2")

HarmMetrics2016 = as.data.frame(HarmMetrics2016)

dates = extractDates() # important to run before getHarmonics below
dates = dates[grepl("2016|2017|2018",dates)] # 2017
NewColDates = paste0("X", gsub("-", ".", dates))

# Calculate NDVI
ndvi = (b5 - b4) / (b5 + b4)

ndvi = ndvi[,NewColDates]

HarmMetrics2017 = t(pbapply(as.matrix(ndvi), 1, getHarmonics))

colnames(HarmMetrics2017)= c("min", "max", "intercept", "co",
                             "si", "co2", "si2", "trend", "phase1",
                             "amplitude1", "phase2", "amplitude2")

HarmMetrics2017 = as.data.frame(HarmMetrics2017)

dates = extractDates() # important to run before getHarmonics below
dates = dates[grepl("2017|2018|2019",dates)] # 2018
NewColDates = paste0("X", gsub("-", ".", dates))

# Calculate NDVI
ndvi = (b5 - b4) / (b5 + b4)

ndvi = ndvi[,NewColDates]

HarmMetrics2018 = t(pbapply(as.matrix(ndvi), 1, getHarmonics))

colnames(HarmMetrics2018)= c("min", "max", "intercept", "co",
                             "si", "co2", "si2", "trend", "phase1",
                             "amplitude1", "phase2", "amplitude2")

HarmMetrics2018 = as.data.frame(HarmMetrics2018)

## Bind data ##

# Stats made by the function
train2015 <- getDataDF("2014|2015|2016")
train2016 <- getDataDF("2015|2016|2017")
train2017 <- getDataDF("2016|2017|2018")
train2018 <- getDataDF("2017|2018|2019")

# Column bind harmonics
change_train2015 <- cbind(train2015, HarmMetrics2015)
change_train2016 <- cbind(train2016, HarmMetrics2016)
change_train2017 <- cbind(train2017, HarmMetrics2017)
change_train2018 <- cbind(train2018, HarmMetrics2018)

## Fractions ##
filename = paste0("raw/training_data_2015_100m_20190402_V4_August.xlsx") #use this csv!!!
locationPoints = read_excel(filename, col_names = TRUE)
locationPoints$ï..rowid = NULL #remove duplicate ID column
rm(filename)

# Get fractions for each year (2015 fractions repeated)
trainingRaw2015 = locationPoints
trainingRaw2016 = locationPoints
trainingRaw2017 = locationPoints
trainingRaw2018 = locationPoints

# merge fractions based on location_id
change_train2015 <- merge(change_train2015,trainingRaw2015, by = c("location_id"))
change_train2016 <- merge(change_train2016,trainingRaw2016, by = c("location_id"))
change_train2017 <- merge(change_train2017,trainingRaw2017, by = c("location_id"))
change_train2018 <- merge(change_train2018,trainingRaw2018, by = c("location_id"))

## Drop unimportant columns and NA rows ##

# Clean columns
change_train2015$geom <- NULL
change_train2015$x.y <- NULL
change_train2015$y.y <- NULL

change_train2016$geom <- NULL
change_train2016$x.y <- NULL
change_train2016$y.y <- NULL

change_train2017$geom <- NULL
change_train2017$x.y <- NULL
change_train2017$y.y <- NULL

change_train2018$geom <- NULL
change_train2018$x.y <- NULL
change_train2018$y.y <- NULL

colnames(change_train2015)[3] = "x"
colnames(change_train2015)[4] = "y"
colnames(change_train2016)[3] = "x"
colnames(change_train2016)[4] = "y"
colnames(change_train2017)[3] = "x"
colnames(change_train2017)[4] = "y"
colnames(change_train2018)[3] = "x"
colnames(change_train2018)[4] = "y"

# Drop NA rows

change_train2015 <- change_train2015[!is.na(change_train2015$trend),]
change_train2016 <- change_train2016[!is.na(change_train2016$trend),] 
change_train2017 <- change_train2017[!is.na(change_train2017$trend),] 
change_train2018 <- change_train2018[!is.na(change_train2018$trend),]

# Function to update classes and most dominant class

classes = loadClassNames()

updateClasses <- function(df) {
  
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
  
  sum(rowSums(df[, classes]) != 100) 
  sum(rowSums(df[, classes]) == 0) 
  
  # Remove rows with Zero fractions
  RelevantClasses = df[, classes]
  ClassSums = rowSums(RelevantClasses)
  ZeroRows = ClassSums == 0
  
  RelevantClasses = RelevantClasses[!ZeroRows,]
  df = df[!ZeroRows,]
  ClassSums = ClassSums[!ZeroRows]
  
  sum(rowSums(df[, classes]) != 100) 
  sum(rowSums(df[, classes]) == 0) 
  
  # Rescale classes to 100%
  df[,classes] = RelevantClasses / (ClassSums / 100)
  sum(rowSums(df[, classes]) != 100) 
  sum(round(rowSums(df[, classes])) != 100) 
  
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

# Update classes
change_train2015 <- updateClasses(change_train2015)
change_train2016 <- updateClasses(change_train2016)
change_train2017 <- updateClasses(change_train2017)
change_train2018 <- updateClasses(change_train2018)

## Make one data set to be used in the LSTM ##

# Only use similar location ids
similar_id_df <- change_train2015
similar_id_df <- subset(similar_id_df, location_id %in% change_train2016$location_id)
similar_id_df <- subset(similar_id_df, location_id %in% change_train2017$location_id)
similar_id_df <- subset(similar_id_df, location_id %in% change_train2018$location_id)

change_train2015 <- subset(change_train2015, location_id %in% similar_id_df$location_id)
change_train2016 <- subset(change_train2016, location_id %in% similar_id_df$location_id) 
change_train2017 <- subset(change_train2017, location_id %in% similar_id_df$location_id) 
change_train2018 <- subset(change_train2018, location_id %in% similar_id_df$location_id)

# Row bind and order by location_id and then reference year
change_train <- rbind(change_train2015, change_train2016, change_train2017, change_train2018)
change_train <- change_train[with(change_train,order(location_id,reference_year)),]

# Rearrange and drop some columns so it is clear to understand

# Old colnames
# [1] "location_id"      "x"              "y"              "b1_median"      "b2_median"      "b3_median"      "b4_median"      "b5_median"     
# [9] "b6_median"      "b7_median"      "nbr_median"     "ndmi_median"    "ndvi_median"    "nbr_iqr"        "ndmi_iqr"       "ndvi_iqr"      
# [17] "min"            "max"            "intercept"      "co"             "si"             "co2"            "si2"            "trend"         
# [25] "phase1"         "amplitude1"     "phase2"         "amplitude2"     "rowid"          "location_id"    "bare"           "crops"         
# [33] "grassland"      "shrub"          "tree"           "urban_built_up" "water"          "reference_year" "validation_id"  "change_at_300m"
# [41] "year_fraction"  "season"         "dominant_lc"    "lc"             "centroid_x"     "centroid_y"     "continent"      "total"   

change_train <- change_train[, c("location_id", "location_id", "validation_id", "reference_year", "x", "y", "b1_median", "b2_median", "b3_median", 
                         "b4_median", "b5_median", "b6_median", "b7_median", "nbr_median", "ndmi_median", "ndvi_median", "nbr_iqr", 
                         "ndmi_iqr", "ndvi_iqr", "min", "max", "intercept", "co", "si", "co2", "si2", "trend",
                         "phase1", "amplitude1", "phase2", "amplitude2",  "bare", "crops",
                         "grassland", "shrub", "tree", "urban_built_up", "water",
                         "dominant_lc", "lc", "change_at_300m")]

## Write final dataframe to file ##

write.csv(change_train, paste0(linkData, "Processed/Training/change_train2015.csv"))




