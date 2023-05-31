# Access libraries
library(devtools)
library(sf)
library(pbapply)
library(probaV)
options(unzip = "internal")
install_github("JornDallinga/probaV")
library(probaV)
library(zoo)
library(data.table)

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

## Train input ##

linkIIASAtrain = paste0(linkData, "Processed/Training/LSTimeSeries/IIASAChangeFiltered.gpkg")
nameBands <- st_layers(linkIIASAtrain)
coords = read.csv(paste0(linkData, "Processed/Training/LSTimeSeries/IIASAtrainingCoords.csv"))
coords = coords[,c("x", "y")]
b1Landsat <- st_read(linkIIASAtrain, nameBands$name[1])
ID = b1Landsat[,c("sample_id")]

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

estimate_missing <- function(row){
  row[is.na(row)] <- mean(row, na.rm = TRUE)
  return(row)
}

estimate_na <- function(band, nr) {
  # Use na.approx (zoo) to estimate NA values
  band_new <- t(apply(band, 1, FUN = zoo::na.approx, na.rm=FALSE))
  band_new <- as.data.frame(band_new)
  # Few NA values remain, use average for them
  band_new <- t(apply(band_new, 1, estimate_missing))
  band_new <- as.data.frame(band_new)
  # Give the same colnames (with unique band name)
  colnames_band <- colnames(band)
  colnames_band_new <- paste0(colnames_band, "_", nr)
  colnames(band_new) <- colnames_band_new
  return(band_new)
}

# Apply
b1 = estimate_na(b1, "1")
b2 = estimate_na(b2, "2")
b3 = estimate_na(b3, "3")
b4 = estimate_na(b4, "4")
b5 = estimate_na(b5, "5")
b6 = estimate_na(b6, "6")
b7 = estimate_na(b7, "7")

# Only use 2015-2018
b1 = b1[,colnames(b1)[grepl("2015|2016|2017|2018", colnames(b1))]]
b2 = b2[,colnames(b2)[grepl("2015|2016|2017|2018", colnames(b2))]]
b3 = b3[,colnames(b3)[grepl("2015|2016|2017|2018", colnames(b3))]]
b4 = b4[,colnames(b4)[grepl("2015|2016|2017|2018", colnames(b4))]]
b5 = b5[,colnames(b5)[grepl("2015|2016|2017|2018", colnames(b5))]]
b6 = b6[,colnames(b6)[grepl("2015|2016|2017|2018", colnames(b6))]]
b7 = b7[,colnames(b7)[grepl("2015|2016|2017|2018", colnames(b7))]]

# Sequence bands (one column with acq. dates as rows)
sequenced_df <- function(df, name) {
  # Convert to matrix and transpose (columns to rows and vice versa)
  matrix_df <- as.matrix(df)
  matrix_df <- t(matrix_df)
  # Convert back to data frame and make as vector (all into one column)
  new_df <- as.data.frame(matrix_df)
  vector_df <- unlist(new_df, use.names=F)
  # Return final data frame with unique name
  sequence_df <- as.data.frame(vector_df)
  colnames(sequence_df) <- name
  return(sequence_df)
}

# Sequence
seq_b1 <- sequenced_df(b1, "b1")
seq_b2 <- sequenced_df(b2, "b2")
seq_b3 <- sequenced_df(b3, "b3")
seq_b4 <- sequenced_df(b4, "b4")
seq_b5 <- sequenced_df(b5, "b5")
seq_b6 <- sequenced_df(b6, "b6")
seq_b7 <- sequenced_df(b7, "b7")

# Every 92 (number of acq. dates in 2015-2018) rows represents a sample
# Make ID, coords and acq. dates sequence columns

# ID
ID$geom <- NULL
seq_ID <- unlist(ID)
seq_ID <- rep(seq_ID, each = 92)
seq_ID <- as.data.frame(seq_ID)
# x
seq_x <- coords$x
seq_x <- unlist(seq_x)
seq_x <- rep(seq_x, each = 92)
seq_x <- as.data.frame(seq_x)
# y
seq_y <- coords$y
seq_y <- unlist(seq_y)
seq_y <- rep(seq_y, each = 92)
seq_y <- as.data.frame(seq_y)
# dates
dates <- extractDates()
seq_dates <- dates[grep("2015|2016|2017|2018", dates)]
seq_dates <- rep(seq_dates, times = 33961) # Now times the amount of samples
seq_dates <- as.data.frame(seq_dates)

# Cbind band all into one big data frame
dense_train <- cbind(seq_ID, seq_x, seq_y, seq_dates, seq_b1, seq_b2, seq_b3, seq_b4, seq_b5, seq_b6, seq_b7)

# Rename columns
colnames(dense_train)[colnames(dense_train) %in% c("seq_ID", "seq_x", "seq_y", "seq_dates")] <- c("sample_id", "x", "y", "date")

# Write to csv 
write.csv(dense_train, paste0(linkData, "Processed/Training/dense_train_input.csv"))

# (+ one without X and Y)
dense_train_noXY <- dense_train
dense_train_noXY$x <- NULL
dense_train_noXY$y <- NULL

# No XY for testing (not necessary)
write.csv(dense_train_noXY, paste0(linkData, "Processed/Training/dense_train_input_noXY.csv"))

## Train targets ##

# Fractions
filename = paste0(linkData, "raw/Data_Global_quoted_New.xlsx")
samplePoints = read_excel(filename, col_names = TRUE)
samplePoints$ï..rowid = NULL #remove duplicate ID column
rm(filename)

# Get fractions for each year
trainingRaw2015 = samplePoints[samplePoints$reference_year == "2015",]
trainingRaw2016 = samplePoints[samplePoints$reference_year == "2016",]
trainingRaw2017 = samplePoints[samplePoints$reference_year == "2017",]
trainingRaw2018 = samplePoints[samplePoints$reference_year == "2018",]

# They all have one row in which all classes are NA, remove it
trainingRaw2015 <- trainingRaw2015[!is.na(trainingRaw2015$tree),]
trainingRaw2016 <- trainingRaw2016[!is.na(trainingRaw2016$tree),]
trainingRaw2017 <- trainingRaw2017[!is.na(trainingRaw2017$tree),]
trainingRaw2018 <- trainingRaw2018[!is.na(trainingRaw2018$tree),]

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
  
  # Reclassify and merge classes
  # ClassMap = c(lichen="grassland", snow="bare", fl.grass="grassland", fl.lichen="grassland")
  ClassMap = c(burnt="grassland", fallow_shifting_cultivation="crops", 
               wetland_herbaceous="grassland", lichen_and_moss="grassland", snow_and_ice="bare")
  
  for (class in 1:length(ClassMap)){
    if (names(ClassMap[class]) %in% names(df))
      df[[ClassMap[class]]] = df[[ClassMap[class]]] + df[[names(ClassMap[class])]]
  }
  
  df$geometry <- NULL
  
  # # Change the names also in the 'dominant_lc' column
  # df$dominant_lc <- ifelse(df$dominant_lc == "burnt", "grassland", df$dominant_lc)
  # df$dominant_lc <- ifelse(df$dominant_lc == "fallow_shifting_cultivationt", "crops", df$dominant_lc)
  # df$dominant_lc <- ifelse(df$dominant_lc == "wetland_herbaceous", "grassland", df$dominant_lc)
  # df$dominant_lc <- ifelse(df$dominant_lc == "lichen_and_moss", "grassland", df$dominant_lc)
  # df$dominant_lc <- ifelse(df$dominant_lc == "snow_and_ice", "bare", df$dominant_lc)
  
  # Check to see if now adds up to 100
  df$total <- df$bare + df$shrub + df$grassland + df$crops + df$urban_built_up + df$water + df$tree
  
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
  
  # # Update dominant land cover class
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
  
  # Check to see if now adds up to 100
  df$total <- df$bare + df$shrub + df$grassland + df$crops + df$urban_built_up + df$water + df$tree
  
  return(df)

}

# Update classes
targets2015 <- updateClasses(trainingRaw2015)
targets2016 <- updateClasses(trainingRaw2016)
targets2017 <- updateClasses(trainingRaw2017)
targets2018 <- updateClasses(trainingRaw2018)

# Make one target data set

# Only use similar sample ids
similar_id_df <- targets2015
similar_id_df <- subset(similar_id_df, sample_id %in% targets2016$sample_id)
similar_id_df <- subset(similar_id_df, sample_id %in% targets2017$sample_id)
similar_id_df <- subset(similar_id_df, sample_id %in% targets2018$sample_id)

targets2015 <- subset(targets2015, sample_id %in% similar_id_df$sample_id)
targets2016 <- subset(targets2016, sample_id %in% similar_id_df$sample_id) 
targets2017 <- subset(targets2017, sample_id %in% similar_id_df$sample_id) 
targets2018 <- subset(targets2018, sample_id %in% similar_id_df$sample_id)

# Row bind and order by sample_id and then reference year
dense_train_targets <- rbind(targets2015, targets2016, targets2017, targets2018)
dense_train_targets <-dense_train_targets[with(dense_train_targets,order(sample_id,reference_year)),]

# Rearrange and drop some columns so it is clear to understand

# Old colnames
# [1] "rowid"          "location_id"    "sample_id"      "x"              "y"              "bare"           "crops"          "grassland"     
# [9] "shrub"          "tree"           "urban_built_up" "water"          "reference_year" "validation_id"  "change_at_300m" "year_fraction" 
# [17] "season"         "dominant_lc"    "lc"             "centroid_x"     "centroid_y"     "continent"      "total"  

targets <- dense_train_targets[, c("sample_id", "location_id", "validation_id", "reference_year", "x", "y", "bare", "crops",
                         "grassland", "shrub", "tree", "urban_built_up", "water",
                         "dominant_lc", "lc", "change_at_300m")]

## Write final dataframe to file ##

write.csv(targets, paste0(linkData, "Processed/Training/dense_train_targets.csv"))

# + one with X and Y

targets_noXY <- targets
targets_noXY$x <- NULL
targets_noXY$y <- NULL

write.csv(targets_noXY, paste0(linkData, "Processed/Training/dense_train_targets_noXY.csv"))

## Since some samples were omitted for the targets, omit the same samples for the inputs and overwrite old input ##

traininputs <- read.csv(paste0(linkData, "Processed/Training/dense_train_input.csv"))

traininputs_new <- subset(traininputs, sample_id %in% targets$sample_id) # now they have the same (and amount of) samples!

# reWrite to csv 
write.csv(traininputs_new, paste0(linkData, "Processed/Training/dense_train_input.csv"))

# (+ one without X and Y)
traininputs_new_noXY <- traininputs_new
traininputs_new_noXY$x <- NULL
traininputs_new_noXY$y <- NULL

write.csv(traininputs_new_noXY, paste0(linkData, "Processed/Training/dense_train_input_noXY.csv"))
