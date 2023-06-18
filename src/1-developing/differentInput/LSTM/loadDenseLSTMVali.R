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

## Validation input ##

linkWURvali = paste0(linkData, "Processed/Validation/LSTimeSeries/WURChangeFiltered.gpkg")
nameBands <- st_layers(linkWURvali)
b1Landsat <- st_read(linkWURvali, nameBands$name[1])
ID = b1Landsat[,c("location_id")]
coords = b1Landsat[, c("sample_x", "sample_y")]
colnames(coords)[colnames(coords) %in% c("sample_x", "sample_y")] <- c("x", "y")

# Validation bands data
b1 <- st_read(linkWURvali, nameBands$name[1])
b2 <- st_read(linkWURvali, nameBands$name[2])
b3 <- st_read(linkWURvali, nameBands$name[3])
b4 <- st_read(linkWURvali, nameBands$name[4])
b5 <- st_read(linkWURvali, nameBands$name[5])
b6 <- st_read(linkWURvali, nameBands$name[6])
b7 <- st_read(linkWURvali, nameBands$name[7])
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

# VIs
nbr = (b5 - b7) / (b5 + b7)
ndmi = (b5 - b6) / (b5 + b6)
ndvi = (b5 - b4) / (b5 + b4)

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
ndvi_full = estimate_na(ndvi, "ndvi")
ndmi_full = estimate_na(ndmi, "ndmi")
nbr_full = estimate_na(nbr, "nbr")

# b1 = estimate_na(b1, "1")
# b2 = estimate_na(b2, "2")
# b3 = estimate_na(b3, "3")
# b4 = estimate_na(b4, "4")
# b5 = estimate_na(b5, "5")
# b6 = estimate_na(b6, "6")
# b7 = estimate_na(b7, "7")

# Only use 2015-2018
ndvi_full = ndvi_full[,colnames(ndvi_full)[grepl("2015|2016|2017|2018", colnames(ndvi_full))]]
ndmi_full = ndmi_full[,colnames(ndmi_full)[grepl("2015|2016|2017|2018", colnames(ndmi_full))]]
nbr_full = nbr_full[,colnames(nbr_full)[grepl("2015|2016|2017|2018", colnames(nbr_full))]]

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
ndvi_full <- sequenced_df(ndvi_full, "ndvi_full")
ndmi_full <- sequenced_df(ndmi_full, "ndmi_full")
nbr_full <- sequenced_df(nbr_full, "nbr_full")

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
seq_dates <- rep(seq_dates, times = 30712) # Now times the amount of samples (30,712)
seq_dates <- as.data.frame(seq_dates)

# Cbind band all into one big data frame
dense_vali <- cbind(seq_ID, seq_x, seq_y, seq_dates, ndvi_full, ndmi_full, nbr_full)

# Rename columns
colnames(dense_vali)[colnames(dense_vali) %in% c("seq_ID", "seq_x", "seq_y", "seq_dates")] <- c("location_id", "x", "y", "date")

# Write to csv 
write.csv(dense_vali, paste0("Processed/Validation/dense_vali_2015_input.csv"))

## Validation targets ##

# Fractions
filename = paste0("raw/reference_global_100m_orig&change_year2015-2019_20210407.xlsx")
samplePoints = read_excel(filename, col_names = TRUE)
samplePoints$ï..rowid = NULL #remove duplicate ID column
rm(filename)

# Get fractions for each year
validationRaw2015 = samplePoints[samplePoints$dataYear == "2015",] # column 'reference_year' is a bit weird here, so 'dataYear'
validationRaw2016 = samplePoints[samplePoints$dataYear == "2016",]
validationRaw2017 = samplePoints[samplePoints$dataYear == "2017",]
validationRaw2018 = samplePoints[samplePoints$dataYear == "2018",]

# Function to update classes and most dominant class

classes = loadClassNames()

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

# Update classes
classes = c("tree", "shrub", "grassland", "crops", "urban_built_up", "bare", "water",
            "burnt", "fallow_shifting_cultivation", "wetland_herbaceous", "lichen_and_moss",
            "snow_and_ice", "not_sure")
targets2015 <- updateClasses(validationRaw2015)
classes = c("tree", "shrub", "grassland", "crops", "urban_built_up", "bare", "water",
            "burnt", "fallow_shifting_cultivation", "wetland_herbaceous", "lichen_and_moss",
            "snow_and_ice", "not_sure")
targets2016 <- updateClasses(validationRaw2016)
classes = c("tree", "shrub", "grassland", "crops", "urban_built_up", "bare", "water",
            "burnt", "fallow_shifting_cultivation", "wetland_herbaceous", "lichen_and_moss",
            "snow_and_ice", "not_sure")
targets2017 <- updateClasses(validationRaw2017)
classes = c("tree", "shrub", "grassland", "crops", "urban_built_up", "bare", "water",
            "burnt", "fallow_shifting_cultivation", "wetland_herbaceous", "lichen_and_moss",
            "snow_and_ice", "not_sure")
targets2018 <- updateClasses(validationRaw2018)

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
dense_vali_targets <- rbind(targets2015, targets2016, targets2017, targets2018)
dense_vali_targets <-dense_vali_targets[with(dense_vali_targets,order(location_id,reference_year)),]

# Rearrange and drop some columns so it is clear to understand

# Old colnames
# [1] "...1"             "sample_id"        "dataYear"         "subpix_mean_x"    "subpix_mean_y"    "validation_id"    "userid"          
# [8] "email"            "timestamp"        "confidence"       "location_id"      "sample_x"         "sample_y"         "metadata"        
# [15] "reference_year"   "flooded"          "change_Yes"       "change_No"        "group"            "orig_sampleid"    "incl.p_com"      
# [22] "des_weight"       "collection"       "bare"             "crops"            "grassland"        "lichen"           "shrub"           
# [29] "snow"             "tree"             "urban_built_up"   "water"            "class_s50"        "class_s50_plus5"  "class_s50_minus5"
# [36] "chClass"          "chSubPixel"       "dominant_lc"      "total"    

targets <- dense_vali_targets[, c("sample_id", "location_id", "validation_id", "dataYear", "subpix_mean_x", "subpix_mean_y", "bare", "crops",
                         "grassland", "shrub", "tree", "urban_built_up", "water",
                         "dominant_lc")]
colnames(targets)[colnames(targets) %in% c("dataYear", "subpix_mean_x", "subpix_mean_y")] <- c("reference_year", "x", "y")

## Write final dataframe to file ##

write.csv(targets, paste0(linkData, "Processed/Validation/dense_vali_targets_VIseq.csv"))

