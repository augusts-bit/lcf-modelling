## Packages ##
library(data.table)
library(devtools)
library(sf)
library(pbapply)

# Source utils
source("utils/extractDates.R")

# Link to data folder
linkData <- "Data/"

## Read validation data as made before ##

# Change vali is the same as the vali made before as they are from the WUR change data set 
change_vali2015 <- read.csv(paste0(linkData, "Processed/Validation/vali2015.csv"))
change_vali2016 <- read.csv(paste0(linkData, "Processed/Validation/vali2016.csv"))
change_vali2017 <- read.csv(paste0(linkData, "Processed/Validation/vali2017.csv"))
change_vali2018 <- read.csv(paste0(linkData, "Processed/Validation/vali2018.csv"))

# Shift columns (now it's weird for some reason)
# names(change_vali2015)[1:(ncol(change_vali2015)-1)] = names(change_vali2015)[2:(ncol(change_vali2015))]
# names(change_vali2016)[1:(ncol(change_vali2016)-1)] = names(change_vali2016)[2:(ncol(change_vali2016))]
# names(change_vali2017)[1:(ncol(change_vali2017)-1)] = names(change_vali2017)[2:(ncol(change_vali2017))]
# names(change_vali2018)[1:(ncol(change_vali2018)-1)] = names(change_vali2018)[2:(ncol(change_vali2018))]

# Change column names 
colnames(change_vali2015)[3] <- "x"
colnames(change_vali2015)[4] <- "y"
colnames(change_vali2016)[3] <- "x"
colnames(change_vali2016)[4] <- "y"
colnames(change_vali2017)[3] <- "x"
colnames(change_vali2017)[4] <- "y"
colnames(change_vali2018)[3] <- "x"
colnames(change_vali2018)[4] <- "y"

setnames(change_vali2015, old = c('nbr2015median','ndmi2015median','ndvi2015median', 'nbr2015iqr', 'ndmi2015iqr', 'ndvi2015iqr'), 
         new = c('nbr_median','ndmi_median','ndvi_median', 'nbr_iqr', 'ndmi_iqr', 'ndvi_iqr'))
setnames(change_vali2016, old = c('nbr2016median','ndmi2016median','ndvi2016median', 'nbr2016iqr', 'ndmi2016iqr', 'ndvi2016iqr'), 
         new = c('nbr_median','ndmi_median','ndvi_median', 'nbr_iqr', 'ndmi_iqr', 'ndvi_iqr'))
setnames(change_vali2017, old = c('nbr2017median','ndmi2017median','ndvi2017median', 'nbr2017iqr', 'ndmi2017iqr', 'ndvi2017iqr'), 
         new = c('nbr_median','ndmi_median','ndvi_median', 'nbr_iqr', 'ndmi_iqr', 'ndvi_iqr'))
setnames(change_vali2018, old = c('nbr2018median','ndmi2018median','ndvi2018median', 'nbr2018iqr', 'ndmi2018iqr', 'ndvi2018iqr'), 
         new = c('nbr_median','ndmi_median','ndvi_median', 'nbr_iqr', 'ndmi_iqr', 'ndvi_iqr'))

## This data misses band median data ##

# Link to data folder
linkData <- "C:/Users/augus/Documents/Studie/MGI/Thesis/Rob's/Data/"

# Get column dates
dates = extractDates()
NewColDates = paste0("X", gsub("-", ".", dates))

# Read bands
linkWURchange = paste0(linkData, "Processed/Validation/LSTimeSeries/WURChangeFiltered.gpkg")
nameBands <- st_layers(linkWURchange)

b1Landsat <- st_read(linkWURchange, nameBands$name[1])
coords = b1Landsat[,c("sample_x", "sample_y")]
setnames(coords, old = c('sample_x','sample_y'),
         new = c('x','y'))
ID = b1Landsat[,c("location_id")]

# Validation bands data
b1 <- st_read(linkWURchange, nameBands$name[1])
b2 <- st_read(linkWURchange, nameBands$name[2])
b3 <- st_read(linkWURchange, nameBands$name[3])
b4 <- st_read(linkWURchange, nameBands$name[4])
b5 <- st_read(linkWURchange, nameBands$name[5])
b6 <- st_read(linkWURchange, nameBands$name[6])
b7 <- st_read(linkWURchange, nameBands$name[7])
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

# Function to get the band medians

getMedians <- function(years) {
  
  # Bands 
  years_b1 = b1[,colnames(b1)[grepl(years, colnames(b1))]]
  years_b2 = b2[,colnames(b2)[grepl(years, colnames(b2))]]
  years_b3 = b3[,colnames(b3)[grepl(years, colnames(b3))]]
  years_b4 = b4[,colnames(b4)[grepl(years, colnames(b4))]]
  years_b5 = b5[,colnames(b5)[grepl(years, colnames(b5))]]
  years_b6 = b6[,colnames(b6)[grepl(years, colnames(b6))]]
  years_b7 = b7[,colnames(b7)[grepl(years, colnames(b7))]]
  
  # Medians (and IQRs for VIs)
  b1_median = as.numeric(apply(years_b1, 1, function(x){median(x, na.rm = TRUE)}))
  b2_median = as.numeric(apply(years_b2, 1, function(x){median(x, na.rm = TRUE)}))
  b3_median = as.numeric(apply(years_b3, 1, function(x){median(x, na.rm = TRUE)}))
  b4_median = as.numeric(apply(years_b4, 1, function(x){median(x, na.rm = TRUE)}))
  b5_median = as.numeric(apply(years_b5, 1, function(x){median(x, na.rm = TRUE)}))
  b6_median = as.numeric(apply(years_b6, 1, function(x){median(x, na.rm = TRUE)}))
  b7_median = as.numeric(apply(years_b7, 1, function(x){median(x, na.rm = TRUE)}))
  
  # save median and iqr in one table
  stats = data.frame(ID,coords, b1_median, b2_median, b3_median, b4_median,
                     b5_median, b6_median, b7_median)
  
  return(stats)
}

bandmedians2015 <- getMedians("2014|2015|2016")
bandmedians2016 <- getMedians("2015|2016|2017")
bandmedians2017 <- getMedians("2016|2017|2018")
bandmedians2018 <- getMedians("2017|2018|2019")

# Merge to original validation dfs
change_vali2015 <- merge(change_vali2015, bandmedians2015, by = c("location_id"))
change_vali2016 <- merge(change_vali2016, bandmedians2016, by = c("location_id"))
change_vali2017 <- merge(change_vali2017, bandmedians2017, by = c("location_id"))
change_vali2018 <- merge(change_vali2018, bandmedians2018, by = c("location_id"))

## Make one data set to be used in the LSTM ##

# Only use similar sample ids
similar_id_df <- change_vali2015
similar_id_df <- subset(similar_id_df, sample_id %in% change_vali2016$sample_id)
similar_id_df <- subset(similar_id_df, sample_id %in% change_vali2017$sample_id)
similar_id_df <- subset(similar_id_df, sample_id %in% change_vali2018$sample_id)

change_vali2015 <- subset(change_vali2015, sample_id %in% similar_id_df$sample_id)
change_vali2016 <- subset(change_vali2016, sample_id %in% similar_id_df$sample_id) 
change_vali2017 <- subset(change_vali2017, sample_id %in% similar_id_df$sample_id) 
change_vali2018 <- subset(change_vali2018, sample_id %in% similar_id_df$sample_id)

# Row bind and order by sample_id and then reference year (or actually 'dataYear' cause something is wrong)
change_vali <- rbind(change_vali2015, change_vali2016, change_vali2017, change_vali2018)
change_vali <- change_vali[with(change_vali,order(sample_id,dataYear)),]
# change_vali <- change_vali[with(change_vali,order(sample_id,reference_year)),]

# Something was wrong with original reference_year column. Drop this and rename dataYear to this.
change_vali$reference_year <- NULL
colnames(change_vali)[which(names(change_vali) == "dataYear")] <- "reference_year"

# Rename x and y columns
colnames(change_vali)[which(names(change_vali) == "x.x")] <- "x"
colnames(change_vali)[which(names(change_vali) == "y.x")] <- "y"
# colnames(change_vali)[3] <- "x"
# colnames(change_vali)[4] <- "y"

# Rearrange and drop some columns so it is clear to understand (and match the training data set)

# Old colnames
# [1] "location_id"      "X"                "x"                "y"                "x.x.1"            "y.x.1"            "nbr_median"      
# [8] "ndmi_median"      "ndvi_median"      "nbr_iqr"          "ndmi_iqr"         "ndvi_iqr"         "min"              "max"             
# [15] "intercept"        "co"               "si"               "co2"              "si2"              "trend"            "phase1"          
# [22] "amplitude1"       "phase2"           "amplitude2"       "...1"             "sample_id"        "reference_year"   "subpix_mean_x"   
# [29] "subpix_mean_y"    "validation_id"    "userid"           "email"            "timestamp"        "confidence"       "sample_x.y"      
# [36] "sample_y.y"       "metadata"         "flooded"          "change_Yes"       "change_No"        "group"            "orig_sampleid"   
# [43] "incl.p_com"       "des_weight"       "collection"       "bare"             "crops"            "grassland"        "lichen"          
# [50] "shrub"            "snow"             "tree"             "urban_built_up"   "water"            "class_s50"        "class_s50_plus5" 
# [57] "class_s50_minus5" "chClass"          "chSubPixel"       "dominant_lc"      "total"            "geom"             "x.y"             
# [64] "y.y"              "geom.1"           "b1_median"        "b2_median"        "b3_median"        "b4_median"        "b5_median"       
# [71] "b6_median"        "b7_median"     


change_vali <- change_vali[, c("sample_id", "location_id", "validation_id", "reference_year", "x", "y", "b1_median", "b2_median", "b3_median", 
                         "b4_median", "b5_median", "b6_median", "b7_median", "nbr_median", "ndmi_median", "ndvi_median", "nbr_iqr", 
                         "ndmi_iqr", "ndvi_iqr", "min", "max", "intercept", "co", "si", "co2", "si2", "trend",
                         "phase1", "amplitude1", "phase2", "amplitude2",  "bare", "crops",
                         "grassland", "shrub", "tree", "urban_built_up", "water", "dominant_lc", "change_Yes", "change_No")]

## Write final dataframe to file ##

write.csv(change_vali, paste0(linkData, "Processed/Validation/vali20152018.csv"))




