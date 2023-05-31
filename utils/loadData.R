# Load training data

# Access libraries
install.packages("readxl")

library(readxl)
library(sf)

loadClassNames <- function(){
  classes = c("tree", "shrub", "grassland", "crops", "urban_built_up", "bare", "water")
  return(classes)
}

loadNDVIcovarsNames <- function(){
  covars = c("x","y","min","max","intercept","co","si","co2","si2","trend",
             "phase1","amplitude1","phase2","amplitude2")
  return(covars)
}

loadFeaturesNames <- function(){
  covars = c("x","y","min","max","intercept","co","si","co2","si2","trend",
             "phase1","amplitude1","phase2","amplitude2",
             "nbr_median","ndmi_median", "ndvi_median", "nbr_iqr", "ndmi_iqr", "ndvi_iqr")
  return(covars)
}

loadTrainingData <- function(){
  
  # TrainX
  
  # Read in training features from ndvi temporal harmonic features
  harmonics = st_read(paste0(linkData, "Processed/IIASAtrainingHarmonics.gpkg"), "NDVI")
  st_geometry(harmonics)=NULL
  
  # Create a training location ID just as was done for the validation IDs
  
  # Read in b4 and b5 (for ndvi stats)
  linkRawTraining = "raw/IIASATraining2015_Landsat8_TS.gpkg"
  nameBands <- st_layers(linkRawTraining)
  
  b4Training <- st_read(linkRawTraining, nameBands$name[4])
  # b5Training <- st_read(linkRawTraining, nameBands$name[5])
  st_geometry(b4Training)=NULL
  # st_geometry(b5Training)=NULL
  
  tempTraining = subset(b4Training, select = c("location_id", "x", "y"))
  write.csv(tempTraining, paste0(linkData, "Processed/IIASAtrainingLocationID.csv"), row.names=F)
  
  locationID = read.csv("Processed/IIASAtrainingLocationID.csv")
  harmonics = cbind(locationID,harmonics)
  rm(locationID)
  
  # Read in "new features" and combine
  VIs = st_read("Processed/IIASAtrainingVIs.gpkg")
  st_geometry(VIs)=NULL
  # sfeatures = merge(harmonics,VIs)
  features = cbind(harmonics,VIs)
  
  # TrainY
  filename = "raw/training_data_2015_100m_20190402_V4_August.xlsx" #use this csv!!!
  # samplePoints = read.csv2(filename, header = T, sep = ",", dec = ".") # creates num columns
  # samplePoints = read.csv(filename, header = T, sep = ",", dec = ".")
  samplePoints = read_excel(filename, col_names = TRUE)
  samplePoints$ï..rowid = NULL #remove duplicate ID column
  rm(filename)
  
  
  # Remove rows that have NA in some key columns
  # Before = nrow(features)
  # dropRows = apply(features, 1, function(x){any(!is.finite(x))})
  # features = features[!dropRows,]
  # After = nrow(features)
  # print(paste("Dropped NAs, data frame size reduced from", Before, "to", After)) # 150.405 -> 149.775
  
  features_dropped_correct <- features[!is.na(features$trend),] #149.775
  features <- features_dropped_correct
  
  samplePoints = subset(samplePoints, location_id %in% features$location_id)
  all(samplePoints$location_id %in% features$location_id)
  all(features$location_id %in% samplePoints$location_id)
  
  # Update dominant lc first
  table(samplePoints$dominant_lc)
  classes = c("tree", "shrub", "grassland", "crops", "urban_built_up", "bare", "water",
              "burnt", "fallow_shifting_cultivation", "wetland_herbaceous", "lichen_and_moss",
              "snow_and_ice", "not_sure")
  classFractions = samplePoints[,classes[classes %in% names(samplePoints)]]
  DominantClasses = apply(classFractions, 1, which.max)
  classes[DominantClasses]
  samplePoints$dominant_lc = factor(classes[DominantClasses]) # new column with dominant land cover
  sum(classes[DominantClasses] == samplePoints$dominant_lc)
  
  # Drop rows dominated by not_sure
  samplePoints = samplePoints[!samplePoints$dominant_lc == "not_sure",]
  
  # Merge classes
  ClassMap = c(burnt="grassland", fallow_shifting_cultivation="crops", 
               wetland_herbaceous="grassland", lichen_and_moss="grassland", snow_and_ice="bare")
  
  for (class in 1:length(ClassMap)){
    #print(class)
    if (names(ClassMap[class]) %in% names(samplePoints))
      samplePoints[[ClassMap[class]]] = samplePoints[[ClassMap[class]]] + samplePoints[[names(ClassMap[class])]]
  }
  
  classes = c("tree", "shrub", "grassland", "crops", "urban_built_up", "bare", "water")
  
  sum(rowSums(samplePoints[, classes]) != 100) # many rows not valid 5.269 (4.661)
  
  sum(rowSums(samplePoints[, classes]) == 0) # -> 5.048 (4.440) rows with zero fractions
  
  # Remove rows with Zero fractions
  RelevantClasses = samplePoints[, classes]
  ClassSums = rowSums(RelevantClasses)
  ZeroRows = ClassSums == 0
  
  if (any(ZeroRows)){
    print(paste("Dropping", sum(ZeroRows), "samples because all their relevant fractions are zero"))
    RelevantClasses = RelevantClasses[!ZeroRows,]
    samplePoints = samplePoints[!ZeroRows,]
    ClassSums = ClassSums[!ZeroRows]
  }
  sum(rowSums(samplePoints[, classes]) != 100) # still some rows not valid (221)
  sum(rowSums(samplePoints[, classes]) == 0) # -> 0 rows with zero fractions
  
  # Rescale classes to 100%
  samplePoints[,classes] = RelevantClasses / (ClassSums / 100)
  sum(rowSums(samplePoints[, classes]) != 100) # still some rows not valid (only 35?)
  sum(round(rowSums(samplePoints[, classes])) != 100) # now 0 rows with round!
  
  # Update dominant class
  classFractions = samplePoints[,classes[classes %in% names(samplePoints)]]
  DominantClasses = apply(classFractions, 1, which.max)
  classes[DominantClasses]
  samplePoints$dominant_lc = factor(classes[DominantClasses])
  rm(classFractions)
  rm(RelevantClasses)
  
  # Remove old columns
  samplePoints$burnt=NULL
  samplePoints$fallow_shifting_cultivation=NULL
  samplePoints$lichen_and_moss=NULL
  samplePoints$snow_and_ice=NULL
  samplePoints$wetland_herbaceous=NULL
  samplePoints$not_sure=NULL
  
  # done merging land cover classes and rescaling to 100% for train and vali
  # use same points for harmonics (trainY) with subset
  features = subset(features, location_id %in% samplePoints$location_id)
  
  # Merge TrainX and TrainY
  #merge(samplePoints, harmonics) # this merge leads to a loss of 69 points (mistake in round numerical values)
  dataTrain = merge(samplePoints, features, by="location_id") # this does not result in loss of points, but adds new columns for coords
  dataTrain[which(!(dataTrain$y.x == dataTrain$y.y)),c("location_id","x.x","x.y","y.x","y.y")]
  # so new coords are still same, but not recognised as identical (different digits)
  # Change column names and remove duplicate coords columns
  names(dataTrain)[names(dataTrain) == "x.x"] = "x"
  names(dataTrain)[names(dataTrain) == "y.x"] = "y"
  dataTrain$x.y=NULL
  dataTrain$y.y=NULL
  
  names(dataTrain)[grepl("nbr|ndmi|ndvi",names(dataTrain))] = c("nbr_median","ndmi_median", "ndvi_median",
                                                                "nbr_iqr", "ndmi_iqr", "ndvi_iqr")
  
  # print(paste("Training points reduced from", Before, "to", nrow(dataTrain)))
  
  return(dataTrain)
}


loadChangeTrainingData <- function(year = FALSE){
  
  # TrainY
  filename = "raw/Data_Global_quoted.csv"
  samplePoints = read.csv(filename) # creates num columns
  
  # TrainX
  if (year=="2015"){
    harmonics = st_read("Processed/2015/IIASAchangeHarmonics.gpkg", "NDVI")
    VIs = st_read("Processed/2015/IIASAchangeVIs.gpkg")
    samplePoints = samplePoints[samplePoints$reference_year == "2015",]
  }
  else if (year=="2016"){
    harmonics = st_read("Processed/2016/IIASAchangeHarmonics.gpkg", "NDVI")
    VIs = st_read("Processed/2016/IIASAchangeVIs.gpkg")
    samplePoints = samplePoints[samplePoints$reference_year == "2016",]
  }
  else if (year=="2017"){
    harmonics = st_read("Processed/2017/IIASAchangeHarmonics.gpkg", "NDVI")
    VIs = st_read("Processed/2017/IIASAchangeVIs.gpkg")
    samplePoints = samplePoints[samplePoints$reference_year == "2017",]
  }
  else if (year=="2018"){
    harmonics = st_read("Processed/2018/IIASAchangeHarmonics.gpkg", "NDVI")
    VIs = st_read("Processed/2018/IIASAchangeVIs.gpkg")
    samplePoints = samplePoints[samplePoints$reference_year == "2018",]
  }
  else {
    harmonics = st_read(paste0(linkData, "Processed/IIASAchangeHarmonics.gpkg"), "NDVI")
  }
  
  # SF to DF
  st_geometry(harmonics)=NULL
  if (year != FALSE){
    st_geometry(VIs)=NULL
    features = merge(harmonics,VIs)
  }
  else {features = harmonics}
  
  names(features)[names(features) == "centroid_x"] = "x"
  names(features)[names(features) == "centroid_y"] = "y"
  features$sample_id = as.integer(features$sample_id)
  
  # Remove rows that have NA in some key columns. TODO: make function of this
  Before = nrow(features)
  dropRows = apply(features, 1, function(x){any(!is.finite(x))})
  features = features[!dropRows,]
  After = nrow(features)
  print(paste("Dropped NAs, size reduced from", Before, "to", After)) # 150.405 -> 149.775
  samplePoints = subset(samplePoints, sample_id %in% features$sample_id)
  all(samplePoints$sample_id %in% features$sample_id)
  all(features$sample_id %in% samplePoints$sample_id)
  
  # Update dominant lc first
  table(samplePoints$dominant_lc)
  classes = c("tree", "shrub", "grassland", "crops", "urban_built_up", "bare", "water",
              "burnt", "fallow_shifting_cultivation", "wetland_herbaceous", "lichen_and_moss",
              "snow_and_ice", "not_sure")
  classFractions = samplePoints[,classes[classes %in% names(samplePoints)]]
  DominantClasses = apply(classFractions, 1, which.max)
  classes[DominantClasses]
  samplePoints$dominant_lc = factor(classes[DominantClasses]) # new column with dominant land cover
  sum(classes[DominantClasses] == samplePoints$dominant_lc)
  
  # Drop rows dominated by not_sure
  samplePoints = samplePoints[!samplePoints$dominant_lc == "not_sure",]
  
  # Merge classes
  ClassMap = c(burnt="grassland", fallow_shifting_cultivation="crops", 
               wetland_herbaceous="grassland", lichen_and_moss="grassland", snow_and_ice="bare")
  
  for (class in 1:length(ClassMap)){
    #print(class)
    if (names(ClassMap[class]) %in% names(samplePoints))
      samplePoints[[ClassMap[class]]] = samplePoints[[ClassMap[class]]] + samplePoints[[names(ClassMap[class])]]
    #print("done")
  }
  
  classes = c("tree", "shrub", "grassland", "crops", "urban_built_up", "bare", "water")
  
  sum(rowSums(samplePoints[, classes]) != 100) 
  
  sum(rowSums(samplePoints[, classes]) == 0)
  
  # Remove rows with Zero fractions
  RelevantClasses = samplePoints[, classes]
  ClassSums = rowSums(RelevantClasses)
  ZeroRows = ClassSums == 0
  
  if (any(ZeroRows)){
    print(paste("Dropping", sum(ZeroRows), "samples because all their relevant fractions are zero"))
    RelevantClasses = RelevantClasses[!ZeroRows,]
    samplePoints = samplePoints[!ZeroRows,]
    ClassSums = ClassSums[!ZeroRows]
  }
  sum(rowSums(samplePoints[, classes]) != 100) 
  sum(rowSums(samplePoints[, classes]) == 0) 
  
  # Rescale classes to 100%
  samplePoints[,classes] = RelevantClasses / (ClassSums / 100)
  sum(rowSums(samplePoints[, classes]) != 100) 
  sum(round(rowSums(samplePoints[, classes])) != 100) 
  
  # Update dominant class
  classFractions = samplePoints[,classes[classes %in% names(samplePoints)]]
  DominantClasses = apply(classFractions, 1, which.max)
  classes[DominantClasses]
  samplePoints$dominant_lc = factor(classes[DominantClasses])
  rm(classFractions)
  rm(RelevantClasses)
  
  # Remove old columns
  samplePoints$burnt=NULL
  samplePoints$fallow_shifting_cultivation=NULL
  samplePoints$lichen_and_moss=NULL
  samplePoints$snow_and_ice=NULL
  samplePoints$wetland_herbaceous=NULL
  samplePoints$not_sure=NULL
  
  # done merging land cover classes and rescaling to 100% for train and vali
  # use same points for features (trainY) with subset
  features = subset(features, sample_id %in% samplePoints$sample_id)
  
  # Merge TrainX and TrainY
  #merge(samplePoints, harmonics) # this merge leads to a loss of 69 points (mistake in round numerical values)
  dataTrain = merge(samplePoints, features, by="sample_id") # this does not result in loss of points, but adds new columns for coords
  dataTrain[which(!(dataTrain$y.x == dataTrain$y.y)),c("sample_id","x.x","x.y","y.x","y.y")]
  # so new coords are still same, but not recognised as identical (different digits)
  # Change column names and remove duplicate coords columns
  names(dataTrain)[names(dataTrain) == "x.x"] = "x"
  names(dataTrain)[names(dataTrain) == "y.x"] = "y"
  dataTrain$x.y=NULL
  dataTrain$y.y=NULL
  
  #names(dataTrain)[names(dataTrain) == "nbr2015median"] = "nbr_median"
  #names(dataTrain)[names(dataTrain) == "ndmi2015median"] = "ndmi_median"
  #names(dataTrain)[names(dataTrain) == "ndvi2015median"] = "ndvi_median"
  #names(dataTrain)[names(dataTrain) == "nbr2015iqr"] = "nbr_iqr"
  #names(dataTrain)[names(dataTrain) == "ndmi2015iqr"] = "ndmi_iqr"
  #names(dataTrain)[names(dataTrain) == "ndvi2015iqr"] = "ndvi_iqr"
  names(dataTrain)[grepl("nbr|ndmi|ndvi",names(dataTrain))] = c("nbr_median","ndmi_median", "ndvi_median",
                                                                "nbr_iqr", "ndmi_iqr", "ndvi_iqr")
  
  print(paste("Training points reduced from", Before, "to", nrow(dataTrain)))
  
  return(dataTrain)
}


loadChangeValidationData <- function(year=FALSE){
  
  # ValiY: read csv file
  filename = "raw/reference_global_100m_orig&change_year2015-2019_20210407.xlsx"
  # validationRaw = read.csv(filename)
  validationRaw = read_excel(filename, col_names = TRUE)
  rm(filename)
  names(validationRaw)[names(validationRaw) == "subpix_mean_x"] = "x"
  names(validationRaw)[names(validationRaw) == "subpix_mean_y"] = "y"
  validationRaw$ï..=NULL
  
  # ValiX: read GPKG of features
  if (year=="2015"){
    # HarmMetrics = st_read("Processed/2015/WURchangeHarmonics.gpkg", "NDVI")
    HarmMetrics = st_read("Processed/2015/WURvalidationHarmonics.gpkg", "NDVI")
    VIs = st_read("Processed/2015/WURchangeVIs.gpkg")
    validationRaw = validationRaw[validationRaw$dataYear == "2015",]
  }
  else if (year=="2016"){
    HarmMetrics = st_read("Processed/2016/WURchangeHarmonics.gpkg", "NDVI")
    VIs = st_read("Processed/2016/WURchangeVIs.gpkg")
    validationRaw = validationRaw[validationRaw$dataYear == "2016",]
  }
  else if (year=="2017"){
    HarmMetrics = st_read("Processed/2017/WURchangeHarmonics.gpkg", "NDVI")
    VIs = st_read("Processed/2017/WURchangeVIs.gpkg")
    validationRaw = validationRaw[validationRaw$dataYear == "2017",]
  }
  else if (year=="2018"){
    HarmMetrics = st_read("Processed/2018/WURchangeHarmonics.gpkg", "NDVI")
    VIs = st_read("Processed/2018/WURchangeVIs.gpkg")
    validationRaw = validationRaw[validationRaw$dataYear == "2018",]
  }
  else {
    HarmMetrics = st_read(paste0("Processed/WURchangeHarmonics.gpkg"), "NDVI")
  }
  
  #HarmMetrics = st_read(paste0(linkData, "Processed/WURvalidationHarmonics.gpkg"), "NDVI")
  st_geometry(HarmMetrics)=NULL
  if (year != FALSE){
    st_geometry(VIs)=NULL
    # features = merge(HarmMetrics,VIs)
    features = cbind(HarmMetrics,VIs)
  }
  else {features=HarmMetrics}
  
  names(features)[names(features) == "sample_x"] = "x"
  names(features)[names(features) == "sample_y"] = "y"
  # features$location_id = as.integer(features$location_id)
  features$sample_id = as.integer(features$sample_id)
  
  # Remove rows that have NA's in column
  # Before = nrow(features)
  # dropRows = apply(features, 1, function(x){any(!is.finite(x))})
  # sum(apply(features, 1, function(x){any(!is.finite(x))}))
  
  features_dropped_correct <- features[!is.na(features$trend),]
  
  # 2015: 136 rows dropped
  # 2016: 130 rows dropped  
  # 2017: 138 rows dropped
  # 2018: 143 rows dropped
  
  # features = features[!dropRows,]
  features = features_dropped_correct
  # After = nrow(features)
  # print(paste("Dropped NAs, data frame size reduced from", Before, "to", After)) # 21.752 -> 21.705
  # validationRaw = subset(validationRaw, location_id %in% features$location_id)
  validationRaw = subset(validationRaw, sample_id %in% features$sample_id)
  featuressubset = subset(features, sample_id %in% validationRaw$sample_id)
  # all(validationRaw$location_id %in% features$location_id)
  # all(features$location_id %in% validationRaw$location_id)
  all(validationRaw$sample_id %in% features$sample_id)
  all(features$sample_id %in% validationRaw$sample_id)
  
  # First rename colnames to common colnames
  names(validationRaw)[names(validationRaw) == "trees"] = "tree"
  names(validationRaw)[names(validationRaw) == "grass"] = "grassland"
  names(validationRaw)[names(validationRaw) == "urban"] = "urban_built_up"
  
  # Now calculate most dominant class + add as column
  classes = loadClassNames()
  classFractions = validationRaw[,classes[classes %in% names(validationRaw)]]
  DominantClasses = apply(classFractions, 1, which.max)
  classes[DominantClasses]
  validationRaw$dominant_lc = factor(classes[DominantClasses]) # new column with dominant land cover
  
  # Now Reclassify and merge classes
  ClassMap = c(lichen="grassland", snow="bare", fl.grass="grassland", fl.lichen="grassland")
  
  for (class in 1:length(ClassMap)){
    if (names(ClassMap[class]) %in% names(validationRaw))
      validationRaw[[ClassMap[class]]] = validationRaw[[ClassMap[class]]] + validationRaw[[names(ClassMap[class])]]
  }
  
  sum(rowSums(validationRaw[, classes]) != 100)
  sum(rowSums(validationRaw[, classes]) == 0)
  #keep only the 7 land cover classes, and remove other columns
  validationRaw$lichen = NULL
  validationRaw$snow = NULL
  validationRaw$fl.grass = NULL
  validationRaw$fl.lichen = NULL
  
  # Update dominant land cover class
  table(validationRaw$dominant_lc)
  classes = loadClassNames()
  classFractions = validationRaw[,classes[classes %in% names(validationRaw)]]
  DominantClasses = apply(classFractions, 1, which.max)
  #classes[DominantClasses]
  validationRaw$dominant_lc = factor(classes[DominantClasses])
  table(validationRaw$dominant_lc)
  rm(classFractions)
  
  # Merge valiRaw with Features
  # dataVali = merge(validationRaw, features, by="location_id")
  # dataVali = merge(validationRaw, features, by="sample_id")
  dataVali = merge(validationRaw, featuressubset, by="sample_id")
  dataVali[which(!(dataVali$y.x == dataVali$y.y)),c("sample_id","x.x","x.y","y.x","y.y")]
  # seems like the same problem, still same coords but not identical.
  # however, now this is happening to almost every row
  # double-check this with rounding the coords colums -> DONE: still round problem
  # Change column names and remove duplicate coords columns
  names(dataVali)[names(dataVali) == "x.x"] = "x"
  names(dataVali)[names(dataVali) == "y.x"] = "y"
  dataVali$x.y=NULL
  dataVali$y.y=NULL
  
  names(dataVali)[grepl("nbr|ndmi|ndvi",names(dataVali))] = c("nbr_median","ndmi_median", "ndvi_median",
                                                              "nbr_iqr", "ndmi_iqr", "ndvi_iqr")
  
  # print(paste("Training points reduced from", Before, "to", nrow(dataVali)))
  
  return(dataVali)
}



