install.packages("data.table")

## Set libraries ##
library(sf)
library(pbapply)
library(ranger)
library(data.table)

## Source utils ##
source("utils/loadData.R")

# Link to data folder
linkData <- "Data/"

## Read training and validation data ##
train2015 = read.csv(paste0(linkData, "Processed/Training/train2015.csv"))
vali2015 = read.csv(paste0(linkData, "Processed/Validation/2015/vali2015.csv"))
vali2016 = read.csv(paste0(linkData, "Processed/Validation/2016/vali2016.csv"))
vali2017 = read.csv(paste0(linkData, "Processed/Validation/2017/vali2017.csv"))
vali2018 = read.csv(paste0(linkData, "Processed/Validation/2018/vali2018.csv"))

# Change column names 
colnames(vali2015)[3] <- "x"
colnames(vali2015)[4] <- "y"
colnames(vali2016)[3] <- "x"
colnames(vali2016)[4] <- "y"
colnames(vali2017)[3] <- "x"
colnames(vali2017)[4] <- "y"
colnames(vali2018)[3] <- "x"
colnames(vali2018)[4] <- "y"

setnames(train2015, old = c('nbr2015median','ndmi2015median','ndvi2015median', 'nbr2015iqr', 'ndmi2015iqr', 'ndvi2015iqr'), 
         new = c('nbr_median','ndmi_median','ndvi_median', 'nbr_iqr', 'ndmi_iqr', 'ndvi_iqr'))
setnames(vali2015, old = c('nbr2015median','ndmi2015median','ndvi2015median', 'nbr2015iqr', 'ndmi2015iqr', 'ndvi2015iqr'), 
         new = c('nbr_median','ndmi_median','ndvi_median', 'nbr_iqr', 'ndmi_iqr', 'ndvi_iqr'))
setnames(vali2016, old = c('nbr2016median','ndmi2016median','ndvi2016median', 'nbr2016iqr', 'ndmi2016iqr', 'ndvi2016iqr'), 
         new = c('nbr_median','ndmi_median','ndvi_median', 'nbr_iqr', 'ndmi_iqr', 'ndvi_iqr'))
setnames(vali2017, old = c('nbr2017median','ndmi2017median','ndvi2017median', 'nbr2017iqr', 'ndmi2017iqr', 'ndvi2017iqr'), 
         new = c('nbr_median','ndmi_median','ndvi_median', 'nbr_iqr', 'ndmi_iqr', 'ndvi_iqr'))
setnames(vali2018, old = c('nbr2018median','ndmi2018median','ndvi2018median', 'nbr2018iqr', 'ndmi2018iqr', 'ndvi2018iqr'), 
         new = c('nbr_median','ndmi_median','ndvi_median', 'nbr_iqr', 'ndmi_iqr', 'ndvi_iqr'))

# If wanted, oversample urban samples
# All urban samples, times 5
# All urban samples where urban is 100%, 5 times
df_urban <- train2015[train2015$urban_built_up > 0, ]
df_urban_100 <- train2015[train2015$urban_built_up == 100, ]
df_urban <- df_urban[rep(seq_len(nrow(df_urban)), each = 5), ]
df_urban_100 <- df_urban_100[rep(seq_len(nrow(df_urban_100)), each = 5), ]

# bind the original data frame with the repeated rows
train2015_oversampled <- rbind(train2015, df_urban)
# train2015_oversampled <- rbind(train2015_oversampled, df_urban_100)


## One could make the following code more efficient. Now, at every iteration the class models are remade. But that of course is not necessary.


## Random Forest. With median voting (important) ##

# Set median voting settings
PredictType="quantiles"
quantreg = ifelse(PredictType=="response", FALSE, TRUE)
PredictQuantiles = 0.5

# Load class names
classes = loadClassNames()

# Create initial formula string
subformula <- "~x+y+min+max+intercept+co+si+co2+si2+trend+phase1+amplitude1+phase2+amplitude2+nbr_median+ndmi_median+ndvi_median+nbr_iqr+ndmi_iqr+ndvi_iqr"

runRandomForest <- function(train, vali, year){
  
  n = 1
  
  # Apply RF for each class
  for (class in classes){
    # print(paste0("Class (", n, "/", length(classes), "): ", class))
    
    # Adjust formula
    formula = paste0(class, subformula)
    
    # Run RF
    rfmodel = ranger(formula, train, seed = 0xbadcafe, quantreg=quantreg)
    
    # Make predictions for every year (2015, 2016, 2017, 2018)
    # Apply RF-model for predictions
    output = predict(rfmodel, vali, type=PredictType, quantiles=PredictQuantiles)$predictions
    
    # Store all classes as string
    assign(paste0("output", n), output)
    
    n = n + 1
    
    
    #if (class=="shrub"){break}
  }

  # Save strings in dataframe
  # And rescale, because total has to be 100
  
  if (year=="2015"){
    total <- output1 + output2 + output3 + output4 + output5 + output6 + output7 
    
    df <- vali2015[, c("sample_id", "location_id", "x", "y")]
    df$tree <- (output1/total) * 100
    df$shrub <- (output2/total) * 100
    df$grassland <- (output3/total) * 100
    df$crops <- (output4/total) * 100
    df$urban_built_up <- (output5/total) * 100
    df$bare <- (output6/total) * 100
    df$water <- (output7/total) * 100
    df$total <- df$tree + df$shrub + df$grassland + df$crops + df$urban_built_up + df$bare + df$water
  }
  
  if (year=="2016"){
    total <- output1 + output2 + output3 + output4 + output5 + output6 + output7 
    
    df <- vali2016[, c("sample_id", "location_id", "x", "y")]
    df$tree <- (output1/total) * 100
    df$shrub <- (output2/total) * 100
    df$grassland <- (output3/total) * 100
    df$crops <- (output4/total) * 100
    df$urban_built_up <- (output5/total) * 100
    df$bare <- (output6/total) * 100
    df$water <- (output7/total) * 100
    df$total <- df$tree + df$shrub + df$grassland + df$crops + df$urban_built_up + df$bare + df$water
  }
  
  if (year=="2017"){
    total <- output1 + output2 + output3 + output4 + output5 + output6 + output7 
    
    df <- vali2017[, c("sample_id", "location_id", "x", "y")]
    df$tree <- (output1/total) * 100
    df$shrub <- (output2/total) * 100
    df$grassland <- (output3/total) * 100
    df$crops <- (output4/total) * 100
    df$urban_built_up <- (output5/total) * 100
    df$bare <- (output6/total) * 100
    df$water <- (output7/total) * 100
    df$total <- df$tree + df$shrub + df$grassland + df$crops + df$urban_built_up + df$bare + df$water
  }
  
  if (year=="2018"){
    total <- output1 + output2 + output3 + output4 + output5 + output6 + output7 
    
    df <- vali2018[, c("sample_id", "location_id", "x", "y")]
    df$tree <- (output1/total) * 100
    df$shrub <- (output2/total) * 100
    df$grassland <- (output3/total) * 100
    df$crops <- (output4/total) * 100
    df$urban_built_up <- (output5/total) * 100
    df$bare <- (output6/total) * 100
    df$water <- (output7/total) * 100
    df$total <- df$tree + df$shrub + df$grassland + df$crops + df$urban_built_up + df$bare + df$water
  }
  
  # Return final df
  return(df)
  
}

# Run random forest and obtain predictions
predict2015 <- runRandomForest(train2015,vali2015, "2015")
predict2016 <- runRandomForest(train2015,vali2016, "2016")
predict2017 <- runRandomForest(train2015,vali2017, "2017")
predict2018 <- runRandomForest(train2015,vali2018, "2018")

# Write predictions to file
write.csv(predict2015, paste0(linkData, "Output/RF/RFpredict2015.csv"))
write.csv(predict2016, paste0(linkData, "Output/RF/RFpredict2016.csv"))
write.csv(predict2017, paste0(linkData, "Output/RF/RFpredict2017.csv"))
write.csv(predict2018, paste0(linkData, "Output/RF/RFpredict2018.csv"))
