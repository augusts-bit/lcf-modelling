library(pbapply)
library(dplyr)
library(philentropy)
library(FNN)

source("utils/SCM.R")
source("utils/errorFunctions.R")
source("utils/loadData.R")
classes = loadClassNames()
  
# Link to data folder
linkData <- "Data/"

## Use aggregated (mean) of dense predictions, since reference data is only yearly

## Important: the following code uses lists of both prediction and validation data frames for each year one

# Therefore, make sure that the multi-year data frames are split by year: e.g.,

multi_year_pred <- read.csv(paste0(linkData, "Processed/Output/LSTM/LSTM_pred.csv"))

pred2015 <- subset(multi_year_pred, reference_year == 2015)
pred2016 <- subset(multi_year_pred, reference_year == 2016)
pred2017 <- subset(multi_year_pred, reference_year == 2017)
pred2018 <- subset(multi_year_pred, reference_year == 2018)

write.csv(pred2015, (paste0(linkData, "Processed/Output/LSTM/predict2015.csv")))
write.csv(pred2016, (paste0(linkData, "Processed/Output/LSTM/predict2016.csv")))
write.csv(pred2017, (paste0(linkData, "Processed/Output/LSTM/predict2017.csv")))
write.csv(pred2018, (paste0(linkData, "Processed/Output/LSTM/predict2018.csv")))

# The same applies for the validation (see subsetVali.R)


####################################################### Define predictions and validation #########################################################

dir <- paste0(linkData, "Processed/Output/LSTM/") 
pattern <- "predict.*\\.csv" # pattern of csv files

predfiles = list.files(dir, 
                       pattern = pattern)  
preds = lapply(paste0(dir,predfiles), read.csv)

lapply(preds, nrow)
preds = lapply(preds, function(x) x[complete.cases(x),])
lapply(preds, nrow)

# Validation
dir_all_vali <- paste0(linkData, "Processed/Validation/All Validation/")
dir_change_vali <- paste0(linkData, "Processed/Validation/Change Validation/")
dir_possible_vali <- paste0(linkData, "Processed/Validation/Possible Change Validation/")

pattern_all_vali <- "vali.*\\.csv"
pattern_change_vali <- "v_change.*\\.csv"
pattern_possible_vali <- "p_change.*\\.csv"

######### Specify which one to assess
dir_vali <- dir_all_vali
pattern_vali <- pattern_all_vali

valfiles = list.files(dir_vali, 
                      pattern = pattern_vali)
vals = lapply(paste0(dir_vali, valfiles), read.csv)
lapply(vals, nrow) 

classnames = loadClassNames()

########################################################## Harmonise preds and vals ###############################################################

harmonise = function(i, df1, df2)
{
  classids = intersect(df1[[i]][["location_id"]], df2[[i]][["location_id"]])
  h1 = df1[[i]][df1[[i]][["location_id"]] %in% classids,]
  h1[order(h1[["location_id"]]),]
}

harmonised_preds = lapply(1:length(preds), harmonise, df1=preds, df2=vals)
harmonised_vals = lapply(1:length(vals), harmonise, df1=vals, df2=preds)

sapply(harmonised_preds, nrow) == sapply(harmonised_vals, nrow)
sapply(1:length(harmonised_vals),
    # function(i) all(harmonised_vals[[i]][["sample_id"]] == harmonised_preds[[i]][["sample_id"]])
    function(i) all(harmonised_vals[[i]][["location_id"]] == harmonised_preds[[i]][["location_id"]])
)
# We are consistent


########################################################### Get OA, RMSE and MAE #################################################################

# Get OAs
pbsapply(1:length(harmonised_vals),
    function(i) SCM(harmonised_preds[[i]][classnames]/100,
                    harmonised_vals[[i]][classnames]/100,
                    totals=TRUE, plot=FALSE)$P_overall_accuracy,
    cl=length(harmonised_vals)
)

# Uncertainty
pbsapply(1:length(harmonised_vals),
         function(i) SCM(harmonised_preds[[i]][classnames]/100,
                         harmonised_vals[[i]][classnames]/100,
                         totals=TRUE, plot=FALSE)$U_overall_accuracy,
         cl=length(harmonised_vals)
)


# Get year average matrix
years = c("2015", "2016", "2017", "2018")
OAdf <- data.frame(matrix(ncol = 7, nrow = 4))
colnames(OAdf) <- strsplit(classnames, ",")
rownames(OAdf) <- strsplit(years, ",")

OAdf_list <- list()
n = 1

for (year in years) {
  year_OA = as.data.frame(SCM(harmonised_preds[[n]][classnames]/100,
               harmonised_vals[[n]][classnames]/100,
               totals=TRUE, plot=FALSE)$P)

  OAdf_list[[year]] <- year_OA

  n = n + 1
}

avg_OAdf <- Reduce("+", OAdf_list)/length(OAdf_list)

## Get RMSE and MAE
RMSEdf <- getRMSE(harmonised_preds, harmonised_vals)
MAEdf <- getMAE(harmonised_preds, harmonised_vals)

# Get relative RMSE and MAE
combine_vals <- function(df_list) {
  common_cols <- Reduce(intersect, lapply(df_list, names))
  do.call(rbind, lapply(df_list, `[`, common_cols))
}

# Apply the function to the list of data frames
harmvals_all <- combine_vals(harmonised_vals)
harmvals_avg <- t(as.data.frame(colMeans(harmvals_all[classes])))

rRMSEdf <- RMSEdf[5,][classes] / harmvals_avg
rMAEdf <- MAEdf[5,][classes] / harmvals_avg




########################################################### Calculate dissimilarity and slope ##############################################

common_ids <- intersect(intersect(harmonised_preds[[1]]$location_id, harmonised_preds[[2]]$location_id), intersect(harmonised_preds[[3]]$location_id, harmonised_preds[[4]]$location_id))
preds_filtered_2015 <- harmonised_preds[[1]][harmonised_preds[[1]]$location_id %in% common_ids, ]
preds_filtered_2016 <- harmonised_preds[[2]][harmonised_preds[[2]]$location_id %in% common_ids, ]
preds_filtered_2017 <- harmonised_preds[[3]][harmonised_preds[[3]]$location_id %in% common_ids, ]
preds_filtered_2018 <- harmonised_preds[[4]][harmonised_preds[[4]]$location_id %in% common_ids, ]
filtered_preds <- list(preds_filtered_2015, preds_filtered_2016, preds_filtered_2017, preds_filtered_2018)

vals_filtered_2015 <- harmonised_vals[[1]][harmonised_vals[[1]]$location_id %in% common_ids, ]
vals_filtered_2016 <- harmonised_vals[[2]][harmonised_vals[[2]]$location_id %in% common_ids, ]
vals_filtered_2017 <- harmonised_vals[[3]][harmonised_vals[[3]]$location_id %in% common_ids, ]
vals_filtered_2018 <- harmonised_vals[[4]][harmonised_vals[[4]]$location_id %in% common_ids, ]
filtered_vals <- list(vals_filtered_2015, vals_filtered_2016, vals_filtered_2017, vals_filtered_2018)

# Calculate dissimilarity and slope per time series
CalcRelativeDiss <- function(preds_ls, vals_ls) {
  
  ########## Relative class accuracies ###########
  
  # Calculate dissimilarity for fractions with change relatively
  
  diss_nochange_ls <- list()
  diss_change_ls <- list()
  
  MAE_nochange_ls <- list()
  MAE_change_ls <- list()
  
  vals_diss_nochange_ls <- list()
  vals_diss_change_ls <- list()
  
  slope_nochange_ls <- list()
  slope_change_ls <- list()
  
  MAE_slope_nochange_ls <- list()
  MAE_slope_change_ls <- list()
  
  vals_slope_nochange_ls <- list()
  vals_slope_change_ls <- list()
  
  for(class in classes) {
    
    ## Omit samples where class is not in validation or predicted
    present <- as.data.frame(ifelse(vals_ls[[1]][[class]] == 0 
                                    & vals_ls[[2]][[class]] == 0
                                    & vals_ls[[3]][[class]] == 0
                                    & vals_ls[[4]][[class]] == 0
                                         & preds_ls[[1]][[class]] < 1
                                         & preds_ls[[2]][[class]] < 1 
                                         & preds_ls[[3]][[class]] < 1
                                         & preds_ls[[4]][[class]] < 1, 0, 1))
    
    # Change in validation
    abs_diff <- abs(vals_ls[[1]][[class]]-vals_ls[[2]][[class]]) + abs(vals_ls[[2]][[class]]-vals_ls[[3]][[class]]) + abs(vals_ls[[3]][[class]]-vals_ls[[4]][[class]]) 
    abs_diff <- as.data.frame(abs_diff)
    
    # Determine change by true validation
    dir_vali <- dir_all_vali
    pattern_vali <- pattern_all_vali
    
    valfiles = list.files(dir_vali, 
                          pattern = pattern_vali)
    vals = lapply(paste0(dir_vali, valfiles), read.csv)
    lapply(vals, nrow) 
    
    classnames = loadClassNames()
    
    # Harmonises df1 to df2 based on location id
    harmonise = function(i, df1, df2)
    {
      classids = intersect(df1[[i]][["location_id"]], df2[[i]][["location_id"]])
      h1 = df1[[i]][df1[[i]][["location_id"]] %in% classids,]
      h1[order(h1[["location_id"]]),]
    }
    
    harmonised_preds = lapply(1:length(preds), harmonise, df1=preds, df2=vals)
    harmonised_vals = lapply(1:length(vals), harmonise, df1=vals, df2=preds)
    
    sapply(harmonised_preds, nrow) == sapply(harmonised_vals, nrow)
    sapply(1:length(harmonised_vals),
           # function(i) all(harmonised_vals[[i]][["sample_id"]] == harmonised_preds[[i]][["sample_id"]])
           function(i) all(harmonised_vals[[i]][["location_id"]] == harmonised_preds[[i]][["location_id"]])
    )
    
    vals_filtered_2015 <- harmonised_vals[[1]][harmonised_vals[[1]]$location_id %in% common_ids, ]
    vals_filtered_2016 <- harmonised_vals[[2]][harmonised_vals[[2]]$location_id %in% common_ids, ]
    vals_filtered_2017 <- harmonised_vals[[3]][harmonised_vals[[3]]$location_id %in% common_ids, ]
    vals_filtered_2018 <- harmonised_vals[[4]][harmonised_vals[[4]]$location_id %in% common_ids, ]
    filtered_vals <- list(vals_filtered_2015, vals_filtered_2016, vals_filtered_2017, vals_filtered_2018)
    
    abs_diff <- abs(filtered_vals[[1]][[class]]-filtered_vals[[2]][[class]]) + abs(filtered_vals[[2]][[class]]-filtered_vals[[3]][[class]]) + abs(filtered_vals[[3]][[class]]-filtered_vals[[4]][[class]]) 
    abs_diff <- as.data.frame(abs_diff)
    
    
    # Change indices
    change_yes <- as.data.frame(ifelse(apply(abs_diff, 1, max) > 0, 1, 0))
    
    # Retain no-change and change as lists
    vals_nochange_ls <- list()
    preds_nochange_ls <- list()
    vals_change_ls <- list()
    preds_change_ls <- list()
    
    for(i in 1:4) {
      preds_df <- preds_ls[[i]]
      preds_df$present <- present
      preds_df$change <- change_yes
      
      vals_df <- vals_ls[[i]]
      vals_df$present <- present
      vals_df$change <- change_yes
      
      # subset where present and no change, and present and change
      preds_nochange <- subset(preds_df, change_yes == 0) # & present == 1
      vals_nochange <- subset(vals_df,  change_yes == 0)
      preds_change <- subset(preds_df, change_yes == 1) 
      vals_change <- subset(vals_df, change_yes == 1)
      
      # Append to lists
      vals_nochange_ls <- append(vals_nochange_ls, list(vals_nochange))
      preds_nochange_ls <- append(preds_nochange_ls, list(preds_nochange))
      vals_change_ls <- append(vals_change_ls, list(vals_change))
      preds_change_ls <- append(preds_change_ls, list(preds_change))
    }
    
    # Difference between time series fractions
    calcDiss <- function(preds_ls, vals_ls) {
      
      # Predicted time step dissimilarity
      pt_diss1 <- abs(preds_ls[[1]][classes] - preds_ls[[2]][classes])
      pt_diss2 <- abs(preds_ls[[2]][classes] - preds_ls[[3]][classes])
      pt_diss3 <- abs(preds_ls[[3]][classes] - preds_ls[[4]][classes])
      pt_diss <- (pt_diss1 + pt_diss2 + pt_diss3) / 3 # Average dissimilarity per time step
      pt_diss <- as.data.frame(pt_diss[[class]])

      # MAE
      MAE1 <- abs((preds_ls[[1]][classes] - preds_ls[[2]][classes]) - (vals_ls[[1]][classes] - vals_ls[[2]][classes]))
      MAE2 <- abs((preds_ls[[2]][classes] - preds_ls[[3]][classes]) - (vals_ls[[2]][classes] - vals_ls[[3]][classes]))
      MAE3 <- abs((preds_ls[[3]][classes] - preds_ls[[4]][classes]) - (vals_ls[[3]][classes] - vals_ls[[4]][classes]))
      MAE_diss <- (MAE1 + MAE2 + MAE3) / 3
      MAE_diss <- as.data.frame(MAE_diss[[class]])
      
      MAE1 <- abs((preds_ls[[1]][classes] - vals_ls[[1]][classes]))
      MAE2 <- abs((preds_ls[[2]][classes] - vals_ls[[2]][classes]))
      MAE3 <- abs((preds_ls[[3]][classes] - vals_ls[[3]][classes]))
      MAE4 <- abs((preds_ls[[4]][classes] - vals_ls[[4]][classes]))
      MAE_diss <- (MAE1 + MAE2 + MAE3+MAE4) / 4
      MAE_diss <- as.data.frame(MAE_diss[[class]])
      
      # Vals time step dissimilarity
      vals_diss1 <- abs(vals_ls[[1]][classes] - vals_ls[[2]][classes])
      vals_diss2 <- abs(vals_ls[[2]][classes] - vals_ls[[3]][classes])
      vals_diss3 <- abs(vals_ls[[3]][classes] - vals_ls[[4]][classes])
      vals_diss <- (vals_diss1 + vals_diss2 + vals_diss3) / 3 # Average dissimilarity per time step
      vals_diss <- as.data.frame(vals_diss[[class]])
      
      return(list(pt_diss, MAE_diss, vals_diss))
    }
    
    # Slope
    calcSlope <- function(preds_ls, vals_ls) {
      
      # Slope (source: https://classroom.synonym.com/calculate-trendline-2709.html)
      a <- 4 * (1*preds_ls[[1]][[class]]+2*preds_ls[[2]][[class]]+3*preds_ls[[3]][[class]]+4*preds_ls[[4]][[class]])
      b <- (1+2+3+4) * (preds_ls[[1]][[class]]+preds_ls[[2]][[class]]+preds_ls[[3]][[class]]+preds_ls[[4]][[class]])
      c <- 4 * ((1**2)+(2**2)+(3**2)+(4**2))
      d <- (1+2+3+4)**2
      preds_slopes <- as.data.frame((a-b) / (c-d))
      preds_slope <- abs(preds_slopes) # Slope over entire sample time series
      
      a <- 4 * (1*vals_ls[[1]][[class]]+2*vals_ls[[2]][[class]]+3*vals_ls[[3]][[class]]+4*vals_ls[[4]][[class]])
      b <- (1+2+3+4) * (vals_ls[[1]][[class]]+vals_ls[[2]][[class]]+vals_ls[[3]][[class]]+vals_ls[[4]][[class]])
      c <- 4 * ((1**2)+(2**2)+(3**2)+(4**2))
      d <- (1+2+3+4)**2
      vals_slopes <- as.data.frame((a-b) / (c-d)) # Slope over entire sample time series
      vals_slope <- abs(vals_slopes)
      
      MAE <- abs((preds_slopes) - (vals_slopes))
       
      return(list(preds_slope, MAE, vals_slope))
    }
    
    # Get dissimilarity
    diss_nochange <- calcDiss(preds_nochange_ls, vals_nochange_ls)[[1]]
    diss_change <- calcDiss(preds_change_ls, vals_change_ls)[[1]]
    
    # Get MAE dissimilarity
    MAE_nochange <- calcDiss(preds_nochange_ls, vals_nochange_ls)[[2]]
    MAE_change <- calcDiss(preds_change_ls, vals_change_ls)[[2]]
    
    # Get vals dissimilarity
    vals_diss_nochange <- calcDiss(preds_nochange_ls, vals_nochange_ls)[[3]]
    vals_diss_change <- calcDiss(preds_change_ls, vals_change_ls)[[3]]
    
    # Get slope
    slope_nochange <- calcSlope(preds_nochange_ls, vals_nochange_ls)[[1]]
    slope_change <- calcSlope(preds_change_ls, vals_change_ls)[[1]]
    
    # Get MAE slope
    MAE_slope_nochange <- calcSlope(preds_nochange_ls, vals_nochange_ls)[[2]]
    MAE_slope_change <- calcSlope(preds_change_ls, vals_change_ls)[[2]]
    
    # Get vals slope
    vals_slope_nochange <- calcSlope(preds_nochange_ls, vals_nochange_ls)[[3]]
    vals_slope_change <- calcSlope(preds_change_ls, vals_change_ls)[[3]]

    # Append to list
    diss_nochange_ls <- append(diss_nochange_ls, list(diss_nochange))
    diss_change_ls <- append(diss_change_ls, list(diss_change))
    MAE_nochange_ls <- append(MAE_nochange_ls, list(MAE_nochange))
    MAE_change_ls <- append(MAE_change_ls, list(MAE_change))
    vals_diss_nochange_ls <- append(vals_diss_nochange_ls, list(vals_diss_nochange))
    vals_diss_change_ls <- append(vals_diss_change_ls, list(vals_diss_change))
    
    slope_nochange_ls <- append(slope_nochange_ls, list(slope_nochange))
    slope_change_ls <- append(slope_change_ls, list(slope_change))
    MAE_slope_nochange_ls <- append(MAE_slope_nochange_ls, list(MAE_slope_nochange))
    MAE_slope_change_ls <- append(MAE_slope_change_ls, list(MAE_slope_change))
    vals_slope_nochange_ls <- append(vals_slope_nochange_ls, list(vals_slope_nochange))
    vals_slope_change_ls <- append(vals_slope_change_ls, list(vals_slope_change))
    
  }
  
  # Create data frames
  diss_nochange_df <- as.data.frame(colMeans(as.data.frame(do.call(rbind, diss_nochange_ls))))
  colnames(diss_nochange_df)[1] <- "Diss"
  diss_change_df <- as.data.frame(colMeans(as.data.frame(do.call(rbind, diss_change_ls))))
  colnames(diss_change_df)[1] <- "Diss"
  MAE_nochange_df <- as.data.frame(colMeans(as.data.frame(do.call(rbind, MAE_nochange_ls))))
  colnames(MAE_nochange_df)[1] <- "D- MAE"
  MAE_change_df <- as.data.frame(colMeans(as.data.frame(do.call(rbind, MAE_change_ls))))
  colnames(MAE_change_df)[1] <- "D- MAE"
  vals_nochange_df <- as.data.frame(colMeans(as.data.frame(do.call(rbind, vals_diss_nochange_ls))))
  colnames(vals_nochange_df)[1] <- "D- vals"
  vals_change_df <- as.data.frame(colMeans(as.data.frame(do.call(rbind, vals_diss_change_ls))))
  colnames(vals_change_df)[1] <- "D- vals"
  
  slope_nochange_df <- as.data.frame(colMeans(as.data.frame(do.call(rbind, slope_nochange_ls))))
  colnames(slope_nochange_df)[1] <- "Slope"
  slope_change_df <- as.data.frame(colMeans(as.data.frame(do.call(rbind, slope_change_ls))))
  colnames(slope_change_df)[1] <- "Slope"
  MAE_slope_nochange_df <- as.data.frame(colMeans(as.data.frame(do.call(rbind, MAE_slope_nochange_ls))))
  colnames(MAE_slope_nochange_df)[1] <- "S- MAE"
  MAE_slope_change_df <- as.data.frame(colMeans(as.data.frame(do.call(rbind, MAE_slope_change_ls))))
  colnames(MAE_slope_change_df)[1] <- "S- MAE"
  vals_slope_nochange_df <- as.data.frame(colMeans(as.data.frame(do.call(rbind, vals_slope_nochange_ls))))
  colnames(vals_slope_nochange_df)[1] <- "S- vals"
  vals_slope_change_df <- as.data.frame(colMeans(as.data.frame(do.call(rbind, vals_slope_change_ls))))
  colnames(vals_slope_change_df)[1] <- "S- vals"
  
  # Combine
  diss <- rbind(diss_nochange_df, diss_change_df)
  MAE <- rbind(MAE_nochange_df, MAE_change_df)
  diss_vals <- rbind(vals_nochange_df, vals_change_df)
  slope <- rbind(slope_nochange_df, slope_change_df)
  MAE_slope <- rbind(MAE_slope_nochange_df, MAE_slope_change_df)
  slope_vals <- rbind(vals_slope_nochange_df, vals_slope_change_df)
  total <- cbind(diss, MAE, diss_vals, slope, MAE_slope, slope_vals)
  rownames(total) <- c("No change", "Change")
  
  return(total)
}

diss_slope_timeseries <- CalcRelativeDiss(filtered_preds, filtered_vals)

# Calculate dissimilarity and slope per sample site
CalcTrend <- function(preds_ls, vals_ls) {
  
  ########## Dissimilarity ###########
  
  # Divide between change and no-change
  diff_20152016_vals <- vals_ls[[2]][classes] - vals_ls[[1]][classes]
  diff_20162017_vals <- vals_ls[[3]][classes] - vals_ls[[2]][classes]
  diff_20172018_vals <- vals_ls[[4]][classes] - vals_ls[[3]][classes]
  total_diff_vals <- abs(diff_20152016_vals) + abs(diff_20162017_vals) + abs(diff_20172018_vals)
  total_diff_vals <- as.data.frame(total_diff_vals)
  change_indices <- as.data.frame(ifelse(apply(total_diff_vals, 1, max) > 0, 1, 0))
  
  steep_threshold <- 30
  steep_change_indices <- as.data.frame(ifelse(apply(diff_20152016_vals, 1, max) > steep_threshold |
                                                 apply(diff_20162017_vals, 1, max) > steep_threshold |
                                                 apply(diff_20172018_vals, 1, max) > steep_threshold, 1, 0))
  
  # lists with no change and change
  vals_nochange_ls <- list()
  vals_change_ls <- list()
  preds_nochange_ls <- list()
  preds_change_ls <- list()
  
  # lists with steep(er) change
  vals_steepchange_ls <- list()
  preds_steepchange_ls <- list()
  
  for(i in 1:4) {
    preds_df <- preds_ls[[i]]
    preds_df$change <- change_indices
    preds_df$steep_change <- steep_change_indices
    
    vals_df <- vals_ls[[i]]
    vals_df$change <- change_indices
    vals_df$steep_change <- steep_change_indices
    
    preds_nochange <- subset(preds_df, change == 0)
    preds_change <- subset(preds_df, change == 1)
    preds_steep_change <- subset(preds_df, steep_change == 1)
    
    vals_nochange <- subset(vals_df, change == 0)
    vals_change <- subset(vals_df, change == 1)
    vals_steep_change <- subset(vals_df, steep_change == 1)
    
    vals_nochange_ls <- append(vals_nochange_ls, list(vals_nochange))
    vals_change_ls <- append(vals_change_ls, list(vals_change))
    vals_steepchange_ls <- append(vals_steepchange_ls, list(vals_steep_change))
    
    preds_nochange_ls <- append(preds_nochange_ls, list(preds_nochange))
    preds_change_ls <- append(preds_change_ls, list(preds_change))
    preds_steepchange_ls <- append(preds_steepchange_ls, list(preds_steep_change))
  }
  
  # Difference between time series fractions
  calcDiss <- function(preds_ls, vals_ls) {
    
    # Predicted time step dissimilarity
    pt_diss1 <- abs(preds_ls[[1]][classes] - preds_ls[[2]][classes])
    pt_diss2 <- (abs(preds_ls[[2]][classes] - preds_ls[[1]][classes]) + abs(preds_ls[[2]][classes] - preds_ls[[3]][classes])) / 2
    pt_diss3 <- (abs(preds_ls[[3]][classes] - preds_ls[[2]][classes]) + abs(preds_ls[[3]][classes] - preds_ls[[4]][classes])) / 2
    pt_diss4 <- abs(preds_ls[[4]][classes] - preds_ls[[3]][classes])
    pt_diss <- (pt_diss1 + pt_diss2 + pt_diss3 + pt_diss4) / 4 # Average dissimilarity per time step
    
    pt_diss1 <- abs(preds_ls[[1]][classes] - preds_ls[[2]][classes])
    pt_diss2 <- abs(preds_ls[[2]][classes] - preds_ls[[3]][classes])
    pt_diss3 <- abs(preds_ls[[3]][classes] - preds_ls[[4]][classes])
    pt_diss <- (pt_diss1 + pt_diss2 + pt_diss3) / 3 # Average dissimilarity per time step
    
    v_diss1 <- abs(vals_ls[[1]][classes] - vals_ls[[2]][classes])
    v_diss2 <- abs(vals_ls[[2]][classes] - vals_ls[[3]][classes])
    v_diss3 <- abs(vals_ls[[3]][classes] - vals_ls[[4]][classes])
    v_diss <- (v_diss1 + v_diss2 + v_diss3) / 3
    
    # MAE
    MAE1 <- abs((preds_ls[[1]][classes] - vals_ls[[1]][classes]))
    MAE2 <- abs((preds_ls[[2]][classes] - vals_ls[[2]][classes]))
    MAE3 <- abs((preds_ls[[3]][classes] - vals_ls[[3]][classes])) 
    MAE4 <- abs((preds_ls[[4]][classes] - vals_ls[[4]][classes])) 
    MAE_diss <- (MAE1 + MAE2 + MAE3 + MAE4) / 4
    MAE_diss <- as.data.frame(MAE_diss)
    
    # OA
    # OA1 <- SCM(preds_ls[[1]][classes]/100, vals_ls[[1]][classes]/100, totals=TRUE, plot=FALSE)$P_overall_accuracy
    # OA2 <- SCM(preds_ls[[2]][classes]/100, vals_ls[[2]][classes]/100, totals=TRUE, plot=FALSE)$P_overall_accuracy
    # OA3 <- SCM(preds_ls[[3]][classes]/100, vals_ls[[3]][classes]/100, totals=TRUE, plot=FALSE)$P_overall_accuracy
    # OA4 <- SCM(preds_ls[[4]][classes]/100, vals_ls[[4]][classes]/100, totals=TRUE, plot=FALSE)$P_overall_accuracy
    # OA <- as.data.frame((OA1 + OA2 + OA3 + OA4) / 4)
    # colnames(OA)[1] <- "OA"
    
    MAE_diss1 <- abs((preds_ls[[1]][classes] - preds_ls[[2]][classes])-(vals_ls[[1]][classes] - vals_ls[[2]][classes]))
    MAE_diss2 <- abs((preds_ls[[2]][classes] - preds_ls[[3]][classes])-(vals_ls[[2]][classes] - vals_ls[[3]][classes]))
    MAE_diss3 <- abs((preds_ls[[3]][classes] - preds_ls[[4]][classes])-(vals_ls[[3]][classes] - vals_ls[[4]][classes]))
    MAE_diss <- (MAE_diss1 + MAE_diss2 + MAE_diss3) / 3
    
    return(list(pt_diss, MAE_diss, v_diss))
  }
  
  # Slope
  calcSlope <- function(preds_ls, vals_ls) {
    
    # Slope (source: https://classroom.synonym.com/calculate-trendline-2709.html)
    a <- 4 * (1*preds_ls[[1]][classes]+2*preds_ls[[2]][classes]+3*preds_ls[[3]][classes]+4*preds_ls[[4]][classes])
    b <- (1+2+3+4) * (preds_ls[[1]][classes]+preds_ls[[2]][classes]+preds_ls[[3]][classes]+preds_ls[[4]][classes])
    c <- 4 * ((1**2)+(2**2)+(3**2)+(4**2))
    d <- (1+2+3+4)**2
    preds_slope1 <- as.data.frame((a-b) / (c-d))
    preds_slope <- as.data.frame(abs(preds_slope1)) # Absolute slope over entire sample time series
    
    # preds_slope <- abs(preds_ls[[1]][classes] - preds_ls[[4]][classes])/3 

    a <- 4 * (1*vals_ls[[1]][classes]+2*vals_ls[[2]][classes]+3*vals_ls[[3]][classes]+4*vals_ls[[4]][classes])
    b <- (1+2+3+4) * (vals_ls[[1]][classes]+vals_ls[[2]][classes]+vals_ls[[3]][classes]+vals_ls[[4]][classes])
    c <- 4 * ((1**2)+(2**2)+(3**2)+(4**2))
    d <- (1+2+3+4)**2
    vals_slope1 <- as.data.frame((a-b) / (c-d))
    vals_slope <- as.data.frame(abs(vals_slope1)) # Slope over entire sample time series

    slope <- abs(preds_slope1 - vals_slope1)
    
    preds_slope1 <- as.data.frame(preds_ls[[1]][classes] - preds_ls[[4]][classes])/3
    preds_slope <- as.data.frame(preds_slope1)
    vals_slope1 <-  as.data.frame(vals_ls[[1]][classes] - vals_ls[[4]][classes])/3
    vals_slope <- as.data.frame(vals_slope1)
    
    slope <- as.data.frame(abs(preds_slope1 - vals_slope1))

    return(list(preds_slope, vals_slope, slope))
  }
  
  # No change
  diss <- calcDiss(preds_nochange_ls, vals_nochange_ls)[[1]]
  diss <- colMeans(as.data.frame(diss))
  diss <- colMeans(as.data.frame(diss))
  
  MAE <- calcDiss(preds_nochange_ls, vals_nochange_ls)[[2]]
  MAE <- colMeans(as.data.frame(MAE))
  MAE <- colMeans(as.data.frame(MAE))
  
  actual_diss <- calcDiss(preds_nochange_ls, vals_nochange_ls)[[3]]
  actual_diss <- colMeans(as.data.frame(actual_diss))
  actual_diss <- colMeans(as.data.frame(actual_diss))
  
  slope <- calcSlope(preds_nochange_ls, vals_nochange_ls)[[1]]
  slope <- colMeans(as.data.frame(slope))
  slope <- colMeans(as.data.frame(slope))
  
  vals_slope <- calcSlope(preds_nochange_ls, vals_nochange_ls)[[2]]
  vals_slope <- colMeans(as.data.frame(vals_slope))
  vals_slope <- colMeans(as.data.frame(vals_slope))
  
  MAE_slope <- calcSlope(preds_nochange_ls, vals_nochange_ls)[[3]]
  MAE_slope <- colMeans(as.data.frame(MAE_slope))
  MAE_slope <- colMeans(as.data.frame(MAE_slope))

  no_change <- cbind(diss, actual_diss, MAE, slope, vals_slope, MAE_slope)
  
  # change
  diss <- calcDiss(preds_change_ls, vals_change_ls)[[1]]
  diss <- colMeans(as.data.frame(diss))
  diss <- colMeans(as.data.frame(diss))
  
  MAE <- calcDiss(preds_change_ls, vals_change_ls)[[2]]
  MAE <- colMeans(as.data.frame(MAE))
  MAE <- colMeans(as.data.frame(MAE))
  
  actual_diss <- calcDiss(preds_change_ls, vals_change_ls)[[3]]
  actual_diss <- colMeans(as.data.frame(actual_diss))
  actual_diss <- colMeans(as.data.frame(actual_diss))
  
  slope <- calcSlope(preds_change_ls, vals_change_ls)[[1]]
  slope <- colMeans(as.data.frame(slope))
  slope <- colMeans(as.data.frame(slope))
  
  vals_slope <- calcSlope(preds_change_ls, vals_change_ls)[[2]]
  vals_slope <- colMeans(as.data.frame(vals_slope))
  vals_slope <- colMeans(as.data.frame(vals_slope))
  
  MAE_slope <- calcSlope(preds_change_ls, vals_change_ls)[[3]]
  MAE_slope <- colMeans(as.data.frame(MAE_slope))
  MAE_slope <- colMeans(as.data.frame(MAE_slope))
  
  change <- cbind(diss, actual_diss, MAE, slope, vals_slope, MAE_slope)
  
  # Steep change
  diss <- calcDiss(preds_steepchange_ls, vals_steepchange_ls)[[1]]
  diss <- colMeans(as.data.frame(diss))
  diss <- colMeans(as.data.frame(diss))
  
  steep_change <- diss
  
  # Total
  total <- rbind(no_change, change)
  rownames(total) <- c("No change", "Change")

  return(as.data.frame(total))
}

TrendDiff <- CalcTrend(filtered_preds, filtered_vals)

# Calc diss per time step
calcDissYear <- function(preds_ls, vals_ls) {
  
  no_change_ls <- list()
  change_ls <- list()
  
  vali_no_change_ls <- list()
  vali_change_ls <- list()
  
  MAE_no_change_ls <- list()
  MAE_change_ls <- list()
  
  p_MAE_no_change_ls <- list()
  p_MAE_change_ls <- list()
  
  for (class in classes) {
    
    # Determine change by true validation
    dir_vali <- dir_all_vali
    pattern_vali <- pattern_all_vali
    
    valfiles = list.files(dir_vali, 
                          pattern = pattern_vali)
    vals = lapply(paste0(dir_vali, valfiles), read.csv)
    lapply(vals, nrow) 
    
    classnames = loadClassNames()
    
    # Harmonises df1 to df2 based on location id
    harmonise = function(i, df1, df2)
    {
      classids = intersect(df1[[i]][["location_id"]], df2[[i]][["location_id"]])
      h1 = df1[[i]][df1[[i]][["location_id"]] %in% classids,]
      h1[order(h1[["location_id"]]),]
    }
    
    harmonised_preds = lapply(1:length(preds), harmonise, df1=preds, df2=vals)
    harmonised_vals = lapply(1:length(vals), harmonise, df1=vals, df2=preds)
    
    sapply(harmonised_preds, nrow) == sapply(harmonised_vals, nrow)
    sapply(1:length(harmonised_vals),
           # function(i) all(harmonised_vals[[i]][["sample_id"]] == harmonised_preds[[i]][["sample_id"]])
           function(i) all(harmonised_vals[[i]][["location_id"]] == harmonised_preds[[i]][["location_id"]])
    )
    
    vals_filtered_2015 <- harmonised_vals[[1]][harmonised_vals[[1]]$location_id %in% common_ids, ]
    vals_filtered_2016 <- harmonised_vals[[2]][harmonised_vals[[2]]$location_id %in% common_ids, ]
    vals_filtered_2017 <- harmonised_vals[[3]][harmonised_vals[[3]]$location_id %in% common_ids, ]
    vals_filtered_2018 <- harmonised_vals[[4]][harmonised_vals[[4]]$location_id %in% common_ids, ]
    filtered_vals <- list(vals_filtered_2015, vals_filtered_2016, vals_filtered_2017, vals_filtered_2018)
    
    # Change per year to year
    abs_diff1 <- as.data.frame(abs(filtered_vals[[1]][[class]] - filtered_vals[[2]][[class]])) # 2015 - 2016
    abs_diff2 <- as.data.frame(abs(filtered_vals[[2]][[class]] - filtered_vals[[3]][[class]]))
    abs_diff3 <- as.data.frame(abs(filtered_vals[[3]][[class]] - filtered_vals[[4]][[class]]))
    
    # Change indices
    change_yes1 <- as.data.frame(ifelse(apply(abs_diff1, 1, max) > 0, 1, 0))
    change_yes2 <- as.data.frame(ifelse(apply(abs_diff2, 1, max) > 0, 1, 0))
    change_yes3 <- as.data.frame(ifelse(apply(abs_diff3, 1, max) > 0, 1, 0))
    
    # Subset based on change
    preds_ls[[1]]$change1 <- change_yes1
    preds_ls[[2]]$change1 <- change_yes1
    preds_ls[[2]]$change2 <- change_yes2
    preds_ls[[3]]$change2 <- change_yes2
    preds_ls[[3]]$change3 <- change_yes3
    preds_ls[[4]]$change3 <- change_yes3
    df1_change1_no <- subset(preds_ls[[1]], change_yes1 == 0)
    df1_change1_yes <- subset(preds_ls[[1]], change_yes1 == 1)
    df2_change1_no <- subset(preds_ls[[2]], change_yes1 == 0)
    df2_change1_yes <- subset(preds_ls[[2]], change_yes1 == 1)
    df2_change2_no <- subset(preds_ls[[2]], change_yes2 == 0)
    df2_change2_yes <- subset(preds_ls[[2]], change_yes2 == 1)
    df3_change2_no <- subset(preds_ls[[3]], change_yes2 == 0)
    df3_change2_yes <- subset(preds_ls[[3]], change_yes2 == 1)
    df3_change3_no <- subset(preds_ls[[3]], change_yes3 == 0)
    df3_change3_yes <- subset(preds_ls[[3]], change_yes3 == 1)
    df4_change3_no <- subset(preds_ls[[4]], change_yes3 == 0)
    df4_change3_yes <- subset(preds_ls[[4]], change_yes3 == 1)
    
    # Vals
    vals_ls[[1]]$change1 <- change_yes1
    vals_ls[[2]]$change1 <- change_yes1
    vals_ls[[2]]$change2 <- change_yes2
    vals_ls[[3]]$change2 <- change_yes2
    vals_ls[[3]]$change3 <- change_yes3
    vals_ls[[4]]$change3 <- change_yes3
    v_df1_change1_no <- subset(vals_ls[[1]], change_yes1 == 0)
    v_df1_change1_yes <- subset(vals_ls[[1]], change_yes1 == 1)
    v_df2_change1_no <- subset(vals_ls[[2]], change_yes1 == 0)
    v_df2_change1_yes <- subset(vals_ls[[2]], change_yes1 == 1)
    v_df2_change2_no <- subset(vals_ls[[2]], change_yes2 == 0)
    v_df2_change2_yes <- subset(vals_ls[[2]], change_yes2 == 1)
    v_df3_change2_no <- subset(vals_ls[[3]], change_yes2 == 0)
    v_df3_change2_yes <- subset(vals_ls[[3]], change_yes2 == 1)
    v_df3_change3_no <- subset(vals_ls[[3]], change_yes3 == 0)
    v_df3_change3_yes <- subset(vals_ls[[3]], change_yes3 == 1)
    v_df4_change3_no <- subset(vals_ls[[4]], change_yes3 == 0)
    v_df4_change3_yes <- subset(vals_ls[[4]], change_yes3 == 1)
    
    # Change / no change difference
    p_no_change <- (abs(df1_change1_no[[class]] - df2_change1_no[[class]]) + abs(df2_change2_no[[class]] - df3_change2_no[[class]]) + abs(df3_change3_no[[class]] - df4_change3_no[[class]])) / 3
    p_change <- (abs(df1_change1_yes[[class]] - df2_change1_yes[[class]]) + abs(df2_change2_yes[[class]] - df3_change2_yes[[class]]) + abs(df3_change3_yes[[class]] - df4_change3_yes[[class]])) / 3
    p_no_change <- as.data.frame(p_no_change)
    p_change <- as.data.frame(p_change)
    
    v_no_change <- (abs(v_df1_change1_no[[class]] - v_df2_change1_no[[class]]) + abs(v_df2_change2_no[[class]] - v_df3_change2_no[[class]]) + abs(v_df3_change3_no[[class]] - v_df4_change3_no[[class]])) / 3
    v_change <- (abs(v_df1_change1_yes[[class]] - v_df2_change1_yes[[class]]) + abs(v_df2_change2_yes[[class]] - v_df3_change2_yes[[class]]) + abs(v_df3_change3_yes[[class]] - v_df4_change3_yes[[class]])) / 3
    v_no_change <- as.data.frame(v_no_change)
    v_change <- as.data.frame(v_change)
    
    MAE_no_change <- (abs((df1_change1_no[[class]] - df2_change1_no[[class]]) - (v_df1_change1_no[[class]] - v_df2_change1_no[[class]])) 
    + abs((df2_change2_no[[class]] - df3_change2_no[[class]]) - (v_df2_change2_no[[class]] - v_df3_change2_no[[class]]))
    + abs((df3_change3_no[[class]] - df4_change3_no[[class]]) - (v_df3_change3_no[[class]] - v_df4_change3_no[[class]]))) / 3
    
    MAE_no_change <- (abs(df1_change1_no[[class]] - v_df1_change1_no[[class]])+abs(df2_change1_no[[class]] - v_df2_change1_no[[class]])
                    + abs(df2_change2_no[[class]] - v_df2_change2_no[[class]])+abs(df3_change2_no[[class]] - v_df3_change2_no[[class]])
                    + abs(df3_change3_no[[class]] - v_df3_change3_no[[class]])+abs(df4_change3_no[[class]] - v_df4_change3_no[[class]])) / 6
    
    MAE_change <- (abs((df1_change1_yes[[class]] - df2_change1_yes[[class]]) - (v_df1_change1_yes[[class]] - v_df2_change1_yes[[class]])) 
                     + abs((df2_change2_yes[[class]] - df3_change2_yes[[class]]) - (v_df2_change2_yes[[class]] - v_df3_change2_yes[[class]]))
                     + abs((df3_change3_yes[[class]] - df4_change3_yes[[class]]) - (v_df3_change3_yes[[class]] - v_df4_change3_yes[[class]]))) / 3
    
    MAE_change <- (abs(df1_change1_yes[[class]] - v_df1_change1_yes[[class]])+abs(df2_change1_yes[[class]] - v_df2_change1_yes[[class]])
                      + abs(df2_change2_yes[[class]] - v_df2_change2_yes[[class]])+abs(df3_change2_yes[[class]] - v_df3_change2_yes[[class]])
                      + abs(df3_change3_yes[[class]] - v_df3_change3_yes[[class]])+abs(df4_change3_yes[[class]] - v_df4_change3_yes[[class]])) / 6
   
    MAE_no_change <- as.data.frame(MAE_no_change)
    MAE_change <- as.data.frame(MAE_change)
    
    # Predicted dissimilarity
    p_MAE_no_change <- (abs(df1_change1_no[[class]] - v_df1_change1_no[[class]]) + abs(df2_change1_no[[class]] - v_df2_change1_no[[class]])
    + abs(df2_change2_no[[class]] - v_df2_change2_no[[class]]) + abs(df3_change2_no[[class]] - v_df3_change2_no[[class]])
    + abs(df3_change3_no[[class]] - v_df3_change3_no[[class]]) + abs(df4_change3_no[[class]] - v_df4_change3_no[[class]])) / 6
    
    p_MAE_change <- (abs(df1_change1_yes[[class]] - v_df1_change1_yes[[class]]) + abs(df2_change1_yes[[class]] - v_df2_change1_yes[[class]])
                        + abs(df2_change2_yes[[class]] - v_df2_change2_yes[[class]]) + abs(df3_change2_yes[[class]] - v_df3_change2_yes[[class]])
                        + abs(df3_change3_yes[[class]] - v_df3_change3_yes[[class]]) + abs(df4_change3_yes[[class]] - v_df4_change3_yes[[class]])) / 6
    
    p_MAE_no_change <- as.data.frame(p_MAE_no_change)
    p_MAE_change <- as.data.frame(p_MAE_change)
    
    # Append to lists
    no_change_ls <- append(no_change_ls, list(p_no_change))
    change_ls <- append(change_ls, list(p_change))
    
    vali_no_change_ls <- append(vali_no_change_ls, list(v_no_change))
    vali_change_ls <- append(vali_change_ls, list(v_change))
    
    MAE_no_change_ls <- append(MAE_no_change_ls, list(MAE_no_change))
    MAE_change_ls <- append(MAE_change_ls, list(MAE_change))
    
    p_MAE_no_change_ls <- append(p_MAE_no_change_ls, list(p_MAE_no_change))
    p_MAE_change_ls <- append(p_MAE_change_ls, list(p_MAE_change))
  }
  
  # Rbind and take mean
  diss_nochange_df <- as.data.frame(colMeans(as.data.frame(do.call(rbind, no_change_ls))))
  colnames(diss_nochange_df)[1] <- "Diss"
  diss_change_df <- as.data.frame(colMeans(as.data.frame(do.call(rbind, change_ls))))
  colnames(diss_change_df)[1] <- "Diss"
  
  vali_diss_nochange_df <- as.data.frame(colMeans(as.data.frame(do.call(rbind, vali_no_change_ls))))
  colnames(vali_diss_nochange_df)[1] <- "V Diss"
  vali_diss_change_df <- as.data.frame(colMeans(as.data.frame(do.call(rbind, vali_change_ls))))
  colnames(vali_diss_change_df)[1] <- "V Diss"
  
  MAE_nochange_df <- as.data.frame(colMeans(as.data.frame(do.call(rbind, MAE_no_change_ls))))
  colnames(MAE_nochange_df)[1] <- "D- MAE"
  MAE_change_df <- as.data.frame(colMeans(as.data.frame(do.call(rbind, MAE_change_ls))))
  colnames(MAE_change_df)[1] <- "D- MAE"
  
  p_MAE_nochange_df <- as.data.frame(colMeans(as.data.frame(do.call(rbind, p_MAE_no_change_ls))))
  colnames(p_MAE_nochange_df)[1] <- "P- MAE"
  p_MAE_change_df <- as.data.frame(colMeans(as.data.frame(do.call(rbind, p_MAE_change_ls))))
  colnames(p_MAE_change_df)[1] <- "P- MAE"
  
  # Combine
  diss <- rbind(diss_nochange_df, diss_change_df)
  vali_diss <- rbind(vali_diss_nochange_df, vali_diss_change_df) 
  MAE <- rbind(MAE_nochange_df, MAE_change_df)
  p_MAE <- rbind(p_MAE_nochange_df, p_MAE_change_df)
  total <- cbind(diss, vali_diss, MAE, p_MAE)
  rownames(total) <- c("No change", "Change")
  
  return(total)
  
}

DissYear <- calcDissYear(filtered_preds, filtered_vals)




############################################ Histograms of majority and zero fractions #############################################################

total_preds <- rbind(harmonised_preds[[1]][classes], harmonised_preds[[2]][classes], harmonised_preds[[3]][classes], harmonised_preds[[4]][classes])
total_vals <- rbind(harmonised_vals[[1]][classes], harmonised_vals[[2]][classes], harmonised_vals[[3]][classes], harmonised_vals[[4]][classes])
preds_max <- as.data.frame(apply(total_preds, 1, max))
hist(preds_max$`apply(total_preds, 1, max)`, main = "", xlab = "Maximum fraction", ylab = "Frequency", breaks = 100)

preds_zeros <- as.data.frame(apply(total_preds < 0.5, 1, sum))
vals_zeros <- as.data.frame(apply(total_vals < 0.5, 1, sum))

preds_values <- table(preds_zeros$`apply(total_preds < 0.5, 1, sum)`)
vals_values <- table(vals_zeros$`apply(total_vals < 0.5, 1, sum)`)

barplot(preds_values, main = "Prediction", xlab = "Zero classes", ylab = "Frequency", ylim = c(0, 60000))
barplot(vals_values, main = "Validation", xlab = "Zero classes", ylab = "Frequency", ylim = c(0, 60000))



####################################################### Calculate distributions ################################################################### 

vals_prev <- as.data.frame(ifelse(total_vals > 0, 1, 0))
preds_prev <- as.data.frame(ifelse(total_preds > 0.5, 1, 0))


# Calculate fraction accuracy

# Correctly 0%
preds_zeros <- as.data.frame(rowSums(total_preds < 0.5))
preds_zeros <- colSums(preds_zeros)
vals_zeros <- as.data.frame(rowSums(total_vals < 0.5))
vals_zeros <- colSums(vals_zeros)
correct_zeros <- as.data.frame(rowSums(total_vals == 0 & total_preds < 0.5))
correct_zeros <- colSums(correct_zeros)
missed_zeros <- as.data.frame(vals_zeros - correct_zeros)

# Correctly low (<25%) (don't count where observation is 0%)
preds_low <- as.data.frame(rowSums(total_preds > 0.5 & total_preds < 25.5))
preds_low <- colSums(preds_low)
vals_low <- as.data.frame(rowSums(total_vals > 0.5 & total_vals < 25.5))
vals_low <- colSums(vals_low)
correct_low <- as.data.frame(rowSums(abs(total_vals - total_preds) < 10 & total_vals > 0 & (total_preds > 0.5 & total_preds < 25.5))) 
correct_low <- colSums(correct_low)
missed_low <- as.data.frame(vals_low - correct_low)

# Correctly mid (50%)
preds_mid <- as.data.frame(rowSums(total_preds > 25.5 & total_preds < 75.5))
preds_mid <- colSums(preds_mid)
vals_mid <- as.data.frame(rowSums(total_vals > 25.5 & total_vals < 75.5))
vals_mid <- colSums(vals_mid)
correct_mid <- as.data.frame(rowSums(abs(total_vals - total_preds) < 10 & (total_preds > 25.5 & total_preds < 75.5)))
correct_mid <- colSums(correct_mid)
missed_mid <- as.data.frame(vals_mid - correct_mid)

# Correctly high (75%) (don't count where observation is 100%)
preds_high <- as.data.frame(rowSums(total_preds > 75.5 & total_preds < 99.5))
preds_high <- colSums(preds_high)
vals_high <- as.data.frame(rowSums(total_vals > 75.5 & total_vals < 99.5))
vals_high <- colSums(vals_high)
correct_high <- as.data.frame(rowSums(abs(total_vals - total_preds) < 10 & total_vals < 99.5 & (total_preds > 75.5 & total_preds < 99.5))) 
correct_high <- colSums(correct_high)
missed_high <- as.data.frame(vals_high - correct_high)

# Correctly 100%
preds_hundred <- as.data.frame(rowSums(total_preds > 99.5))
preds_hundred <- colSums(preds_hundred)
vals_hundred <- as.data.frame(rowSums(total_vals > 99.5))
vals_hundred <- colSums(vals_hundred)
correct_hundred <- as.data.frame(rowSums(total_vals == 100 & total_preds > 99.5))
correct_hundred <- colSums(correct_hundred)
missed_hundred <- as.data.frame(vals_hundred - correct_hundred)

cnames <- c("<0.5%", "0.5-25.5%", "25.5-75.5%", "75.5-99.5%", ">99.5%")
fr_prev <- cbind(preds_zeros, preds_low, preds_mid, preds_high, preds_hundred)
colnames(fr_prev) <- cnames
g_prev <- cbind(correct_zeros, correct_low, correct_mid, correct_high, correct_hundred)
colnames(g_prev) <- cnames
m_prev <- cbind(missed_zeros, missed_low, missed_mid, missed_high, missed_hundred)
colnames(m_prev) <- cnames

rnames <- c("Incorrect", "Correct", "Missed")
comb_prev_df <- as.data.frame(rbind(fr_prev, g_prev, m_prev))
rownames(comb_prev_df) <- rnames 

# In percentages
predicted_perc <- ((comb_prev_df[1,] - comb_prev_df[2,])/(comb_prev_df[1,]+comb_prev_df[3,]))*100
good_perc <- (comb_prev_df[2,]/(comb_prev_df[1,]+comb_prev_df[3,]))*100
missed_perc <- (comb_prev_df[3,]/(comb_prev_df[1,]+comb_prev_df[3,]))*100

comb_prev_df <- as.data.frame(rbind(good_perc, predicted_perc, missed_perc))

# write.csv(comb_prev_df, "C:/Users/augus/Documents/Studie/MGI/Thesis/Report/Figures/ZeroInflation/csvs/combprevdf.csv")

# Calculate correctly predicted classes (Zeroinflation)
preds_zeros <- as.data.frame(apply(total_preds < 0.5, 1, sum))
vals_zeros <- as.data.frame(apply(total_vals < 0.5, 1, sum))
# preds_zeros <- colSums(preds_zeros)

correct_zeros <- as.data.frame(rowSums(total_vals == 0 & total_preds < 0.5))

correct_0_zeros <- as.data.frame(rowSums(preds_zeros == 0 & vals_zeros == 0))
correct_1_zeros <- as.data.frame(rowSums(rowSums(total_preds < 0.5)==1 & rowSums(total_vals < 0.5)==1 & total_preds < 0.5 & total_vals == 0))
correct_2_zeros <- as.data.frame(rowSums(rowSums(total_preds < 0.5)==2 & rowSums(total_vals < 0.5)==2 & total_preds < 0.5 & total_vals == 0))
correct_3_zeros <- as.data.frame(rowSums(rowSums(total_preds < 0.5)==3 & rowSums(total_vals < 0.5)==3 & total_preds < 0.5 & total_vals == 0))
correct_4_zeros <- as.data.frame(rowSums(rowSums(total_preds < 0.5)==4 & rowSums(total_vals < 0.5)==4 & total_preds < 0.5 & total_vals == 0))
correct_5_zeros <- as.data.frame(rowSums(rowSums(total_preds < 0.5)==5 & rowSums(total_vals < 0.5)==5 & total_preds < 0.5 & total_vals == 0))
correct_6_zeros <- as.data.frame(rowSums(rowSums(total_preds < 0.5)==6 & rowSums(total_vals < 0.5)==6 & total_preds < 0.5 & total_vals == 0))

number_predicted <- c(sum(preds_zeros == 0), sum(preds_zeros == 1), sum(preds_zeros == 2), sum(preds_zeros == 3),
                    sum(preds_zeros == 4), sum(preds_zeros == 5), sum(preds_zeros == 6))
number_good <- c(sum(correct_0_zeros == 1), sum(correct_1_zeros == 1), sum(correct_2_zeros == 2), sum(correct_3_zeros == 3), sum(correct_4_zeros == 4),
                 sum(correct_5_zeros == 5), sum(correct_6_zeros == 6))
number_vals <- c(sum(vals_zeros == 0), sum(vals_zeros == 1), sum(vals_zeros == 2), sum(vals_zeros == 3),
                      sum(vals_zeros == 4), sum(vals_zeros == 5), sum(vals_zeros == 6))
number_missed <- number_vals - number_good

number_false <- number_predicted - number_good

zero_inf_pred <- as.data.frame(rbind(number_good, number_missed, number_false))
rnames <- c("Correct", "Missed", "Incorrect")
cnames <- c("0", "1", "2", "3", "4", "5", "6")
rownames(zero_inf_pred) <- rnames
colnames(zero_inf_pred) <- cnames


# Together with Jaccard similarity
hgen <- as.data.frame(rowSums(vals_prev))
hpreds <- subset(total_preds, hgen$`rowSums(vals_prev)` > 0) # for heterogeneous > 1
hvals <- subset(total_vals, hgen$`rowSums(vals_prev)` > 0)

hpreds_distr <- data.frame(
  fraction_1 = as.data.frame(ifelse(hpreds$tree > 0.5, 1, 0)),
  fraction_2 = as.data.frame(ifelse(hpreds$shrub > 0.5, 3, 2)),
  fraction_3 = as.data.frame(ifelse(hpreds$grassland > 0.5, 5, 4)),
  fraction_4 = as.data.frame(ifelse(hpreds$crops > 0.5, 7, 6)),
  fraction_5 = as.data.frame(ifelse(hpreds$urban_built_up > 0.5, 9, 8)),
  fraction_6 = as.data.frame(ifelse(hpreds$bare > 0.5, 11, 10)),
  fraction_7 = as.data.frame(ifelse(hpreds$water > 0.5, 13, 12))
)

hvals_distr <- data.frame(
  fraction_1 = as.data.frame(ifelse(hvals$tree > 0.5, 1, 0)),
  fraction_2 = as.data.frame(ifelse(hvals$shrub > 0.5, 3, 2)),
  fraction_3 = as.data.frame(ifelse(hvals$grassland > 0.5, 5, 4)),
  fraction_4 = as.data.frame(ifelse(hvals$crops > 0.5, 7, 6)),
  fraction_5 = as.data.frame(ifelse(hvals$urban_built_up > 0.5, 9, 8)),
  fraction_6 = as.data.frame(ifelse(hvals$bare > 0.5, 11, 10)),
  fraction_7 = as.data.frame(ifelse(hvals$water > 0.5, 13, 12))
)

getJaccard <- function(zero_numb) {
  
  new_hpreds_distr <- subset(hpreds_distr, preds_zeros == zero_numb)
  new_hvals_distr <- subset(hvals_distr, preds_zeros == zero_numb)
  
  new_preds <- subset(total_preds, preds_zeros == zero_numb)
  new_vals <- subset(total_vals, preds_zeros == zero_numb)
  
  num_rows <- nrow(new_preds)
  jaccard_sim <- numeric(num_rows)
  
  for (i in 1:num_rows) {
    
    test1 <- new_hpreds_distr[i,]
    test2 <- new_hvals_distr[i,]
    
    # Exclude correctly predicted zero fractions
    valid_indices <- which(new_vals[i,] != 0 | new_preds[i,] > 0.5)
    
    test1 <- as.matrix(test1[, valid_indices])
    test2 <- as.matrix(test2[, valid_indices])
    
    intersection <- length(intersect(test1, test2))
    union <- length(union(test1, test2))
    jaccard_similarity <- intersection / union
    
    # Add to total jaccard
    jaccard_sim[i] <- jaccard_similarity
    
  }
  
  return(as.data.frame(colMeans(as.data.frame(jaccard_sim))*100))
  
}

Jacc_0_zeros <- getJaccard(0)
Jacc_1_zeros <- getJaccard(1)
Jacc_2_zeros <- getJaccard(2)
Jacc_3_zeros <- getJaccard(3)
Jacc_4_zeros <- getJaccard(4)
Jacc_5_zeros <- getJaccard(5)
Jacc_6_zeros <- getJaccard(6)

jacc_zeros <- c(Jacc_0_zeros,  Jacc_1_zeros, Jacc_2_zeros,  Jacc_3_zeros,  Jacc_4_zeros,  Jacc_5_zeros,  Jacc_6_zeros)
jacc_number <- c(sum(preds_zeros == 0)*(Jacc_0_zeros/100),  sum(preds_zeros == 1)*(Jacc_1_zeros/100), sum(preds_zeros == 2)*(Jacc_2_zeros/100),  
                 sum(preds_zeros == 3)*(Jacc_3_zeros/100),  sum(preds_zeros == 4)*(Jacc_4_zeros/100),  sum(preds_zeros == 5)*(Jacc_5_zeros/100),
                 sum(preds_zeros == 6)*(Jacc_6_zeros/100))
predict_min_jacc <- c(sum(preds_zeros == 0)-(sum(preds_zeros == 0)*(Jacc_0_zeros/100)), sum(preds_zeros == 1)-(sum(preds_zeros == 1)*(Jacc_1_zeros/100)), 
                      sum(preds_zeros == 2)-(sum(preds_zeros == 2)*(Jacc_2_zeros/100)), sum(preds_zeros == 3)-(sum(preds_zeros == 3)*(Jacc_3_zeros/100)),  
                      sum(preds_zeros == 4)-(sum(preds_zeros == 4)*(Jacc_4_zeros/100)), sum(preds_zeros == 5)-(sum(preds_zeros == 5)*(Jacc_5_zeros/100)),
                      sum(preds_zeros == 6)-(sum(preds_zeros == 6)*(Jacc_6_zeros/100)))

zero_inf_pred <- as.data.frame(rbind(jacc_number, predict_min_jacc))
rnames <- c("Jaccard correct", "Total predicted")
cnames <- c("0", "1", "2", "3", "4", "5", "6")
rownames(zero_inf_pred) <- rnames
colnames(zero_inf_pred) <- cnames

zero_inf_pred_df <- as.data.frame(apply(zero_inf_pred, 2, function(x) as.numeric(as.character(x))))
rownames(zero_inf_pred_df) <- rnames
colnames(zero_inf_pred_df) <- cnames

# For plotting of Jaccard bars, save (skip if not wanted)
write.csv(zero_inf_pred_df, paste0(linkData, "Processed/Output/Jaccard/JaccBar.csv"))



###################################################### Calculate 0 and 100% fractions ############################################################

# Calculate correctly predicted zero
threshold <- 0.5
zero_correct <- data.frame(
  fraction_1 = as.data.frame(sum(total_vals$tree == 0 & total_preds$tree < threshold)),
  fraction_2 = as.data.frame(sum(total_vals$shrub == 0 & total_preds$shrub < threshold)),
  fraction_3 = as.data.frame(sum(total_vals$grassland == 0 & total_preds$grassland < threshold)),
  fraction_4 = as.data.frame(sum(total_vals$crops == 0 & total_preds$crops < threshold)),
  fraction_5 = as.data.frame(sum(total_vals$urban_built_up == 0 & total_preds$urban_built_up < threshold)),
  fraction_6 = as.data.frame(sum(total_vals$bare == 0 & total_preds$bare < threshold)),
  fraction_7 = as.data.frame(sum(total_vals$water == 0 & total_preds$water < threshold))
)

threshold <- 99.5
hundred_correct <- data.frame(
  fraction_1 = as.data.frame(sum(total_vals$tree == 100 & total_preds$tree > threshold)),
  fraction_2 = as.data.frame(sum(total_vals$shrub == 100 & total_preds$shrub > threshold)),
  fraction_3 = as.data.frame(sum(total_vals$grassland == 100 & total_preds$grassland > threshold)),
  fraction_4 = as.data.frame(sum(total_vals$crops == 100 & total_preds$crops > threshold)),
  fraction_5 = as.data.frame(sum(total_vals$urban_built_up == 100 & total_preds$urban_built_up > threshold)),
  fraction_6 = as.data.frame(sum(total_vals$bare == 100 & total_preds$bare > threshold)),
  fraction_7 = as.data.frame(sum(total_vals$water == 100 & total_preds$water > threshold))
)

true_zero_correct <- data.frame(
  fraction_1 = as.data.frame(sum(total_vals$tree == 0)),
  fraction_2 = as.data.frame(sum(total_vals$shrub == 0)),
  fraction_3 = as.data.frame(sum(total_vals$grassland == 0)),
  fraction_4 = as.data.frame(sum(total_vals$crops == 0)),
  fraction_5 = as.data.frame(sum(total_vals$urban_built_up == 0)),
  fraction_6 = as.data.frame(sum(total_vals$bare == 0)),
  fraction_7 = as.data.frame(sum(total_vals$water == 0))
)

true_hundred_correct <- data.frame(
  fraction_1 = as.data.frame(sum(total_vals$tree == 100)),
  fraction_2 = as.data.frame(sum(total_vals$shrub == 100)),
  fraction_3 = as.data.frame(sum(total_vals$grassland == 100)),
  fraction_4 = as.data.frame(sum(total_vals$crops == 100)),
  fraction_5 = as.data.frame(sum(total_vals$urban_built_up == 100)),
  fraction_6 = as.data.frame(sum(total_vals$bare == 100)),
  fraction_7 = as.data.frame(sum(total_vals$water == 100))
)

print(sum(zero_correct) / sum(true_zero_correct))
print(sum(hundred_correct) / sum(true_hundred_correct))



#################################################### If interested, KL divergence #############################################################

# Kl divergence
# test <- as.matrix(rbind(total_preds[1,]/100, total_vals[1,]/100))
# 
# testKL <- KL(test, unit='log')
# 
# num_rows <- nrow(total_preds)
# 
# kl_divergence <- numeric(num_rows)
# 
# for (i in 1:num_rows) {
#   test <- as.matrix(rbind(total_preds[i,] / 100, total_vals[i,] / 100))
#   kl_divergence[i] <- suppressMessages(KL(test, unit = "log"))
# }
# 
# print(colMeans(as.data.frame(kl_divergence)))


############################################################# Jaccard overall ###################################################################

# Jaccard
hgen <- as.data.frame(rowSums(vals_prev))
hpreds <- subset(total_preds, hgen$`rowSums(vals_prev)` > 0)
hvals <- subset(total_vals, hgen$`rowSums(vals_prev)` > 0)

hpreds_distr <- data.frame(
  fraction_1 = as.data.frame(ifelse(hpreds$tree > 0.5, 1, 0)),
  fraction_2 = as.data.frame(ifelse(hpreds$shrub > 0.5, 3, 2)),
  fraction_3 = as.data.frame(ifelse(hpreds$grassland > 0.5, 5, 4)),
  fraction_4 = as.data.frame(ifelse(hpreds$crops > 0.5, 7, 6)),
  fraction_5 = as.data.frame(ifelse(hpreds$urban_built_up > 0.5, 9, 8)),
  fraction_6 = as.data.frame(ifelse(hpreds$bare > 0.5, 11, 10)),
  fraction_7 = as.data.frame(ifelse(hpreds$water > 0.5, 13, 12))
)

hvals_distr <- data.frame(
  fraction_1 = as.data.frame(ifelse(hvals$tree > 0.5, 1, 0)),
  fraction_2 = as.data.frame(ifelse(hvals$shrub > 0.5, 3, 2)),
  fraction_3 = as.data.frame(ifelse(hvals$grassland > 0.5, 5, 4)),
  fraction_4 = as.data.frame(ifelse(hvals$crops > 0.5, 7, 6)),
  fraction_5 = as.data.frame(ifelse(hvals$urban_built_up > 0.5, 9, 8)),
  fraction_6 = as.data.frame(ifelse(hvals$bare > 0.5, 11, 10)),
  fraction_7 = as.data.frame(ifelse(hvals$water > 0.5, 13, 12))
)


num_rows <- nrow(hpreds_distr)

jaccard_sim <- numeric(num_rows)

for (i in 1:num_rows) {

  test1 <- hpreds_distr[i,]
  test2 <- hvals_distr[i,]

  # Exclude zero fractions
  valid_indices <- which(hvals[i,] != 0 | hpreds[i,] > 0.5)

  test1 <- as.matrix(test1[, valid_indices])
  test2 <- as.matrix(test2[, valid_indices])

  intersection <- length(intersect(test1, test2))
  union <- length(union(test1, test2))
  jaccard_similarity <- intersection / union

  # Add to total jaccard
  jaccard_sim[i] <- jaccard_similarity

}

print(colMeans(as.data.frame(jaccard_sim)))

