## In this script vali data is subsetted based on change for error measurement in change areas.
# Furthermore, yearly data frames are created in order to calculate errors per year

# Link to data folder
linkData <- "Data/"

# Full validation for all years
vali <- read.csv(paste0(linkData, "Processed/Validation/vali20152018.csv"))

## Change samples based by scanning for change in records
library(dplyr)

# Group the data frame by location_id and check if there is any change in value1 or value2 within each group
vali_change <- vali %>%
  group_by(location_id) %>%
  filter(n_distinct(tree) > 1 | n_distinct(shrub) > 1 | n_distinct(grassland) > 1 | n_distinct(crops) > 1 |
           n_distinct(urban_built_up) > 1 | n_distinct(bare) > 1 | n_distinct(water) > 1) %>%
  ungroup()

# write to csv
write.csv(vali_change, paste0(linkData, "Processed/Validation/vali_change.csv"))

vali_change2015 <- subset(vali_change, dataYear == 2015)
vali_change2016 <- subset(vali_change, dataYear == 2016)
vali_change2017 <- subset(vali_change, dataYear == 2017)
vali_change2018 <- subset(vali_change, dataYear == 2018)

write.csv(vali_change2015, paste0(linkData, "Processed/Validation/Change Validation/v_change2015.csv"))
write.csv(vali_change2016, paste0(linkData, "Processed/Validation/Change Validation/v_change2016.csv"))
write.csv(vali_change2017, paste0(linkData, "Processed/Validation/Change Validation/v_change2017.csv"))
write.csv(vali_change2018, paste0(linkData, "Processed/Validation/Change Validation/v_change2018.csv"))

## "Possible" change
# These were collected for sites with 'possible' change, even though a lot do not have changing fractions (?)

orig_vali2015 <- read.csv(paste0(linkData, "Processed/Validation/2015/vali2015.csv"))
non_orig_vali2015 <- orig_vali2015[!orig_vali2015$collection == "original",]

# Subset change validation data frame
possible_change <- vali[vali$location_id %in% non_orig_vali2015$location_id, ]

# Let's see how many actually had recorded change
possible_real_change <- possible_change %>%
  group_by(location_id) %>%
  filter(n_distinct(tree) > 1 | n_distinct(shrub) > 1 | n_distinct(grassland) > 1 | n_distinct(crops) > 1 |
           n_distinct(urban_built_up) > 1 | n_distinct(bare) > 1 | n_distinct(water) > 1) %>%
  ungroup()

# only 36% (so Why use it??)

# possible_real_change has records 3,203 samples, vali_change records 3,614 samples, indicating that it even misses actual change samples.  

# Write to file anyway
write.csv(possible_change, paste0(linkData, "Processed/Validation/possible_change.csv"))

vali_p_change2015 <- subset(possible_change, dataYear == 2015)
vali_p_change2016 <- subset(possible_change, dataYear == 2016)
vali_p_change2017 <- subset(possible_change, dataYear == 2017)
vali_p_change2018 <- subset(possible_change, dataYear == 2018)

write.csv(vali_p_change2015, paste0(linkData, "Processed/Validation/Possible Change Validation/p_change2015.csv"))
write.csv(vali_p_change2016, paste0(linkData, "Processed/Validation/Possible Change Validation/p_change2016.csv"))
write.csv(vali_p_change2017, paste0(linkData, "Processed/Validation/Possible Change Validation/p_change2017.csv"))
write.csv(vali_p_change2018, paste0(linkData, "Processed/Validation/Possible Change Validation/p_change2018.csv"))

# Count when change mostly happens

change_vali <- read.csv(paste0(linkData, "Processed/Validation/vali_change.csv"))

df_grouped <- change_vali %>%
  group_by(location_id) %>%
  mutate(tree_change = ifelse(tree != lag(tree, default = 0), 1, 0),
         shrub_change = ifelse(shrub != lag(shrub, default = 0), 1, 0),
         grassland_change = ifelse(grassland != lag(grassland, default = 0), 1, 0),
         crops_change = ifelse(crops != lag(crops, default = 0), 1, 0),
         urban_change = ifelse(urban_built_up != lag(urban_built_up, default = 0), 1, 0),
         bare_change = ifelse(bare != lag(bare, default = 0), 1, 0),
         water_change = ifelse(water != lag(water, default = 0), 1, 0))

df_summarized <- df_grouped %>%
  group_by(dataYear) %>%
  summarize(tree_changes = sum(tree_change),
            shrub_changes = sum(shrub_change),
            grassland_changes = sum(grassland_change),
            crops_changes = sum(crops_change),
            urban_changes = sum(urban_change),
            bare_changes = sum(bare_change),
            water_changes = sum(water_change))

df_summarized$Sum <- rowSums(df_summarized)

# Count class prevalence
vali <- read.csv(paste0(linkData, "Processed/Validation/vali20152018.csv"))
vali_grouped <- vali %>% group_by(location_id)

vali_grouped <- vali_grouped %>% 
  mutate(tree_present = if_else(any(tree > 0), 1, 0),
         shrub_present = if_else(any(shrub > 0), 1, 0),
         grassland_present = if_else(any(grassland > 0), 1, 0),
         crops_present = if_else(any(crops > 0), 1, 0),
         urban_present = if_else(any(urban_built_up > 0), 1, 0),
         bare_present = if_else(any(bare > 0), 1, 0),
         water_present = if_else(any(water > 0), 1, 0))

tree_prevalence <- vali_grouped %>% summarize(tree_prevalence = sum(tree_present)/n())
shrub_prevalence <- vali_grouped %>% summarize(shrub_prevalence = sum(shrub_present)/n())
grassland_prevalence <- vali_grouped %>% summarize(grassland_prevalence = sum(grassland_present)/n())
crops_prevalence <- vali_grouped %>% summarize(crops_prevalence = sum(crops_present)/n())
urban_prevalence <- vali_grouped %>% summarize(urban_prevalence = sum(urban_present)/n())
bare_prevalence <- vali_grouped %>% summarize(bare_prevalence = sum(bare_present)/n())
water_prevalence <- vali_grouped %>% summarize(water_prevalence = sum(water_present)/n())

tree_prevalence <- colSums(as.data.frame(tree_prevalence))
tree_prevalence
shrub_prevalence <- colSums(as.data.frame(shrub_prevalence))
shrub_prevalence
grassland_prevalence <- colSums(as.data.frame(grassland_prevalence))
grassland_prevalence
crops_prevalence <- colSums(as.data.frame(crops_prevalence))
crops_prevalence
urban_prevalence <- colSums(as.data.frame(urban_prevalence))
urban_prevalence
bare_prevalence <- colSums(as.data.frame(bare_prevalence))
bare_prevalence
water_prevalence <- colSums(as.data.frame(water_prevalence))
water_prevalence

# Calculate average class fraction
all_vali <- read.csv(paste0(linkData, "Processed/Validation/vali20152018.csv"))
change_vali <- read.csv(paste0(linkData, "Processed/Validation/vali_change.csv"))

all_vali_avg <- as.data.frame(colMeans(all_vali))
change_vali_avg <- as.data.frame(colMeans(change_vali))



