# Import packages
library(sf)
library(dplyr)
library(pbapply)
source("C:/Users/augus/Documents/Studie/MGI/Thesis/Rob's/lcfMapping-main/lcfMapping-main/utils/extractDates.R")
source("C:/Users/augus/Documents/Studie/MGI/Thesis/Rob's/lcfMapping-main/lcfMapping-main/utils/filterBands.R")
source("C:/Users/augus/Documents/Studie/MGI/Thesis/Rob's/lcfMapping-main/lcfMapping-main/utils/dataManagement.R")

# Link to data folder
linkData <- "C:/Users/augus/Documents/Studie/MGI/Thesis/Rob's/Data/"

# Retrieve band layers
linkLandsatIIASAChange <- paste0(linkData, "raw/IIASAChange20152018_Landsat8_TS.gpkg")
nameBands <- st_layers(linkLandsatIIASAChange)

# Read in data per band
b1Landsat <- st_read(linkLandsatIIASAChange, nameBands$name[1])
b2Landsat <- st_read(linkLandsatIIASAChange, nameBands$name[2])
b3Landsat <- st_read(linkLandsatIIASAChange, nameBands$name[3])
b4Landsat <- st_read(linkLandsatIIASAChange, nameBands$name[4])
b5Landsat <- st_read(linkLandsatIIASAChange, nameBands$name[5])
b6Landsat <- st_read(linkLandsatIIASAChange, nameBands$name[6])
b7Landsat <- st_read(linkLandsatIIASAChange, nameBands$name[7])

# Remove geometries
st_geometry(b1Landsat) = NULL
st_geometry(b2Landsat) = NULL
st_geometry(b3Landsat) = NULL
st_geometry(b4Landsat) = NULL
st_geometry(b5Landsat) = NULL
st_geometry(b6Landsat) = NULL
st_geometry(b7Landsat) = NULL

# Get Dates
dates = extractDates()
NewColDates = paste0("X", gsub("-", ".", dates))

# change column names to make them match with other bands
colnames(b1Landsat)[4:194] = NewColDates
colnames(b2Landsat)[4:194] = NewColDates
colnames(b3Landsat)[4:194] = NewColDates
colnames(b4Landsat)[4:194] = NewColDates
colnames(b5Landsat)[4:194] = NewColDates
colnames(b6Landsat)[4:194] = NewColDates
colnames(b7Landsat)[4:194] = NewColDates

# Get XY coords
DFcoords = b1Landsat[c("centroid_x","centroid_y")]
colnames(DFcoords)[1] = "x"
colnames(DFcoords)[2] = "y"
write.csv(DFcoords, paste0(linkData, "Processed/IIASAChangeCoords.csv"), row.names=F)

# APPLY FILTER ON BAND 2 (BLUE)
b2Filtered <- filterBands(b2Landsat, smoothLoessPlot, dates) # takes some time...
mean(is.na(b2Filtered))
mean(is.na(b2Landsat[,NewColDates]))

# Write and store Filtered band as SF
b2FilteredSF <- DFtoSF(b2Filtered)
# st_write(b2FilteredSF, "C:/Users/augus/Documents/Studie/MGI/Thesis/Rob's/Data/Processed/b2Filtered.gpkg")

# Remove new NAs for all bands
# Now done for the band itself...

# Apply b2 filter to all bands
b1Filtered = applyFilter(b1Landsat, b2Filtered)

mean(is.na(b2Filtered[,NewColDates])) 
mean(is.na(b1Landsat[,NewColDates]))
mean(is.na(b1Filtered[,NewColDates])) 

b3Filtered = applyFilter(b3Landsat, b2Filtered)
b4Filtered = applyFilter(b4Landsat, b2Filtered)
b5Filtered = applyFilter(b5Landsat, b2Filtered)
b6Filtered = applyFilter(b6Landsat, b2Filtered)
b7Filtered = applyFilter(b7Landsat, b2Filtered)

# Change to SF format
b1FilteredSF <- DFtoSF(b1Filtered)
b2FilteredSF <- DFtoSF(b2Filtered)
b3FilteredSF <- DFtoSF(b3Filtered)
b4FilteredSF <- DFtoSF(b4Filtered)
b5FilteredSF <- DFtoSF(b5Filtered)
b6FilteredSF <- DFtoSF(b6Filtered)
b7FilteredSF <- DFtoSF(b7Filtered)

# Save as one gpkg with multiple layers
st_write(b1FilteredSF, paste0(linkData,"Processed/IIASAChangeFiltered.gpkg"), "b1")
st_write(b2FilteredSF, paste0(linkData,"Processed/IIASAChangeFiltered.gpkg"), "b2")
st_write(b3FilteredSF, paste0(linkData,"Processed/IIASAChangeFiltered.gpkg"), "b3")
st_write(b4FilteredSF, paste0(linkData,"Processed/IIASAChangeFiltered.gpkg"), "b4")
st_write(b5FilteredSF, paste0(linkData,"Processed/IIASAChangeFiltered.gpkg"), "b5")
st_write(b6FilteredSF, paste0(linkData,"Processed/IIASAChangeFiltered.gpkg"), "b6")
st_write(b7FilteredSF, paste0(linkData,"Processed/IIASAChangeFiltered.gpkg"), "b7")

# test multiple layers
st_read(paste0(linkData, "Processed/IIASAChangeFiltered.gpkg"), "b2")
