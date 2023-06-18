# Import packages
library(sf)
library(dplyr)
library(pbapply)
source("utils/extractDates.R")
source("utils/filterBands.R")
source("utils/dataManagement.R")

# Link to data folder (adjust to where raw Landsat time series and reference fractions is stored)
linkData <- "Data/"

# Retrieve band layers
linkLandsatIIASAchange <- paste0(linkData, "raw/IIASAChange20152018_Landsat8_TS.gpkg")
nameBands <- st_layers(linkLandsatIIASAchange)

# Read in data per band
b1Landsat <- st_read(linkLandsatIIASAchange, nameBands$name[1])
b2Landsat <- st_read(linkLandsatIIASAchange, nameBands$name[2])
b3Landsat <- st_read(linkLandsatIIASAchange, nameBands$name[3])
b4Landsat <- st_read(linkLandsatIIASAchange, nameBands$name[4])
b5Landsat <- st_read(linkLandsatIIASAchange, nameBands$name[5])
b6Landsat <- st_read(linkLandsatIIASAchange, nameBands$name[6])
b7Landsat <- st_read(linkLandsatIIASAchange, nameBands$name[7])

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
ColDates = paste0("X", gsub("-", ".", dates), "_SR_B1")
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
DFcoords = b1Landsat[c("x","y")]
write.csv(DFcoords, paste0(linkData, "Processed/Training/LSTimeSeries/IIASAChangeCoords.csv"), row.names=F)

## TEST and EXPERIMENT BELOW
# Apply filter to remove outliers
b1Filtered <- filterBands(b1Landsat[1:1000,], smoothLoessPlot, dates)
mean(is.na(b1Filtered))
mean(is.na(b1Landsat[1:1000,NewColDates]))

# st_write and write.csv test
b1FilterTest = b1Filtered
coordsData = read.csv(paste0(linkData, "Processed/Training/LSTimeSeries/IIASAChangeCoords.csv"))
b1FilterTest = cbind(b1FilterTest, x=coordsData$x, y=coordsData$y)
coords = c("x","y")
tempSF = st_as_sf(b1FilterTest, coords=coords, dim="XY", remove=FALSE, crs=4326)
names(tempSF)[names(tempSF) == "geometry"] = "geom"
st_geometry(tempSF) = "geom"

# APPLY FILTER ON BAND 2 (BLUE)
b2Filtered <- filterBands(b2Landsat, smoothLoessPlot, dates) # takes some time...
mean(is.na(b2Filtered))
mean(is.na(b2Landsat[,NewColDates]))

# Remove new NAs for all bands
# Now done for the band itself...

# Apply b2 filter to all bands
b1Filtered = applyFilter(b1Landsat, b2Filtered)

mean(is.na(b2Filtered[,NewColDates])) # 45% NA
mean(is.na(b1Landsat[,NewColDates])) # 43% NA
mean(is.na(b1Filtered[,NewColDates])) # 45% NA

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
st_write(b1FilteredSF, paste0(linkData,"Processed/Training/LSTimeSeries/IIASAChangeFiltered.gpkg"), "b1")
st_write(b2FilteredSF, paste0(linkData,"Processed/Training/LSTimeSeries/IIASAChangeFiltered.gpkg"), "b2")
st_write(b3FilteredSF, paste0(linkData,"Processed/Training/LSTimeSeries/IIASAChangeFiltered.gpkg"), "b3")
st_write(b4FilteredSF, paste0(linkData,"Processed/Training/LSTimeSeries/IIASAChangeFiltered.gpkg"), "b4")
st_write(b5FilteredSF, paste0(linkData,"Processed/Training/LSTimeSeries/IIASAChangeFiltered.gpkg"), "b5")
st_write(b6FilteredSF, paste0(linkData,"Processed/Training/LSTimeSeries/IIASAChangeFiltered.gpkg"), "b6")
st_write(b7FilteredSF, paste0(linkData,"Processed/Training/LSTimeSeries/IIASAChangeFiltered.gpkg"), "b7")

# test multiple layers
st_read(paste0(linkData, "Processed/Training/LSTimeSeries/IIASAChangeFiltered.gpkg"), "b2")

