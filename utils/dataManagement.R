# Function to convert Dataframe to SF class
DFtoSF <- function(DF, coords = c("x","y"), validation=FALSE){
  
  # Add coordinates if necessary
  if (validation==FALSE){
    if (!"x" %in% colnames(DF) && !"y" %in% colnames(DF)){
      if (!exists("coordsData")){
        # coordsData = read.csv(paste0(linkData, "Processed/IIASAtrainingCoords.csv"))
        # coordsData = read.csv(paste0(linkData, "Processed/IIASAChangeCoords.csv"))
        coordsData = read.csv(paste0(linkData, "Processed/WURChangeCoords.csv"))
      }
      DF = cbind(DF, x=coordsData$x, y=coordsData$y)
    } 
  }
  
  # Convert to SF data with geometry
  tempSF = st_as_sf(DF, coords=coords, dim="XY", remove=FALSE, crs=4326)
  names(tempSF)[names(tempSF) == "geometry"] = "geom"
  st_geometry(tempSF) = "geom"
  
  return(tempSF)
}

