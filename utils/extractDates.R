writeDates = function(){
  
  string = colnames(b1Landsat)[4:194]                                           # (from Landsat band 1, get only the date columns)
  stringSub = substr(string, 2, 11)                                             # (in the dates, remove non-date characters)    
  stringFinal = gsub("\\.", "-", stringSub)                                     # (in the dates, change points to hyphens)
  DFdates = data.frame(date=as.Date(stringFinal))                               # (read the dates as actual dates)
  write.csv(DFdates, paste0(linkData, "Processed/dates.csv"), row.names=FALSE)  # (write dates to csv)
  
  return(TRUE)
}

extractDates = function(){
  
  DateCSV = read.csv(file="Processed/dates.csv")
  dates = as.Date(DateCSV$date)
  
  return(dates)
}