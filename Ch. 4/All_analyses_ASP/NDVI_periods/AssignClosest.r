


# FUNCTION TO ASSIGN CLOSEST DATE

# Adds to the original dataset with positions a column 
# with the name of the raster that has the closest date to the position


assign.closest <- function(data_positions, # Data file with coordinates and date (columns Longitud, Latitude, Date)
                           int){ # Matrix with date intervals
  
  require(lubridate)
  
  int <- list() # Create object with format intervals (to evaluate later with %within%)
  for  (i in 1:nrow(mat)){
    int[[i]] <- interval(ymd(mat[i,1]), ymd(mat[i,2]))
  }
  
  
  # ASSIGN DATE TO CLOSEST RASTER
  
  data_positions$closest_date <- NA
  data_positions$closest_date <- as.Date(data_positions$closest_date)
  
  for (i in 1:nrow(data_positions)){ 
    
    # Date to assign to raster
    
    d <- data_positions$Date[i] 
    
    
    # Vector with the interval that contains the date
    
    within.int <- NULL 
    
    for (j in 1:length(int)){
      within.int <- c(within.int, ymd(d) %within% int[[j]]) 
    }
    int.date <- mat[within.int == TRUE, ]
    
    if (sum(int.date == d) == 2) { 
      
      data_positions[i,4] <- d }else{
        
        # Get time differences
        
        dif1 <- abs(as.numeric(difftime(d, int.date[1], units = "days")))
        dif2 <- abs(as.numeric(difftime(d, int.date[2], units = "days")))
        compare_dif <- c(dif1,dif2)
        data_positions[i,4] <- int.date[compare_dif == min(compare_dif)]
      }
  }
  return(data_positions)
}

