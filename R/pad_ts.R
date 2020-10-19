#' Pad a timeseries with a year at the start and the end
#' @description This function pads a timeseries with a year of data at the start
#' and the end of the timeseries. One year of LAI data is added to the start and end
#' of the timeseries based on median daily LAI for the timeserires. This is done
#' so that a smoothed seasonal cycle can be retrieved from the timeseries using
#' the phenofit R package for the entire timeseries. .
#'
#' @param ts timeseries
#' @param t_res The temporal resolution of the LAI data
#' 
#' @return Returns a timeseries with an additional year added to the start and
#' end of the timeseries
#' @export

#===============================================================================
#Function to pad the timeseries with a year at the start and the end
#Created 7/10/2020
#===============================================================================
pad_ts <- function(ts, t_res){
  #Find the start and end date
    date_range <- range(ts[, "date"])
    
  #Calculate daily median LAI values across the entire timeseries
    median_lai <- aggregate(Lai ~ DOY, data = ts, FUN = median, na.rm = TRUE)
    
  #-------------------------------------------------
  #Create the prequel timeseries  
  #-------------------------------------------------
    #Generate a set of days for the previous year (365 days)
      preq_dates <- seq.Date(from = date_range[1] - 365, to = date_range[1] - t_res, by = "day")
    
    #Add DOY
      preq_df <- setNames(data.frame(preq_dates, as.numeric(strftime(preq_dates, format = "%j"))), 
        c("date", "DOY"))
    
    #Merge together median LAI values
      prequel <- merge(preq_df, median_lai)
        prequel$w <- 1
        
  #-------------------------------------------------
  #Create the sequel timeseries  
  #-------------------------------------------------
    #Generate a set of days for the previous year (365 days)
      seq_dates <- seq.Date(from = date_range[2] + t_res, to = date_range[2] + 365, by = "day")
    
    #Add DOY
      seq_df <- setNames(data.frame(seq_dates, as.numeric(strftime(seq_dates, format = "%j"))), 
        c("date", "DOY"))
    
    #Merge together median LAI values
      sequel <- merge(seq_df, median_lai)  
        sequel$w <- 1
  
  #-------------------------------------------------
  #Create the final padded timeseries    
  #-------------------------------------------------
    bound <- rbind(sequel, ts[, c("DOY", "date", "Lai", "w")], prequel)
      
    final <- bound[order(bound[, "date"]), ]

  return(final)
  
} #End pad_ts