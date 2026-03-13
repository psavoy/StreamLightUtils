#' Downloads NLDAS light data
#' @description This function downloads NLDAS incoming shortwave radiation data
#' (w m-2) for a given Latitude and Longitude.
#'
#' @param Site_ID The site ID, for example "NC_NHC"
#' @param Lat The site Latitude
#' @param Lon The site Longitude
#' @param startDate The starting date for the download (YYYY-MM-DD)
#' @param endDate The ending date for the download (YYYY-MM-DD). Default value is 
#' the current date via Sys.Date().
#' @param access_token An Earthdata access token. Requires an Earthdata profile
#' https://urs.earthdata.nasa.gov/. From your account page click generate token
#' and copy the token into a local file, which will need to be read in as your
#' access_token. e.g. access_token <- readLines("path/token.txt")
#' @param save_dir The save directory for files to be placed in. For example, "C:/myfolder
#'
#' @return Returns a time series of incoming shortwave solar radiation from the start
#' date to the end date.
#' @export

#===============================================================================
#Function for downloading NLDAS shortwave radiation data via the giovanni api
#https://disc.gsfc.nasa.gov/information/documents?title=Giovanni%20In%20The%20Cloud:%20Time%20Series%20Service
#Created 3/11/2026
#===============================================================================
nldas_get <- function(Site_ID, Lat, Lon, startDate, endDate = Sys.Date(), access_token, save_dir){
  #The initial string to build the URL
    http_string <- "https://api.giovanni.earthdata.nasa.gov/timeseries?data=NLDAS_FORA0125_H_2_0_"
  
  #Create location and time strings
    location_string <- paste0("&location=[", Lat, ",", Lon, "]")
    timeframe_string <- paste0("&time=", startDate, "T00:00:00/", endDate, "T23:00:00")
  
  #Generate the url
    url <- paste0(http_string, "SWdown", location_string, timeframe_string, "&version=2.0")
  
  #Try the request
    req_resp <- try(
      request(url) |>
        req_auth_bearer_token(access_token) |> 
        req_perform()
    )

  #Handle request or throw error message
  if(class(req_resp) == "try-error"){
    message("asdf")
  } else{
    #Get NLDAS data
      nldas <- req_resp |>
        resp_body_raw() |> 
        readr::read_csv(skip = 14, col_names = TRUE, show_col_types = FALSE) |>
        data.frame()  

    #Set column names  
      names(nldas) <- c("pos_time", "SW")
    
    #Adding in Year, DOY, and hour information
      nldas[, "Year"] <- as.numeric(format(nldas[, "pos_time"], format = "%Y", tz = "UTC"))
      nldas[, "DOY"] <- as.numeric(format(nldas[, "pos_time"], format = "%j", tz = "UTC"))
      nldas[, "Hour"] <- as.numeric(format(nldas[, "pos_time"], format = "%H", tz = "UTC"))
    
    #Selecting the final column
      final <- nldas[, c("Year", "DOY", "Hour", "SW")] 
      
    #Export the final output
      saveRDS(
        final,
        paste0(save_dir, "/", Site_ID, "_NLDAS.rds")
      )
      
  } #end if statement

} #end nldas_get function
