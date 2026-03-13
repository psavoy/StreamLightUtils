#' Bulk downloads NLDAS light data for multiple sites
#' @description This function downloads NLDAS incoming shortwave radiation data
#' (w m-2). When using the nldas_get function to download data from many sites, sometimes
#' sites may fail to download data (possibly due to volume of requests). This function
#' adds in an additional check to continually attempt to download data for any missing
#' locations until no more locations can be successfully downloaded. For example, sites
#' outside of the U.S. do not have NLDAS data and therefore will never download.
#' 
#' The startDate and endDate arguments may either both be specified in the function,
#' or provided as columns named "startDate" and "endDate" in the site_locs table.
#'
#' @param site_locs A table with Site_ID, Lat, and Lon. Optionally, also startDate and endDate.
#' @param startDate An optional parameter. By default, if nothing is provided the function
#' assumes that site_locs has a column that contains startDate. Alternatively, a single
#' startDate can be provided as an argument for the download (YYYY-MM-DD).
#' @param endDate An optional parameter. By default, if nothing is provided the function
#' assumes that site_locs has a column that contains endDate. Alternatively, a single
#' endDate can be provided as an argument for the download (YYYY-MM-DD).
#' @param access_token An Earthdata access token. Requires an Earthdata profile
#' https://urs.earthdata.nasa.gov/. From your account page click generate token
#' and copy the token into a local file, which will need to be read in as your
#' access_token. e.g. access_token <- readLines("path/token.txt")
#' @param save_dir The save directory for files to be placed in. For example, "C:/myfolder
#'
#' @return Returns a time series of incoming shortwave solar radiation from the start
#' date to the most recent available data
#'
#' @export
    
#===============================================================================
#Function for bulk downloading NLDAS shortwave radiation data via the giovanni api
#https://disc.gsfc.nasa.gov/information/documents?title=Giovanni%20In%20The%20Cloud:%20Time%20Series%20Service
#Created 3/11/2026
#===============================================================================
nldas_get_bulk <- function(site_locs, startDate, endDate, access_token, save_dir){
  #Download NLDAS data for all sites
    #Use function arguments for start and end date
    if(hasArg(startDate) == TRUE & hasArg(endDate) == TRUE){
      mapply(
        nldas_get,
        Site_ID = site_locs[, "Site_ID"],
        Lat = site_locs[, "Lat"],
        Lon = site_locs[, "Lon"],
        startDate = startDate,
        endDate = endDate,
        access_token = access_token,
        save_dir = save_dir
      )
    }
  
    #Read start and end date from site_locs
    if(hasArg(startDate) == FALSE & hasArg(startDate) == FALSE){
      mapply(
        nldas_get,
        Site_ID = site_locs[, "Site_ID"],
        Lat = site_locs[, "Lat"],
        Lon = site_locs[, "Lon"],
        startDate = site_locs[, "startDate"],
        endDate = site_locs[, "endDate"],
        access_token = access_token,
        save_dir = save_dir
      )
    }

  #Get sites with downloaded data
    dl_sites <- stringr::str_sub(list.files(save_dir)[grep("*_NLDAS.rds",
      list.files(save_dir))], 1, -11)

  #Check sites with missing data
    missing_sites <- site_locs[site_locs[, "Site_ID"] %in% setdiff(site_locs[, "Site_ID"],
      dl_sites), ]

  #Set the initial number of missing sites
    missing_number <- nrow(missing_sites)

  #If there are missing sites, retry downloading
    if(missing_number > 0){
      for(i in 1:missing_number){
        #Retry downloading NLDAS data
          #Use function arguments for start and end date
          if(hasArg(startDate) == TRUE & hasArg(endDate) == TRUE){
            mapply(
              nldas_get,
              Site_ID = missing_sites[, "Site_ID"],
              Lat = missing_sites[, "Lat"],
              Lon = missing_sites[, "Lon"],
              startDate = startDate,
              endDate = endDate,
              access_token = access_token,
              save_dir = save_dir
            )
          }
        
          #Read start and end date from missing_sites
          if(hasArg(startDate) == FALSE & hasArg(startDate) == FALSE){
            mapply(
              nldas_get,
              Site_ID = missing_sites[, "Site_ID"],
              Lat = missing_sites[, "Lat"],
              Lon = missing_sites[, "Lon"],
              startDate = missing_sites[, "startDate"],
              endDate = missing_sites[, "endDate"],
              access_token = access_token,
              save_dir = save_dir
            )
          }

        #Check the number of missing sites
          dl <- stringr::str_sub(list.files(save_dir)[grep("*_NLDAS.rds",
            list.files(save_dir))], 1, -11)

        #Find remaining missing sites
          ms <- site_locs[site_locs[, "Site_ID"] %in% setdiff(site_locs[, "Site_ID"],
            dl), ]

        #Find # of remaining missing sites
          mn <- nrow(ms)

        #Check to see if no more missing sites have been downloaded
          if(mn == missing_number) break()

        #Update the number of missing sites
          if(mn != missing_number){message("Attempting to download missing sites")}
          if(mn != missing_number){missing_number <- mn}

      } #End for loop

    } #End if statement

} #End nldas_get_bulk function
