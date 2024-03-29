#' Processes downloaded NLDAS light data
#' @description This function processes downloaded NLDAS incoming shortwave radiation
#' data (w m-2) for a given Latitude and Longitude.
#'
#' @param read_dir The read directory for downloaded files. For example, "C:/myfolder
#' @param Site_IDs Site name(s), for example "NC_UEno"
#' @param write_output Logical value indicating whether to write each individual driver
#' file to disk. Default value is FALSE.
#' @param save_dir Optional parameter when write_output = TRUE. The save directory 
#' for files to be placed in. For example, "C:/
#' 
#' @return Returns a time series of incoming light data
#' @export

#===============================================================================
#Function for processing the downloaded data
#Created 11/27/2017
#Last updated 11/13/2018
#===============================================================================
NLDAS_proc <- function(read_dir, Site_IDs, write_output = FALSE, save_dir = NULL){
  #Get a list of all downloaded NLDAS data
    setwd(read_dir)
    downloaded <- list.files(read_dir)[grep("*_NLDAS.asc", list.files(read_dir))]

  #Get the names of downloaded sites
    downloaded_names <- stringr::str_sub(downloaded, 1, -11)

  #Function for processing each site
    NLDAS_site <- function(file_name, write_output, save_dir){
      #Reading in the table, skipping the first 40 lines of header information
      #and removing the last row which contains a calculated mean value
        nldas <- read.table(file_name, skip = 40, nrows = length(readLines(file_name,
          warn = FALSE)) - 41)

          colnames(nldas) <- c("Date", "hour_raw", "light")

      #Adding in date and time information
        #Extracting the hour information
          nldas[, "Time"] <- as.numeric(substr(nldas[,"hour_raw"], 1, 2))

        #Adding a POSIX time column
          nldas[, "pos_time"] <- as.POSIXct(paste(nldas[, "Date"], " ",
            as.matrix(sprintf("%02d", nldas[, "Time"])), sep = ""), format = "%Y-%m-%d %H",
            tz = "UTC")

        #Adding in Year, DOY, and hour information
          nldas[, "Year"] <- as.numeric(format(nldas[, "pos_time"], format = "%Y", tz = "UTC"))
          nldas[, "DOY"] <- as.numeric(format(nldas[, "pos_time"], format = "%j", tz = "UTC"))
          nldas[, "Hour"] <- as.numeric(format(nldas[, "pos_time"], format = "%H", tz = "UTC"))

        #Selecting the final column
          final <- nldas[, c("Year", "DOY", "Hour", "light")]
            colnames(final)[4] <- "SW"

        #If write_output == TRUE, save the driver to disk
          if(write_output == TRUE){
            saveRDS(final, paste0(save_dir, "/", stringr::str_sub(file_name, 1, -11), "_NLDAS_processed.rds"))
          } else{
            return(final)
          } #End if else statement

    } #End NLDAS_site function

  #Apply the function to make all driver files
    if(write_output == TRUE){
      lapply(downloaded, FUN = NLDAS_site, write_output = write_output, save_dir = save_dir)
    } else{
      processed <- lapply(downloaded, FUN = NLDAS_site, write_output = write_output, save_dir = save_dir)
        names(processed) <- downloaded_names
      
      return(processed)
    } #End if else statement    

  #Notify the user with a list of sites that did not have data
    missing <- Site_IDs[!(Site_IDs %in% downloaded_names)]

    if(length(missing) != 0){
      print(paste("The following sites did not successfully download NLDAS data:",
        paste(missing, sep = "", collapse = ", ")))
    } #End if statement

} #End NLDAS_proc
