#' Compiles downloaded NLDAS data in preparation for making driver files
#' @description This function compiles all of the downloaded NLDAS data into
#' a list, with each element named with the Site_ID. This step is in preparation
#' for making driver files using the make_driver function.
#'
#' @param read_dir The read directory for downloaded files. For example, "C:/myfolder
#' @param write_output Logical value indicating whether to write each individual driver
#' file to disk. Default value is FALSE.
#' @param save_dir Optional parameter when write_output = TRUE. The save directory 
#' for files to be placed in. For example, "C:/
#' 
#' @return Returns a list of downloaded NLDAS data, named by Site_ID
#' @export

#===============================================================================
#Create a named list of downloaded NLDAS data in prep for making driver files
#Created 3/11/2026
#===============================================================================
nldas_prep <- function(read_dir, write_output = FALSE, save_dir = NULL){
  #Get downloaded files
    downloaded_files <- list.files(read_dir)
    
  #Get site ids
    site_ids <- stringr::str_sub(downloaded_files, 1, -11)

  #Compile downloaded NLDAS data into a list
    compiled <- lapply(
      FUN = function(x){readRDS(paste0(read_dir, "/", x, "_NLDAS.rds"))},
      site_ids
    )
    
    names(compiled) <- site_ids
    
  #Write or return output
  if(write_output == TRUE){
    saveRDS(
      compiled,
      paste0(save_dir, "/", "NLDAS_compiled.rds")
    )
  } else{
    return(compiled)  
  }
  
} #end nldas_prep function

