#' Assigns weights to LAI data based on the 5-level QC score
#' @description This function unpacks downloaded MODIS LAI data from the lpdaac
#' AppEEARS download portal into a list with each item in the list representing
#' a single site. 
#'
#' @param SCF_QC The 5-level QC score (FparLai_QC_SCF_QC)
#' @param wmin The minimum weight for a data point
#' @param wmid The middle weight for a data point
#' @param wmax The maximum weight for a data point (best quality)
#'
#' @return Returns weights based on the 5-level QC score
#' @export
#' 
#===============================================================================
#Define my version of the weights function (adapted from phenofit)   
#Created 7/10/2020
#===============================================================================
QC_weights <- function(SCF_QC, wmin = 0.2, wmid = 0.5, wmax = 1){
  #Create a blank vector of weights
    weights <- rep(NA, length(SCF_QC)) #default zero
   
  #Assign weights based on the 5-level confidence score
    weights[SCF_QC %in% c("000", "001")] <- wmax
    weights[SCF_QC %in% c("010", "011")] <- wmid
    weights[SCF_QC %in% c("100")] <- wmin
  
  return(weights)
    
} #End QC_weights function