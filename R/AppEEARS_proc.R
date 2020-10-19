#' Filters and gap-fills downloaded MODIS AppEEARS data
#' @description This function filters and gap-fills downloaded MODIS LAI data from
#' the lpdaac AppEEARS download portal. There are several choices for processing
#' this data:
#'
#' \itemize{
#'   \item The LAI data is processed using the phenofit R package 
#'   https://github.com/kongdd/phenofit. A rough fit is done using a weighted
#'   Whittaker smoother and then curve fitting is done using an assymetric
#'   Gaussian function. 
#'   \item The LAI data is processed by using a Savitsky-Golay filter to remove
#'   noise and then missing data is interpolated with a spline.
#'   \item The LAI data is processed by using a Savitsky-Golay filter to remove
#'   noise and then missing data is interpolated by fitting a double logistic curve
#'   using the greenbrown R package.
#' }
#'
#' @param Site The site name
#' @param fit_method There are several options available from the phenofit package including
#' AG", "Beck", "Elmore", "Gu", "Klos", "Zhang". Additionally, "spline" is available #'
#' @param Plot Binary, where plot = TRUE generates a plot and plot = FALSE does not
#' @export

#===============================================================================
#Function for filtering and gap-filling downloaded MODIS data
#Created 8/13/2018
#Updated 7/14/2020 to include phenofit
#===============================================================================
AppEEARS_proc <- function(Site, fit_method, plot = FALSE){
  #If fit_method == phenofit, smoothing using Whittaker smoother and fit using an
  #assymetric Gaussian function in the phenofit package
    if(fit_method %in% c("AG", "Beck", "Elmore", "Gu", "Klos", "Zhang")){processed <- LAI_proc_phenofit(Site, fit_method)}
  
  #If fit_method == spline, use the spine function
    if(fit_method == "spline"){processed <- LAI_proc_spline(Site)}

  #If pro_type == dbl_log use the double logistic function
    if(fit_method == "dbl_log"){processed <- LAI_proc_dbl(Site)}

  #Calculating some fitting statistics for the Savitzky-Golay filter
    proc_na_rm <- na.omit(processed)
    r2 <- round(summary(lm(proc_na_rm[, "Lai"] ~ proc_na_rm[, "LAI_proc"]))$adj.r.squared, 2)
    RMSE <- round(sqrt(mean((proc_na_rm[, "Lai"] - proc_na_rm[, "LAI_proc"])^2 , na.rm = TRUE )), 2)
    MAE <- round(mean(abs(proc_na_rm[, "Lai"] - proc_na_rm[, "LAI_proc"]), na.rm = TRUE), 2)

  #-------------------------------------------------
  #Plotting the results
  #-------------------------------------------------
  if(plot == TRUE){
    #Observed MODIS data
      ylabel <- expression(paste("LAI ", "(m"^{2}, "m"^{-2},")")) #Set y label

      plot(processed[, "Date"], processed[, "Lai"], pch = 20, col = "grey60",
        ylim = c(0, 7), xlab = "Time (Days)", ylab = ylabel, main = paste("Site= ",
        unique(Site[, "ID"])," r2 = ", r2, " ;RMSE = ", RMSE, " ;MAE = ", MAE, sep = ""))

    #Continuous daily LAI (from spline fit to SGF filtered data)
      lines(processed[, "Date"], processed[, "LAI_proc"], col = "black", lwd = 2)      
  } #End if statement

  return(processed)

} #End AppEEARS_proc function
