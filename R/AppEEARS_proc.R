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
#' @param unpacked_LAI Output from the AppEEARS_unpack_QC function
#' @param fit_method There are several options available from the phenofit package including
#' AG", "Beck", "Elmore", "Gu", "Klos", "Zhang". 
#' @param Plot Binary, where plot = TRUE generates a plot and plot = FALSE does not
#' @param write_output Binary indicating whether to write each individual driver
#' file to disk. Default value is FALSE.
#' @param save_dir Optional parameter when write_output = TRUE. The save directory 
#' for files to be placed in. For example, "C:/
#' 
#' @export

#===============================================================================
#Function for filtering and gap-filling downloaded MODIS data
#Created 8/13/2018
#Updated 7/14/2020 to include phenofit
#===============================================================================
AppEEARS_proc <- function(unpacked_LAI, fit_method, plot = FALSE, write_output = FALSE, save_dir = NULL){
  #Internal wrapper to cycle over sites
    site_proc <- function(Site, fit_method, plot, write_output, save_dir){
      #Get the site of interest
        SOI <- unpacked_LAI[[Site]]
      
      #If fit_method == phenofit, smoothing using Whittaker smoother and fit using an
      #assymetric Gaussian function in the phenofit package
        if(fit_method %in% c("AG", "Beck", "Elmore", "Gu", "Klos", "Zhang")){processed <- LAI_proc_phenofit(SOI, fit_method = fit_method)}
      
      # #If fit_method == spline, use the spline function
      #   if(fit_method == "spline"){processed <- LAI_proc_spline(SOI)}
      # 
      # #If pro_type == dbl_log use the double logistic function
      #   if(fit_method == "dbl_log"){processed <- LAI_proc_dbl(SOI)}
    
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
    
      #-------------------------------------------------
      #Saving or returning the output
      #-------------------------------------------------
        #If write_output == TRUE, save the driver to disk
          if(write_output == TRUE){
            saveRDS(processed, paste0(save_dir, "/", Site, "_LAI_processed.rds"))
          } else{
            return(processed)
          } #End if else statement    
        
    } #End site_proc function
 
  #Apply the function to 
    if(write_output == TRUE){
      lapply(
        names(unpacked_LAI), 
        FUN = site_proc, 
        fit_method = fit_method,
        plot = plot,
        write_output = write_output,
        save_dir = save_dir
      )
    } else{
      LAI_final <- lapply(
        names(unpacked_LAI), 
        FUN = site_proc, 
        fit_method = fit_method,
        plot = plot,
        write_output = write_output,
        save_dir = save_dir
      )
      
      names(LAI_final) <- names(unpacked_LAI)
      
      return(LAI_final)
      
    } #End if else statement
    
} #End AppEEARS_proc function