#' Interpolates LAI into daily values
#' @description This function generates a continuous series of LAI from downloaded
#' from the lpdaac AppEEARS download portal. This is done through the phenofit
#' R package https://github.com/kongdd/phenofit. A rough fit is done using a
#' weighted Whittaker smoother and then curve fitting is done using an assymetric
#' Gaussian function.
#'
#' \itemize{
#'   \item Dongdong Kong, R package: A state-of-the-art Vegetation Phenology 
#'   extraction package, phenofit version 0.2.2, https://github.com/kongdd/phenofit
#'
#'
#'   \item Zhang, Q., Kong, D., Shi, P., Singh, V.P., Sun, P., 2018. Vegetation phenology 
#'   on the Qinghai-Tibetan Plateau and its response to climate change (1982–2013). 
#'   Agric. For. Meteorol. 248, 408–417. https://doi.org/10.1016/j.agrformet.2017.10.026
#' }
#' 
#' @param Site The Site ID
#' @param fit_method The curve fitting method to pass onto phenofit ("AG", "Beck", 
#' "Elmore", "Gu", "Klos", "Zhang")
#' @export
#' 
#===============================================================================
#Function for LAI smoothing and curve fitting using the phenofit package
#Created 7/13/2020
#===============================================================================
LAI_proc_phenofit <- function(Site, fit_method){
  #
    ts <- Site
    
  #Add a date column
    ts$date <- as.Date(ts[, "pos_time"])
      
  #Calculate weights from 5-level QC score
    ts$w <- QC_weights(SCF_QC = ts[, "FparLai_QC_SCF_QC"], wmin = 0.2, wmid = 0.5, 
      wmax = 1)    
    
  #Calculate the temporal resolution of the dataset
    date_diff <- diff(ts[, "date"], lag = 1)
    uniqv <- unique(date_diff)
    t_res <- uniqv[which.max(tabulate(match(date_diff, uniqv)))] 
    
  #Pad the timeseries with a year at the start and the end
    ts_padded <- pad_ts(ts = ts, t_res = t_res)
    
  #Set fill values as NA. Note, for right now this only catches the 255 fill value
  #since that is the only one that has cropped up. Could adjust to 249-255 if needed. #PS 2022
    ts_padded[ts_padded[, "Lai"] == 255, "Lai"] <- NA
  
  #-------------------------------------------------  
  #Perform the LAI smoothing & curve fitting
  #-------------------------------------------------
    #Get # of scenes per year
      scene_count <- 365 / t_res  
    
    #Check the input LAI data
    #ECOSYSTEM OF CODE NEEDS TO BE UPDATED SO THE SOUTH PARAMETER VARIES BASED ON SITE LOCATION
      lai_input <- phenofit::check_input(
        t = ts_padded$date, 
        y = ts_padded$Lai, 
        w = ts_padded$w, 
        nptperyear = scene_count, 
        south = FALSE, 
        maxgap = scene_count / 4, 
        alpha = 0.02, 
        wmin = 0.2
      )    
      
    #Divide the growing season
    #MAY WANT TO CHECK ON LAMBDA VALUE IN THE FUTURE
      breaks <- phenofit::season_mov(
        lai_input, 
        options = list(
          FUN = "smooth_wWHIT", 
          wFUN = "wTSM",
          maxExtendMonth = 12,
          r_min = 0.1, 
          IsPlot = FALSE, 
          IsPlot.OnlyBad = FALSE, 
          print = FALSE,
          iters = 4,
          lambda = 1600  
        )
      )  
      
    #Curve fitting
      lai_fit <- phenofit::curvefits(
        lai_input, 
        brks = breaks,
        options = list(
          methods = c(fit_method), 
          wFUN = wTSM, 
          nextend = 2, 
          maxExtendMonth = 3, 
          minExtendMonth = 1, 
          minPercValid = 0.2, 
          print = FALSE, 
          verbose = FALSE, 
          iters = 2, 
          use.rough = FALSE,
          use.y0 = TRUE          
        )
      )    
      
    #Check the curve fitting parameters
      l_param <- get_param(lai_fit)
      d_fit <- get_fitting(lai_fit)

    #Get GOF information
      d_gof <- get_GOF(lai_fit)
      
    # #Visualization
    #   g <- phenofit::plot_phenofit(d_fit, breaks, NULL, title.ylab = "LAI", "Time")
    #   grid::grid.newpage(); grid::grid.draw(g)# plot to check the curve fitting      
      
    #Bind together the curve fitting data
      fit_bound <- setNames(data.frame(
        d_fit[d_fit$meth == fit_method, ]$t, 
        d_fit[d_fit$meth == fit_method, ]$ziter2  
      ), c("date", "Lai_fit"))
    
  #-------------------------------------------------  
  #Generating continuous series of LAI
  #------------------------------------------------- 
    #Generate a complete set of dates bewteen the first and last record
      full_dates <- setNames(data.frame(seq.Date(from = min(ts[, "date"]), 
        to = max(ts[, "date"]), by = "day")), "date")
            
      full_dates$Year <- as.numeric(format(full_dates[, "date"], "%Y"))
      full_dates$DOY <- as.numeric(format(full_dates[, "date"], "%j"))
      
    #Interpolate to daily values
      interpolated <- approx(fit_bound[, "date"], fit_bound[, "Lai_fit"], xout = full_dates[, "date"])
      
      interpolated_df <- setNames(data.frame(interpolated$x, interpolated$y), c("date", "LAI_proc"))
      
    #Mergeall of the data together
      dfs <- list(full_dates, ts[, c("date", "Lai")], interpolated_df)
      final_merged <- plyr::join_all(dfs, by = "date")
        colnames(final_merged)[1] <- "Date"
      
  return(final_merged)
 
} #End LAI_proc_phenofit function
