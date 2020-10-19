#' Determines timezone from Latitude and Longitude
#' @description This function determines the IANA/Olson time zone identifier from
#' latitude and longitude. This is a helper function which is used to assist in
#' converting downloaded data from UTC to local time.
#'
#' Timezone information is extracted from the included tz_world dataset. The original
#' tz_world data was created by Eric Muller and can be found at (http://efele.net/maps/tz/world/)
#'
#'
#' @param Lat The site Latitude
#' @param Lon The site Longitude
#' @param site_crs The coordinate reference system of the points, preferably designated
#' as an EPSG code. For example, the most common geographic system is WGS84 and its EPSG 
#' code is 4326. 
#'
#' @return Returns the IANA/Olson time zone identifier for a given Latitude and Longitude
#' @export

#===============================================================================
#Function for retrieving timezone information based on latitude and longitude
#===============================================================================
get_tz <- function(Lat, Lon, site_crs){
  #Import the tz_world dataset if it is not already loaded
    if(!exists("tz_world")){data("tz_world", package = "StreamLightUtils")}

  #Create a simple features object from the site location
    site_location <- data.frame(Lat, Lon)
    
  #Check if the data is in WGS84, if not reproject to WGS84
    if(site_crs == 4326){
      xy_sf <- sf::st_as_sf(site_location, coords = c('Lon', 'Lat'), crs = site_crs)
    } else{
      xy_native <- sf::st_as_sf(site_location, coords = c('Lon', 'Lat'), crs = site_crs)
      xy_sf <- sf::st_transform(xy_native, crs = 4326)
    } #End if else statement
    
  #Return the timezone name
    tz_name <- paste(as.data.frame(sf::st_join(xy_sf, tz_world, join = sf::st_intersects))$TZID)

  #Error catch, sometimes points very near boundaries of large water bodies return
  #NA due to imprecise georeferencing either on the part of the map or point
  #Note, this is rudimentary and should be made more robust
    if(tz_name == "NA"){
      #Create a 1 degree box around the point
        bbox_xy <- setNames(data.frame(
          rbind(
            c(Lon - 1, Lat + 1),
            c(Lon + 1, Lat + 1),
            c(Lon + 1, Lat - 1),
            c(Lon - 1, Lat - 1)
          )), c("Lon", "Lat")
        )
      
        bbox_lat <- sf::st_as_sf(bbox_xy, coords = c('Lon', 'Lat'), crs = 4326)
        
      #Find timezones that intersect the box
        tz_intersect <- tz_world[unique(as.numeric(paste(sf::st_intersects(bbox_lat, tz_world)))), ]        
        
      #Find the UTM zone for the site
        lonlat2UTM = function(lonlat) {
          utm = (floor((lonlat[1] + 180) / 6) %% 60) + 1
          if(lonlat[2] > 0) {
            utm + 32600
          } else{
            utm + 32700
          }
        } #End lonlat2UTM

        utm_zone <- lonlat2UTM(c(Lon, Lat)) 
        
      #Reproject the data into UTM so distances can be calculated
        xy_utm <- sf::st_transform(xy_sf, crs = utm_zone)
        tz_intersect_utm <- sf::st_transform(tz_intersect, crs = utm_zone)
        bbox_utm <- sf::st_transform(bbox_lat, crs = utm_zone) 
        
      #Find the nearest timezone 
        tz_name <- paste(as.data.frame(tz_intersect_utm)[which.min(sf::st_distance(tz_intersect_utm, xy_utm)), "TZID"])
        
    } #End if statement        

  #Return the timezone name
    return(tz_name)

} #End get_tz function
