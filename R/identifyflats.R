#' Identify flats in elevation data
#' 
#' The function DOES THIS AND THAT 
#' 
#' @param GRIDobj \code{SpatRaster} with DEM
#' 
#' @return Returns a \code{GRIDobj}, logical data set with flats
#' 
#' @examples
#' ## load example data set
#' data(srtm_bigtujunga30m_utm11)
#' DEM <- terra::unwrap(srtm_bigtujunga30m_utm11)
#'                                          
#' ## TO BE DONE
#' 
#' @export identifyflats

identifyflats <- function(
  
  GRIDobj
    
) {
  
  ## extract elevation values from SpatRaster
  z <- terra::values(GRIDobj, mat = FALSE)
  
  ## create empty output vector
  z_0 <- single(length(z))
  
  ## get lowest elevation and subtract defined values
  z_min = min(z, na.rm = TRUE) - 999
  
  ## identify NA values and replace them by minimum value
  z_na = is.na(z)
  z[z_na] = z_min
  
  ## Identify flats using libtopotoolbox
  flats <- .C("wrap_identifyflats",
               outputR=as.integer(z_0),
               as.single(z),
               as.integer(dim(GRIDobj)[2:1]))$outputR

  ## re-introduce NA-values
  flats[z_na] = NA
  
  ## paste new elevation values into GRIDobj
  terra::values(GRIDobj) <- flats
  
  ## return GRIDobj
  return(GRIDobj)
}