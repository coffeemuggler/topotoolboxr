#' Test if GRIDobj contains no FALSE (or zero) value 
#' 
#' The function checks if there is any FALSE or zero value in the grid 
#' values and returns a logical value on the result.
#' 
#' @param GRIDobj \code{SpatRaster} to be tested
#' 
#' @return \code{Logcial} value of the test result
#' 
#' @examples
#' 
#' ## load example data set
#' data(srtm_bigtujunga30m_utm11)
#' DEM <- terra::unwrap(srtm_bigtujunga30m_utm11)
#' 
#' ## test if any FALSE values are contained
#' tf(GRIDobj = DEM)
#' 
#' ## introduce FALSE values and test again
#' terra::values(DEM)[10,] <- FALSE
#' tf(GRIDobj = DEM)
#' 
#' @author Michael Dietze, Wolfgang Schwanghart
#' 
#' @export tf

tf <- function(

  GRIDobj

) {
  
  ## CHECKS OF INPUT DATA -----------------------------------------------------
  
  ## check input data set
  if(inherits(GRIDobj, "SpatRaster") == FALSE) {
    stop("GRIDobj seems to be no SpatRaster object!")
  }
  
  ## ANALYSIS PART ------------------------------------------------------------
  
  ## check if grid values contain any FALSE or zero value
  x <- !any(terra::values(GRIDobj) == FALSE)
  
  ## RETURN OUTPUT ------------------------------------------------------------
  return(x)
}