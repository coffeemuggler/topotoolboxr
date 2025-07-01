#' Convert GRIDobj to a data frame of X, Y and Z values
#' 
#' The function extracts the grid values and coordinates from a \code{GRIDobj}
#' and returns a data frame of tupels.
#'  
#' @param GRIDobj \code{SpatRaster} to be converted
#'  
#' @return A \code{list} with the elements \code{$X} (vector of X-values), 
#' \code{$Y} (vector of Y-values), \code{$Z} (numeric matrix of grid values).
#' 
#' @examples
#' 
#' ## load example data set
#' data(srtm_bigtujunga30m_utm11)
#' DEM <- terra::unwrap(srtm_bigtujunga30m_utm11)
#' 
#' ## extract XYZ information
#' XYZ <- GRIDobj2xyz(GRIDobj = DEM)
#' 
#' ## plot elevation profile of the first horizontal line of the DEM
#' plot(x = XYZ$X[1:1197], y = XYZ$Z[1:1197])
#' 
#' 
#' @author Michael Dietze, Wolfgang Schwanghart
#' 
#' @export GRIDobj2xyz

GRIDobj2xyz <- function(
    
  GRIDobj
                          
) {
  
  ## CHECKS OF INPUT DATA -----------------------------------------------------
  
  ## check input data set
  if(inherits(GRIDobj, "SpatRaster") == FALSE) {
    stop("GRIDobj seems to be no SpatRaster object!")
  }
  
  ## EXTRACT X,Y,Z data -------------------------------------------------------
  
  ## extract coordinates
  XY <- terra::crds(x = GRIDobj)
  
  ## extract grid values and build correctly flipped and transposed matrix
  Z <- terra::values(GRIDobj)[,1]
  
  ## RETURN OUTPUT ------------------------------------------------------------
  
  ## return output
  return(data.frame(X = XY[,1],
              Y = XY[,2],
              Z = Z))
}