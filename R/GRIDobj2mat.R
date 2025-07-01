#' Convert GRIDobj to a matrix and coordinate vectors
#' 
#' The function extracts the grid values and coordinates from a \code{GRIDobj}
#' and returns a list of X and Y coordinates as well as the corresponding grid 
#' values as matrix.
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
#' XYZ <- GRIDobj2mat(GRIDobj = DEM)
#' 
#' ## plot image of DEM
#' image(x = XYZ$X, y = XYZ$Y, z = t(XYZ$Z), col = terrain.colors(100))
#' 
#' @author Michael Dietze, Wolfgang Schwanghart
#' 
#' @export GRIDobj2mat

GRIDobj2mat <- function(
    
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
  X <- sort(unique(XY[,1]))
  Y <- sort(unique(XY[,2]))
  
  ## extract grid values and build correctly flipped and transposed matrix
  Z <- matrix(data = terra::values(GRIDobj), 
              nrow = dim(GRIDobj)[1], 
              byrow = TRUE)
  #Z <- t(Z[nrow(Z):1,])
  
  ## RETURN OUTPUT ------------------------------------------------------------
  
  ## return output
  return(list(X = X,
              Y = Y,
              Z = Z))
}