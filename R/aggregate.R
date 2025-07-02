#' Aggregate a GRIDobj or point data set to the resolution of a GRIDobj
#' 
#' This function resamples the grid A to match the extent and resolution of 
#' grid B. B must spatially overlap with A. By default, the function uses the 
#' mean to calculate new grid values, but any other function that takes 
#' returns a scalar (e.g. median, std, ...) can be used, too.
#' 
#' Values to be aggregated can also be supplied as list of coordinates
#' (and attributes). This is particularly useful if point density for
#' each pixel is greater than one. 
#' 
#' @param A \code{SpatRaster} that will be aggregated
#' 
#' @param B \code{SpatRaster} of coarser resolution to be aggregated to
#' 
#' @param xy \code{data.frame} or \code{matrix} with x and y coordinates 
#' of irregular spaced geodata points. The output will be converted to a 
#' spatial grid of the same resolution and CRS as B. Grid values will be 
#' assigned \code{TRUE}.
#' 
#' @param xyz \code{data.frame} or \code{matrix} with x, y and z coordinates 
#' of irregular spaced geodata points. The output will be converted to a 
#' spatial grid of the same resolution and CRS as B.
#' 
#' @param aggfun \code{Character} value, name of the function that is used 
#' to calculate the new aggregated values. Keywords may be  
#' \code{"mean"}, \code{"median"}, \code{"sd"}, \code{"min"}, \code{"max"}. 
#' Default is \code{"mean"}.
#' 
#' @return Returns an aggregated \code{GRIDobj}
#' 
#' @examples
#' 
#' ## load example data set
#' data(srtm_bigtujunga30m_utm11)
#' DEM <- terra::unwrap(srtm_bigtujunga30m_utm11)
#' 
#' ## create GRIDobj with 100 m resolution and the same extent as DEM
#' B <- terra::rast(res = c(1000, 1000), ext = terra::ext(DEM))
#' 
#' ## aggregate and plot DEM based on coarse GRIDobj B
#' C <- aggregate(A = DEM, B = B)
#' plot_GRIDobj(C)
#' 
#' ## create 100 random points and aggregate to geometry of GRIDobj B
#' pts <- data.frame(x = runif(100, terra::ext(DEM)[1], terra::ext(DEM)[2]),
#'                   y = runif(100, terra::ext(DEM)[3], terra::ext(DEM)[4]),
#'                   z = rnorm(100))
#' D <- aggregate(B = B, xyz = pts)
#' plot_GRIDobj(D)
#' 
#' @export aggregate

aggregate <- function(
  
  A,
  B,
  xy,
  xyz,
  aggfun = "mean"
    
){
  
  ## CHECKS OF INPUT DATA -----------------------------------------------------
  
  ## check input data set B
  if(missing(B) == TRUE) {
    
    stop("GRIDobj B is missing. Cannot aggregate!")
    
  } else if(inherits(B, "SpatRaster") == FALSE) {
    
      stop("GRIDobj seems to be no SpatRaster object!")
    }
  
  ## ANALYSIS PART ------------------------------------------------------------
  
  ## case A, GRIDobj A is given
  if(missing(A) == FALSE) {
    
    ## check input data set A
    if(inherits(A, "SpatRaster") == FALSE) {
      
      stop("GRIDobj seems to be no SpatRaster object!")
    } 
    
    ## convert GRIDobj to xyz form
    xyz <- GRIDobj2xyz(GRIDobj = A)
    
  ## case B, xy or xyz points are given
  } else if(missing(xy) | missing(xyz) == FALSE) {

    ## check/set z values if not provided
    if(missing(xyz) == TRUE) {
      
      xyz <- cbind(xy, rep(TRUE, nrow(xy)))
    }
  }
  
  ## build GRIDobj from spatial points
  C <- terra::rasterize(x = xyz, y = B, value = xyz[,3], fun = aggfun)
  
  ## RETURN OUTPUT ------------------------------------------------------------
  
  ## return GRIDobj
  return(C)
}
