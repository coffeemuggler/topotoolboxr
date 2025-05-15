#' Fill pits, sinks or topographic depressions
#' 
#' The function removes topographic depressions in a Digital Elevation Model 
#' (DEM). Use this function to enable a continuous flow towards the DEM edges. 
#' 
#' OPEN QUESTIONS:
#' 
#' 1) is Matlabs Image Processing Toolbox still an option under R?
#' 
#' 2) is implementation of -999 versus -Inf for NA values correct?
#' 
#' 3) What is the meaning of B (bc)? Does it replace sinks and maxdepth?
#' 
#' Sinks are typically artefacts emerging by the generation of the DEM.
#' However, there may also be closed basins or sinkholes and as such those 
#' sinks are important real features of DEMs. In order to account for such 
#' sinks, \code{fillsinks()} allows to specify a maximum depth of sinks, that 
#' will be filled, or to employ a logical data set (\code{sinks}) that is 
#' \code{TRUE} where sinks should remain (minima imposition). Note that for 
#' the latter option, there will be one regional minima for each connected 
#' component in the sinks data set.
#' 
#' @param GRIDobj \code{SpatRaster} with DEM
#' 
#' @param maxdepth \code{Numerical} value, maximum depth of sinks that will 
#' be filled
#' 
#' @param sinks \code{Logical} matrix or \code{GRIDobj} of the same size as 
#' the DEM. \code{TRUE} values in sinks are treated as sinks in the digital 
#' elevation model and are not filled.
#' 
#' @param B \code{SpatRaster} or boundary matrix. 
#' 
#' @return Returns a \code{GRIDobj}, digital elevation model with filled sinks
#' 
#' @examples
#' ## load example data set
#' data(srtm_bigtujunga30m_utm11)
#' DEM <- terra::unwrap(srtm_bigtujunga30m_utm11)
#'                                          
#' ## fill sinks
#' DEM_filled <- fillsinks(GRIDobj = DEM)
#' 
#' ## plot difference
#' plot_GRIDobj(DEM_filled - DEM, col = rev(heat.colors(200)))
#' 
#' @export fillsinks

fillsinks <- function(
    
  GRIDobj, 
  maxdepth,
  sinks,
  B
  
) {
  
  ## check maxdepth parameter and set md flag
  if(missing(maxdepth) == FALSE) {
    if(maxdepth > 0 & is.numeric(maxdepth)) {
      md <- TRUE
    } else {
      stop("Parameter maxdepth must be numeric and greater than zero!")
    }
  } else {
    md <- FALSE
  }
  
  ## check that optional sinks grid matches DEM grid
  if(missing(sinks) == FALSE) {
    
    if(is.matrix(sinks) | inherits(sinks, "SpatRaster") == FALSE) {
      stop("Sinks is neither a matrix nor a SpatRaster!")
    } else if(dim(t(sinks)) != dim(GRIDobj)[1:2]) {
      stop(paste0("Sinks geometry (", dim(t(sinks))[1], " by ", 
                  dim(t(sinks))[2], ") does not match GRIDobj (", 
                  dim(GRIDobj)[1], " by ", dim(GRIDobj)[2], ")"))
    } else {
      md <- FALSE
    }
  }
  
  ## extract elevation values from SpatRaster
  z <- terra::values(GRIDobj, mat = FALSE)
  
  ## create empty output vector
  z_0 <- single(length(z))
  
  ## get lowest elevation and subtract defined values
  z_min = min(z, na.rm=TRUE) - 999
  
  ## identify NA values and replace them by minimum value
  z_na = is.na(z)
  z[z_na] = z_min
  
  ## Case 1: no maxdepth value given
  if(md == FALSE) {
    
    ## create boundary matrix and NA-corrected matrix
    if (missing(B) == TRUE) {
      B <- matrix(data = 0, nrow = dim(GRIDobj)[2], ncol = dim(GRIDobj)[1])
      B[is.na(terra::values(GRIDobj))] <- 1
      B[1,] <- 1
      B[,1] <- 1
      B[nrow(B),] <- 1
      B[,ncol(B)] <- 1
    }
    
    ## fill sinks
    z_fill <- .C("wrap_fillsink",
                 outputR=as.single(z_0),
                 as.single(z),
                 as.integer(B),
                 as.integer(dim(GRIDobj)[2:1]))$outputR
    
    ## re-introduce NA-values
    z_fill[z_na] = NA
  }
  
  ## Case2 : maxdepth value given
  if(md == TRUE) {
    
    stop("Option maxdepth is currently not supported!")
  }
  
  ## paste new elevation values into GRIDobj
  terra::values(GRIDobj) <- z_fill
  
  ## return GRIDobj
  return(GRIDobj)
}
