#' Anisotropic coefficient of variation (ACV) 
#' 
#' The anisotropic coefficient of variation describes the general geometry 
#' of the local land surface and can be used to distinguish elongated from 
#' oval landforms.
#' 
#' @param GRIDobj \code{GRIDobj}, DEM to be processed
#' 
#' @return \code{GRIDobj} with anisotropic coefficient of variation values
#' 
#' @examples
#' 
#' ## load example data set
#' data(srtm_bigtujunga30m_utm11)
#' DEM <- terra::unwrap(srtm_bigtujunga30m_utm11)
#' 
#' ## calculate ACV
#' ACV <- acv(GRIDobj = DEM)
#' 
#' ## plot result
#' plot_GRIDobj(GRIDobj = ACV, col = "viridis")
#' 
#' @export acv

acv <- function(
    
  GRIDobj
  
) {
  
  ## CHECKS OF INPUT DATA -----------------------------------------------------
  
  ## check input data set
  if(inherits(GRIDobj, "SpatRaster") == FALSE) {
    stop("GRIDobj seems to be no SpatRaster object!")
  }
  
  ## DEFINITIONS --------------------------------------------------------------
  
  ## define convolution kernel
  k <- rbind(c(1, 0, 1, 0, 1),
             c(0, 0, 0, 0, 0),
             c(1, 0, 0, 0, -1),
             c(0, 0, 0, 0, 0),
             c(-1, 0, -1, 0, -1))
  
  ## define 3-by-4 filter kernel
  F3 <- list(rbind(c(0, 0, 0),
                   c(1, 0, -1),
                   c(0, 0, 0)),
             rbind(c(1, 0, 0),
                   c(0, 0, 0),
                   c(0, 0, -1)),
             rbind(c(0, 1, 0),
                   c(0, 0, 0),
                   c(0, -1, 0)),
             rbind(c(0, 0, 1),
                   c(0, 0, 0),
                   c(-1, 0, 0)))
  
  ## define 4-by-4 filter kernel
  F5 <- list(rbind(c(0, 0, 0, 0, 0),
                   c(0, 0, 0, 0, 0),
                   c(1, 0, 0, 0, -1),
                   c(0, 0, 0, 0, 0),
                   c(0, 0, 0, 0, 0)),
             rbind(c(1, 0, 0, 0, 0),
                   c(0, 0, 0, 0, 0),
                   c(0, 0, 0, 0, 0),
                   c(0, 0, 0, 0, 0),
                   c(0, 0, 0, 0, -1)),
             rbind(c(0, 0, -1, 0, 0),
                   c(0, 0, 0, 0, 0),
                   c(0, 0, 0, 0, 0),
                   c(0, 0, 0, 0, 0),
                   c(0, 0, -1, 0, 0)),
             rbind(c(0, 0, 0, 0, -1),
                   c(0, 0, 0, 0, 0),
                   c(0, 0, 0, 0, 0),
                   c(0, 0, 0, 0, 0),
                   c(1, 0, 0, 0, 0)))
  
  ## CALCULATION PART ---------------------------------------------------------
  
  ## extract matrix from GRIDobj
  m <- GRIDobj2mat(GRIDobj)$Z
  
  ## pad by repeated values
  m <- rbind(m[c(1, 1),], m, m[c(dim(m)[1], dim(m)[1]),])
  m <- cbind(m[,c(1, 1)], m, m[,c(dim(m)[2], dim(m)[2])])
  
  ## calculate dz
  dz <- OpenImageR::convolution(image = m, kernel = k, mode = "same") / 4

  ## create initial anisotropy matrix
  A <- m * 0
  
  ## apply 5-pixel filter to padded matrix
  for(i in 1:4) {
    A <- (A + OpenImageR::convolution(image = m, 
                                      kernel = F5[[i]], 
                                      mode = "same") - dz)^2
  }
  
  ## remove one set of marginal pixels
  m <- m[(2:(nrow(m)-1)),(2:(ncol(m)-1))]
  A <- A[(2:(nrow(A)-1)),(2:(ncol(A)-1))]
  dz <- dz[(2:(nrow(dz)-1)),(2:(ncol(dz)-1))]
  
  ## apply 3-pixel filter to reduced padded matrix
  for(i in 1:4) {
    A <- (A + OpenImageR::convolution(image = m, 
                                      kernel = F3[[i]],
                                      mode = "same") - dz)^2
  }
  
  ## remove another set of marginal pixels
  m <- m[(2:(nrow(m)-1)),(2:(ncol(m)-1))]
  A <- A[(2:(nrow(A)-1)),(2:(ncol(A)-1))]
  dz <- dz[(2:(nrow(dz)-1)),(2:(ncol(dz)-1))]
  
  ## get maximum dz or use threshold value
  dz = max(abs(dz), 0.001)
  
  ## calculate log-scaled CoV
  C = log(1 + sqrt(A/8) / dz)
  
  ## convert output back to GRIDobj
  terra::values(GRIDobj) <- as.numeric(x = t(C))
  
  ## RETURN OUTPUT ------------------------------------------------------------
  return(GRIDobj)
}
