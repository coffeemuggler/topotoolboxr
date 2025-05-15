#' Calculate the gradien
#'
#' This will make the Gradient8 function will do something
#' 
#' @param GRIDobj \code{SpatRaster} with DEM
#' 
#' @param use_mp Future feature that will allow to parallelize the code
#'
#' @return Returns a \code{GRIDobj} with gradient results
#' 
#' @examples
#' ## load example data set
#' data(srtm_bigtujunga30m_utm11)
#' DEM <- terra::unwrap(srtm_bigtujunga30m_utm11)
#' 
#' ## calculate gradient
#' G <- gradient8(GRIDobj = DEM)
#' 
#' ## plot gradient
#' plot_GRIDobj(GRIDobj = G, col = grey.colors(200))
#' 
#' @export gradient8

gradient8 <- function(
    
  GRIDobj, 
  use_mp = FALSE
  
) {
  
  ## check/set parallel option
  use_mp <- as.numeric(use_mp)
  
  if(use_mp == 0) {
    
    ## create output array
    g <- single(length(terra::values(GRIDobj))) 
    
    ## calculate and assign
    terra::values(GRIDobj) <- .C(
      "wrap_gradient8",
      outputR=as.single(g),
      as.single(terra::values(GRIDobj, mat = FALSE)),
      as.single(terra::res(GRIDobj)[1]),
      as.integer(use_mp), 
      as.integer(dim(GRIDobj)[2:1]))$outputR
  } else {
    
    stop("use_mp option is not supported!")
  }
  
  ## return output
  return(GRIDobj)
}
