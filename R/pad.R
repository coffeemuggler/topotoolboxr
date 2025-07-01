#' Add or remove a border of pixels around a GRIDobj
#' 
#' This function adds or removes a border of constant values or values 
#' calculated from a focal filter along all edges of a \code{GRIDobj}. The 
#' number of iterative steps of adding marginal pixels can be controlled, too. 
#' If the number is negative, the \code{GRIDobj} is cropped by the number of 
#' defined pixels at each grid border.
#' 
#' @param GRIDobj \code{SpatRaster} to be processed
#' 
#' @param px \code{Numeric} value, the number of pixels to add (positive 
#' number) or remove (negavtive number). Default is \code{1}.
#' 
#' @param val \code{Numeric} value or \code{Characer} keyword. If a numeric 
#' value is used, that value is repeated for all marginal pixels. If on out of 
#' the following keywords is used, the respective function is used to calculate 
#' the values for the marginal pixels: \code{"mean"} or \code{"median"}. 
#' 
#' @return A \code{GRIDobj} with a padded margin and updated extent information
#' 
#' @examples
#' 
#' ## load example data set
#' data(srtm_bigtujunga30m_utm11)
#' DEM <- terra::unwrap(srtm_bigtujunga30m_utm11)
#' 
#' ## pad with zeros
#' DEM_0 <- pad(GRIDobj = DEM)
#' 
#' ## pad with mean values of surrounding pixels
#' DEM_mn <- pad(GRIDobj = DEM, val = "mean")
#' 
#' ## pad with two boundaries of NA values
#' DEM_2na <- pad(GRIDobj = DEM, px = 2, val = NA)
#' 
#' ## remove ten sets of pixels
#' DEM_10off <- pad(GRIDobj = DEM, px = -10)
#' 
#' @author Michael Dietze, Wolfgang Schwanghart
#' 
#' @export pad

pad <- function(
    
  GRIDobj,
  px = 1,
  val
                
) {
  
  ## check/set val to fill in
  if(missing(val)) {
    val = 0
  }
  
  ## define padding function as internal function
  f <- function(GRIDobj, px, val) {
    
    ## get CRS and update extent information
    R <- terra::crs(GRIDobj)
    E <- terra::ext(GRIDobj)
    E[1] <- E[1] - terra::res(GRIDobj)[1]
    E[2] <- E[2] + terra::res(GRIDobj)[1]
    E[3] <- E[3] - terra::res(GRIDobj)[2]
    E[4] <- E[4] + terra::res(GRIDobj)[2]
    
    ## extract grid matrix from GRIDobj
    Z <- GRIDobj2mat(GRIDobj = GRIDobj)$Z
    
    if(val %in% c("mean", "meadian")) {
      
      ## pad grid with marginal NA values
      Z_xp <- cbind(rep(NA, dim(Z)[1]), Z, rep(NA, dim(Z)[1]))
      Z_xp <- rbind(rep(NA, dim(Z_xp)[2]), Z_xp, rep(NA, dim(Z_xp)[2]))

      ## convert matrix back to GRIDobj
      Z_xp <- GRIDobj(Z = Z_xp, R = R)
      terra::ext(Z_xp) <- E
      terra::crs(Z_xp) <- R
      
      ## calculate focal mean to define marginal values
      Z_xp <- terra::focal(x = Z_xp, w = 3, fun = "mean", na.rm = TRUE)
      
      ## convert back to matrix
      Z_xp <- GRIDobj2mat(GRIDobj = Z_xp)$Z
      
      ## replace inner part by original values
      Z_xp[2:(nrow(Z_xp)-1), 2:(ncol(Z_xp)-1)] <- Z
      
    } else if(is.numeric(val) | is.na(val)) {
      
      ## pad grid with marginal user-defined values
      Z_xp <- cbind(rep(val, dim(Z)[1]), Z, rep(val, dim(Z)[1]))
      Z_xp <- rbind(rep(val, dim(Z_xp)[2]), Z_xp, rep(val, dim(Z_xp)[2]))
    } else {
      
      stop("Operation not possible, invalid val argument!")
    }
    
    ## convert matrix back to GRIDobj
    Z_xp <- GRIDobj(Z = Z_xp, R = R)
    terra::ext(Z_xp) <- E
    
    ## return output
    return(Z_xp)
  }
  
  ## option A: adding marginal pixels
  if(px > 0) {
    
    ## process as many loops as marginal pixels need to be added
    while(px >= 1) {
      
      ## call padding function
      GRIDobj <- f(GRIDobj, px, val)
      
      ## update number of pixels to process
      px <- px - 1
    }
  }
  
  ## option B: removing marginal pixels
  if(px < 0) {
    
    E <- terra::ext(GRIDobj)
    E[1] <- E[1] - px * terra::res(GRIDobj)[1]
    E[2] <- E[2] + px * terra::res(GRIDobj)[1]
    E[3] <- E[3] - px * terra::res(GRIDobj)[2]
    E[4] <- E[4] + px * terra::res(GRIDobj)[2]
    
    GRIDobj <- terra::crop(x = GRIDobj, y = E)
  }
  
  ## return output
  return(GRIDobj)

}
