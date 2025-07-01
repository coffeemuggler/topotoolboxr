#' Compute costs
#' 
#' The function DOES THIS AND THAT 
#' 
#' @param flats \code{SpatRaster} with flats
#' 
#' @param dem \code{SpatRaster} with DEM
#' 
#' @param filled \code{SpatRaster} with filled DEM
#' 
#' @return Returns a \code{GRIDobj}, logical data set with computed costs
#' 
#' @examples
#' ## load example data set
#' ## TO BE DONE
#' 
#' @export computecosts

computecosts <- function(
  
  flats,
  dem,
  filled
    
) {
  
  ## extract values from input objects
  x_dem <- terra::values(dem, mat = FALSE)
  x_flats <- terra::values(flats, mat = FALSE)
  x_filled <- terra::values(filled, mat = FALSE)
  
  ## extract grid dimensions
  x_dim <- dim(x_dem)[2:1]
  
  ## identify NA values 
  i_na_dem <- is.na(x_dem)
  i_na_filled <- is.na(x_filled)
  
  ## replace NA values by minimum value
  x_dem[i_na_dem] = min(x_dem, na.rm = TRUE) - 999
  x_filled[i_na_filled] = min(x_filled, na.rm = TRUE) - 999
  
  ## create empty output vector
  x_out <- single(length(x_dem))
  y_out <- single(length(x_dem))
  
  ## Identify flats using libtopotoolbox
  x_costs <- .C("wrap_gwdtcomputecosts",
                costsR=as.single(x_out),
                connsR=as.integer(y_out),
                as.integer(x_flats),
                as.single(x_dem),
                as.single(x_filled),
                as.integer(x_dim))$costsR

  ## re-introduce NA-values
  x_costs[i_na_dem] = NA
  
  ## create and paste new elevation values into output object
  costs <- dem
  terra::values(costs) <- x_costs
  
  ## return GRIDobj
  return(costs)
}