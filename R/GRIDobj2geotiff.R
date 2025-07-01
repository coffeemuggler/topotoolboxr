#' Save GRIDobj as geotiff file
#' 
#' GRIDobj2geotiff writes a GRIDobj to a geotiff file. The 
#' \code{file} must be a character indicating the relative or absolute 
#' file path. The extension should be either \code{.tif} or \code{.tiff}. 
#' A potentially existing file will be overwritten without notice.
#'  
#' This function is a convenience function, mainly for mapping the Matlab 
#' Topotoolbox capabilities to R. It wraps the workhorse function 
#' \code{terra::writeRaster()} (see examples) that in turn uses the  
#' [GDAL driver names](https://gdal.org/en/latest/drivers/raster/index.html).
#'  
#' @param GRIDobj \code{SpatRaster} to be exported
#'  
#' @param file \code{Character} value, file name and path where to save 
#' the exported \code{GRIDobj}.
#'  
#' @return Writes a geotiff data set as file.
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' ## load example data set
#' data(srtm_bigtujunga30m_utm11)
#' DEM <- terra::unwrap(srtm_bigtujunga30m_utm11)
#'                                          
#' ## save example data set as ASCII file
#' GRIDobj2geotiff(GRIDobj = DEM, file = "export/srtm_bigtujunga30m_utm11.tif")
#' 
#' ## the same but using the terra function
#' terra::writeRaster(x = DEM, 
#'                    filename = "export/srtm_bigtujunga30m_utm11.txt", 
#'                    filetype = "GTiff")
#' 
#' }
#' 
#' @export GRIDobj2geotiff

GRIDobj2geotiff <- function(
    
  GRIDobj, 
  file
                          
) {
  
  ## CHECKS OF INPUT DATA -----------------------------------------------------
  
  ## check input data set
  if(inherits(GRIDobj, "SpatRaster") == FALSE) {
    stop("GRIDobj seems to be no SpatRaster object!")
  }
  
  ## strip path and check if it exists
  path <- strsplit(x = file, split = "/", fixed = TRUE)[[1]]
  path <- paste(path[-length(path)], collapse = "/")
  
  if(dir.exists(paths = path) == FALSE) {
    stop("Path to save GRIDobj does not exist!")
  }
  
  ## EXPORT OF GRIDobj --------------------------------------------------------
  terra::writeRaster(x = GRIDobj, 
                     filename = file, 
                     filetype = "GTiff", 
                     overwrite = TRUE)
}