#' Create instance of a GRIDobj
#' 
#' GRIDobj creates an instance of the grid class, which basically builds on 
#' the \code{SpatRaster} structure.
#' 
#' Note that while throughout this help text GRIDobj is associated with
#' gridded digital elevation models, instances of GRIDobj can contain
#' other gridded, single band, datasets such as flow accumulation grids, 
#' gradient grids etc.
#' 
#' Coordinate reference systems must be provided in PROJ-string nomenclature. 
#' See the official PROJ website for further information, examples and an 
#' introduction: https://proj.org/en/stable/usage/quickstart.html.
#' 
#' The function will stop if the resolution of the imported data set in x 
#' and y direction is not identical. In that case, consider manual import of 
#' the data set via \code{terra::rast()} and resampling to identical  
#' resolution (cf. \code{terra::resample()}).
#' 
#' DEM = GRIDobj(Z) creates a GRIDobj of a numeric matrix \code{Z} and a 
#' default resolution of \code{1} m.
#' 
#' DEM = GRIDobj(Z,cs) creates a GRIDobj from the elevations stored in 
#' the matrix Z and \code{cs} is a positive value defining the spatial 
#' resolution. 
#' 
#' DEM = GRIDobj(Z,R) creates a GRIDobj from the elevations stored in 
#' the matrix Z. R is a MapCellsReference, MapPostingsReference,
#' GeographicCellsReference or GeoGeographicPostingsReference object.
#' 
#' DEM = GRIDobj(X,Y,Z) creates a GRIDobj from the coordinate matrices
#' or vectors X and Y and the matrix Z. The elements of Z refer to the
#' elevation of each pixel.
#' 
#' DEM = GRIDobj('ESRIasciiGrid.txt') creates a GRIDobj from an ESRI 
#' Ascii grid exported from other GI systems. 
#' 
#' DEM = GRIDobj('GeoTiff.tif') creates a GRIDobj from a Geotiff. Note
#' that reading image files may not reliably detect missing data.
#' 
#' DEM = GRIDobj(filename) tries to create a GRIDobj from other formats
#' supported by the Mapping Toolbox function readgeoraster.
#' 
#' DEM = GRIDobj() opens a dialog box to read either an ESRI Ascii Grid
#' or a Geotiff.
#' 
#' DEM = GRIDobj(NULL) creates an empty instance of GRIDobj. The Matlab 
#' equivalent is \code{GRIDobj([])}. 
#' 
#' DEM = GRIDobj(FLOWobj or GRIDobj or STREAMobj,class) creates an
#' instance of GRIDobj with all common properties (e.g., spatial
#' referencing) inherited from another instance of a FLOWobj, GRIDobj 
#' or STREAMobj class. DEM.Z is set to all zeros where class can be
#' integer classes or double or single. By default, class is double.
#' 
#' DEM = GRIDobj(FLOWobj or GRIDobj or STREAMobj, Z) creates an
#' instance of GRIDobj with all common properties (e.g., spatial
#' referencing) inherited from another instance of a FLOWobj, GRIDobj 
#' or STREAMobj class. The second input argument Z is written to DEM.Z.
#' 
#' @param Z \code{Numeric} matrix with z-coordinates. If set to \code{NULL}, 
#' an empty GRIDobj will be created, with a resolution of 1 by 1 pixel.
#' 
#' @param X \code{Numeric} vector or matrix with x-coordinates
#' 
#' @param Y \code{Numeric} vector or matrix with y-coordinates
#' 
#' @param file \code{Character} value with DEM to import. Supported file
#' types are GeoTIF, ESRI Grid, and further. The import uses the GDAL library 
#' via the package terra. The input file must contain one layer only and has 
#' to be in a projected coordinate reference system. If the keyword 
#' \code{"interactive"} is used, a file selection window will open to let the 
#' user select a file interactively.
#' 
#' @param obj \code{Topotoolbox} object of class \code{FLOWobj}, 
#' \code{STREAMobj} or \code{GRIDobj}, from which a new GRIDobj will be 
#' created
#' 
#' @param R \code{Character} value, coordinate reference system definition 
#' in PROJ-string notation. See details and examples for further information.
#' 
#' @param cs \code{Numerical} value, spatial resolution of the grid in m. Only 
#' needed when merely \code{Z} is provided. Note that cs is a vector of length 
#' one, hence the grid must have the the same resolution in x- and y-direction.
#' Default value if \code{1}.
#' 
#' @param \dots Further arguments passed to the function, see details. 
#' 
#' @return Returns a \code{SpatRaster} object 
#' 
#' @examples
#' 
#' ## Example 1: Load a DEM and plot it
#' 
#' tif <- paste0(system.file("extdata", package="topotoolboxr"), "/", 
#'               "srtm_bigtujunga30m_utm11.tif")
#' 
#' DEM <- GRIDobj(file = tif)
#' plot_GRIDobj(DEM)
#' 
#' ## Example 2: convert matrix (volcano example data set) to GRIDobj
#' 
#' data(volcano)
#' Z <- volcano[87:1,61:1] # flip to show correctly
#' image(t(volcano[,61:1])) # flip again to account for image function
#' 
#' DEM <- GRIDobj(Z = Z)
#' plot_GRIDobj(DEM, interactive = TRUE)
#' 
#' ## Example 3: convert matrix to GRIDobj with X and Y coordinates
#' X <- 1:ncol(Z) * 10
#' Y <- 1:nrow(Z) * 10
#' 
#' DEM <- GRIDobj(X = X, Y = Y, Z = Z)
#' 
#' ## Example 4: convert matrix to GRIDobj with defined cell size
#' 
#' DEM <- GRIDobj(Z = Z, cs = 10)
#' 
#' ## Example 5: create empty GRIDobj
#' DEM <- GRIDobj()
#' 
#' ## Example 6: create a 100 by 50 cell GRIDobj with only 1's
#' DEM <- GRIDobj(Z = matrix(rep(1, 50 * 100), ncol = 100))
#' 
#' ## Example 7: create GRIDobj with coordinate reference system (CRS)
#' 
#' DEM <- GRIDobj(Z = Z, R = "+proj=utm +zone=60 +south +datum=WGS84")
#' 
#' ## Example 8: add CRS after GRIDobj is created
#'  DEM <- GRIDobj(Z = Z)
#'  terra::crs(DEM) <- "+proj=utm +zone=60 +south +datum=WGS84"
#' 
#' @export GRIDobj

GRIDobj <- function(
    
  Z = NULL, 
  X, 
  Y, 
  file, 
  obj, 
  R, 
  cs = 1,
  ...

) {
  
  ## extract extra arguments
  dots <- list(...)
  
  ## handle case of empty GRIDobj
  if(is.null(Z)) {Z <- matrix(0)}
  
  ## set X and Y to cs resolution if only Z is provided
  if(missing(X)) {X <- seq(1, ncol(Z)) * cs}
  if(missing(Y)) {Y <- seq(1, nrow(Z)) * cs}
  
  ## convert coordinate matrix to vector
  X <- unique(X)
  Y <- unique(Y)
  
  ## get cellsize in x and y direction
  cs_x <- mean(diff(X))
  cs_y <- mean(diff(Y))
  
  ## check if cellsizes are identical
  if(all.equal(cs_x, cs_y) == FALSE) {
    
    ## stop if not identical
    stop("Spatraster resolution in x and y direction are not equal!")
  } else {
    
    ## assign x direction cellsize as general cellsize
    cs <- cs_x
    
    ## set cellsize to 1 for lazarus case
    if(is.nan(cs)) {cs <- 1}
  }
  
  if(missing(file) == TRUE & missing(obj) == TRUE) {
    ## Case 1: create GRIDobj from X, Y, Z
    
    ## create SpatRaster 
    DEM <- try(terra::rast(nlyrs = 1,
                           xmin = min(X) - cs, xmax = max(X),
                           ymin = min(Y) - cs, ymax = max(Y),
                           resolution = c(cs, cs), 
                           vals = Z,
                           crs = terra::crs("")), silent = TRUE)
    
    ## check success
    if(inherits(DEM, "try-error")) {
      stop("Cannot build GRIDobj from Z values, check geometry!")
    }

  } else if(missing(file) == FALSE) {
    ## Case 2: read GRIDobj from file
    
    ## import data set
    DEM <- terra::rast(x = file)
    
    ## check if DEM file can be read
    if(inherits(DEM, "try-error")) {
      stop("Could not read DEM file!")
    }
    
    ## check if DEM file only has one layer
    if(terra::nlyr(DEM) != 1) {
      stop("Spatraster has more than one layer!")
    }
    
    ## check identical x and y resolution of DEM file
    if(!all.equal(terra::xres(DEM), terra::yres(DEM))) {
      stop("Spatraster resolution in x and y direction are not equal!")
    }
    
    ## check that DEM file has a projected CRS
    if(terra::is.lonlat(DEM)) {
      stop("SpatRaster coordinate reference system is not projected")
    }
    
  } else   if(missing(obj) == FALSE) {
    
    ## Case 3: create GRIDobj from another TT object
    #### TO BE DONE LATER ---------------------------------------------------------- ####
    
  }
  
  ## optionally, assign CRS
  if(missing(R) == FALSE) {terra::crs(DEM) <- R}

  ## return output
  return(DEM)
}
