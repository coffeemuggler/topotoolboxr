#' Create a FLOWobj object
#' 
#' FLOWobj creates a flow direction object which can be used to
#' calculate terrain attributes such as flow accumulation, drainage
#' basin delineation, flow path extraction, etc. A \code{FLOWobj} is derived
#' from a digital elevation model (DEM) which is stored as a \code{GRIDobj}.
#' 
#' \code{FLOWobj(DEM, "single")} calculates single flow directions. This means
#' that each pixel has only one downstream neighbor. Despite these
#' limitations, \code{"single"} will be mostly the algorithm of choice because
#' single flow directions are the basis for stream network and their
#' analysis.
#' 
#' \code{FLOWobj(DEM, "multi")} derives multiple flow direction (MFD) and
#' \code{FLOWobj(DEM, "dinf")} derives D infinity according to the method 
#' by Tarboton (1997). Both algorithms cannot be used to calculate
#' stream networks or drainage basins, for example.
#' 
#' To convert a transfer matrix \code{M} to a \code{FLOWobj}, use the 
#' function \code{M2FLOWobj()}.
#' 
#' Supported arguments for DEM preprocessing are the following: 
#' 
#' sinks, a logical matrix of the same size as the \code{GRIDobj}. 
#' \code{TRUE} values in sinks are treated as sinks in the digital elevation  
#' model and are not filled or carved, if the preprocessing option fill or  
#' carve are chosen.
#' 
#' internaldrainage, a logical value, default is \code{FALSE}, set this 
#' parameter value to \code{TRUE} if flow directions should be derived in the 
#' lowest, flat regions of internal drainage basins. By default, this parameter 
#' is set to \code{FALSE} since this information is usually not required and 
#' flow paths will stop when entering flat, internally drained sections.
#' 
#' cheight, a numerical value, adjust cost matrix if preprocessing option 
#' \code{carve} has been chosen.
#' 
#' verbose, a logical value, verbose output in the command window to 
#' track computational progress. Particularly interesting when working with 
#' very large matrices.
#' 
#' @param GRIDobj \code{GRIDobj} object used for the operation
#' 
#' @param type \code{Character} keyword defining the method used to 
#' calculate the flow direction information. One out of \code{"single"},
#' \code{"multi"} and \code{"dinf"}. Default is \code{"single"}
#' 
#' @param preprocess \code{Character} value, option to preprocess the 
#' \code{GRIDobj}. One out of \code{"fill"} and \code{"none"}. 
#' Default is \code{"fill"}. See details for supported additional parameters.
#' 
#' @param \dots Further arguments passed to the internally used DEM 
#' preprocessing functions \code{fillsinks()}. See details for supported 
#' arguments.
#' 
#' @return Returns a \code{FLOWobj} object 
#' 
#' @references
#' Freeman, T. G.: Calculating catchment area with divergent flow based on 
#' a regular grid, Computers & Geosciences, 17, 413–422, 
#' https://doi.org/10.1016/0098-3004(91)90048-I, 1991.
#' 
#' Tarboton, D. G. (1997). A new method for the determination of flow 
#' directions and upslope areas in grid digital elevation models. 
#' Water Resources Research, 33(2), 309-319.
#' 
#' Eddins, S. (2016). Upslope area function. Mathworks File Exchange, 
#' https://www.mathworks.com/matlabcentral/fileexchange/15818-upslope-area-functions
#' 
#' Schwanghart, W., Groom, G., Kuhn, N. J., and Heckrath, G.: Flow network
#' derivation from a high resolution DEM in a low relief, agrarian landscape, 
#' Earth Surface Processes and Landforms, 38, 1576–1586, 
#' https://doi.org/10.1002/esp.3452, 2013.
#' 
#' @examples
#' 
#' ## LATER
#' x = 1
#' 
#' @author Michael Dietze, Wolfgang Schwanghart
#' 
#' @export FLOWobj

FLOWobj <- function(
    
  GRIDobj, 
  type = "single",
  preprocess = "fill",
  ...

) {
  
  ## extract additional arguments
  ags <- list(...)
  
  ## index NA values in GRIDobj
  i_na <- is.na(terra::values(GRIDobj))
  
  ## check/create boundary GRIDobj
  if("B" %in% names(ags)) {
    
    ## set automatic NA restoration flag to FALSE
    opt_restore_na <- TRUE
    
    ## extract boundary information
    B <- ags$B
    
    ## check boundary information for consistency
    if(ext(B) != ext(GRIDobj)) {
      stop("Boundary object extent does not match GRIDobj extent!")
    }
    
    if(crs(B) != crs(GRIDobj)) {
      stop("Boundary object CRS does not match GRIDobj CRS!")
    }
    
    if(dim(B) != dim(GRIDobj)) {
      stop("Boundary object grid size does not match GRIDobj grid size!")
    }
    
  } else {

    ## build boundary information from GRIDobj to matrix to GRIDobj
    B <- matrix(data = 0, nrow = dim(GRIDobj)[2], ncol = dim(GRIDobj)[1])
    B[c(1, nrow(B)),] <- 1
    B[,c(1, ncol(B))] <- 1
    B[i_na] <- 1
    B <- rast(t(B), extent = GRIDobj)
    opt_restore_na <- TRUE
  }
  
  ## WHAT IS FILLSINKS_HYBRID ? ###############################################
  
  ## OPTION CARVE NEEDS TO BE IMPLEMENTED #####################################
  
  DEM <- GRIDobj
  
  ## preprocess DEM
  if(preprocess == "fill") {
    
    DEM_filled <- fillsinks(GRIDobj= GRIDobj, B = B)
  } else {
    
    DEM_filled <- DEM
  }
  
  ## restore NA values
  if(opt_restore_na == TRUE) {
    
    terra::values(DEM_filled)[i_na] <- NA
    terra::values(DEM_flats)[i_na] <- NA
  }
  
  ## identify flats
  DEM_flats <- identifyflats(GRIDobj = DEM)
  
  ## compute costs GRIDobj
  DEM_costs_conns <- computecosts(flats = DEM_flats, 
                                  dem = DEM,
                                  filled = DEM_filled)
  
  # dist = np.zeros_like(flats, dtype=np.float32)
  # prev = conncomps  # prev: dtype=np.int64
  # heap = queue      # heap: dtype=np.int64
  # back = np.zeros_like(flats, dtype=np.int64)
  # _grid.gwdt(dist, prev, costs, flats, heap, back, dims)
  # 
  # node = heap  # node: dtype=np.int64
  # direction = np.zeros_like(dem, dtype=np.uint8)
  # _grid.flow_routing_d8_carve(
  #  node, direction, filled_dem, dist, flats, dims)
  # 
  # # ravel is used here to flatten the arrays. The memory order should not matter
  # # because we only need a block of contiguous memory interpreted as a 1D array.
  # source = np.ravel(conncomps)  # source: dtype=int64
  # target = np.ravel(back)       # target: dtype=int64
  # edge_count = _grid.flow_routing_d8_edgelist(
  #   source, target, node, direction, dims)
  # 
  # self.path = grid.path
  # self.name = grid.name
  # 
  # # raster metadata
  # self.direction = direction  # dtype=np.unit8
  # 
  # self.stream = node
  # self.source = source[0:edge_count]  # dtype=np.int64
  # self.target = target[0:edge_count]  # dtype=np.int64
  # 
  # self.shape = grid.shape
  # self.cellsize = grid.cellsize
  # self.strides = tuple(s // grid.z.itemsize for s in grid.z.strides)
  # self.order: Literal['F', 'C'] = ('F' if grid.z.flags.f_contiguous
  #                                  else 'C')
  # 
  # # georeference
  # self.bounds = grid.bounds
  # self.transform = grid.transform
  # self.crs = grid.crs

  return(1)
}
