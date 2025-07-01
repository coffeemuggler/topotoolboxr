#' Plot a GRIDobj
#' 
#' The function plots a GRIDobj. The data structure is converted to a terra 
#' SpatRaster object and then passed to the standard terra plot routine.
#' 
#' For the interactive case, the following additional arguments may be useful:
#' 
#' 1) \code{agg}, aggregation factor to reduce the resolution of the GRIDobj 
#'    to plot, e.g. use \code{agg = 10} to reduce the size by factor 10
#'    
#' 2) \code{exa}, exaggeration factor for the elevation, which is useful to 
#'    improve visibility of topography. Default is \code{1} (no exaggeration).
#'    
#' Available colourspace keywords can be seen on the [HCL palettes website](https://colorspace.r-forge.r-project.org/articles/hcl_palettes.html) 
#' , such as \code{"blues3"}, \code{"BuGn"}, \code{"Viridis"},
#' \code{"Plasma"}, \code{"Batlow"}. 
#'    
#' @param GRIDobj \code{GRIDobj} object to plot
#'
#' @param interactive \code{Logical} value, option to create an interactive 
#' plot via \code{plotly} instead of a static map. Default is \code{FALSE}. 
#' The option is only useful if the dataset is a DEM, hence the z-values
#' are elevation.
#' 
#' @param \dots Further arguments passed to the plot function. These can be 
#' keywords for the colour palette (e.g. \code{col = "inferno"}) and number of 
#' colours (e.g. \code{n = 10}). The default colour palette is 256 colours  
#' from \code{"terrain"}. See details.
#' 
#' @return Graphic output of a spectrogram.
#' 
#' @examples
#' 
#' ## load example data set
#' data(srtm_bigtujunga30m_utm11)
#' srtm_bigtujunga30m_utm11 <- terra::unwrap(srtm_bigtujunga30m_utm11)
#' 
#' ## plot data set as2D map
#' plot_GRIDobj(GRIDobj = srtm_bigtujunga30m_utm11)
#' 
#' ## plot data set as 3D interactive scene (requires package plotly), note 
#' ## that the scence is aggregated by factor 10 and exaggerated by factor 2
#' # plot_GRIDobj(GRIDobj = srtm_bigtujunga30m_utm11, 
#' #              interactive = TRUE, agg = 10, exa = 2)
#' 
#' ## plot data set in customised colour
#' plot_GRIDobj(GRIDobj = srtm_bigtujunga30m_utm11, col = "viridis")
#' 
#' ## plot data set without legend
#' plot_GRIDobj(GRIDobj = srtm_bigtujunga30m_utm11, legend = FALSE)
#' 
#' 
#' @export plot_GRIDobj

plot_GRIDobj <- function(
    
  GRIDobj, 
  interactive = FALSE,
  ...
  
){
  
  ## check data structure
  if(inherits(x = GRIDobj, what = "SpatRaster") == FALSE) {
    stop("Data set is not a valid SpatRaster!")
  }
  
  ## extract extra arguments
  dots <- list(...)
  
  ## check/set number of colours n
  if ("n" %in% names(dots)) {
    exa <- dots$n
  } else {
    n <- 256
  }
  
  ## check/set colour scale
  if ("col" %in% names(dots)) {
    col <- dots$col
  } else {
    col <- "terrain"
  }
  
  ## create plot colour palette
  col_plt <- colorspace::sequential_hcl(n = n, palette = col)

  ## check/set exa
  if ("exa" %in% names(dots)) {
    exa <- dots$exa
  } else {
    exa <- 1
  }
  
  ## check/set xlim
  if ("xlim" %in% names(dots)) {
    xlim <- dots$xlim
  } else {
    xlim <- terra::ext(GRIDobj)[1:2,]
  }
  
  ## check/set ylim
  if ("ylim" %in% names(dots)) {
    ylim <- dots$ylim
  } else {
    ylim <- terra::ext(GRIDobj)[3:4,]
  }
  
  ## check/set zlim
  if ("zlim" %in% names(dots)) {
    zlim <- dots$zlim
  } else {
    zlim <- range(terra::values(GRIDobj), na.rm = TRUE)
  }
  
  ## check/set legend option
  if ("legend" %in% names(dots)) {
    legend <- dots$legend
  } else {
    legend <- TRUE
  }
  
  ## check/set axes option
  if ("axes" %in% names(dots)) {
    axes <- dots$axes
  } else {
    axes <- TRUE
  }
  
  ## check/set annotation option
  if ("ann" %in% names(dots)) {
    ann <- dots$ann
  } else {
    ann <- TRUE
  }
  
  ## check/set add option
  if ("add" %in% names(dots)) {
    add <- dots$add
  } else {
    add <- FALSE
  }
  
  ## set z values out of range to max/min values
  terra::values(GRIDobj)[terra::values(GRIDobj) < zlim[1]] <- zlim[1]
  terra::values(GRIDobj)[terra::values(GRIDobj) > zlim[2]] <- zlim[2]
  
  ## check plot type
  if(interactive == FALSE) {
    
    ## plot terra raster
    terra::plot(GRIDobj, xlim = xlim, ylim = ylim, zlim = zlim, col = col_plt,
                legend = legend, axes = axes, ann = ann, add = add)
    
  } else {
    
    ## check if package plotly is installed
    if (requireNamespace("plotly", quietly = TRUE) == FALSE) {
      
      stop("Package plotly is not installed!")
    }
    
    ## optionally aggregate raster
    if ("agg" %in% names(dots)) {
      GRIDobj <- terra::aggregate(x = GRIDobj, dots$agg)
    }
    
    ## optionally exaggerate vertically
    if ("exa" %in% names(dots)) {
      GRIDobj <- GRIDobj * exa
    }
    
    
    
    ## normalise extents for scaling    
    rng_x <- abs(terra::ext(GRIDobj)[2,] - terra::ext(GRIDobj)[1,])
    rng_y <- abs(terra::ext(GRIDobj)[4,] - terra::ext(GRIDobj)[3,])
    rng_z <- abs(max(terra::values(GRIDobj) * exa, na.rm = TRUE) - 
                   min(terra::values(GRIDobj) * exa, na.rm = TRUE))
    rng_max <- max(rng_x, rng_y, rng_z)
     
    ## plot terra raster via plotly 
    fig <- plotly::plot_ly(x = unique(terra::crds(GRIDobj)[,1]), 
                   y = unique(terra::crds(GRIDobj)[,2]), 
                   z = matrix(terra::values(GRIDobj),
                              nrow = dim(GRIDobj)[1], 
                              byrow = TRUE),
                   colors = col_plt)
    fig <- plotly::add_surface(fig)
    plotly::layout(fig, 
                   scene = list(aspectmode = "manual", 
                                xaxis = list(range = xlim),
                                yaxis = list(range = ylim),
                                zaxis = list(range = zlim * exa),
                                aspectratio = list(x = rng_x / rng_max, 
                                                y = rng_y / rng_max, 
                                                z = rng_z / rng_max),
                             camera = list(eye = list(x = 0,
                                                      y = 0,
                                                      z = 1.5),
                             center = list(x = 0,
                                           y = 0,
                                           z = 0))))
  }
  
}