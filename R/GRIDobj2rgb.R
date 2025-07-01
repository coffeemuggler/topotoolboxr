#' Convert GRIDobj object to RGB matrix
#' 
#' The function converts the values of a \code{GRIDobj} into 8 Bit numeric 
#' RGB values or hexadexcimal definitions of the colour codes. It requires 
#' the definition of a colour space (see examples).
#' 
#' Available colourspace keywords are the R palettes (\code{"heat.colors"},
#' \code{"topo.colors"}, \code{"terrain.colors"}). In addition, keywords for 
#' the [HCL palettes](https://colorspace.r-forge.r-project.org/articles/hcl_palettes.html) 
#' can be defined, such as \code{"blues3"}, \code{"BuGn"}, \code{"Viridis"},
#' \code{"Plasma"}, \code{"Batlow"}. 
#'  
#' @param GRIDobj \code{SpatRaster} to be converted
#' 
#' @param hex \code{Logical} value, option to return one matrix with 
#' hexadecimal colour codes instead of three matrices with 8 Bit RGB colour 
#' values. Default is \code{FALSE}. 
#' 
#' @param col \code{Character} value, name of the colour space to use. 
#' Default is \code{"terrain.colors"}.
#' 
#' @param n \code{Numeric} value, number of different colours to generate.
#' Default is \code{256} (8 Bit).
#' 
#' @return A \code{character} matrix with hexadecimal RGB values of the 
#' input data set. 
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' ## load example data
#' data(srtm_bigtujunga30m_utm11)
#' DEM <- terra::unwrap(srtm_bigtujunga30m_utm11)
#' 
#' ## convert DEM to RGB colour information
#' rgb <- GRIDobj2rgb(GRIDobj = DEM)
#' 
#' ## convert DEM to hex code using the heat-colors palette and ten colours
#' hex <- GRIDobj2rgb(GRIDobj = DEM, hex = TRUE, col = "inferno", n = 10)
#' 
#' ## plot image (required package 'grid')
#' grid::grid.raster(image = t(hex$Z))
#'
#'}
#' 
#' @author Michael Dietze, Wolfgang Schwanghart
#' 
#' @export GRIDobj2rgb

GRIDobj2rgb <- function(
    
  GRIDobj,
  hex = FALSE,
  col = "terrain.colors",
  n = 256
                          
) {
  
  ## CHECKS OF INPUT DATA -----------------------------------------------------
  
  ## check input data set
  if(inherits(GRIDobj, "SpatRaster") == FALSE) {
    stop("GRIDobj seems to be no SpatRaster object!")
  }
  
  ## DO THE ANALYSIS ----------------------------------------------------------
  
  ## extract xym information from GRIDobj
  xym <- GRIDobj2mat(GRIDobj = GRIDobj)
  
  ## convert Z matrix to vector
  z <- as.numeric(xym$Z)
  
  ## classify z values
  cl_z <- findInterval(x = z, vec = seq(from = min(z, na.rm = TRUE), 
                                        to = max(z, na.rm = TRUE), 
                                        length.out = n))
  
  ## create colour codes
  if(col %in% c("topo.colors", "terrain.colors", "heat.colors")) {
    
    cl_c <- data.frame(id = 1:n, col = do.call(col, list(n)))
    
  } else {
    
    cl_c <- data.frame(id = 1:n,
                       col = colorspace::sequential_hcl(n = n, palette = col))
  }
  
  
  ## assign colours             #### TO BE IMPROVED FOR SPEED AT SOME TIME ####
  for(i in 1:n) {
    cl_z[cl_z == i] <- cl_c$col[cl_c$id == i]
  }

  ## get number of matrix rows
  m <- nrow(xym$Z)
    
  ## assign colours to matrix
  if(hex == FALSE) {
    
    rgb <- t(col2rgb(col = cl_z))
    
    xym$Z <- list(R = matrix(data = rgb[,1], nrow = m, byrow = FALSE),
                  G = matrix(data = rgb[,2], nrow = m, byrow = FALSE),
                  B = matrix(data = rgb[,3], nrow = m, byrow = FALSE))
  } else {
    
    xym$Z <- matrix(data = cl_z, nrow = nrow(xym$Z), byrow = FALSE)
  }

  ## RETURN OUTPUT ------------------------------------------------------------
  
  ## return output
  return(xym)
}