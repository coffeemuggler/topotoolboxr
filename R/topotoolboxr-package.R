## usethis namespace: start
#' @useDynLib topotoolboxr, .registration = TRUE, .fixes = "C_"
#' 
#' @importFrom terra res values crds crs ext rast project aggregate
#' @importFrom grDevices heat.colors topo.colors terrain.colors
#' 
## usethis namespace: end

NULL

#' A digital elevation model of the Big Tujunga catchment 
#'
#' The dataset contains a wrapped SpatRaster data set, representing a 
#' digital elevation model (DEM) of the Big Tujunga catchment, USA. After 
#' loading the example data, it needs to be unwrapped in order to use it.
#'
#' @docType data
#' @keywords datasets
#' @name srtm_bigtujunga30m_utm11
#' @usage data(srtm_bigtujunga30m_utm11)
#' @format A SpatRaster with 643 rows and 1197 columns, referenced in the 
#' CRS UTM zone 11N.
NULL

