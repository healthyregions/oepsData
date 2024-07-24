#' Standardize scale
#'
#' Take user defined scale and try to map it to one of a selection of possible 
#' inputs.
#'
#' @param scale String containing messy scale whose identity needs guessed.
#' 
#' @returns One of "state", "tract", "county", or "zcta".
standardize_scale <- function(scale) {
  if (grepl("state", scale, ignore.case = TRUE)) {
    return("state")
  }
  
  if (grepl("tract", scale, ignore.case = TRUE)) {
    return("tract")
  }
  
  if (grepl("county|counties", scale, ignore.case = TRUE)) {
    return("county")
  }
  
  if (grepl("zcta", scale, ignore.case = TRUE)) {
    return("zcta")
  }
}

#' Retrieve OEPS geometries
#' 
#' Download OEPS geometries of a specified scale.
#' 
#' @param scale String specifying geographic scale to pull from.
#' One of 'state', 'tract', 'county', or 'zcta'
#' @param quiet Boolean specifying whether to output st_read console output.
#' Defaults to FALSE.
#' 
#' @import sf
#' @returns Simple feature collection containing the specified geometries.
retrieve_geometry <- function(scale, quiet = FALSE) {
  shape_url <- "/vsicurl/https://raw.githubusercontent.com/GeoDaCenter/opioid-policy-scan/main/data_final/geometryFiles/"
  
  if (scale == "state") {
    shape_url <- paste0(shape_url, "state/states2010.shp")
  }
  
  if (scale == "tract") {
    shape_url <- paste0(shape_url, "tract/tracts2010.shp")
  }
  
  if (scale == "county") {
    shape_url <- paste0(shape_url, "county/counties2010.shp")
  }
  
  if (scale == "zcta") {
    shape_url <- paste0(shape_url, "zcta/zctas2010.shp")
  }
  
  return(sf::st_read(shape_url, quiet = quiet))
}

#'
#'
#'
cache_all_geometries <- function() {
  
}