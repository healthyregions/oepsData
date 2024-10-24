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
#' Either download OEPS geometries, or use cached versions.
#'
#' @param scale String specifying geographic scale to pull from.
#' @param quiet Boolean specifying whether to output st_read console output.
#' @param cache Boolean specifying whether to use the cache. If t
#'
#' @returns Simple feature object containing the specified geometry.
retrieve_geometry <- function(scale, quiet = FALSE, cache=TRUE) {
  if (cache) {
    expected_file_path <- file.path(cache_dir(), paste0(scale, "2010", ".shp"))

    if (file.exists(expected_file_path)) return( sf::st_read(expected_file_path, quiet=quiet))
  }

  geom <- retrieve_geometry_from_url(scale, quiet=quiet)

  if (cache) cache_file(geom, file_name=paste0(scale, "2010", ".shp"))

  return(geom)
}


#' Retrieve OEPS geometries from URL
#'
#' Download OEPS geometries of a specified scale.
#'
#' @param scale String specifying geographic scale to pull from.
#' One of 'state', 'tract', 'county', or 'zcta'
#' @param quiet Boolean specifying whether to output st_read console output.
#' Defaults to FALSE.
#'
#' @import sf
#' @returns Simple feature object containing the specified geometry.
retrieve_geometry_from_url <- function(scale, quiet = FALSE) {

  if (scale == "state") {
    shape_url <- "/vsizip//vsicurl/https://herop-geodata.s3.us-east-2.amazonaws.com/oeps/state-2010-500k-shp.zip"
  }

  if (scale == "tract") {
    shape_url <- "/vsizip//vsicurl/https://herop-geodata.s3.us-east-2.amazonaws.com/oeps/tract-2010-500k-shp.zip"
  }

  if (scale == "county") {
    shape_url <- "/vsizip//vsicurl/https://herop-geodata.s3.us-east-2.amazonaws.com/oeps/county-2010-500k-shp.zip"
  }

  if (scale == "zcta") {
    shape_url <- "/vsizip//vsicurl/https://herop-geodata.s3.us-east-2.amazonaws.com/oeps/zcta-2018-500k-shp.zip"
  }

  return(sf::st_read(shape_url, quiet = quiet)['HEROP_ID'])
}

#' Cache all OEPS geometries.
#'
#' Downloads and caches all OEPS geometries, overwriting any previously cached
#' geometries.
#'
#' @param scales String or list of strings specifying which geometries to cache.
#' Must be one of "state", "tract", "county", "zcta" or "all". Defaults to "all."
#'
#' @export
cache_geometries <- function(scales='all') {

  if (!all(grepl("state|tract|county|zcta|all", scales, ignore.case = TRUE))) {
    stop('Input scale must be one of "state", "tract", "county", "zcta", or "all"')
  }

  if (any(grepl("all", scales, ignore.case = TRUE))) {
    scales <- c('state', 'tract', 'county', 'zcta')
  }

  geoms <- lapply(scales,
                  function(scale) {
                    message(paste("Retrieving", scale, "level geometries..."))
                    retrieve_geometry_from_url(scale, quiet=TRUE)
                    }
                  )

  file_names <- lapply(X=scales, FUN=get_cached_geometry_name)

  mapply(FUN=cache_file, geoms, file_names)

  return(invisible(NULL))
}

#' Cached geometry name
#'
#' Retrieve the file_name -- but not location -- of a cached geometry file.
#'
#' @param scale Character specifying scale of the requested geometry. Should be
#' one of "state", "tract", "zcta", or "county".
#' @param cb Boolean specifying whether or not the file is a cartographic boundary
#' file. Defaults to FALSE.
#' @param year Numeric or character specifying the year the geometries correspond
#' to. Defaults to 2010.
#'
#' @returns string containing the file name of geometry satisfying the given
#' parameters.
get_cached_geometry_name <- function(scale, cb=FALSE, year=2010) {

  if (cb) {
    return(paste0("herop_", scale, "_", year, "_cb.shp"))
  }

  return(paste0("herop_", scale, "_", year, ".shp"))
}