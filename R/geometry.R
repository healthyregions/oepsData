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
    shape_url <- paste0(shape_url, "zcta/zctas2018.shp")
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


#' Translate to FIPS
#'
#' Take State and possibly County names and return the FIPS code of the relevant
#' unit.
#'
#' @param state Name, abbreviation, or FIPS of the state to be converted
#' @param county Name or abbreviation of the county to be converted. Optional
#' 
#' @returns FIPS code for the state.
translate_to_fips <- function(state, county=NULL) {
  
  state <- tolower(state)
  
  if (state %in% states_id_table$NAME) {
    relevant_state <- state == states_id_table$NAME
    state_fips <- states_id_table[relevant_state,]$STATEFP
  } else if (state %in% states_id_table$ABBR) {
    relevant_state <- state == states_id_table$ABBR 
    state_fips <- states_id_table[relevant_state,]$STATEFP
  } else if (state %in% states_id_table$STATEFP) {
    state_fips <- state
  } else {
    stop('State must be either a state name, abbreviation, or FIPS.')
  }
  
  if (is.null(county)) {
    return(state_fips)
  }
  
  county <- gsub('\\s?county', '', tolower(county))
  
  # check for validity
  rel_counties <- county_id_table[county_id_table$STATEFP==state_fips,]
  valid_options <- c(rel_counties$GEOID, rel_counties$NAME, rel_counties$COUNTYFP)
  if (!(all(county %in% valid_options))) {
    stop('Counties must be valid county names, COUNTYFP, or GEOIDs from the same state.')
  } 
  
  geoids <- map(county, .f <- function(cnty) {
    if (cnty %in% rel_counties$GEOID) {
      return(cnty)
    } else if (cnty %in% rel_counties$STATE_FP) {
      geoid <- rel_counties[rel_counties$STATE_FP == cnty]$GEOID
      return(geoid)
    } else if (cnty %in% rel_counties$NAME) {
      geoid <- rel_counties[rel_counties$NAME == cnty]$NAME
      return(geoid)
    }
  })
  
  return(geoids)
}