#' Standardize scale
#'
#' Take user defined scale and try to map it to one of a selection of possible 
#' inputs.
#'
#' @param scale String containing messy scale whose identity needs guessed.
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

#' Tidify non-merge key variables
#'
#' Convert dataframe from wide to tidy format, ignoring merge keys specified
#' in `merge_keys`.
#' @seealso [merge_keys()]
#' 
#' @param df Dataframe to convert to wide format.
#' 
#' @return Dataframe in tidy format.
#' @import utils
tidify_data <- function(df) {
  included_keys <- names(df)[names(df) %in% merge_keys()]
  observation_variables <- names(df)[!names(df) %in% included_keys]
  
  tidy_df <- cbind(df[included_keys], stack(df[observation_variables]))
  tidy_df <- tidy_df[c("HEROP_ID", "GEOID", "ind", "values")]
  names(tidy_df) <- c("HEROP_ID", "GEOID", "variable", "value") 
  
  return(tidy_df)
}

#' Merge keys
#'
#' Returns the vector of merge keys used by all data within this package.
#' 
#' @returns A vector of strings (the names of the merge key columns).
merge_keys <- function() { 
  merge_keys <- c("HEROP_ID", "GEOID", "TRACTCE", "STATEFP", "COUNTYFP") 
}

#' Filter by state
#'
#' Filters a given dataframe to only include entries from a given state.
#' Dataframe must possess a HEROP_ID column.
#' 
#' @param df Dataframe to filter. Must contain HEROP_ID. 
#' @param states String or vector of strings detailing which states to filter by.
#' 
#' @returns Dataframe containing only observations which occurred in a given state.
filter_by_state <- function(df, states) {
  
  # TODO: add a reference to a look-up table to parse abbreviations, state names
  
  stopifnot("HEROP_ID" %in% names(df))

  if (length(states) > 1) {
    return(Reduce(rbind, lapply(states, filter_by_state, df=df)))
  }
  
  state_fp <- gsub(" ", "0", sprintf("%2s", states))
  return(df[substr(df$HEROP_ID, 6, 7) == state_fp,])
}

#' Filter by county
#'
#' Filters a given dataframe to only include observations from a given
#' county. Dataframe must possess a HEROP_ID column.
#'
#' @param df Dataframe to filter. Must contain HEROP_ID.
#' @param counties String or vector of strings detailing which counties to filter to.
#'
#' @returns Dataframe containing only observations in the requested counties.
filter_by_county <- function(df, counties) {
  
  # TODO: add a reference to a look-up table to parse county names.
  # TODO: make robust to 5 character long FIPS.s
  
  stopifnot("HEROP_ID" %in% names(df))
  
  if (length(counties) > 1) {
    return(Reduce(rbind, lapply(counties, filter_by_county, df=df)))
  }
  
  if (nchar(counties) <= 3) {
    county_fp <- gsub(" ", "0", sprintf("%3s", counties))
    return(df[substr(df$HEROP_ID, 8, 10) == county_fp,])
  }
  
  county_fp <- gsub(" ", "0", sprintf("%5s", counties))
  return(df[substr(df$HEROP_ID, 6, 10) == county_fp,])
}