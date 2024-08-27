#' Merge keys
#'
#' Returns the vector of merge keys used by all data within this package.
#' 
#' @returns A vector of strings (the names of the merge key columns).
merge_keys <- function() { 
  merge_keys <- c("HEROP_ID", "GEOID", "TRACTCE", "STATEFP", "COUNTYFP") 
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

#' Filter by geographies
#'
#' Filters a given dataframe to only include entires with specified state and 
#' counties. If no state or counties are specififed, does nothing.
#'
#' @param df Dataframe to filter.
#' @param states String or vector of strings detailing which states to filter on.
#' Can be the states name, abbreviation, or GEOID. Optional. If null, counties must 
#' either not be specified or must be GEOIDS. Defaults NULL.
#' @param counties String or vector of strings detailing which counties to filter on.
#' Can be county names, abbreviations, or GEOIDS. Optional. If multiple counties are
#' passed, counties must either be from the same state or be GEOIDS.
#'
#' @returns Dataframe containing only observations which satisfy the filter.
filter_by_geography <- function(df, states, counties) {
  
  if (is.null(states) & is.null(counties)) {
    return(df)
  }
  
  if (is.null(counties) & !is.null(states)) {
    states <- state_to_fips(states)
    df <- filter_by_state(df, states)
    return(df)
  }
  
  if (is.null(states) & !is.null(counties)) {
    if (!(all(counties %in% county_id_table$GEOID))) {
      stop('If state is not specified, counties must be GEOIDS.')
    }
    df <- filter_by_county(df, counties)
    return(df)
  }
  
  # states and counties both specified
  state_fips <- state_to_fips(states)
  county_geoids <- county_to_fips(counties, state_fips)
  
  df <- filter_by_county(df, county_geoids)
}

#' Identify a state's FIPS code.
#'
#' Takes a states name, abbreviation, or FIPS code and returns the 
#' FIPS code.
state_to_fips <- function(state) {
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
  
  return(state_fips)
}

#' Identify counties GEOID
#' 
#' Takes one or more county names, alongside their relevant
#' state, and return the counties' GEOIDS.
#' 
#' @param county As in [filter_by_geography]
#' @param state_fips The FIPS code for one of the 50 states or DC.
#' 
#' @returns Single County GEOID or list of GEOIDS. 
county_to_fips <- function(county, state_fips) {
  county <- gsub('\\s?county', '', tolower(county))
  
  # check for validity
  rel_counties <- county_id_table[county_id_table$STATEFP==state_fips,]
  
  valid_options <- c(rel_counties$GEOID, rel_counties$NAME, rel_counties$COUNTYFP)
  if (!(all(county %in% valid_options))) {
    stop('Counties must be valid county names, COUNTYFP, or GEOIDs from the same state.')
  } 
  
  geoids <- lapply(county, FUN=function(cnty) {
    if (cnty %in% rel_counties$GEOID) {
      return(cnty)
    } else if (cnty %in% rel_counties$COUNTYFP) {
      geoid <- rel_counties[tolower(rel_counties$COUNTYFP) == cnty,]$GEOID
      return(geoid)
    } else if (cnty %in% rel_counties$NAME) {
      geoid <- rel_counties[tolower(rel_counties$NAME) == cnty,]$GEOID
      return(geoid)
    } 
  })
  
  return(unlist(geoids, use.names=FALSE))
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
