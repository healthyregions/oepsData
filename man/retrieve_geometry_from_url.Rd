% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geometry.R
\name{retrieve_geometry_from_url}
\alias{retrieve_geometry_from_url}
\title{Retrieve OEPS geometries from URL}
\usage{
retrieve_geometry_from_url(scale, quiet = FALSE)
}
\arguments{
\item{scale}{String specifying geographic scale to pull from.
One of 'state', 'tract', 'county', or 'zcta'}

\item{quiet}{Boolean specifying whether to output st_read console output.
Defaults to FALSE.}
}
\value{
Simple feature object containing the specified geometry.
}
\description{
Download OEPS geometries of a specified scale.
}
