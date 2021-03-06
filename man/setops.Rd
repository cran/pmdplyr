% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tbl_pb_methods.R
\name{setops}
\alias{setops}
\alias{intersect.tbl_pb}
\alias{union.tbl_pb}
\alias{union_all.tbl_pb}
\alias{setdiff.tbl_pb}
\title{Set operations}
\usage{
\method{intersect}{tbl_pb}(x, y, ...)

\method{union}{tbl_pb}(x, y, ...)

\method{union_all}{tbl_pb}(x, y, ...)

\method{setdiff}{tbl_pb}(x, y, ...)
}
\arguments{
\item{x}{objects to perform set function on (ignoring order)}

\item{y}{objects to perform set function on (ignoring order)}

\item{...}{other arguments passed on to methods}
}
\description{
These functions overwrite the set functions provided in base to make them generic to be used to
join pibbles. See  \link[dplyr]{setops} for details.
}
