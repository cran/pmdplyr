% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tbl_pb_methods.R
\name{join.tbl_pb}
\alias{join.tbl_pb}
\alias{left_join.tbl_pb}
\alias{inner_join.tbl_pb}
\alias{right_join.tbl_pb}
\alias{full_join.tbl_pb}
\alias{semi_join.tbl_pb}
\alias{nest_join.tbl_pb}
\alias{anti_join.tbl_pb}
\title{Join two pibbles together}
\usage{
\method{left_join}{tbl_pb}(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)

\method{inner_join}{tbl_pb}(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)

\method{right_join}{tbl_pb}(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)

\method{full_join}{tbl_pb}(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)

\method{semi_join}{tbl_pb}(x, y, by = NULL, copy = FALSE, ...)

\method{nest_join}{tbl_pb}(x, y, by = NULL, copy = FALSE, keep = FALSE, name = NULL, ...)

\method{anti_join}{tbl_pb}(x, y, by = NULL, copy = FALSE, ...)
}
\arguments{
\item{x}{A pair of data frames, data frame extensions (e.g. a tibble), or
lazy data frames (e.g. from dbplyr or dtplyr). See \emph{Methods}, below, for
more details.}

\item{y}{A pair of data frames, data frame extensions (e.g. a tibble), or
lazy data frames (e.g. from dbplyr or dtplyr). See \emph{Methods}, below, for
more details.}

\item{by}{A character vector of variables to join by.

If \code{NULL}, the default, \verb{*_join()} will perform a natural join, using all
variables in common across \code{x} and \code{y}. A message lists the variables so that you
can check they're correct; suppress the message by supplying \code{by} explicitly.

To join by different variables on \code{x} and \code{y}, use a named vector.
For example, \code{by = c("a" = "b")} will match \code{x$a} to \code{y$b}.

To join by multiple variables, use a vector with length > 1.
For example, \code{by = c("a", "b")} will match \code{x$a} to \code{y$a} and \code{x$b} to
\code{y$b}. Use a named vector to match different variables in \code{x} and \code{y}.
For example, \code{by = c("a" = "b", "c" = "d")} will match \code{x$a} to \code{y$b} and
\code{x$c} to \code{y$d}.

To perform a cross-join, generating all combinations of \code{x} and \code{y},
use \code{by = character()}.}

\item{copy}{If \code{x} and \code{y} are not from the same data source,
and \code{copy} is \code{TRUE}, then \code{y} will be copied into the
same src as \code{x}.  This allows you to join tables across srcs, but
it is a potentially expensive operation so you must opt into it.}

\item{suffix}{If there are non-joined duplicate variables in \code{x} and
\code{y}, these suffixes will be added to the output to disambiguate them.
Should be a character vector of length 2.}

\item{...}{Other parameters passed onto methods.}

\item{keep}{Should the join keys from both \code{x} and \code{y} be preserved in the
output? Only applies to \code{nest_join()}, \code{left_join()}, \code{right_join()}, and
\code{full_join()}.}

\item{name}{The name of the list column nesting joins create. If \code{NULL} the name of \code{y} is used.}
}
\description{
These are generic functions that dispatch to individual pibble methods. pibble structure from \code{x} will be maintained. pibble structure from \code{y} will be lost. See  \link[dplyr]{join} for
complete documentation.
}
