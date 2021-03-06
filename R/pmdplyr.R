#' \code{pmdplyr} package
#'
#' Suite of tools extending the \code{dplyr} package to perform data manipulation. These tools are geared towards use in panel data and hierarchical data.
#'
#' Unlike other suites dealing with panel data, all functions in \code{pmdplyr} are designed to work even when considering a set of variables that do not uniquely identify rows. This is handy when working with any kind of hierarchical data, or panel data where there are multiple observations per individual per time period, like student/term/class education data.
#'
#' \code{pmdplyr} contains the following functions:
#'
#' \itemize{
#'     \item{\code{\link{between_i}} and \code{\link{within_i}}}{ Standard between and within panel calculations.}
#'     \item{\code{\link{fixed_check}}}{ Checks a list of variables for consistency within a panel structure.}
#'     \item{\code{\link{fixed_force}}}{ Forces a list of variables to be constant within a panel structure.}
#'     \item{\code{\link{id_variable}}}{ Takes a list of variables that make up an individual identifier and turns it into a single variable.}
#'     \item{\code{\link{time_variable}}}{ Takes a time variable, or set of time variables, and turns them into a single well-behaved integer time variable of the kind required by most panel functions.}
#'     \item{\code{\link{inexact_join}}}{ Wrapper for the \code{dplyr} \code{\link[dplyr]{join}} functions which allows for a variable to be matched inexactly, for example joining a time variable in \code{x} to the most recent previous value in \code{y}.}
#'     \item{\code{\link{safe_join}}}{ Set of wrappers for the \code{dplyr::\link[dplyr]{join}} and \code{pmdplyr::inexact_join} functions which checks before merging whether each data set is uniquely identified as expected.}
#'     \item{\code{\link{pibble}}, \code{\link{as_pibble}}, and \code{\link{is_pibble}}}{ Set the panel structure for a data set, or check if it is already set.}
#'     \item{\code{\link{panel_convert}}}{ Converts between the panel data types \code{pmdplyr::pibble}, \code{tsibble::tsibble}, \code{plm::pdata.frame}, and \code{panelr::panel_data}.}
#'     \item{\code{\link{mutate_cascade}}}{ A wrapper for \code{dplyr} \code{\link[dplyr]{mutate}} which runs one period at a time, allowing changes in one period to finalize before the next period is calculated.}
#'     \item{\code{\link{mutate_subset}}}{ A wrapper for \code{dplyr} \code{\link[dplyr]{mutate}} that performs a calculation on a subset of data, and then applies the result to all the observations (within group).}
#'     \item{\code{\link{panel_fill}}}{ Fills in gaps in the panel. Can also fill in at the beginning or end of the data to create a perfectly balanced panel.}
#'     \item{\code{\link{panel_locf}}}{ A last-observation-carried-forward function for panels. Fills in \code{NA}s with recent nonmissing observations.}
#'     \item{\code{\link{tlag}}}{ Lags a variable in time.}
#' }
#'
#' @docType package
#' @name pmdplyr
NULL
#' @importFrom magrittr %>%
#' @importFrom lubridate %m-%


## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") utils::globalVariables(c("."))
