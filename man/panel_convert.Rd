% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/panel_convert.R
\name{panel_convert}
\alias{panel_convert}
\title{Convert between panel data types}
\usage{
panel_convert(data, to, ...)
}
\arguments{
\item{data}{Data frame - a \code{pibble}, \code{tsibble}, \code{pdata.frame}, or \code{panel_data} object.}

\item{to}{Character variable set to \code{"pmdplyr", "pibble", "tbl_pb", "tsibble", "tbl_ts", "plm", "pdata.frame", "panelr"} or \code{"panel_data"} indicating the type/package to be converted to.}

\item{...}{Additional arguments to be sent to, respectively, \code{as_pibble()}, \code{tsibble::as_tsibble()}, \code{plm::pdata.frame()}, or \code{panelr::panel_data()}.}
}
\description{
This function takes panel data objects declared using \code{pmdplyr} (\code{pibble}/\code{tbl_pb}), \code{tsibble} (\code{tsibble}/\code{tbl_ts}), \code{plm} (\code{pdata.frame}), and \code{panelr} (\code{panel_data}) and converts to one of the other three formats for use with functions in those packages.
}
\details{
Any grouping will be lost. You must have the relevant package installed to convert to the type for that package. Conversions from \code{pdata.frame} will be improved if \code{sjlabelled} is also installed.

When using \code{panel_convert}, be aware of the requirements that each type has:

 \tabular{lcccc}{
   Feature/Requirement \tab  \code{pibble}   \tab  \code{tsibble} \tab  \code{pdata.frame} \tab  \code{panel_data} \cr
                         \tab           \tab                \tab                 \tab \cr
   ID                    \tab \code{.i} \tab \code{key}     \tab \code{index[1]} \tab \code{id} \cr
   Time                  \tab \code{.t} \tab \code{index}   \tab \code{index[2]} \tab \code{wave} \cr
   Gap control           \tab \code{.d} \tab \code{regular} \tab No              \tab No  \cr
   ID must exist         \tab No        \tab No             \tab Yes             \tab Yes \cr
   Time must exist       \tab No        \tab Yes            \tab Yes             \tab Yes[1] \cr
   Only one ID variable[2]\tab No        \tab No             \tab Yes             \tab Yes \cr
   Unique identification \tab No        \tab Yes            \tab No[3]           \tab No[3]
 }

 [1] \code{pdata.frame} does not require that time be provided, but if not provided will create it based on original ordering of the data. The \code{pdata.frame} option to set \code{index} equal to an integer for a balanced panel and have it figure out the rest by itself is not supported.

 [2] Use \code{pmdplyr::id_variable()} to generate a single ID variable from multiple if one is required.

 [3] \code{pdata.frame} and \code{panel_data} do not require that ID and time uniquely identify the observations on declaring the data, but functions in these packages may not work correctly without unique identification.

In addition to the above, be aware that the different packages have different requirements on which variable classes can be Time variables. \code{pmdplyr::time_variable()} can build an integer variable that will work in all packages.

You may run into some trouble if your data contains variables by the names \code{panel_convert_id}, \code{panel_convert_time}, \code{pibble_d}, or \code{panel_convert_regular}.
}
\examples{
# Only run examples if the relevant packages are installed
pkgs <- utils::installed.packages()

data(Scorecard)

# The example will turn a pibble to everything else
# But starting with another type will of course work!
S_pibble <- as_pibble(Scorecard, .i = unitid, .t = year)

# Get a tsibble
if ("tsibble" \%in\% pkgs) {
  head(panel_convert(S_pibble, to = "tsibble"))
}

# Now for pdata.frame
if ("plm" \%in\% pkgs) {
  head(panel_convert(S_pibble, to = "plm"))
}

# And finally panel_data
if ("panelr" \%in\% pkgs) {
  head(panel_convert(S_pibble, to = "panelr"))
}

}
