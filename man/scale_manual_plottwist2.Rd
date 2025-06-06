% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale_manual.R
\name{scale_manual_plottwist2}
\alias{scale_manual_plottwist2}
\alias{scale_symbol_manual}
\alias{scale_pattern_manual}
\alias{scale_pattern_background_manual}
\alias{scale_pattern_linewidth_manual}
\alias{scale_pattern_scale_manual}
\title{Create more discrete scales}
\usage{
scale_symbol_manual(..., values, breaks = waiver(), na.value = NA)

scale_pattern_manual(..., values, breaks = waiver(), na.value = NA)

scale_pattern_background_manual(..., values, breaks = waiver(), na.value = NA)

scale_pattern_linewidth_manual(..., values, breaks = waiver(), na.value = NA)

scale_pattern_scale_manual(..., values, breaks = waiver(), na.value = NA)
}
\arguments{
\item{...}{
  Arguments passed on to \code{\link[ggplot2:discrete_scale]{discrete_scale}}
  \describe{
    \item{\code{limits}}{One of:
\itemize{
\item \code{NULL} to use the default scale values
\item A character vector that defines possible values of the scale and their
order
\item A function that accepts the existing (automatic) values and returns
new ones. Also accepts rlang \link[rlang:as_function]{lambda} function
notation.
}}
    \item{\code{drop}}{Should unused factor levels be omitted from the scale?
The default, \code{TRUE}, uses the levels that appear in the data;
\code{FALSE} includes the levels in the factor. Please note that to display
every level in a legend, the layer should use \code{show.legend = TRUE}.}
    \item{\code{na.translate}}{Unlike continuous scales, discrete scales can easily show
missing values, and do so by default. If you want to remove missing values
from a discrete scale, specify \code{na.translate = FALSE}.}
    \item{\code{name}}{The name of the scale. Used as the axis or legend title. If
\code{waiver()}, the default, the name of the scale is taken from the first
mapping used for that aesthetic. If \code{NULL}, the legend title will be
omitted.}
    \item{\code{labels}}{One of:
\itemize{
\item \code{NULL} for no labels
\item \code{waiver()} for the default labels computed by the
transformation object
\item A character vector giving labels (must be same length as \code{breaks})
\item An expression vector (must be the same length as breaks). See ?plotmath for details.
\item A function that takes the breaks as input and returns labels
as output. Also accepts rlang \link[rlang:as_function]{lambda} function
notation.
}}
    \item{\code{guide}}{A function used to create a guide or its name. See
\code{\link[ggplot2:guides]{guides()}} for more information.}
    \item{\code{call}}{The \code{call} used to construct the scale for reporting messages.}
    \item{\code{super}}{The super class to use for the constructed scale}
  }}

\item{values}{a set of aesthetic values to map data values to. The values
will be matched in order (usually alphabetical) with the limits of the
scale, or with \code{breaks} if provided. If this is a named vector, then the
values will be matched based on the names instead. Data values that don't
match will be given \code{na.value}.}

\item{breaks}{One of:
\itemize{
\item \code{NULL} for no breaks
\item \code{waiver()} for the default breaks (the scale limits)
\item A character vector of breaks
\item A function that takes the limits as input and returns breaks
as output
}}

\item{na.value}{The aesthetic value to use for missing (\code{NA}) values}
}
\description{
These functions allow you to specify your own set of mappings from levels in
the data to the new aesthetics included in plottwist2, following the format
of the scale_manual_x functions in ggplot2.
}
\examples{
ggplot2::ggplot(data = iris,
ggplot2::aes(x = Sepal.Length,
             y = Petal.Length)) +
 geom_symbol(ggplot2::aes(symbol = Species)) +
 scale_symbol_manual(values = c(3, 1, 8))

}
