% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aest_area_plot.R
\name{aest_area_plot}
\alias{aest_area_plot}
\title{Area Plot}
\usage{
aest_area_plot(
  df,
  x_var,
  y_var,
  group_var,
  stack_or_fill = "stack",
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  caption_size = 5
)
}
\arguments{
\item{df}{a dataframe}

\item{x_var}{The variable to plot on the x axis}

\item{y_var}{The variable to plot on the y axis}

\item{group_var}{The grouping variable}

\item{stack_or_fill}{stack: original scale, fill: proportions, Default: 'stack'}

\item{title}{optional title, Default: NULL}

\item{subtitle}{optional subtitle, Default: NULL}

\item{caption}{If not provided, the caption is variable name and original file name, Default: NULL}

\item{caption_size}{font size for caption, Default: 5}
}
\value{
OUTPUT_DESCRIPTION
}
\description{
makes either a stacked or filled (proportions) area plot
}
\details{
DETAILS
}
\examples{
\dontrun{
if(interactive()){
 #EXAMPLE1
 }
}
}
\seealso{
\code{\link[scales]{label_number}}
}
