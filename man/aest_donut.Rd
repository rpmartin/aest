% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aest_donut.R
\name{aest_donut}
\alias{aest_donut}
\title{Donut plot}
\usage{
aest_donut(
  df,
  y_var,
  group_var,
  center_text = NULL,
  center_text_size = 7,
  caption = NULL,
  lab_size = 5
)
}
\arguments{
\item{df}{dataframe that contains}

\item{y_var}{The measure being plotted}

\item{group_var}{a grouping variable}

\item{center_text}{what text to put in middle, Default: NULL}

\item{center_text_size}{how large center font is, Default: 7}

\item{caption}{user provided caption, Default: NULL}

\item{lab_size}{font size for labels, Default: 5}
}
\value{
a donut chart
}
\description{
takes a dataframe with a grouping variable and a measure and makes a donut.
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
\code{\link[scales]{label_percent}}
}
