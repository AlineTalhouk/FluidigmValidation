% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filterPassDepth.R
\name{filterPassDepth}
\alias{filterPassDepth}
\title{Title function to subset data that has passed filter and minimum depth requirement. Important: do not call this function if you intend to filter according to quality number}
\usage{
filterPassDepth(data, minDepth)
}
\arguments{
\item{data}{: original data, filter with PASS and other measures and depths of all values}

\item{minDepth}{: minimum depth requirement}
}
\value{
data that has passed filter and have quality 100 and at or deeper than the minimum depth
}
\description{
Title function to subset data that has passed filter and minimum depth requirement. Important: do not call this function if you intend to filter according to quality number
}
\author{
Johnson Liu
}

