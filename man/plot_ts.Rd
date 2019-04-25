% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_ts.R
\name{plot_ts}
\alias{plot_ts}
\title{Plot time series}
\usage{
plot_ts(x, n)
}
\arguments{
\item{x}{is either a \code{somsp} or a \code{regs} object}

\item{n}{is either the set of nodes for \code{somsp} or the number of regions for \code{regs}}
}
\description{
Plots the time series of SOM nodes or regions mean
}
\details{
In case of \code{regs}, all the regions are ploted.
}
\examples{
my_som <- somspa(aa), relen = 1000, grid = somgrid(6, 6, "hexagonal"))
plot_ts(my_som, 5)
plot_ts(my_som, c(1, 5, 18, 21))
plot_ts(my_som, 1:max(my_som$summary$node))

my_regions <- somregs(my_som, 10)
plot_ts(my_regions, 2)
plot_ts(my_regions, 10)

}