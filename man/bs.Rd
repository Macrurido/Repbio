% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bs.R
\name{bs}
\alias{bs}
\title{bs}
\usage{
bs(df, B, n, semilla)
}
\arguments{
\item{df}{A data frame}

\item{B}{An integer value indicating the number of of resamples}

\item{n}{An integer value indicating the number of observations}

\item{semilla}{An integer value is a seed to initialize a pseudorandom number generator.}
}
\value{
A matrix **mb** that includes both the original data set and the ith-generated resamples.
}
\description{
The function `bs` generates a matrix that resamples length with replacement.
The original data set is stored in the first row, while the resamples are stored
in the ith row Bi+1, where B is the total number of resamples.}
\examples{
  \dontrun{
      df = tmp
      B <- 2000
      n <- 3000
      semilla <- 100
# Reproducing the same output every time
      ejemplo <- bs(df,B,n,semilla)

# Reproducing the different output every time
      semilla <- NULL
      ejemplo <- bs(df,B,n,semilla)
    }
}
