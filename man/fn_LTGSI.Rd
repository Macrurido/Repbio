% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_LTGSI.R
\name{fn_LTGSI}
\alias{fn_LTGSI}
\title{fn_LTGSI}
\usage{
fn_LTGSI(x, y, df, xl, yl, Icut, ylimit)
}
\arguments{
\item{x}{A vector containing the values of the independent variable.}

\item{y}{A vector containing the values of the dependent variable.}

\item{df}{A data frame}

\item{xl}{A label for the X-axis}

\item{yl}{A label for the y-axis}

\item{Icut}{A threshold Value}

\item{ylimit}{A vector value with a minimum and maximum limit for y-axis}
}
\value{
A scatter plot of the y value by factor level across the x range also includes a horizontal dashed line representing the threshold value.
Colors are manipulated with the rgb() function of the grDevices package
}
\description{
The scattering plot is generated without any labels or marks for the X-axis, which is very simple. A horizontal line is included with the threshold value, which serves as the basis for the figures that will be generated through the analysis, whether they are simple or compound figures.
}
\examples{
  \dontrun{
  x <-x
  y <- y
  df <- tmp
  xl <- etiquetas[1]
  yl <- etiquetas[3]
  Icut <- 2.3
  ylimit <- c(0,45)
 ejemplo <- fn_LTGSI(x, y, df, xl, yl, Icut, ylimit)
  }
}
