% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ANOVA_1.R
\name{ANOVA_1}
\alias{ANOVA_1}
\title{ANOVA_1}
\usage{
ANOVA_1(formula, df, criterio)
}
\arguments{
\item{formula}{A formula specifying the model}
\item{df}{A data frame}
\item{criterio}{Criteria for separating active reproductive organisms}
}
\value{
A txt file with the ANOVA results
}
\description{
This function performs 1-way ANOVA using the function aov() from the R Stats
    Package, it returns the list av1 and save the ANOVA results as txt file.


Inside the function: The Analysis of Variance is performed using the aov() function
    from package stats (version 3.6.2). Results are aved in a txt file named as:
    ANOVA plus the name of the model variables, acronym criteria established to
    classify active females to reproduction (criterio) and the txt extension.


The object "criterio" must be defined outside the function.
    For the analysis performed in Salas-Singh et al. (2022), the "criterion" object presents two options:
    BP: Separate immature from mature;
    AR: Separate reproductively active mature organisms from reproductively inactive mature and immature organisms.
}
\examples{
  \dontrun{
    formula <- LT~Phase
    df <- tmp
    criterio <- "AR"
    ejemplo <- ANOVA_1(formula,df,criterio)
  }
}
