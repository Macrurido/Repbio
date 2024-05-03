#' bs
#'
#' The function `bs` generates a matrix that resamples length with replacement.
#' The original data set is stored in the first row, while the resamples are
#' stored in the ith row Bi+1, where B is the total number of resamples.
#'
#' If "seed" is an integer, the same results will be returned
#'         each time the same number is used; if "seed" is null, different
#'         results will be returned each time. For more information, see
#'         "RDocumentation" for the set.seed() function in the simEd package.
#'
#'
#' @param: df: A data frame
#' @param: B: An integer value indicating the number of of resamples
#' @param: n: An integer value indicating the number of observations
#' @param: semilla: An integer value is a seed to initialize a pseudorandom number generator.
#'
#' @return A matrix *mb* that includes both the original data set and the ith-generated resamples
#'
#' @examples
#'  df = tmp
#'B <- 2000
#'n <- 3000
#'semilla <- 100
#' # Reproducing the same output every time
#'ejemplo <- bs(df,B,n,semilla)
#' # Reproducing the different output every time
#'semilla <- NULL
#'ejemplo <- bs(df,B,n,semilla)
#'
#' @export
bs <- function(df,B,n,semilla) {
  set.seed(semilla)
    mb <- matrix(sample(df, size = B * n,
                      replace = TRUE), B, n)
    mb <- rbind(df,mb)  # The resampling data is added with the original data
}     # End function bs
