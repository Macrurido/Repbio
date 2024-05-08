#' modelo
#'
#' Fit the model using the nlsLM function. For more details see the function
#' documentation: nlsLM.
#'
#' @param: x: independent variable.
#' @param: y: dependent variable.
#' @param: mod: A nonlinear model formula including variables and parameters. Will
#'      be coerced to a formula if necessary.
#' @param: bb: A list or numeric vector of starting estimates. If par is a
#'      list, then each element must be of length 1.
#' @param: bmin: A numeric vector of lower bounds on each parameter. If not
#'      given, the default lower bound for each parameter is set to -Inf.
#' @param: bmax: A numeric vector of upper bounds on each parameter. If not
#'      given, the default upper bound for each parameter is set to Inf.
#'
#' @return: Model settings list.
#'
#' @seealso: nlsLM() from minpack.lm (version 1.2-4).
#'
#' @examples:  modelo(x,y,mod,bb,bmin,bmax)
#'
#' @importFrom: minpack.lm %>%
#'
#' @export
modelo <- function(x,y,mod,bb,bmin,bmax){
  nlsLM(formula = mod,
        start = bb,
        lower = bmin,
        upper = bmax
  )
} # End function
