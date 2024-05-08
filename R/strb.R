#' strb
#'
#' This function helps in the visual search of the starting input parameters
#' for nlsLM, to fit the specified model (mod), using a graph for visual
#' adjustment of model parameters (Trial and Error Strategy).
#'
#' @param: b3: The initial value for the medium asymptote. Valid only for double
#'           sigmoid model, NA is assigned for the simple model.
#' @param: b4: The initial value for the coordinate at the inflection point for
#'           simple sigmoid model. For the double sigmoid model, it corresponds
#'           to the first inflection point.
#' @param: b5: The initial value for the amplitudes of the sigmoid, for the double
#'           sigmoid model corresponds to the amplitude of the first sigmoid.
#'           b5= L95a-b4 ,L95a is the lengths at 95 percent maturity for the
#'           complete sigmoid (single model) or the first sigmoid (double model)
#' @param: b6: The coordinate at the second inflection point. Valid only for double
#'           sigmoid model, NA is assigned for the simple model.
#' @param: b7: The initial value for the amplitude of the second sigmoid. Valid
#'           only for double sigmoid model, NA is assigned for the simple model.
#'           b7= L95b-b6 ,L95b is the lengths at 95 percent maturity from the
#'           second distribution.
#' @param: x: A vector of the X-axis values.
#' @param: y: A vector of the Y-axis values.
#' @param: mod: A nonlinear model formula including variables and parameters.
#' @param: modelo: function from Repbio, to fit the specified model (mod).
#' @param: bl: Lower (bl) weight values apply to each parameter (bi).
#' @param: bu: Upper (bu) weight values apply to each parameter (bi).
#' @param: xl: Title label for X axis.
#' @param: yl: Title label for Y axis.
#'
#' @return 1) A plot with the observed, fitted values and its confidence interval.
#' @return 2) Model settings list.
#'
#' @examples  strb(b3,b4,b5,b6,b7,x,y,mod,modelo, bl, bu, xl, yl)
#'
#' @export
#' @importFrom minpack.lm::nlsLM

strb <- function(b3,b4,b5,b6,b7,x,y,mod,modelo, bl, bu, xl, yl){
  bb <- c(b3=b3,b4=b4,b5=b5,b6=b6,b7=b7)
  bb <- bb[!is.na(bb)]
  bmin <- c(bb*bl)
  bmax <- c(bb*bu)
  fit <- modelo(x,y,mod,bb,bmin,bmax)

  plot(x,y,
       xlab=xl,
       ylab= yl)
  lines(x,fitted(fit))

  fit
} # End function
