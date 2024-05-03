#' IC_plot
#'
#'Cumulative percentage of mature females against length. Fitting model for the
#'estimation of the L50 values and its confidence interval.
#'
#'
#' @param: x: A vector of the X-axis values
#' @param: y: A vector of the y-axis values
#' @param: yc: A vector with Y values calculated from the original data
#' @param: MClw: Lower value to be defined for the x-axis
#' @param: MCup: Upper value to be defined for the x-axis
#' @param: CIl: A table with the lower value of nonparametric confidence interval
#' @param: CIh: A table with the upper value of nonparametric confidence interval
#' @param: xl: A label for the X-axis
#' @param: yl: A label for the y-axis
#'
#' @return:
#'It returs a graphic representation of the cumulative percentage of mature
#'females as a function of length (points). The fitted model (black line) and its
#'95% confidence interval (gray shaded area) are included; the horizontal dashed
#'line represents the cumulative ratio of 0.5; the vertical dashed line indicates
#'the L50 value and the outer lines the 95% confidence interval.
#'
#' @seealso: smooth.spline() from stats R package and rgb() function from the grDevices package
#'
#' @examples:  ejemplo <- IC_plot(x,y,yc,MClw,MCup,CIl,CIh,xl,yl)
#'
#' @export:
#' @importFrom: stats %>%
#' @importFrom: grDevices %>%
#'
IC_plot <- function(x,y,yc,MClw,MCup,CIl,CIh,xl,yl){
  # Prediction lines and confidence intervals are smoothed
  smooth_yc <-  smooth.spline(x, yc, spar=0.4)
  CIls <- smooth.spline(x, CIl, spar=0.4)
  CIhs <- smooth.spline(x, CIh, spar=0.4)
  CIl <- as.vector(CIls$yin)
  CIh <- as.vector(CIhs$yin)

     plot(x,y,
       xlab= xl,
       ylab= yl,
       xlim= c(MClw,MCup),
       ylim= c(0,1),
       xaxt= "n",
       yaxt= "n",
       pch= 16, cex=0.8)
  lines(smooth_yc,col = "black", type = "l", lty = 1)

  polygon(c(x,rev(x)), c(CIl,rev(CIh)),
          col=rgb(0.5, 0.5, 0.5,0.2), border=NA)

 } # End function
