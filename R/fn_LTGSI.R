#' fn_LTGSI
#'
#' The scattering plot is generated without any labels or marks for the X-axis,
#'  which is very simple. A horizontal line is included with the threshold value,
#'  which serves as the basis for the figures that will be generated through the
#'  analysis, whether they are simple or compound figures.
#'
#' @param: x: A vector containing the values of the independent variable.
#' @param: y: A vector containing the values of the dependent variable.
#' @param: df: A data frame
#' @param: xl: A label for the X-axis
#' @param: yl: A label for the y-axis
#' @param: Icut: A threshold Value
#' @param: ylimit: A vector value with a minimum and maximum limit for y-axis
#'
#' @return: A scatter plot of the y value by factor level across the x range also
#'  includes a horizontal dashed line representing the threshold value.
#'  Colors are manipulated with the rgb() function of the grDevices package.
#'
#'  @seealso: rgb() from grDevices (version 3.6.2)
#'
#' @examples:
#' x <- seq(1,30,by=1)
#' y <-x*3
#' df <- tmp
#' Icut <- 2
#' ylimit <- c(3,90)
#' ejemplo <- fn_LTGSI(x, y, df, xl, yl, Icut, ylimit)
#'
#' @export:
#' @importFrom: grDevices %>%
#'
fn_LTGSI <- function(x, y, df, xl, yl, Icut, ylimit){
  plot(x , y,
       col = rgb(0,0,0,1/4),
       xlab= xl,
       ylab= yl,
       pch= c(0,1,15,16,17,2)[as.numeric(df$Phase)],
       ylim= ylimit,
       xaxt="n"
      )

      abline(h= Icut,col="black",lty = 2)

}  # End function fn_LTGSI
