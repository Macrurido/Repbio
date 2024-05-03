#' fn_freq
#
#' The interval and class mark are defined and the absolute, cumulative and
#'  relative cumulative frequency is calculated in a table.
#'
#' @param: df: A data frame.
#' @param: Imin: Minimum value to be defined for the x-axis.
#' @param: Imax: Maximum value to be defined for the x-axis.
#' @param: bin: A bin value to define the width of the class interval.
#'
#' @return: Table with the absolute, cumulative and relative cumulative
#'  frequency by interval and class mark.
#'
#' @examples
#' df <- tmp
#' Imin <- 1
#' Imax <- 30
#' bin <- 1
#' ejemplo <-  fn_freq(df, Imin, Imax, bin)
#'
#' @export

fn_freq <- function(df, Imin, Imax, bin){
                  breaks<- seq(Imin, Imax, bin)
                  tmp.cut <- cut(df,breaks, right=FALSE)
                  fre = table(tmp.cut)
                  cum <- cumsum(fre)
                  frc <- cum/max(cum)
                  MCmin <- Imin+(bin/2)
                  MCmax <- Imax-(bin/2)
                  MC<- seq(MCmin,MCmax,bin)
                  cbind(MC,fre,cum,frc)
}   # End function
