#' fn_Boxplot
#'
#' A very simple multiple boxplots (more than one level of factor) is generated
#' with no labels or marks for the X-axis. It serves as the basis for the figures
#' that will be generated through the analysis, whether they are simple or complex.
#'
#' @param: val:  A column containing the numeric values
#' @param: fac:  The factor column (grouping column)
#' @param: df:   The data frame containing the data
#' @param: yrange: A vector with the minimum and maximum values of the y-axis
#'
#' @return: A multiple boxplots which can be upgraded according to needs
#'
#' @examples:
#' For example, in the tmp data frame, the variable to analyze is in the LT
#'      column and the factors in the Phases column.
#'
#'   val <- tmp$LT
#'   fac <- as.factor(tmp$Phase)
#'   df <- tmp
#'   yrange <- c(20,300)
#'
#'    ejemplo <- fn_Boxplot(val,fac,df,yrange)
#'
#' @export:
fn_Boxplot <- function(val,fac,df,yrange){
  boxplot(val~fac,data=df,
          xlab="",
          ylab="",
          col="white",
          border="black",
          notch = FALSE,
          ylim= yrange,
          xaxt="n"
          )
} # End function
