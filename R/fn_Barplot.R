#' fn_Barplot
#'
#' A very simple barplot is generated with no labels or marks for the X-axis,
#'     which serves as the basis for those that will be generated through the
#'     analysis, whether they are simple or compound figures.
#'
#' @param: df: A list of matrices with frequencies values
#' @param: i: A numeric value of the ith object in the list.
#' @param densidad: a vector that specifies the shading lines density (in lines
#'      per inch). By default, it is NULL, which means no shading lines. For
#'      more details, see the help of the barplot() function
#' @param angulo a vector that specifies the slope of shading lines. For
#'      more details, see the help of the barplot() function
#'
#' @return A simple barplot that can be upgraded according to needs.
#'
#' @examples
#' For example, for two sampled years i = 2 and six factor levels in the
#'  analyzed variable.
#' df <- tmp
#' i <- 2
#' densidad <- c(10,10,30,30,30,10)
#' angulo <- c(3,30,60,90,120,150)
#'
#' ejemplo <- fn_Barplot(df,i,densidad,angulo)
#'
#' @export
fn_Barplot <- function(df,i,densidad,angulo){
  barplot(df[[i]],
          xlab="",
          xaxt = "n",
          legend = FALSE,
          density= densidad,
          angle=angulo,
          border="black",
          col="black"
  )
} # End function
