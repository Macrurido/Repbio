#' fn_porcentaje
#'
#' Calculates the percentage of each category (rows) per column (month),
#'       and also includes the total number of data per column (N).
#'
#'For the analysis performed in Salas-Singh et al. (2022), immatures fish may or
#'       may not be included in the analysis. If the "immatures" object is
#'       assigned zero, they are excluded from the analysis, any non-zero
#'       value includes them.
#'
#' @param: df: A data frame array, including the number of data by factor level (rows) and month (column)
#' @param: meses: A vector of characters representing the initials of the months of the year.
#' @param: immatures: An integer value defined outside this function.
#' @param: Phases_legend: A vector with the name or number of the stages of
#'      maturity, as specified in the object *Phases*, outside of this function.
#'      Legend for phases: 1 roman numeral, 2 names
#'
#' @return: countsp: A table with the percentage of each category and N.
#'
#' @examples:
#'  This is an example ... .
#'  The immatures are excluded from the analysis, giving them a value of zero.
#'
#' df <- tmp
#' meses <- c("J","F","M","A","M","J","J","A","S","O","N","D")
#' immatures <- 0
#' Phases_legend <- c("II", "III", "IV", "V", "VI")
#' ejemplo <- fn_porcentaje(df, meses, immatures, Phases_legend)
#'
#' @export:

fn_porcentaje <- function(df, meses, immatures, Phases_legend) {
  # Total data number per column
  n_mes <- round(apply(X= df, MARGIN=2, FUN= sum),0)
  n_mes[n_mes == 0] <- "-"
  # Percentage calculation
  for (ii in 1:length(meses)) {
    if(sum(df[,ii]) == 0){
      df[ii] <-"-"
    }else{
      df[ii] <- round(100*df[,ii]/sum(df[,ii]),0)
    }  #end if
  } #end for

  # Add total data number per column
  countsp <- rbind (df,n_mes)
  names(countsp) <-meses
      if (immatures == 0){
          rownames(countsp) <- c(Phases_legend[-c(1)],"N")
      } else{
          rownames(countsp) <- c(Phases_legend,"N")
      } # end if
  countsp
} # end Function
