#' fn_hsd
#'
#' The row name is used to sort the factor column result from the TukeyHSD test.
#' These are renamed with the name of the phases and finally, they are sorted,
#' in decreasing mode, by the average response values (column 1).
#'
#'
#' @param: hsd_out A TukeyHSD test table, comes from the result of the
#'      Tukey_HSD function
#' @param: Phases_legend: A character vector that has the name or number of maturity stages defined by the user.
#'
#' @return A vector with the names of the phases arranged in descending order of the average response.
#'
#' @examples
#' hsd_out <- tmp
#' Phases_legend <- c("I", "II", "III", "IV", "V", "VI")
#' ejemplo <- fn_hsd(hsd_out,Phases_legend)
#'
#' @export
fn_hsd <- function(hsd_out,Phases_legend){
  hsd_tmp <- hsd_out$groups[order(rownames(hsd_out$groups)), ]
  rownames(hsd_tmp) <- Phases_legend
  hsd_tmp <- hsd_tmp[order(hsd_tmp[,1], decreasing = TRUE),]
  r_names <- rownames(hsd_tmp)
  return <- r_names
} # End fn_hsd
