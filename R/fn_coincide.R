#' fn_coincide
#'
#' The coincidence between microscopy classification and GSI is analyzed. The count
#' is based on the number of pairs that coincide (C) and the number of pairs that
#' are misqualified: mature by immature (MI) and immature by mature (IM)
#'
#' @param: N_Phase: A numerical value indicating the number of factor levels
#'
#' @param: df: A data frame with the values of each individual (row) by variables
#'     (columns)
#'
#' @param: Icut: A numerical value is the threshold value to separate into two
#'     groups
#'
#' @param: Coincide: A table filled with NA, with N+1 rows, one for each factor
#'    level plus 1 for the total, and four columns, the first with the name of
#'    factor levels and the rest for the categories: C, MI and IM
#'
#' @return: Table Coincide: A filled table with the number of coincidences and
#'    discrepancies by factor levels and the total (rows) by category (columns):
#'    Coincidence (C), mature by immature (MI) and immature by mature (IM).
#'
#' @examples:
#'  N_Phase <- 6
#'  df <- tmp
#'  Icut <- 2
#'  Coincide <- data.frame(matrix(NA, ncol = 4, nrow = as.integer(N_Phase+1)))
#'
#'    fn_coincide(N_Phase, df, Icut, Coincide)
#'
#' @export:
fn_coincide <- function(N_Phase, df, Icut, Coincide){
  df$PHIGS <- (ifelse(df$GSI >= Icut, 1, 0))
    for(i in 1:N_Phase+1){
    if(i < N_Phase+1){
      OK <- ifelse(df$Phase == i & df$MATURE == df$PHIGS, 1,0)
      MI <- ifelse(df$Phase == i & df$MATURE > df$PHIGS, 1,0)
      IM <- ifelse(df$Phase == i & df$MATURE < df$PHIGS, 1,0)
      Phase <- paste0("Phase_", as.roman(i))
    }else{
      OK <- ifelse(df$MATURE == df$PHIGS, 1,0)
      MI <- ifelse(df$MATURE > df$PHIGS, 1,0)
      IM <- ifelse(df$MATURE < df$PHIGS, 1,0)
      Phase <- "Total"
    } # End if
    Coincide [i,] <- c(Phase,sum(OK),sum(MI),sum(IM))
  } # End for
  Coincide
} # End Function

