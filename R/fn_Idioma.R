#' fn_Idioma
#'
#' The figure labels are defined as either English (1) or Spanish (2).
#'
#' @param: idioma: An integer value that defines the language of the labels.
#' @param: language: A list containing the labels of each language.
#'
#' @return **etiquetas** A Vector is defined by its labels, which include:
#'     total length, frequency, year, month, Gonadosomatic Index, maturity phase,
#'     and residuals. If more tags are needed, it must be redefined.
#'     They should be changed here in the function.
#'
#' @examples
#' Labels are in English
#'     idioma <-   1
#'         fn_Idioma(idioma,language)
#' Labels are in Spanish
#'     idioma <-   2
#'         fn_Idioma(idioma,language)
#'
#' @export
fn_Idioma <- function(idioma,language){
    if(idioma==1){
        etiqueta<- unlist(language[[1]], use.names = FALSE)
    }else{
        etiqueta<- unlist(language[[2]], use.names = FALSE)
    } # End if
  etiqueta
} # End fn_Idioma
