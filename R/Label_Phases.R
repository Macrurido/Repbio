#' Label_Phases
#'
#' Maturity phase label
#'
#' @description:
#' `Label_Phases` returns the maturity phase label that will be used in tables
#'  or figures. It returns a 'Phases_legend' vector in roman numeral or name in
#'  English or Spanish according to the user's choice.
#'
#' @details:
#' Arguments are 'genero' (Gender),'idioma' (Lenguage) and Phases (maturity phases)
#' genero: Value *1* selects Females and *2* Males
#' idioma: Value *1* selects English language and *2* Spanish language
#' Phases: Value *1* selects the roman numeral and *2* phases name
#'
#' @param: genero: An integer value to define the labels gender
#' @param: idioma: An integer value to define the labels language
#' @param: Phases: An integer value to define the labels of the phases of maturity
#'
#' @return: Phases_legend vector in roman numeral or name in English or Spanish
#' according to the user's choice
#'
#' @examples:
#'
#' Label_Phases(1, 1, 1) # returns roman numeral for females
#' Label_Phases(1, 2, 1) # returns roman numeral for females
#' Label_Phases(1, 1, 2) # returns phases name in English for females
#' Label_Phases(1, 2, 2) # returns phases name in Spanish for females
#' Label_Phases(2, 1, 1) # returns roman numeral for males
#' Label_Phases(2, 2, 1) # returns roman numeral for males
#' Label_Phases(2, 1, 2) # returns phases name in English for males
#' Label_Phases(2, 2, 2) # returns phases name in Spanish for males
#'
#' @export:
Label_Phases <- function(genero,idioma, Phases) {
  if(genero==1){
    if(Phases==1){
      Phases_legend <- c("I", "II", "III","IV","V","VI")
    }else{
      if(idioma==1){
        Phases_legend <- c("Immature","Developing","Spawning \n capable",
                           "Actively \n spawning","Spent","Regenerating")
      }else{
        #Sys.setlocale("LC_ALL", "ES_ES.UTF-8")  # Spanish characters
        Phases_legend <- c("Inmaduro","Desarrollo","Capaz de \n desovar",
                           "Desovador \n activo","Desovado",
                           "Regeneraci\u00F3n")
      } # End if idioma
    } # End if Phases
  }else{
    if(Phases==1){
      Phases_legend <- c("I", "II", "III","IV")
    }else{
      if(idioma==1){
        Phases_legend <- c("Immature","Developing","Actively \n spawning",
                           "Resting")
      }else{
        #Sys.setlocale("LC_ALL", "ES_ES.UTF-8")  # Spanish characters
        Phases_legend <- c("Inmaduro","Desarrollo",
                           "Eyaculador \n activo","Regeneraci\u00F3n")
      } # End if idioma
    } # End if Phases
  } # End if genero

} # End function
