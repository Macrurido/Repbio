#' ANOVA_1
#'
#' This function performs 1-way ANOVA using the function aov() from the R Stats
#'    Package, it returns the list av1 and save the ANOVA results as txt file.
#'
#' Inside the function: The Analysis of Variance is performed using the aov() function
#'    from package stats (version 3.6.2). Results are aved in a txt file named as:
#'    ANOVA plus the name of the model variables, acronym criteria established to
#'    classify active females to reproduction (criterio) and the txt extension.
#'
#'
#' The object "criterio" must be defined outside the function.
#'     For the analysis performed in Salas-Singh et al. (2022), the "criterion" object presents two options:
#'     BP: Separate immature from mature;
#'     AR: Separate reproductively active mature organisms from reproductively inactive mature and immature organisms.
#'
#'
#' @param: formula: A formula specifying the model
#' @param: df: A data frame values
#' @param: criterio: Criteria for separating active reproductive organisms
#'
#'
#' @return: A txt file with the ANOVA results.
#'
#' @seealso: aov() and anova() from stats R package
#'
#'
#' @examples:
#'    formula <- LT~Phase
#'    df <- tmp
#'    criterio <- "AR"
#'    ejemplo <- ANOVA_1(formula,df,criterio)
#'
#' @importFrom: stats %>%
#' @importFrom: utils %>%
#'
#' @export:
ANOVA_1 <- function(formula,df,criterio){
            av1 <- aov(formula,df)
            anova1 <- anova(av1)

            sink(paste0("04_Tablas/ANOVA_",as.character(formula)[2],
                  as.character(formula)[3],"_",as.character(criterio),".txt"))
                  print(anova1)    # Guardo salida ANOVA en archivo txt
            sink()        # Cerrar sink
            av1
  } #End function
