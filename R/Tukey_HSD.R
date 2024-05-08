#' Tukey_HSD
#'
#' TukeyHSD test (Tukey Honest Significant Differences)
#'
#' This function performs multiple comparisons of treatments by means of Tukey
#' only if the p value of the ANOVA_1 test is equal to or less than pval
#' defined by the user. HSD.test() came from the agricolae package.
#'
#' @param: Prob: A summary from ANOVA-1 function.
#' @param: pval: The significance level (user defined).
#' @param: HSD_main: A mine title (user defined).
#' @param: HSD_trt: Constant( only y=model) or vector treatment applied to each
#'    experimental unit.
#' @param: fn_hsd: This function is included in this package Repbio, it
#'    returns the phase names in descending order by the average of the
#'    response values.
#' @param: etiquetas: A data frame containing the user-defined labels that will
#'    be used throughout the routine.
#' @param: formula: A formula notation y~f where y is the response (independent)
#'    variable and f is a factor (categorical) variable representing group
#'    membership.
#' @param: criterio: Criteria for separating active reproductive organisms.
#' @param: Phases_legend: A character vector with the name or number of maturity
#' stages, as defined by the user.
#'
#' @return: 1) A TukeyHSD test table hsd_out saved as txt file as: Tukey
#'    plus the name of the model variables, acronym criteria established to
#'    classify active females to reproduction (criterio) and the txt extension
#' @return: 2) A TukeyHSD contrast plot saved as pdf file as: Tukey
#'    plus the name of the model variables, acronym criteria established to
#'    classify active females to reproduction (criterio) and the txt extension
#'    Note: criterio is defined outside of this function
#'
#' @seealso:   fn_hsd() from agricolae (version 1.3-7).
#'
#' @examples:
#'    pval <- 0.05
#'    HSD_main <- LT/nPhase
#'    HSD_trt <- Phase
#'    Tukey_HSD(Prob, pval, HSD_main, HSD_trt, fn_hsd, etiquetas, formula, criterio, Phases_legend)
#'
#' @importFrom: agricolae, HSD.test %>%
#' @importFrom: stats %>%
#'
#' @export:
Tukey_HSD <- function(Prob, pval, HSD_main, HSD_trt, fn_hsd, etiquetas,
                      formula, criterio, Phases_legend){
  #  if (summary(Prob)[[1]][1, 5]<= 0.05){
  hsd_out <- agricolae::HSD.test(Prob, HSD_trt, group=TRUE,console=TRUE,
                      main=HSD_main, unbalanced = TRUE)
  sink(paste0("04_Tablas/Tukey_", as.character(formula)[2], "_",
              as.character(formula)[1],"_", criterio, ".txt"))
  print(hsd_out)
  sink()        # End sink

  # LTPh Contrast plot  ----
  r_names <- fn_hsd(hsd_out,Phases_legend) # Phase names are added.

  pdf(paste0("03_Figuras/Tukey_", as.character(formula)[2],"_",
             as.character(formula)[1],"_", criterio, ".pdf"))
  plot(hsd_out, xlab=etiquetas[6], ylab=etiquetas[5],xaxt = "n")
  par(usr=c(0.2,6.8,0.2,7))
  axis(1, labels = r_names, cex.axis=0.75,
       tick = FALSE, at=1:length(r_names))
  dev.off()
  #   } # End if
}  # End function
