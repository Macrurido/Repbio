#' BCA Nonparametric confidence intervals
#'
#' This function takes a data frame (df), requires the significance value (pval)
#'    and calculates the nonparametric confidence intervals (CI):
#'    Symmetric (IC), Bias-corrected (BC) and BC accelerated (BCA)
#'
#' @param: df a data.frame
#' @param: pval significance level defined by the user
#' @param: B an integer value, specify the number of resamples
#'
#' @return: Table b_BCA for each model parameters (row): original (opt), low (lw)
#'    and high (up) CI, bias and coefficient of variation (CV):
#'    Opt,IClw,ICup,BClw,BCup,BCAlw,BCAup,Bias,CV
#'
#'@importFrom: expss %>%
#'
#' @export
#'
#' @examples:
#' df <- tmp
#' B <- 2000
#' pval <- 0.05
#' ejemplo <- BCA(df, pval, B)
#'
#'
BCA <-function(df, pval, B){
  whichcol <- which(apply(is.na(df),2,sum)==0)
  d <- df[,whichcol]
  nrd <- nrow(d)
  # Values are defined
  Opt <- d[1, ]
  x <- (d[2:nrd, ])
  Med <-apply(x,2,median)
  Avg <- apply(x,2,mean)
  de <- apply(x,2,sd)
  # CI Bias-corrected
  menor <- as.vector(sum_col(d[-1 , ] < d[rep(1, nrow(d) - 1), ],na.rm=TRUE)/B )
  Z <-  qnorm(menor , mean= 0, sd= 1,  lower.tail = FALSE)
  Plw <- 1-pnorm(2*Z-1.96,mean= 0, sd= 1, pval, lower.tail = FALSE)
  Pup <- 1-pnorm(2*Z+1.96,mean= 0, sd= 1, pval, lower.tail = FALSE)
  Bias <- (100*((Avg-Opt)/Opt))
  CV <- de/Avg
  # CI Bias-corrected and accelerated BCA
  pk2s <- sum_col(d[-1 , ] - d[rep(1, nrow(d) - 1), ],na.rm=TRUE)
  num <- (pk2s)^3
  den <- (pk2s)^2
  ac <- abs(num/(6*den^(3/2)))
  # CI BCA
  Zlw <- abs((pval/2)+(((pval/2)-Plw)/(1-ac*((pval/2)+Plw))))
  # To avoid errors, when the value of a parameter is repeated (roughly)
  Zlw[menor == 0]  <- pval/2
  Zlw[Zlw > 1]  <- pval/2
  Zup <- abs((1-pval/2)+(((1-pval/2)-Pup)/(1-ac*((1-pval/2)+Pup))))
  Zup[menor == 0]  <- 1-pval/2
  Zup[Zup > 1]  <- 1-pval/2

  # CI: symmetric, BC, BCa
  intlw <-  rep(pval/2,length(whichcol))
  intup <-  rep(1-pval/2,length(whichcol))
  int <-  rbind(intlw,intup,Plw,Pup,Zlw,Zup)
  int[is.na(int)] <- 1  # Tengo que ver como arreglar mientras 1
  int                     # pedir que si es Na,no haga nada y siga
  # The table is saved
  for(l in 1:length(whichcol)){
    ICS <-  quantile(d[,l],int[,l])
    b_BCA[l,] <- unlist(t(c(Opt=Opt[l],ICS,Bias=Bias[l],CV=CV[l])))
  }     # End for l
  print(b_BCA)
}     # End function BCA
