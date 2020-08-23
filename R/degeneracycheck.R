
degeneracy.check <- function(object, N=1000, ...){


  #observed statistics
  observed <- object$observed.stat

  sim <- simulate(object$ergm, nsim=N, output = "stats",
                  constraints=~fixallbut(object$not.fixed))

  # simulated statistics
  simulated.stat <- sim


  percentile<- rep(0,ncol(simulated.stat))
  # percentile for all variables
  for(i in 1:ncol(simulated.stat)){

    percentile[i]<- ecdf(simulated.stat[,i])(observed[i])

  }

  # calculate empirical p-value
  p <-2*pmin(percentile, 1-percentile)

  # create matrix to enter values
  Results<- matrix(NA, ncol(simulated.stat) ,7)
  colnames(Results)<- c("obs", "2.5%", "25%","50%","75%","97.5%","p-value" )
  rownames(Results)<- names(object$san.observed.statistics)

  # enter observed values
  Results[,1]<- observed
  Results[,2:6]<- t(apply( simulated.stat , 2 , quantile , probs =c(0.025, 0.25, 0.5, 0.75, 0.975) , na.rm=TRUE ))
  Results[,7]<- p

  cat("Degeneracy Check:")
  cat(" \n")
  print(Results)
  cat(" \n")
  cat("Small p-values indicate a potentially degenerated model. Check trace plots using plot() for further investigation.")


}
