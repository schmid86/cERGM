
mcmc.diagnostics.cERGM <- function(object, ...){

  m<- mcmc.diagnostics(object$ergm)

  return(m)
}
