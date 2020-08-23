
simulate.cERGM <- function(object, nsim=1, seed=NULL, ...){

  sim.net <- simulate(object$ergm , nsim=nsim, seed=seed,
                      constraints=~fixallbut(object$not.fixed))

  return(sim.net)
}
