
cergm <- function(formula, not.fixed, estimate="MCMLE", init.method="SAN",
                  control=control.ergm(init=NULL), eval.loglik=TRUE, ...){


  # turn not.fixed into a network
  if(is.matrix(not.fixed)){
    # creating a network indicating which nodes are fixed and which are not
    not.fixed.ergm <- network(not.fixed)
  }else{ # else 1
    if(is.vector(not.fixed)){
      not.fixed.ergm <- network(matrix(not.fixed,length(not.fixed),length(not.fixed),byrow=F))
    }else{ # else 2
      if(is.network(not.fixed)){
        not.fixed.ergm <- not.fixed
      }else{ # else 3
        stop("not.fixed has to be a matrix, a vector or a network")
      } # end else3
    }# end else2

  }# end else1


  # get sufficient statistics
  obs <- summary(formula )

  if(init.method=="SAN"){

    # get a network with the same sufficient statistics using simulated annealing
    san.net <- san(formula, constraints=~fixallbut(not.fixed.ergm) , target.stats = obs)

    # replace response network in formula
    tt <- terms(formula)
    new.formula <- reformulate(attr(tt, "term.labels"), "san.net")

    # get sufficient statistic of san simulated network
    san.obs <- summary(new.formula  )

    # get the MPLE of the simulated annealing network
    mple.san <- ergm(new.formula,constraints=~fixallbut(not.fixed.ergm), estimate="MPLE"  ) #
    control$init <- coef(mple.san)

    # estimate model using new starting values
    model <- ergm(formula, constraints=~fixallbut(not.fixed.ergm), control= control,
                  eval.loglik=eval.loglik)

    san.observed.statistics <- san.obs
    san.starting.value <- coef(mple.san)



  }else{


    model <- ergm(formula, constraints=~fixallbut(not.fixed.ergm), estimate=estimate,
                  control=control, eval.loglik=eval.loglik)
    san.observed.statistics <- NULL
    san.starting.value <- NULL


  } # end else

  return.list <- list(san.observed.statistics = san.observed.statistics,
                      san.starting.value=san.starting.value, ergm=model,  formula=formula,
                      observed.stat=obs, not.fixed=not.fixed.ergm )
  class(return.list) <- c("cERGM" )
  return(return.list)

}

