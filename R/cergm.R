
cergm <- function(formula, not.fixed, estimate="MCMLE", init.method="SAN",
                  control=control.ergm(init=NULL), eval.loglik=TRUE, ...){


  # turn not.fixed into a network
  if(is.matrix(not.fixed)){
    # creating a network indicating which nodes are fixed and which are not
    not.fixed.ergm <- network(not.fixed)
  }else{ # else 1
    if(is.vector(not.fixed)){
      # convert vector of nodes which can have outgoing not.fixed ties to
      # network object efficiently so that a (potentially huge) matrix
      # is never created.
      # This way:
      #    not.fixed.ergm <- network(matrix(not.fixed,length(not.fixed),length(not.fixed),byrow=F))
      # is simpler, shorter, and more elegant, but for large vectors is slower
      # and can use huge amounts of memory due to the creation of a dense
      # matrix object. (Unfortunately the network library, unlike igraph for
      # example, does not seem to have the ability to use a sparse matrix object
      # (from Matrix library for example) as the adjacency matrix directly
      # (without implicit conversion to dense matrix object), which
      # would be a simpler and more elegant way of solving this problem.)
      not.fixed.edgelist.df <- data.frame(
                           i = rep(which(not.fixed == 1),
                                   each = length(not.fixed)),
                           j = rep(1:length(not.fixed),
                                   times = length(which(not.fixed == 1)))
                                         )
      # remove self-loops from edge list
      not.fixed.edgelist.df <- not.fixed.edgelist.df[
                                      which(not.fixed.edgelist.df$i !=
                                            not.fixed.edgelist.df$j),
                                                    ]
      not.fixed.ergm <- network(not.fixed.edgelist.df, matrix.type="edgelist")
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
  class(return.list) <- "cERGM"
  return(return.list)

}

