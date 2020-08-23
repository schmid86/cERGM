gof.cERGM<- function(object,  N=500, control=control.simulate.ergm(MCMC.interval=1024),
                     max.idegree=20, max.odegree=20, max.esp.ost=20, max.esp.osp=20,
                     cex.axis=NULL, cex.lab=NULL, cex.main=NULL, ...){


  not.fixed.ergm <- object$not.fixed
  not.fixed.gof <- as.vector(as.matrix.network(object$not.fixed)[,1])


  # distrubtion of observed network

  AM <- as.matrix.network(object$ergm$network)

  # determine the unfixed nodes
  unfixed<- which(not.fixed.gof==1)

  # get outdegree of these nodes
  obs.odeg <- rowSums(AM[unfixed,])
  #hist(unfixed.odeg)

  # get indegree of these nodes
  obs.ideg <- colSums(AM[unfixed,])
  #hist(unfixed.ideg)

  net.t <- network(AM)

  # get edgewise shared partner distribution
  full.esp <- summary(net.t ~ esp(0:max.esp.ost))  # this is esp dist if entire network is unfixed

  # get esp dist of t-1
  AM.red <- AM[-unfixed, -unfixed]
  net.r <- network(AM.red)

  # get edgewise shared partner distribution
  red.esp <- summary(net.r ~ esp(0:max.esp.ost))

  obs.esp.ost<- full.esp-red.esp

  full.esp <- summary(net.t ~ desp(0:max.esp.osp, type="OSP"))   # this is esp dist if entire network is unfixed

  # get edgewise shared partner distribution
  red.esp <- summary(net.r ~ desp(0:max.esp.osp, type="OSP"))

  obs.esp.osp<- full.esp-red.esp

  # get observed vector into right form
  ideg.obs <- rep(0, max.idegree)
  odeg.obs <- rep(0,max.odegree)
  for(k in 0:max.idegree){
    s<- which(names(table(obs.odeg))==k   )


    if(length(s)!=0){
      odeg.obs[k+1]<- table(obs.odeg)[s]
    }
  } # end for k

  for(k in 0:max.odegree){

    r<- which(names(table(obs.ideg))==k   )
    if(length(r)!=0){
      ideg.obs[k+1]<- table(obs.ideg)[r]
    }
  } # end for k


  # simulate networks
  cat("Simulating", N , "networks\n")
  sim <- simulate(object$ergm, nsim=N,  control=control,
                  constraints=~fixallbut(not.fixed.ergm))

  # create matrices to store results
  cat("Calculating distributions of", N , "simulated networks\n")
  ideg.matrix <- matrix(0, N, max.idegree+1)
  colnames(ideg.matrix)<- 0:max.idegree

  odeg.matrix <- matrix(0, N, max.odegree+1)
  colnames(odeg.matrix)<- 0:max.odegree

  esp.matrix.ost <- matrix(0, N, max.esp.ost+1)
  colnames(esp.matrix.ost)<- 0:max.esp.ost

  esp.matrix.osp <- matrix(0, N, max.esp.osp+1)
  colnames(esp.matrix.osp)<- 0:max.esp.osp

  for(i in 1:N){

    A <- as.matrix.network(sim[[i]])

    # determine the unfixed nodes
    unfixed<- which(not.fixed.gof==1)

    # get outdegree of these nodes
    unfixed.odeg <- rowSums(A[unfixed,])


    # get indegree of these nodes
    unfixed.ideg <- colSums(A[unfixed,])

    # ESP OST
    net.t <- sim[[i]]

    # get edgewise shared partner distribution
    full.esp <- summary(net.t ~ esp(0:max.esp.ost))   # this is esp dist if entire network is unfixed

    # get esp dist of t-1
    A.red <- A[-unfixed, -unfixed]
    net.r <- network(A.red)

    # get edgewise shared partner distribution
    red.esp <- summary(net.r ~ esp(0:max.esp.ost))

    unfixed.esp.ost<- full.esp-red.esp

    esp.matrix.ost[i,] <- unfixed.esp.ost

    # ESP OSP
    full.esp <- summary(net.t ~ desp(0:max.esp.osp, type="OSP"))   # this is esp dist if entire network is unfixed

    # get edgewise shared partner distribution
    red.esp <- summary(net.r ~ desp(0:max.esp.osp, type="OSP"))

    unfixed.esp.osp<- full.esp-red.esp

    esp.matrix.osp[i,] <- unfixed.esp.osp


    # fill values into right spot (if an entry doesn't exist, the vector gets messed up)
    for(k in 0:max.odegree){
      s<- which(names(table(unfixed.odeg))==k   )


      if(length(s)!=0){
        odeg.matrix[i,k+1]<- table(unfixed.odeg)[s]
      }
    } # end k

    for(k in 0:max.idegree){
      r<- which(names(table(unfixed.ideg))==k   )
      if(length(r)!=0){
        ideg.matrix[i,k+1]<- table(unfixed.ideg)[r]
      }# end if

    }# end for k

  } # end i

  ### plot
  par(mfrow=c(2,2), oma=c(0,0,2,0))
  boxplot(odeg.matrix, use.col=TRUE, col="lightgrey", main="Outdegree", xlab="Outddegree Distribution",
          cex.axis=cex.axis, cex.lab=cex.lab, cex.main=cex.main)
  lines(odeg.obs, type="l", lwd=2)

  boxplot(ideg.matrix, use.col=TRUE, col="lightgrey", main="Indegree", xlab="Inddegree Distribution",
          cex.axis=cex.axis, cex.lab=cex.lab, cex.main=cex.main)
  lines(ideg.obs, type="l", lwd=2)


  boxplot(esp.matrix.ost, use.col=TRUE, col="lightgrey", main="Edgewise Shared Partners OTP", xlab="ESP Distribution",
          cex.axis=cex.axis, cex.lab=cex.lab, cex.main=cex.main)
  lines(obs.esp.ost, type="l", lwd=2)

  boxplot(esp.matrix.osp, use.col=TRUE, col="lightgrey", main="Edgewise Shared Partners OSP", xlab="ESP Distribution",
          cex.axis=cex.axis, cex.lab=cex.lab, cex.main=cex.main)
  lines(obs.esp.osp, type="l", lwd=2)


  return.list <- list(obs.odeg=odeg.obs, obs.ideg=ideg.obs, obs.esp.ost=obs.esp.ost,
                      obs.esp.osp=obs.esp.osp, sim.odeg=odeg.matrix, sim.ideg=ideg.matrix ,
                      sim.esp.ost=esp.matrix.ost, sim.esp.osp=esp.matrix.osp)

  return(return.list)

} # end function

