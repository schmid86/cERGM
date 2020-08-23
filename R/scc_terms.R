# Custom Terms used for the US Supreme Court Citation Network
# See: https://github.com/desmarais-lab/Supreme_Court_Citation_Network

InitErgmTerm.difftransties<-function (nw, arglist, ...) {
  a <- check.ErgmTerm(nw, arglist, directed=TRUE,
                      varnames = c("attrname"),
                      vartypes = c("character"),
                      defaultvalues = list(NULL),
                      required = c(TRUE))
  attrname <- a$attrname
  nodecov <- get.node.attr(nw, attrname, "difftransties")
  u<-sort(unique(nodecov))
  if(any(is.na(nodecov))){u<-c(u,NA)}
  nodecov <- match(nodecov,u,nomatch=length(u)+1)
  if (length(u)==1)
    warning ("Attribute given to difftransties() has only one value", call.=FALSE)
  coef.names <- paste("difftransties",attrname,sep=".")
  inputs <- c(nodecov)

  list(name="difftransties", coef.names=coef.names, inputs=inputs, minval=0, pkgname = "cERGM")
}
