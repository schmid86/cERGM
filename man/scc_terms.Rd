\name{scc_terms}
\alias{scc_terms}


\alias{difftransties}

\docType{methods}
\title{Terms used for the Citation-ERGM}

\description{
  This page documents the terms that can be used for fitting exponential random graph models of citation networks using the function \code{\link[cERGM]{cergm}} from the \code{\link[cERGM:cERGM-package]{cergm}} package.

  The terms included here are used in the same manner as \code{\link[ergm]{ergm-terms}} from the base \code{ergm} package. This means that in addition to fitting ERGMs, they can be used for network simulation (\code{\link[ergm]{simulate.ergm}}) and assessing goodness-of-fit (\code{\link[ergm]{mcmc.diagnostics}} and \code{\link[ergm]{gof}}).
  }

\section{Terms used in Schmid, Chen, and Desmarais (2020)}{
This set of network terms are used in Schmid, Chen, and Desmarais, 2020. ``Generative Dynamics of Supreme Court Citations: Analysis with a New Statistical Network Model''. (\url{https://github.com/desmarais-lab/Supreme_Court_Citation_Network})
  \describe{

    \item{\code{difftransties(attrname)} (directed)}{\emph{Transitive ties with different sender attribute:}
    This term adds one statistic, equal to the number of ties \eqn{i\rightarrow j}{i-->j} such that there exists a two-path from \eqn{i} to \eqn{j}, if for the nodal attribute \code{attrname}, the sending node \eqn{i} is different from \eqn{j} and the node on the two-path.}
}
  }
