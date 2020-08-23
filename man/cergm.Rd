\name{cergm}
\alias{cergm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Citation Exponential Random Graph Models
%%  ~~function to do ... ~~
}
\description{Function for citation exponential random graph models. This function depends on the \code{\link[ergm]{ergm}}-package and the \code{\link[network]{network}}-package.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cergm(formula, not.fixed, estimate="MCMLE", init.method="SAN" ,
control=control.ergm(init=NULL), eval.loglik=TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{Formula for the ERGM. Model construction works like in the ergm package
with the same model terms etc.) % (for a list of terms, see help("\link[ergm]{ergm-terms}"))
%%     ~~Describe \code{x} here~~
}
\item{not.fixed}{Either a matrix of 1s and 0s indicating the dyads that are not fixed in the model, or a vector of 1s and 0s indicating the nodes in the network that have unfixed dyads, or a network object where an edge indicates that this dyad is unfixed. }
\item{estimate}{If "MPLE," then the maximum pseudolikelihood estimator is returned. If "MLE" (the default), then an approximate maximum likelihood estimator is returned. If "CD", the Monte-Carlo contrastive divergence estimate is returned.}
\item{init.method}{If "SAN", the MCMLE algorithm is being initialized using Simulated Annealing. If "Bernoulli", the MCMLE algorithm is being initialized using the MPLE. If "CD", Contrastive Divergence is being used to start the algorithm.}
\item{control}{Controls for the \code{\link[ergm]{ergm}} function in the ergm-package. See \link[ergm]{control.ergm} for details.}
\item{eval.loglik}{Logical: For dyad-dependent models, if TRUE, use bridge sampling to evaluate the log-likelihoood associated with the fit. Has no effect for dyad-independent models. Since bridge sampling takes additional time, setting to FALSE may speed performance if likelihood values (and likelihood-based values like AIC and BIC) are not needed. }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{Hunter D, Handcock M, Butts C,
Goodreau S, Morris M (2008). \emph{ergm: A
Package to Fit, Simulate and Diagnose
Exponential-Family Models for
Networks.} Journal of Statistical
Software, 24(3), 1-29.

Schmid C, Chen T, Desmarais B (2020). \emph{Generative Dynamics of Supreme Court Citations: Analysis with a New Statistical Network Model.}
%% ~put references to the literature/web site here ~
}
\author{Christian S. Schmid <songhyo86@gmail.com>
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# load Supreme Court Citation Network from 1936-1941
data("scc_1936_1941")

# get vertex attribute "Term". Indicates the Term of each node
terms <- get.vertex.attribute(scc_1936_1941, "Term")

# create a matrix with the sender's term in each row
terms.matrix <- matrix(terms,length(terms),length(terms),byrow=F)

# fix all dyads that can potentially be created in 1941 as 1
unfixed.dyads <- 1*(terms.matrix == 1941)


# alternatively one can also add an unfixed vector that indicates
# which nodes can create edges
unfixed.vector<-  1*(terms==1941)

# test cergm-function
model <- cergm(scc_1936_1941~ edges+ difftransties("Term")+ nodeicov("NumberJusticesPro"),
            not.fixed=unfixed.dyads, estimate="MPLE", init.method="SAN")

summary.cERGM(model)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
