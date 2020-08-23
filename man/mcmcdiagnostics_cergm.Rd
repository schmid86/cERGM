\name{mcmc.diagnostics.cERGM}
\alias{mcmc.diagnostics.cERGM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{MCMC Diagnostics for Citation Exponential Random Graph Models
%%  ~~function to do ... ~~
}
\description{MCMC Diagnostics as performed by \code{\link[ergm]{mcmc.diagnostics}}
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mcmc.diagnostics.cERGM(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A cERGM object
%%     ~~Describe \code{x} here~~
}
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
\author{Christian Schmid <songhyo86@gmail.com>
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

mcmc.diagnostics.cERGM(model)
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
