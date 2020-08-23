\name{scc_1936_1941}
\alias{scc_1936_1941}
\docType{data}
\title{The US Supreme Court Citation Network from 1936 - 1941
%%   ~~ data name/kind ... ~~
}
\description{The US Supreme Court Citation Network from 1936 - 1941. The data originates from the Supreme Court Database and Lissner and Carver (2010)
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("scc_1936_1941")}
\format{
  A network object on 788 nodes. Each node represents a US Supreme Court majority opinion written between 1936 - 1941. An edge from node i to j means that the majority opinion i cites majority opinion j. In addition the network includes the following nodal attributes:
  \describe{
    \item{\code{CaseID}}{The case ID according to the Supreme Court Database (\url{http://scdb.wustl.edu/data.php)}}
    \item{\code{Term}}{The term (1936-1941) the majority opinion was written.}
    \item{\code{NumberJusticesPro}}{The number of justices in the majority.}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{The data was obtained from the Supreme Court Database (\url{http://scdb.wustl.edu/data.php}) and Courtlistener.com
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{Lissner M, Carver B (2010). \emph{Courtlistener.com A platform for researching and staying abreast of the latest law}, \url{http://www.courtlistener.com}

Schmid C, Chen T, Desmarais B (2020). \emph{Generative Dynamics of Supreme Court Citations: Analysis with a New Statistical Network Model.}

Spaeth H, Epstein L, Ruger T, Whittington K, Segal J, Martin A (2014). \emph{Supreme Court Database Code Book}, \url{http://scdb.wustl.edu}
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(scc_1936_1941)
plot(scc_1936_1941)
}
\keyword{datasets}
