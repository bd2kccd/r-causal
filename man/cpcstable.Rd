\name{cpcstable}
\alias{cpcstable}
\title{Conservative stable version of PC ("Peter/Clark") algorithm}
\description{
 \code{cpcstable} is the Conservative version of PC Stable algorithm.
}
\usage{
cpcstable(df, continuous = TRUE, depth = 3, aggressivelyPreventCycles = FALSE, 
	significance = 0.05, verbose = FALSE, java.parameters = NULL, priorKnowledge = NULL)
}
\arguments{
  \item{df}{ Data Frame of the dataset. }
  \item{continuous}{ boolean indicating the dataset is continuous.  
  \code{continuous} is \code{TRUE} by default. }
  \item{depth}{ integer indicating a number of nodes conditioned on in the search. 
  It ranges from -1 for unlimited nodes, otherwise a number 
  >= 0. By default, \code{depth}=3. }
  \item{aggressivelyPreventCycles}{ boolean indicating if cycles are to be aggressively prevented. 
  May be expensive for large graphs (but also useful for large graphs).
  \code{aggressivelyPreventCycles} is \code{FALSE} by default. }
  \item{verbose}{ boolean indicating whether or not if verbose output should 
  be printed.  \code{verbose} is \code{FALSE} by default. }
  \item{java.parameters}{ string indicating an optional parameters for JVM. 
  For example, java.parameters = "-Xmx1024M". By default, 
  \code{java.parameters}=\code{NULL}. }
  \item{priorKnowledge}{ object indicating a prior knowledge of the graph. By default, 
  \code{priorKnowledge}=\code{NULL}. }
}
\details{
This method implements a convervative version of PC, in which the Markov condition is assumed but faithfulness is tested locally. Uses the Pc-Stable adjacency search.s

More detail about cpcstable implementation, please visit \href{http://cmu-phil.github.io/tetrad/tetrad-lib-apidocs/edu/cmu/tetrad/search/CpcStable.html}{the java documentation}.
}
\value{
A list containing the cpcstable's parameters, the input dataset, the result's nodes, 
the result's edges, and the graphNEL causal graph result.
}
\examples{
data("audiology")
#Compute cpcstable
cpcstable <- cpcstable(df=audiology, continuous = FALSE, depth = -1, verbose = TRUE)
cpcstable$parameters #Show the cpcstable's parameters
cpcstable$datasets #Show the dataset
cpcstable$nodes #Show the result's nodes
cpcstable$edges #Show the result's edges
}
\keyword{cpcstable}
\keyword{pc}
\keyword{causal graphs}
