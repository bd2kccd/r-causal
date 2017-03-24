\name{cpc}
\alias{cpc}
\title{Conservative PC ("Peter/Clark") algorithm}
\description{
 \code{cpc} is the Conservative version of PC algorithm.
}
\usage{
cpc(df, continuous = TRUE, depth = 3, aggressivelyPreventCycles = FALSE, 
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
This method implements a convervative version of PC, in which the Markov condition is assumed but faithfulness is tested locally.

More detail about cpc implementation, please visit \href{http://cmu-phil.github.io/tetrad/tetrad-lib-apidocs/edu/cmu/tetrad/search/cpc.html}{the java documentation}.
}
\value{
A list containing the cpc's parameters, the input dataset, the result's nodes, 
the result's edges, and the graphNEL causal graph result.
}
\examples{
data("audiology")
#Compute CPC
cpc <- cpc(df=audiology, continuous = FALSE, depth = -1, verbose = TRUE)
cpc$parameters #Show the cpc's parameters
cpc$datasets #Show the dataset
cpc$nodes #Show the result's nodes
cpc$edges #Show the result's edges
}
\keyword{cpc}
\keyword{pc}
\keyword{causal graphs}
