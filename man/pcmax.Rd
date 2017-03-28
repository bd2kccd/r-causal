\name{pcmax}
\alias{pcmax}
\title{PcMax ("Peter/Clark") algorithm}
\description{
 \code{pcmax} is the PcMax ("Peter/Clark") algorithm.
}
\usage{
pcmax(df, continuous = TRUE, depth = -1, maxPathLength = 3, 
	useHeuristic = TRUE, significance = 0.05,
    verbose = FALSE, java.parameters = NULL, priorKnowledge = NULL)
}
\arguments{
  \item{df}{ Data Frame of the dataset. }
  \item{continuous}{ boolean indicating the dataset is continuous.  
  \code{continuous} is \code{TRUE} by default. }
  \item{depth}{ integer indicating a number of nodes conditioned on in the search. 
  It ranges from -1 for unlimited nodes, otherwise a number 
  >= 0. By default, \code{depth}=3. }
  \item{maxPathLength}{ integer indicating a maximum length for any discriminating path. 
  It ranges from -1 for unlimited degree, otherwise a number 
  >= 0. By default, \code{maxPathLength}=-1. }
  \item{useHeuristic}{ boolean indicating if the heuristic search should be conducted. 
  May be expensive for large graphs (but also useful for large graphs).
  \code{useHeuristic} is \code{FALSE} by default. }
  \item{significance}{ floating point indicating the statistical value (between zero and one) for the conditional independence test.  By default, 
  \code{significance}=0.05. }
  \item{verbose}{ boolean indicating whether or not if verbose output should 
  be printed.  \code{verbose} is \code{FALSE} by default. }
  \item{java.parameters}{ string indicating an optional parameters for JVM. 
  For example, java.parameters = "-Xmx1024M". By default, 
  \code{java.parameters}=\code{NULL}. }
  \item{priorKnowledge}{ object indicating a prior knowledge of the graph. By default, 
  \code{priorKnowledge}=\code{NULL}. }
}
\details{
This method implements a modification of the the PC ("Peter/Clark") algorithm, as specified in Chapter 6 of Spirtes, Glymour, and Scheines, "Causation, Prediction, and Search," 2nd edition, with a modified rule set in step D due to Chris Meek. For the modified rule set, see Chris Meek (1995), "Causal inference and causal explanation with background knowledge."

In addition, the modification to PC by \href{https://arxiv.org/abs/1610.00378}{maximizing p-value} has been included.

More detail about pcmax implementation, please visit \href{http://cmu-phil.github.io/tetrad/tetrad-lib-apidocs/edu/cmu/tetrad/search/PcMax.html}{the java documentation}.
}
\value{
A list containing the pcmax's parameters, the input dataset, the result's nodes, 
the result's edges, and the graphNEL causal graph result.
}
\examples{
data("audiology")
#Compute pcmax
pcmax <- pcmax(df=audiology, continuous = FALSE, depth = -1, verbose = TRUE)
pcmax$parameters #Show the pcmax's parameters
pcmax$datasets #Show the dataset
pcmax$nodes #Show the result's nodes
pcmax$edges #Show the result's edges
}
\keyword{pcmax}
\keyword{causal graphs}
