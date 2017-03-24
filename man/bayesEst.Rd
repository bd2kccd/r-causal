\name{bayesEst}
\alias{bayesEst}
\title{Bayesian Network Estimator}
\description{
 \code{bayesEst} is the revised Greedy Equivalence Search (GES) algorithm developed 
 by Joseph D. Ramsey, Director of Research Computing, Department of Philosophy,
 Carnegie Mellon University, Pittsburgh, PA.
}
\usage{
bayesEst(df, depth = 3, significance = 0.05, verbose = FALSE, 
	java.parameters = NULL, priorKnowledge = NULL)
}
\arguments{
  \item{df}{ Data Frame of the dataset. }
  \item{depth}{ integer indicating a number of nodes conditioned on in the search. 
  It ranges from -1 for unlimited nodes, otherwise a number 
  >= 0. By default, \code{depth}=3. }
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
More detail about bayesEst implementation, please visit \href{http://cmu-phil.github.io/tetrad/tetrad-lib-apidocs/edu/cmu/tetrad/cmd/TetradCmd.html}{the java documentation}.
}
\value{
A list containing the bayesEst's parameters, the input dataset, the result's nodes, 
the result's edges, and the graphNEL causal graph result.
}
\examples{
data("audiology")
#Compute bayes Estimation
bayesEst <- bayesEst(df=audiology, depth = -1, verbose = TRUE)
bayesEst$parameters #Show the bayesEst's parameters
bayesEst$datasets #Show the dataset
bayesEst$nodes #Show the result's nodes
bayesEst$edges #Show the result's edges
}
\keyword{bayesEst}
\keyword{estimation}
\keyword{causal graphs}
