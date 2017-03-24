\name{randomDag}
\alias{randomDag}
\title{Random Dag Graph Generator}
\description{
 \code{randomDag} is the Directed Acyclic Graphs generator.
}
\usage{
randomDag(seed = NULL, numNodes = 10, numEdges = 10, java.parameters = NULL)
}
\arguments{
  \item{numNodes}{ integer indicating a number of nodes being generated. By default, \code{numNodes}=10. }
  \item{numEdges}{ integer indicating a number of edges being generated. By default, \code{numEdges}=10. }
  \item{java.parameters}{ string indicating an optional parameters for JVM. 
  For example, java.parameters = "-Xmx1024M". By default, 
  \code{java.parameters}=\code{NULL}. }
}
\details{
More detail about randomDag implementation, please visit \href{http://cmu-phil.github.io/tetrad/tetrad-lib-apidocs/edu/cmu/tetrad/cmd/TetradCmd.html}{the java documentation}.
}
\value{
A list containing the randomDag's parameters, the input dataset, the result's nodes, 
the result's edges, and the graphNEL causal graph result.
}
\examples{
randomDag <- randomDag(numNodes = 10, numEdges = 10)
randomDag$parameters #Show the randomDag's parameters
randomDag$nodes #Show the result's nodes
randomDag$edges #Show the result's edges
}
\keyword{dag}
\keyword{Directed Acyclic Graphs}
\keyword{causal graphs}
