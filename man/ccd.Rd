\name{ccd}
\alias{ccd}
\title{Cyclic Causal Discovery Search}
\description{
 \code{ccd} is the Cyclic Causal Discovery algorithm.
}
\usage{
ccd(df, continuous = TRUE, depth = 3, significance = 0.05,
    verbose = FALSE, java.parameters = NULL, priorKnowledge = NULL)
}
\arguments{
  \item{df}{ Data Frame of the dataset. }
  \item{continuous}{ boolean indicating the dataset is continuous.  
  \code{continuous} is \code{TRUE} by default. }
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
The Cyclic Causal Discovery algorithm (CCD) is implemented as described by Thomas Richardson and Peter Spirtes in Chapter 7 of Computation, Causation, & Discovery by Glymour and Cooper eds. (pp. 269-271).

The search method returns an instance of a Graph but it also constructs two lists of node triples which represent the underlines and dotted underlines that the algorithm discovers.


More detail about ccd implementation, please visit \href{https://github.com/cmu-phil/tetrad/blob/master/tetrad-lib/src/main/java/edu/cmu/tetrad/search/Ccd.java}{the java code}.
}
\value{
A list containing the ccd's parameters, the input dataset, the result's nodes, and 
the result's edges.
}
\examples{
data("audiology")
#Compute Cyclic Causal Discovery
ccd <- ccd(df=audiology, continuous = FALSE, depth = -1, verbose = TRUE)
ccd$parameters #Show the ccd's parameters
ccd$datasets #Show the dataset
ccd$nodes #Show the result's nodes
ccd$edges #Show the result's edges
}
\keyword{ccd}
\keyword{cyclic causality}
\keyword{causal graphs}
