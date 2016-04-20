\name{fgs.discrete}
\alias{fgs.discrete}
\title{Fast Greedy Search on Discrete Data}
\description{
 \code{fgs} is the revised Greedy Equivalence Search (GES) algorithm developed 
 by Joseph D. Ramsey, Director of Research Computing, Department of Philosophy,
 Carnegie Mellon University, Pittsburgh, PA.
}
\usage{fgs(df, structurePrior = 1.0, samplePrior = 1.0, depth = -1, 
faithfulness = TRUE, numOfThreads = NULL,verbose = FALSE, 
java.parameters = NULL)
}
\arguments{
  \item{df}{ Data Frame of the dataset. }
  \item{structurePrior}{ Structure Prior.  By default, 
  \code{structurePrior}=1.0. }
  \item{samplePrior}{ Sample Prior.  By default, 
  \code{samplePrior}=1.0. }
  \item{depth}{ integer indicating the search depth for the forward 
  reevaluation step. It ranges from -1 for unlimited depth, otherwise a number 
  >= 0. In the forward reevaluation step, subsets of neighbors up to depth 
  in size are considered. Limiting depth can speed up the algorithm. 
  By default, \code{depth}=3. }
  \item{faithfulness}{ boolean indicating whether or not the search assumes 
  that zero effect adjacencies are not in the graph.  \code{faithfulness} is 
  \code{TRUE} by default. }
  \item{numOfThreads}{ A number of threads.  By default, 
  \code{numOfThreads}=2. }
  
  \item{verbose}{ boolean indicating whether or not if verbose output should 
  be printed.  \code{verbose} is \code{FALSE} by default. }
  \item{java.parameters}{ string indicating an optional parameters for JVM. 
  For example, java.parameters = "-Xmx1024M". By default, 
  \code{java.parameters}=\code{NULL}. }
}
\details{
Fast Greedy Search (FGS) is an implementation of the revised GES algorithm, as 
reported in Ramsey (2015) \dQuote{Scaling up Greedy Causal Search for 
Continuous Variables} \emph{arXiv eprint arXiv:1507.07749}. It was developed 
as specified in Chickering (2002) \dQuote{Optimal structure identification 
with greedy search} \emph{Journal of Machine Learning Research}. It works for 
both BayesNets and SEMs.

Some code optimization could be done for the scoring part of the graph for 
discrete models (method scoreGraphChange). Some of Andrew Moore's approaches 
for caching sufficient statistics, for instance.

To speed things up, it has been assumed that variables X and Y with 
zero correlation do not correspond to edges in the graph. This is a restricted 
form of the faithfulness assumption, something FGS does not assume.}
\value{
A list containing the FGS's parameters, the input dataset, the result's nodes, 
the result's edges, and the graphNEL causal graph result.
}
\examples{
data("audiology")    #Load the charity dataset
#Compute FGS search
fgs.discrete <- fgs.discrete(df=audiology,structurePrior=1.0,samplePrior=1.0, 
depth = -1, faithfulness = TRUE, numOfThreads = 2,verbose = TRUE)
fgs$parameters #Show the FGS's parameters
fgs$datasets #Show the dataset
fgs$nodes #Show the result's nodes
fgs$edges #Show the result's edges
}
\keyword{fgs}
\keyword{ges}
\keyword{causal graphs}
\keyword{discrete data}