\name{fges.mixed}
\alias{fges.mixed}
\title{Fast Greedy Equivalence Search on Mixed Data}
\description{
 \code{fges.mixed} is the revised Greedy Equivalence Search (GES) algorithm developed 
 by Joseph D. Ramsey, Director of Research Computing, Department of Philosophy,
 Carnegie Mellon University, Pittsburgh, PA.
}
\usage{
fges.mixed(df, numCategoriesToDiscretize = 4, penaltydiscount = 4,
    structurePrior = 1.0, maxDegree = 3, 
    faithfulnessAssumed = TRUE, numOfThreads = 2, 
    verbose = FALSE, java.parameters = NULL, priorKnowledge = NULL)
}
\arguments{
  \item{df}{ Data Frame of the dataset. }
  \item{numCategoriesToDiscretize}{A number of categories determines the variable type is discrete or continuous. 
  By default, \code{numCategoriesToDiscretize}=4. }
  \item{penaltydiscount}{ floating point indicating the (BIC score) 
  penalty term for continuous searches.  By default, 
  \code{penaltydiscount}=4.0. }
  \item{structurePrior}{ Structure Prior.  By default, 
  \code{structurePrior}=1.0. }
  \item{maxDegree}{ integer indicating a number of degree of any node for the forward 
  reevaluation step. It ranges from -1 for unlimited degree, otherwise a number 
  >= 0. In the forward reevaluation step, subsets of neighbors up to degree 
  in size are considered. Limiting degree can speed up the algorithm. 
  By default, \code{maxDegree}=3. }
  \item{faithfulnessAssumed}{ boolean indicating whether or not the search assumes 
  that zero effect adjacencies are not in the graph.  \code{faithfulnessAssumed} is 
  \code{TRUE} by default. }
  \item{verbose}{ boolean indicating whether or not if verbose output should 
  be printed.  \code{verbose} is \code{FALSE} by default. }
  \item{java.parameters}{ string indicating an optional parameters for JVM. 
  For example, java.parameters = "-Xmx1024M". By default, 
  \code{java.parameters}=\code{NULL}. }
  \item{priorKnowledge}{ object indicating a prior knowledge of the graph. By default, 
  \code{priorKnowledge}=\code{NULL}. }
}
\details{
Fast Greedy Equivalence Search (FGES) is an implementation of the revised GES algorithm, as 
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
form of the faithfulness assumption, something FGES does not assume.

More detail about Fges implementation, please visit \href{http://cmu-phil.github.io/tetrad/tetrad-lib-apidocs/edu/cmu/tetrad/search/Fgs.html}{the java documentation}.
}
\value{
A list containing the FGES's parameters, the input dataset, the result's nodes, 
and the result's edges.
}
\examples{
data("charity")    #Load the charity dataset
fges <- fges.mixed(df = charity, numCategoriesToDiscretize = 4, penaltydiscount = 2, structurePrior = 1.0, maxDegree = -1, 
    faithfulnessAssumed = TRUE, numOfThreads = 2, verbose = TRUE)    #Compute mixed-data FGES search
fges$parameters #Show the FGES's parameters
fges$datasets #Show the dataset
fges$nodes #Show the result's nodes
fges$edges #Show the result's edges
}
\keyword{fges}
\keyword{ges}
\keyword{causal graphs}
\keyword{mixed data}