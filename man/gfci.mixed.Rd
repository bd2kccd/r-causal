\name{gfci.mixed}
\alias{gfci.mixed}
\title{Greedy Fast Causal Inference Algorithm on Mixed Data}
\description{
 \code{gfci} is the revised Fast Causal Inference (FCI) algorithm.
}
\usage{
gfci.mixed(df, numCategoriesToDiscretize = 4, penaltydiscount = 4,
    structurePrior = 1.0, maxDegree = 3, maxPathLength = -1, significance = 0.05,
    completeRuleSetUsed = FALSE, faithfulnessAssumed = TRUE, verbose = FALSE, java.parameters = NULL, 
    priorKnowledge = NULL)
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
  \item{maxPathLength}{ integer indicating a maximum length for any discriminating path. 
  It ranges from -1 for unlimited degree, otherwise a number 
  >= 0. By default, \code{maxPathLength}=-1. }
  \item{significance}{ floating point indicating the statistical value (between zero and one) for the conditional independence test.  By default, 
  \code{significance}=0.05. }
  \item{completeRuleSetUsed}{ boolean indicating whether or not the search applies 
  the complete rule set.  \code{completeRuleSetUsed} is 
  \code{FALSE} by default. }
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
Greedy Fast Causal Inference Search (GFCI) is an implementation of the revised FCI algorithm, as 
published in Ogarrio et al. (2016) \dQuote{A Hybrid Causal Search Algorithm for Latent Variable Models} 
\href{http://www.jmlr.org/proceedings/papers/v52/ogarrio16.pdf}{pdf}. It uses FGES followed by PC adjacency 
removals. Uses conservative collider orientation. Gets sepsets for X---Y from among adjacents of X or of Y.

Following an idea of Spirtes, now it uses more of the information in FGES, to calculating 
possible d-separation paths and to utilize unshielded colliders found by FGES.

More detail about GFci implementation, please visit \href{http://cmu-phil.github.io/tetrad/tetrad-lib-apidocs/edu/cmu/tetrad/search/GFci.html}{the java documentation}.
}
\examples{
data("charity")    #Load the charity dataset
gfci <- gfci.mixed(df = charity, numCategoriesToDiscretize = 4, penaltydiscount = 2, structurePrior = 1.0, maxDegree = -1, 
    faithfulnessAssumed = TRUE, verbose = TRUE)    #Compute gfci search
gfci$parameters #Show the gfci's parameters
gfci$datasets #Show the dataset
gfci$nodes #Show the result's nodes
gfci$edges #Show the result's edges
}
\keyword{gfci}
\keyword{fci}
\keyword{causal graphs}
