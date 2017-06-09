\name{rfci}
\alias{rfci}
\title{Conservative Fast Causal Inference Search}
\description{
 \code{rfci} is the FCI algorithm with Augmented Rules.
}
\usage{
rfci(df, continuous = TRUE, depth = 3, significance = 0.05, 
    completeRuleSetUsed = FALSE, verbose = FALSE, java.parameters = NULL, 
    priorKnowledge = NULL)
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
  \item{completeRuleSetUsed}{ boolean indicating whether or not the search applies 
  the complete rule set.  \code{completeRuleSetUsed} is 
  \code{FALSE} by default. }
  \item{verbose}{ boolean indicating whether or not if verbose output should 
  be printed.  \code{verbose} is \code{FALSE} by default. }
  \item{java.parameters}{ string indicating an optional parameters for JVM. 
  For example, java.parameters = "-Xmx1024M". By default, 
  \code{java.parameters}=\code{NULL}. }
  \item{priorKnowledge}{ object indicating a prior knowledge of the graph. By default, 
  \code{priorKnowledge}=\code{NULL}. }
}
\details{
This method extends Erin Korber's implementation of the Fast Causal Inference algorithm with Jiji Zhang's Augmented FCI rules (found in sec. 4.1 of Zhang's 2006 PhD dissertation, "Causal Inference and Reasoning in Causally Insufficient Systems").

This class is based off a copy of Fci.java taken from the repository on 2008/12/16, revision 7306. The extension is done by extending doFinalOrientation() with methods for Zhang's rules R5-R10 which implements the augmented search. (By a remark of Zhang's, the rule applications can be staged in this way.)


More detail about rfci implementation, please visit \href{http://cmu-phil.github.io/tetrad/tetrad-lib-apidocs/edu/cmu/tetrad/search/Rfci.html}{the java documentation}.
}
\value{
A list containing the rfci's parameters, the input dataset, the result's nodes, and 
the result's edges.
}
\examples{
data("audiology")
#Compute Cyclic Causal Discovery
rfci <- rfci(df=audiology, continuous = FALSE, depth = -1, verbose = TRUE)
rfci$parameters #Show the rfci's parameters
rfci$datasets #Show the dataset
rfci$nodes #Show the result's nodes
rfci$edges #Show the result's edges
}
\keyword{rfci}
\keyword{latent variable}
\keyword{causal graphs}
