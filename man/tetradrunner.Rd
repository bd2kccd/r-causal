\name{tetradrunner}
\alias{tetrad}
\title{Tetrad Algorithm Runner}
\description{
 \code{tetradrunner} is the wrapper of Tetrad algorithms.
}
\usage{
ccd(algoId, dataType, df = NULL, dfs = NULL, testId = NULL, scoreId = NULL, 
	priorKnowledge = NULL, numCategoriesToDiscretize = 4,java.parameters = NULL,...)
}
\arguments{
  \item{algoId}{ Tetrad Algorithm Id. To check what algorithms supported, use \code{tetradrunner.listAlgorithms}.}
  \item{dataType}{ Dataset type either 'continuous','discrete', or 'mixed'. }
  \item{df}{Data frame of dataset.If running multi-dataset algorithms, set it NULL. By default, \code{df}=NULL. }
  \item{dfs}{A list of data frames of datasets.If running singular dataset algorithms, set it NULL. By default, \code{dfs}=NULL. }
  \item{testId}{ Test id indicating the independent test being used by the causal algorithm. To check if the designated algorithm needs an independent test or not, use \code{tetradrunner.getAlgorithmDescription}. To check what tests supported, use \code{tetradrunner.listIndTests}. }
  \item{scoreId}{ Score id indicating the evaluation score being used by the causal algorithm. To check if the designated algorithm needs a score or not, use \code{tetradrunner.getAlgorithmDescription}. To check what tests supported, use \code{tetradrunner.listScores}. }
  \item{priorKnowledge}{ object indicating a prior knowledge of the graph. By default, 
  \code{priorKnowledge}=\code{NULL}. }
  \item{numCategoriesToDiscretize}{ A number of categories of the continuous variable to be discretized.  By default, 
  \code{numCategoriesToDiscretize}=4. }
  \item{java.parameters}{ string indicating an optional parameters for JVM. 
  For example, java.parameters = "-Xmx1024M". By default, 
  \code{java.parameters}=\code{NULL}. }
  \item{...}{ parameters specific to the designed algorithm. To check which parameters the algorithm accepts, use \code{tetradrunner.getAlgorithmParameters}. }
}
\details{
The Tetrad Runner is a R wrapper implemented for running the search algorithms from the Tetrad library.

More detail about Tetrad implementation, please visit \href{http://www.phil.cmu.edu/tetrad/index.html}{the Tetrad project}.
}
\value{
A list containing the result's graph, the result's nodes, and 
the result's edges.
}
\examples{
data("charity")
tetradrunner.getAlgorithmDescription(algoId = 'fges')
tetradrunner.getAlgorithmParameters(algoId = 'fges',scoreId = 'fisher-z')

tetradrunner <- tetradrunner(algoId = 'fges',df = charity,scoreId = 'fisher-z',dataType = 'continuous')
tetradrunner$edges

tetradrunner <- tetradrunner(algoId = 'fges',df = charity,scoreId = 'fisher-z',dataType = 'continuous',
alpha=0.1,faithfulnessAssumed=TRUE,maxDegree=-1,verbose=TRUE)
tetradrunner$edges

# Using the default score
tetradrunner <- tetradrunner(algoId = 'fges',df = charity,dataType = 'continuous')
tetradrunner$edges

# Bootstrapping
tetradrunner <- tetradrunner(algoId = 'fges',df = charity,dataType = 'continuous',
alpha=0.1,faithfulnessAssumed=TRUE,maxDegree=-1,verbose=TRUE,
numberResampling = 10,resamplingEnsemble = 0,addOriginalDataset = TRUE)
tetradrunner$edges #Show the result's edges
tetradrunner$nodes #Show the result's nodes
}
\keyword{tetradrunner}
\keyword{causality search}
\keyword{causal graphs}
