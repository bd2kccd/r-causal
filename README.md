Note: This version of RCausal uses an older version of Tetrad, and our current (tiny) development team has not been able to get to updating it. The projects we are striving to keep up to date are Tetrad and Causal-Cmd, the command-line version of Tetrad. Here is the current version of Tetrad:

https://github.com/cmu-phil/tetrad

Here is the current version of Causal Command (causal-cmd):

https://github.com/bd2kccd/causal-cmd

Here is a link showing how to run Tetrad algorithms in Python through the os.sys command using causal-cmd:

https://github.com/cmu-phil/tetrad/wiki/Running-Tetrad-algorithms-in-Python

Sorry for the inconvenience; we realize rcausal is being used and will try to get to updating it hopefully in the near future.

Best,

Joe Ramsey, 2023-02-16

# r-causal
R Wrapper for Tetrad Library

## R Library Requirement
R >= 3.3.0, 
[stringr](https://cran.r-project.org/web/packages/stringr/),
[rJava](https://cran.r-project.org/web/packages/rJava/index.html) 

## Installation

- Install the R library requirements:
```R
install.packages("stringr")
install.packages("rJava")
```
- Install r-causal from github:

```R
library(devtools)
install_github("bd2kccd/r-causal")
```

## Example
### Continuous Dataset
```R
library(rcausal)
data("charity")   #Load the charity dataset

tetradrunner.getAlgorithmDescription(algoId = 'fges')
#Compute FGES search
tetradrunner <- tetradrunner(algoId = 'fges',df = charity,scoreId = 'sem-bic',
dataType = 'continuous',faithfulnessAssumed=TRUE,maxDegree=-1,verbose=TRUE)

tetradrunner$nodes #Show the result's nodes
tetradrunner$edges #Show the result's edges

graph <- tetradrunner$graph
graph$getAttribute('BIC')

nodes <- graph$getNodes()
for(i in 0:as.integer(nodes$size()-1)){
    node <- nodes$get(i)
    cat(node$getName(),": ",node$getAttribute('BIC'),"\n")
}
```
### Discrete Dataset
```R
library(rcausal)
data("audiology")    #Load the charity dataset

#Compute FGES search
tetradrunner <- tetradrunner(algoId = 'fges',df = audiology,scoreId = 'cg-bic-score',dataType = 'discrete',
faithfulnessAssumed=TRUE,maxDegree=-1,verbose=TRUE)

tetradrunner$nodes #Show the result's nodes
tetradrunner$edges #Show the result's edges

graph <- tetradrunner$graph
graph$getAttribute('BIC')

nodes <- graph$getNodes()
for(i in 0:as.integer(nodes$size()-1)){
    node <- nodes$get(i)
    cat(node$getName(),": ",node$getAttribute('BIC'),"\n")
}
```

### Prior Knowledge

#### Create PriorKnowledge Object
```R
forbid <- list(c('TangibilityCondition','Impact')) # List of forbidden directed edges
require <- list(c('Sympathy','TangibilityCondition')) # List of required directed edges
forbiddenWithin <- c('TangibilityCondition','Imaginability')
class(forbiddenWithin) <- 'forbiddenWithin' # Make this tier forbidden within
temporal <- list(forbiddenWithin, c('Sympathy','AmountDonated'),c('Impact')) # List of temporal node tiers
prior <- priorKnowledge(forbiddirect = forbid, requiredirect = require, addtemporal = temporal)
tetradrunner <- tetradrunner(algoId = 'fges',df = charity,scoreId = 'fisher-z',
dataType = 'continuous',alpha=0.1,faithfulnessAssumed=TRUE,maxDegree=-1,verbose=TRUE, 
priorKnowledge = prior)
```

#### Load Knowledge File
```R
# knowledge file: audiology.prior
# /knowledge
# forbiddirect
# class tymp
# class age_gt_60
# class notch_at_4k
# 
# requiredirect
# history_noise class
#
# addtemporal
# 0* bser late_wave_poor tymp notch_at_4k o_ar_c ar_c airBoneGap air bone o_ar_u airBoneGap
# 1 history_noise history_dizziness history_buzzing history_roaring history_recruitment history_fluctuating history_heredity history_nausea
# 2 class

prior <- priorKnowledgeFromFile('audiology.prior')
tetradrunner <- tetradrunner(algoId = 'fges',df = audiology,scoreId = 'bdeu',dataType = 'discrete',
alpha=0.1,faithfulnessAssumed=TRUE,maxDegree=-1,verbose=TRUE, priorKnowledge = prior)
```

#### Plot a DOT graph
```R
library(DOT)
graph_dot <- tetradrunner.tetradGraphToDot(tetradrunner$graph)
dot(graph_dot)
```

## Useful `rJava` Trouble-shooting Installation in Mac OS X Links

1. [http://stackoverflow.com/questions/26948777/how-can-i-make-rjava-use-the-newer-version-of-java-on-osx/32544358#32544358](http://stackoverflow.com/questions/26948777/how-can-i-make-rjava-use-the-newer-version-of-java-on-osx/32544358#32544358)
2. [http://andrewgoldstone.com/blog/2015/02/03/rjava/](http://andrewgoldstone.com/blog/2015/02/03/rjava/)
3. [https://stackoverflow.com/questions/7019912/using-the-rjava-package-on-win7-64-bit-with-r/7604469#7604469](https://stackoverflow.com/questions/7019912/using-the-rjava-package-on-win7-64-bit-with-r/7604469#7604469)

## Citation

[![DOI](https://zenodo.org/badge/50114582.svg)](https://zenodo.org/badge/latestdoi/50114582)
