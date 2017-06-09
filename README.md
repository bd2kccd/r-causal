# r-causal
R Wrapper for Tetrad Library

## R Library Requirement
R >= 3.3.0, 
[stringr](https://cran.r-project.org/web/packages/stringr/),
[rJava](https://cran.r-project.org/web/packages/rJava/index.html), 

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

#Compute FGES search
fges <- fges(df = charity, penaltydiscount = 2, maxDegree = -1,  
faithfulnessAssumed = TRUE, numOfThreads = 2, verbose = TRUE)    

fges$parameters #Show the FGES's parameters
fges$datasets #Show the dataset
fges$nodes #Show the result's nodes
fges$edges #Show the result's edges
```
### Discrete Dataset
```R
library(rcausal)
data("audiology")    #Load the charity dataset
#Compute FGES search
fges.discrete <- fges.discrete(df=audiology,structurePrior=1.0,samplePrior=1.0, 
maxDegree = -1, faithfulnessAssumed = TRUE, numOfThreads = 2,verbose = TRUE)
fges.discrete$parameters #Show the FGES Discrete's parameters
fges.discrete$datasets #Show the dataset
fges.discrete$nodes #Show the result's nodes
fges.discrete$edges #Show the result's edges
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
fges <- fges(df = charity, penaltydiscount = 2, depth = -1, ignoreLinearDependence = TRUE, 
heuristicSpeedup = TRUE, numOfThreads = 2, verbose = TRUE, priorKnowledge = prior)
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
fges.discrete <- fges.discrete(df=audiology,structurePrior=1.0,samplePrior=1.0, 
depth = -1, heuristicSpeedup = TRUE, numOfThreads = 2,verbose = TRUE, priorKnowledge = prior)
```

### Convert Rgraphviz to igraph one
```R
library(igraph)
igraph <- igraph.from.graphNEL(fges.discrete$graphNEL)
plot(igraph)
```

## Useful `rJava` Trouble-shooting Installation in Mac OS X Links

1. [http://stackoverflow.com/questions/26948777/how-can-i-make-rjava-use-the-newer-version-of-java-on-osx/32544358#32544358](http://stackoverflow.com/questions/26948777/how-can-i-make-rjava-use-the-newer-version-of-java-on-osx/32544358#32544358)
2. [http://andrewgoldstone.com/blog/2015/02/03/rjava/](http://andrewgoldstone.com/blog/2015/02/03/rjava/)
