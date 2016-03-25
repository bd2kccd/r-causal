# r-causal
R Wrapper for Tetrad Library

## R Library Requirement
R >= 3.0.2, 
[stringr](https://cran.r-project.org/web/packages/stringr/),
[rJava](https://cran.r-project.org/web/packages/rJava/index.html), 
[graph](http://bioconductor.org/packages/release/bioc/html/graph.html), 
[RBGL] (http://bioconductor.org/packages/release/bioc/html/RBGL.html), 
[Rgraphviz] (http://bioconductor.org/packages/release/bioc/html/Rgraphviz.html)

## Installation

- Install the R library requirements:
```R
install.packages("stringr")
install.packages("rJava")
source("https://bioconductor.org/biocLite.R") 
# source("http://bioconductor.org/biocLite.R") # If encountering a connection problem with "https", try "http" instead.
biocLite("graph")
biocLite("RBGL")
biocLite("Rgraphviz") # For plotting graph
```
- Install the release version of devtools from CRAN with 
```R
install.packages("devtools")
```
- Install tetradR from github:

```R
library(devtools)
install_github("bd2kccd/r-causal")
```

## Example
```R
library(rcausal)
data("charity")   #Load the charity dataset
fgs <- fgs(df = charity, penaltydiscount = 2, depth = -1, faithfulness = TRUE, verbose = TRUE)    #Compute FGS search
fgs$parameters #Show the FGS's parameters
fgs$datasets #Show the dataset
fgs$nodes #Show the result's nodes
fgs$edges #Show the result's edges
library(Rgraphviz)
plot(fgs$graphNEL) #Plot the causal model
```

## Useful `rJava` Trouble-shooting Installation in Mac OS X Links

1. [http://stackoverflow.com/questions/26948777/how-can-i-make-rjava-use-the-newer-version-of-java-on-osx/32544358#32544358](http://stackoverflow.com/questions/26948777/how-can-i-make-rjava-use-the-newer-version-of-java-on-osx/32544358#32544358)
2. [http://andrewgoldstone.com/blog/2015/02/03/rjava/](http://andrewgoldstone.com/blog/2015/02/03/rjava/)
