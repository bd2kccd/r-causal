# tetradR
R Wrapper for Tetrad Library

## R Library Requirement
[rJava](https://cran.r-project.org/web/packages/rJava/index.html), 
[graph](http://bioconductor.org/packages/release/bioc/html/graph.html),
[RBGL] (http://bioconductor.org/packages/release/bioc/html/RBGL.html)

## Installation

- Install the R library requirements:
```R
install.packages("rJava")
source("https://bioconductor.org/biocLite.R")
biocLite("graph")
biocLite("RBGL")
```
- Install the release version of devtools from CRAN with 
```R
install.packages("devtools")
```
- Install tetradR from github:

```R
library(devtools)
install_github("bd2kccd/tetradR")
```

## Example
```R
library(tetradR)
data("charity")   #Load the charity dataset
fgs <- fgs(df = charity, penaltydiscount = 2, depth = -1, faithfulness = TRUE, verbose = TRUE)    #Compute FGS search
plot(fgs$graphNEL) #Plot the causal models
```

## Useful `rJava` Trouble-shooting Installation in Mac OS X Links

1. [http://stackoverflow.com/questions/26948777/how-can-i-make-rjava-use-the-newer-version-of-java-on-osx/32544358#32544358](http://stackoverflow.com/questions/26948777/how-can-i-make-rjava-use-the-newer-version-of-java-on-osx/32544358#32544358)
2. [http://andrewgoldstone.com/blog/2015/02/03/rjava/](http://andrewgoldstone.com/blog/2015/02/03/rjava/)
