## rule: IGCI, R1TimeLag, R1, R2, R3, R4, Tanh, EB, Skew, SkewE, RSkew, RSkewE, Patel, Patel25, Patel50, Patel75, Patel90, FastICA, RC, Nlo
## score: andersonDarling, skew, kurtosis, fifthMoment, absoluteValue, exp, expUnstandardized, expUnstandardizedInverted, other, logcosh, entropy
lofs <- function(tetradGraph, dfs, dataType = 0, numCategoriesToDiscretize = 4, rule = 'R1', 
	score = 'andersonDarling', alpha = 0.01, epsilon = 1.0, zeta = 0.0, 
	orientStrongerDirection = FALSE, r2Orient2Cycles = TRUE, edgeCorrected = FALSE, 
	selfLoopStrength = 1.0, java.parameters = NULL){
     
    params <- list()
    params$java.parameters <- list()

    if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params <- c(java.parameters = java.parameters)
    }

    datasets <- .jnew("java/util/ArrayList")
	for(i in 1:length(dfs)){
    	df <- dfs[[i]]
    	if(dataType == 0){
	        boxData <- loadContinuousData(df)
    	}else if(dataType == 1){
	        boxData <- loadDiscreteData(df)
    	}else{
	        boxData <- loadMixedData(df, numCategoriesToDiscretize)
    	}
        datasets$add(boxData)
    }
    
	lofs <- list()
    class(lofs) <- "lofs"

    lofs_instance <- .jnew("edu/cmu/tetrad/search/Lofs2", tetradGraph, datasets)
    rule <- .jfield('edu/cmu/tetrad/search/Lofs2$Rule', name=rule)
    score <- .jfield('edu/cmu/tetrad/search/Lofs$Score', name=score)
    lofs_instance$setRule(rule)
    lofs_instance$setScore(score)
    lofs_instance$setAlpha(alpha)
    lofs_instance$setEpsilon(epsilon)
    lofs_instance$setZeta(zeta)
    lofs_instance$setOrientStrongerDirection(orientStrongerDirection)
    lofs_instance$setR2Orient2Cycles(r2Orient2Cycles)
    lofs_instance$setEdgeCorrected(edgeCorrected)
    lofs_instance$setSelfLoopStrength(selfLoopStrength)
    
    lofs_graph <- lofs_instance$orient()
    
    if(!is.null(e <- .jgetEx())){
        .jclear()
        lofs$nodes <- colnames(dfs[[1]])
        lofs$edges <- NULL
        print("Java exception was raised")
        print(e)
    }else{
    	lofs$graph <- lofs_graph
    
        V <- extractTetradNodes(lofs_graph)

        ## Get nodes.
        lofs$nodes <- V
        
        ## extract edges
        lofs_edges <- extractTetradEdges(lofs_graph)
        
        lofs$edges <- lofs_edges
    }
    
    return(lofs)
}
