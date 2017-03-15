tsimages.discrete <- function(dfs, structurePrior = 1.0, samplePrior = 1.0, 
    maxDegree = 3, maxPathLength = -1, significance = 0.05, completeRuleSetUsed = FALSE, 
    faithfulnessAssumed = TRUE, verbose = FALSE, java.parameters = NULL, 
    priorKnowledge = NULL){
    
    params <- list(NULL)
    
    if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params <- c(java.parameters = java.parameters)
    }

    # Data Frame to Tetrad Dataset
    score <- dataFrames2TetradBDeuScoreImages(dfs, structurePrior, samplePrior)
    
	indTest <- .jnew("edu/cmu/tetrad/search/IndTestScore", score, significance)
    
	indTest <- .jcast(indTest, "edu/cmu/tetrad/search/IndependenceTest")
	
    tsimages <- list()
    class(tsimages) <- "tsimages.discrete"

    tsimages$datasets <- deparse(substitute(dfs))

    cat("Datasets:\n")
    cat(deparse(substitute(dfs)),"\n\n")

    # Initiate TsGFci Discrete
    tsimages_instance <- .jnew("edu/cmu/tetrad/search/TsGFci", indTest, score)
    .jcall(tsimages_instance, "V", "setMaxIndegree", as.integer(maxDegree))
    .jcall(tsimages_instance, "V", "setMaxPathLength", as.integer(maxPathLength))
    .jcall(tsimages_instance, "V", "setCompleteRuleSetUsed", completeRuleSetUsed)
    .jcall(tsimages_instance, "V", "setFaithfulnessAssumed", faithfulnessAssumed)
    .jcall(tsimages_instance, "V", "setVerbose", verbose)

    if(!is.null(priorKnowledge)){
        .jcall(tsimages_instance, "V", "setKnowledge", priorKnowledge)
    }

    params <- c(params, structurePrior = as.double(structurePrior))
    params <- c(params, samplePrior = as.double(samplePrior))
    params <- c(params, maxDegree = as.integer(maxDegree))
    params <- c(params, maxPathLength = as.integer(maxPathLength))
    params <- c(params, completeRuleSetUsed = as.logical(completeRuleSetUsed))
    params <- c(params, faithfulnessAssumed = as.logical(faithfulnessAssumed))
    params <- c(params, verbose = as.logical(verbose))

    if(!is.null(priorKnowledge)){
        params <- c(params, prior = priorKnowledge)
    }
    tsimages$parameters <- params

    cat("Graph Parameters:\n")
    cat("structurePrior = ", structurePrior,"\n")
    cat("samplePrior = ", samplePrior,"\n")
    cat("maxDegree = ", as.integer(maxDegree),"\n")
    cat("maxPathLength = ", as.integer(maxPathLength),"\n")
    cat("significance = ", as.numeric(significance),"\n")
    cat("completeRuleSetUsed = ", completeRuleSetUsed,"\n")
    cat("faithfulnessAssumed = ", faithfulnessAssumed,"\n")
    cat("verbose = ", verbose,"\n")
    
    # Search
    tetrad_graph <- .jcall(tsimages_instance, "Ledu/cmu/tetrad/graph/Graph;", "search")

    V <- extractTetradNodes(tetrad_graph)

    tsimages$nodes <- V

    # extract edges
    tsimages_edges <- extractTetradEdges(tetrad_graph)

    tsimages$edges <- tsimages_edges

    return(tsimages)
}