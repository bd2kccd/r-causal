fges.discrete <- function(df, structurePrior = 1.0, samplePrior = 1.0, maxDegree = 3, 
	faithfulnessAssumed = TRUE, numOfThreads = 2, verbose = FALSE, java.parameters = NULL, 
	priorKnowledge = NULL){

    params <- list(NULL)
    
    if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params <- c(java.parameters = java.parameters)
    }
    
    # Data Frame to Tetrad Dataset
    score <- dataFrame2TetradBDeuScore(df, structurePrior, samplePrior)

    fges <- list()
    class(fges) <- "fges.discrete"

    fges$datasets <- deparse(substitute(df))

    cat("Datasets:\n")
    cat(deparse(substitute(df)),"\n")

    # Initiate FGES Discrete
    fges_instance <- .jnew("edu/cmu/tetrad/search/Fgs", score)
    .jcall(fges_instance, "V", "setMaxDegree", as.integer(maxDegree))
    .jcall(fges_instance, "V", "setNumPatternsToStore", as.integer(0))
    .jcall(fges_instance, "V", "setFaithfulnessAssumed", faithfulnessAssumed)
    .jcall(fges_instance, "V", "setParallelism", as.integer(numOfThreads))
    .jcall(fges_instance, "V", "setVerbose", verbose)

    if(!is.null(priorKnowledge)){
        .jcall(fges_instance, "V", "setKnowledge", priorKnowledge)
    }

    params <- c(params, structurePrior = as.double(structurePrior))
    params <- c(params, samplePrior = as.double(samplePrior))
    params <- c(params, maxDegree = as.integer(maxDegree))
    params <- c(params, faithfulnessAssumed = as.logical(faithfulnessAssumed))
    params <- c(params, numOfThreads = numOfThreads)
    params <- c(params, verbose = as.logical(verbose))

    if(!is.null(priorKnowledge)){
        params <- c(params, prior = priorKnowledge)
    }
    
    fges$parameters <- params

    cat("Graph Parameters:\n")
    cat("structurePrior = ", structurePrior,"\n")
    cat("samplePrior = ", samplePrior,"\n")
    cat("maxDegree = ", as.integer(maxDegree),"\n")
    cat("faithfulnessAssumed = ", faithfulnessAssumed,"\n")
    cat("numOfThreads = ", numOfThreads,"\n")
    cat("verbose = ", verbose,"\n")

    # Search
    tetrad_graph <- .jcall(fges_instance, "Ledu/cmu/tetrad/graph/Graph;", 
        "search")

    V <- extractTetradNodes(tetrad_graph)

    fges$nodes <- V

    # extract edges
    fges_edges <- extractTetradEdges(tetrad_graph)

    fges$edges <- fges_edges

    # convert output of FGES into an R object (graphNEL)
    fges_graphNEL = tetradPattern2graphNEL(resultGraph = tetrad_graph,
        verbose = verbose)

    fges$graphNEL <- fges_graphNEL

    return(fges) 
}