fges.discrete <- function(df, structurePrior = 1.0, samplePrior = 1.0, maxDegree = 3, 
	faithfulnessAssumed = TRUE, verbose = FALSE, java.parameters = NULL,
	priorKnowledge = NULL){

    params <- list()
    
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
    fges_instance <- .jnew("edu/cmu/tetrad/search/Fges", score)
    .jcall(fges_instance, "V", "setMaxDegree", as.integer(maxDegree))
    .jcall(fges_instance, "V", "setNumPatternsToStore", as.integer(0))
    .jcall(fges_instance, "V", "setFaithfulnessAssumed", faithfulnessAssumed)
    .jcall(fges_instance, "V", "setVerbose", verbose)

    if(!is.null(priorKnowledge)){
        .jcall(fges_instance, "V", "setKnowledge", priorKnowledge)
    }

    params <- c(params, structurePrior = as.double(structurePrior))
    params <- c(params, samplePrior = as.double(samplePrior))
    params <- c(params, maxDegree = as.integer(maxDegree))
    params <- c(params, faithfulnessAssumed = as.logical(faithfulnessAssumed))
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
    cat("verbose = ", verbose,"\n")

    # Search
    tetrad_graph <- .jcall(fges_instance, "Ledu/cmu/tetrad/graph/Graph;",
        "search", check=FALSE)

    if(!is.null(e <- .jgetEx())){
        .jclear()
        fges$nodes <- colnames(df)
        fges$edges <- NULL
        # print("Java exception was raised")
        # print(e)
    }else{
        V <- extractTetradNodes(tetrad_graph)
    
        fges$nodes <- V
    
        # extract edges
        fges_edges <- extractTetradEdges(tetrad_graph)
    
        fges$edges <- fges_edges
    }

    return(fges)
}
