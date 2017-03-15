imagesSemBic <- function(dfs, penaltydiscount = 4.0, maxDegree = 3, 
    faithfulnessAssumed = TRUE, numOfThreads = 2, 
    verbose = FALSE, java.parameters = NULL, priorKnowledge = NULL){
    
    params <- list(NULL)
    
    if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params <- c(java.parameters = java.parameters)
    }

    # Data Frame to Tetrad Dataset
    score <- dataFrames2TetradSemBicScoreImages(dfs, penaltydiscount)

    imagesSemBic <- list()
    class(imagesSemBic) <- "imagesSemBic"

    imagesSemBic$datasets <- deparse(substitute(dfs))

    cat("Datasets:\n")
    cat(deparse(substitute(dfs)),"\n\n")

    # Initiate imagesSemBic
    imagesSemBic_instance <- .jnew("edu/cmu/tetrad/search/ImagesSemBic", score)
    .jcall(imagesSemBic_instance, "V", "setMaxDegree", as.integer(maxDegree))
    .jcall(imagesSemBic_instance, "V", "setNumPatternsToStore", as.integer(0))
    .jcall(imagesSemBic_instance, "V", "setFaithfulnessAssumed", faithfulnessAssumed)
    .jcall(imagesSemBic_instance, "V", "setParallelism", as.integer(numOfThreads))
    .jcall(imagesSemBic_instance, "V", "setVerbose", verbose)

    if(!is.null(priorKnowledge)){
        .jcall(imagesSemBic_instance, "V", "setKnowledge", priorKnowledge)
    }

    params <- c(params, penaltydiscount = as.double(penaltydiscount))
    params <- c(params, maxDegree = as.integer(maxDegree))
    params <- c(params, faithfulnessAssumed = as.logical(faithfulnessAssumed))
    params <- c(params, numOfThreads = as.integer(numOfThreads))
    params <- c(params, verbose = as.logical(verbose))

    if(!is.null(priorKnowledge)){
        params <- c(params, prior = priorKnowledge)
    }
    imagesSemBic$parameters <- params

    cat("Graph Parameters:\n")
    cat("penalty discount = ", penaltydiscount,"\n")
    cat("maxDegree = ", as.integer(maxDegree),"\n")
    cat("faithfulnessAssumed = ", faithfulnessAssumed,"\n")
    cat("numOfThreads = ", as.integer(numOfThreads),"\n")
    cat("verbose = ", verbose,"\n")

    # Search
    tetrad_graph <- .jcall(imagesSemBic_instance, "Ledu/cmu/tetrad/graph/Graph;", 
        "search")

    V <- extractTetradNodes(tetrad_graph)

    imagesSemBic$nodes <- V

    # extract edges
    imagesSemBic_edges <- extractTetradEdges(tetrad_graph)

    imagesSemBic$edges <- imagesSemBic_edges

    # convert output of imagesSemBic into an R object (graphNEL)
    imagesSemBic_graphNEL = tetradPattern2graphNEL(resultGraph = tetrad_graph,
        verbose = verbose)

    imagesSemBic$graphNEL <- imagesSemBic_graphNEL

    return(imagesSemBic)
}
