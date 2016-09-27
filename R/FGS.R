fgs <- function(df, penaltydiscount = 4.0, maxDegree = 3, 
    ignoreLinearDependence = TRUE, heuristicSpeedup = TRUE, numOfThreads = 2, 
    verbose = FALSE, java.parameters = NULL, priorKnowledge = NULL){
    
    params <- list(NULL)
    
    if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params <- c(java.parameters = java.parameters)
    }

    # Data Frame to Tetrad Dataset
    score <- dataFrame2TetradSemBicScore(df,penaltydiscount)

    fgs <- list()
    class(fgs) <- "fgs"

    fgs$datasets <- deparse(substitute(df))

    cat("Datasets:\n")
    cat(deparse(substitute(df)),"\n\n")

    # Initiate FGS
    fgs_instance <- .jnew("edu/cmu/tetrad/search/Fgs", score)
    .jcall(fgs_instance, "V", "setMaxDegree", as.integer(maxDegree))
    .jcall(fgs_instance, "V", "setNumPatternsToStore", as.integer(0))
    .jcall(fgs_instance, "V", "setIgnoreLinearDependent", ignoreLinearDependence)
    .jcall(fgs_instance, "V", "setHeuristicSpeedup", heuristicSpeedup)
    .jcall(fgs_instance, "V", "setParallelism", as.integer(numOfThreads))
    .jcall(fgs_instance, "V", "setVerbose", verbose)

    if(!is.null(priorKnowledge)){
        .jcall(fgs_instance, "V", "setKnowledge", priorKnowledge)
    }

    params <- c(params, penaltydiscount = as.double(penaltydiscount))
    params <- c(params, maxDegree = as.integer(maxDegree))
    params <- c(params, ignoreLinearDependence = as.logical(ignoreLinearDependence))
    params <- c(params, heuristicSpeedup = as.logical(heuristicSpeedup))
    params <- c(params, numOfThreads = as.integer(numOfThreads))
    params <- c(params, verbose = as.logical(verbose))

    if(!is.null(priorKnowledge)){
        params <- c(params, prior = priorKnowledge)
    }
    fgs$parameters <- params

    cat("Graph Parameters:\n")
    cat("penalty discount = ", penaltydiscount,"\n")
    cat("maxDegree = ", as.integer(maxDegree),"\n")
    cat("ignoreLinearDependence = ", ignoreLinearDependence,"\n")
    cat("heuristicSpeedup = ", heuristicSpeedup,"\n")
    cat("numOfThreads = ", as.integer(numOfThreads),"\n")
    cat("verbose = ", verbose,"\n")

    # Search
    tetrad_graph <- .jcall(fgs_instance, "Ledu/cmu/tetrad/graph/Graph;", 
        "search")

    V <- extractTetradNodes(tetrad_graph)

    fgs$nodes <- V

    # extract edges
    fgs_edges <- extractTetradEdges(tetrad_graph)

    fgs$edges <- fgs_edges

    # convert output of FGS into an R object (graphNEL)
    fgs_graphNEL = tetradPattern2graphNEL(resultGraph = tetrad_graph,
        verbose = verbose)

    fgs$graphNEL <- fgs_graphNEL

    return(fgs)
}
