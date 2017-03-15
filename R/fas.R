fas <- function(df, continuous = TRUE, depth = 3, sepsetsReturnEmptyIfNotFixed = FALSE, 
	significance = 0.05, verbose = FALSE, java.parameters = NULL, priorKnowledge = NULL){
    
    params <- list(NULL)
    
    if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params <- c(java.parameters = java.parameters)
    }

    # Data Frame to Independence Test
    indTest = NULL
    if(continuous){
    	tetradData <- loadContinuousData(df)
    	indTest <- .jnew("edu/cmu/tetrad/search/IndTestFisherZ", tetradData, significance)
    }else{
    	tetradData <- loadDiscreteData(df)
    	indTest <- .jnew("edu/cmu/tetrad/search/IndTestChiSquare", tetradData, 
    		significance)
    }
    
	indTest <- .jcast(indTest, "edu/cmu/tetrad/search/IndependenceTest")

    fas <- list()
    class(fas) <- "fas"

    fas$datasets <- deparse(substitute(df))

    cat("Datasets:\n")
    cat(deparse(substitute(df)),"\n\n")

    # Initiate Fas
    fas_instance <- .jnew("edu/cmu/tetrad/search/Fas", indTest)
    .jcall(fas_instance, "V", "setDepth", as.integer(depth))
    .jcall(fas_instance, "V", "setSepsetsReturnEmptyIfNotFixed", sepsetsReturnEmptyIfNotFixed)
    .jcall(fas_instance, "V", "setVerbose", verbose)

    if(!is.null(priorKnowledge)){
        .jcall(fas_instance, "V", "setKnowledge", priorKnowledge)
    }

	params <- c(params, continuous = continuous)
    params <- c(params, depth = as.integer(depth))
    params <- c(params, sepsetsReturnEmptyIfNotFixed = as.logical(sepsetsReturnEmptyIfNotFixed))
    params <- c(params, significance = significance)
    params <- c(params, verbose = as.logical(verbose))

    if(!is.null(priorKnowledge)){
        params <- c(params, prior = priorKnowledge)
    }
    fas$parameters <- params

    cat("Graph Parameters:\n")
    cat("continuous = ", continuous, "\n")
    cat("depth = ", as.integer(depth),"\n")
    cat("sepsetsReturnEmptyIfNotFixed = ", sepsetsReturnEmptyIfNotFixed, "\n")
    cat("significance = ", as.numeric(significance), "\n")
    cat("verbose = ", verbose, "\n")

    # Search
    tetrad_graph <- .jcall(fas_instance, "Ledu/cmu/tetrad/graph/Graph;", 
        "search")

    V <- extractTetradNodes(tetrad_graph)

    fas$nodes <- V

    # extract edges
    fas_edges <- extractTetradEdges(tetrad_graph)

    fas$edges <- fas_edges

    # convert output of Fas into an R object (graphNEL)
    fas_graphNEL = tetradPattern2graphNEL(resultGraph = tetrad_graph,
        verbose = verbose)

    fas$graphNEL <- fas_graphNEL

    return(fas)
}