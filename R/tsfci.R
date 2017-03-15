tsfci <- function(df, continuous = TRUE, penaltydiscount = 2.0, depth = 3, possibleDsepDepth = -1, maxPathLength = -1, 
    significance = 0.05, completeRuleSetUsed = FALSE, noDSepSearch = FALSE, verbose = FALSE, java.parameters = NULL, priorKnowledge = NULL){
    
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

    tsfci <- list()
    class(tsfci) <- "tsfci"

    tsfci$datasets <- deparse(substitute(df))

    cat("Datasets:\n")
    cat(deparse(substitute(df)),"\n\n")

    # Initiate TSFCI
    tsfci_instance <- .jnew("edu/cmu/tetrad/search/TsFci", indTest)
    .jcall(tsfci_instance, "V", "setPenaltyDiscount", penaltydiscount)
    .jcall(tsfci_instance, "V", "setDepth", as.integer(depth))
    .jcall(tsfci_instance, "V", "setPossibleDsepDepth", as.integer(possibleDsepDepth))
    .jcall(tsfci_instance, "V", "setMaxPathLength", as.integer(maxPathLength))
    .jcall(tsfci_instance, "V", "setCompleteRuleSetUsed", completeRuleSetUsed)
    .jcall(tsfci_instance, "V", "setPossibleDsepSearchDone", !noDSepSearch)
    .jcall(tsfci_instance, "V", "setVerbose", verbose)

    if(!is.null(priorKnowledge)){
        .jcall(tsfci_instance, "V", "setKnowledge", priorKnowledge)
    }

	params <- c(params, continuous = as.logical(continuous))
    params <- c(params, penaltydiscount = penaltydiscount)
    params <- c(params, depth = as.integer(depth))
    params <- c(params, possibleDsepDepth = as.integer(possibleDsepDepth))
    params <- c(params, maxPathLength = as.integer(maxPathLength))
    params <- c(params, completeRuleSetUsed = as.logical(completeRuleSetUsed))
    params <- c(params, noDSepSearch = as.logical(noDSepSearch))
    params <- c(params, significance = significance)
    params <- c(params, verbose = as.logical(verbose))

    if(!is.null(priorKnowledge)){
        params <- c(params, prior = priorKnowledge)
    }
    tsfci$parameters <- params

    cat("Graph Parameters:\n")
    cat("continuous = ", continuous,"\n")
    cat("penaltydiscount = ", penaltydiscount,"\n")
    cat("depth = ", as.integer(depth),"\n")
    cat("possibleDsepDepth = ", as.integer(possibleDsepDepth),"\n")
    cat("maxPathLength = ", as.integer(maxPathLength),"\n")
    cat("completeRuleSetUsed = ", completeRuleSetUsed,"\n")
    cat("noDSepSearch = ", noDSepSearch,"\n")
    cat("significance = ", as.numeric(significance),"\n")
    cat("verbose = ", verbose,"\n")

    # Search
    tetrad_graph <- .jcall(tsfci_instance, "Ledu/cmu/tetrad/graph/Graph;", 
        "search")

    V <- extractTetradNodes(tetrad_graph)

    tsfci$nodes <- V

    # extract edges
    tsfci_edges <- extractTetradEdges(tetrad_graph)

    tsfci$edges <- tsfci_edges

    return(tsfci)
}