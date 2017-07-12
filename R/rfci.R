rfci <- function(df, depth = 3, significance = 0.05,
    completeRuleSetUsed = FALSE, verbose = FALSE, java.parameters = NULL, 
    priorKnowledge = NULL){
    
    params <- list(NULL)
    
    if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params <- c(java.parameters = java.parameters)
    }

    # Data Frame to Independence Test
    tetradData <- loadContinuousData(df)
    indTest <- .jnew("edu/cmu/tetrad/search/IndTestFisherZ", tetradData,
    significance)
    
	indTest <- .jcast(indTest, "edu/cmu/tetrad/search/IndependenceTest")

    rfci <- list()
    class(rfci) <- "rfci"

    rfci$datasets <- deparse(substitute(df))

    cat("Datasets:\n")
    cat(deparse(substitute(df)),"\n\n")

    # Initiate RFCI
    rfci_instance <- .jnew("edu/cmu/tetrad/search/Rfci", indTest)
    .jcall(rfci_instance, "V", "setDepth", as.integer(depth))
    .jcall(rfci_instance, "V", "setCompleteRuleSetUsed", completeRuleSetUsed)
    .jcall(rfci_instance, "V", "setVerbose", verbose)

    if(!is.null(priorKnowledge)){
        .jcall(rfci_instance, "V", "setKnowledge", priorKnowledge)
    }

	params <- c(params, continuous = as.logical(continuous))
    params <- c(params, depth = as.integer(depth))
    params <- c(params, significance = significance)
    params <- c(params, completeRuleSetUsed = as.logical(completeRuleSetUsed))
    params <- c(params, verbose = as.logical(verbose))

    if(!is.null(priorKnowledge)){
        params <- c(params, prior = priorKnowledge)
    }
    rfci$parameters <- params

    cat("Graph Parameters:\n")
    cat("depth = ", as.integer(depth),"\n")
    cat("significance = ", as.numeric(significance),"\n")
    cat("completeRuleSetUsed = ", completeRuleSetUsed, "\n")
    cat("verbose = ", verbose,"\n")

    # Search
    tetrad_graph <- .jcall(rfci_instance, "Ledu/cmu/tetrad/graph/Graph;", 
        "search", check=FALSE)

    if(!is.null(e <- .jgetEx())){
        .jclear()
        rfci$nodes <- NULL
        rfci$edges <- NULL
        print("Java exception was raised")
        print(e)
    }else{
        V <- extractTetradNodes(tetrad_graph)
        
        rfci$nodes <- V
        
        # extract edges
        rfci_edges <- extractTetradEdges(tetrad_graph)
        
        rfci$edges <- rfci_edges
    }

    return(rfci)
}
