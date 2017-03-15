mbfs <- function(df, continuous = TRUE, depth = 3, significance = 0.05,
    aggressivelyPreventCycles = FALSE, verbose = FALSE, java.parameters = NULL, 
    priorKnowledge = NULL){
    
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

    mbfs <- list()
    class(mbfs) <- "mbfs"

    mbfs$datasets <- deparse(substitute(df))

    cat("Datasets:\n")
    cat(deparse(substitute(df)),"\n\n")

    # Initiate mbfs
    mbfs_instance <- .jnew("edu/cmu/tetrad/search/Mbfs", indTest, as.integer(depth))
    .jcall(mbfs_instance, "V", "setAggressivelyPreventCycles", aggressivelyPreventCycles)
    .jcall(mbfs_instance, "V", "setVerbose", verbose)

    if(!is.null(priorKnowledge)){
        .jcall(mbfs_instance, "V", "setKnowledge", priorKnowledge)
    }

    params <- c(params, continuous = as.logical(continuous))
    params <- c(params, depth = as.integer(depth))
    params <- c(params, significance = significance)
    params <- c(params, aggressivelyPreventCycles = aggressivelyPreventCycles)
    params <- c(params, verbose = as.logical(verbose))

    if(!is.null(priorKnowledge)){
        params <- c(params, prior = priorKnowledge)
    }
    mbfs$parameters <- params

    cat("Graph Parameters:\n")
    cat("continuous = ", continuous,"\n")
    cat("depth = ", as.integer(depth),"\n")
    cat("significance = ", as.numeric(significance),"\n")
    cat("aggressivelyPreventCycles = ", aggressivelyPreventCycles,"\n")
    cat("verbose = ", verbose,"\n")

    # Search
    tetrad_graph <- .jcall(mbfs_instance, "Ledu/cmu/tetrad/graph/Graph;", 
        "search")

    V <- extractTetradNodes(tetrad_graph)

    mbfs$nodes <- V

    # extract edges
    mbfs_edges <- extractTetradEdges(tetrad_graph)

    mbfs$edges <- mbfs_edges

    return(mbfs)
}