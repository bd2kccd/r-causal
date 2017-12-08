bayesEst <- function(df, depth = 3, significance = 0.05, verbose = FALSE, 
	java.parameters = NULL, priorKnowledge = NULL){
    
    params <- list(NULL)
    
    if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params <- c(java.parameters = java.parameters)
    }

    # Data Frame to Independence Test
    tetradData <- loadDiscreteData(df)
    indTest <- .jnew("edu/cmu/tetrad/search/IndTestChiSquare", tetradData, significance)
	indTest <- .jcast(indTest, "edu/cmu/tetrad/search/IndependenceTest")

    bayesEst <- list()
    class(bayesEst) <- "bayesEst"

    bayesEst$datasets <- deparse(substitute(df))

    cat("Datasets:\n")
    cat(deparse(substitute(df)),"\n\n")

    # Initiate CPC
    cpc_instance <- .jnew("edu/cmu/tetrad/search/Cpc", indTest)
    .jcall(cpc_instance, "V", "setDepth", as.integer(depth))
    .jcall(cpc_instance, "V", "setVerbose", verbose)

    if(!is.null(priorKnowledge)){
        .jcall(ccd_instance, "V", "setKnowledge", priorKnowledge)
    }

    params <- c(params, depth = as.integer(depth))
    params <- c(params, significance = significance)
    params <- c(params, verbose = as.logical(verbose))

    if(!is.null(priorKnowledge)){
        params <- c(params, prior = priorKnowledge)
    }
    bayesEst$parameters <- params

    cat("Graph Parameters:\n")
    cat("depth = ", as.integer(depth),"\n")
    cat("significance = ", as.numeric(significance),"\n")
    cat("verbose = ", verbose,"\n")

    # Search
    tetrad_graph <- .jcall(cpc_instance, "Ledu/cmu/tetrad/graph/Graph;", 
        "search", check=FALSE)
    
    if(!is.null(e <- .jgetEx())){
        .jclear()
        bayesEst$nodes <- NULL
        bayesEst$edges <- NULL
        bayesEst$dag <- NULL
        bayesEst$bayesPm <- NULL
        bayesEst$bayesIm <- NULL
        print("Java exception was raised")
        print(e)
    }else{
        dagPatternIt <- .jnew("edu/cmu/tetrad/search/DagInPatternIterator", tetrad_graph)
        dag_graph <- .jcall(dagPatternIt, "Ledu/cmu/tetrad/graph/Graph;", "next")
        dag <- .jnew("edu/cmu/tetrad/graph/Dag", dag_graph)
        dag <- .jcast(dag, "edu/cmu/tetrad/graph/Graph")
        pm <- .jnew("edu/cmu/tetrad/bayes/BayesPm", dag)
        est <- .jnew("edu/cmu/tetrad/bayes/MlBayesEstimator")
        im <- .jcall(est, "Ledu/cmu/tetrad/bayes/BayesIm;", "estimate", pm, tetradData)
        
        bayesEst$graph <- dag_graph
        
        V <- extractTetradNodes(dag_graph)
        
        bayesEst$nodes <- V
        
        # extract edges
        bayesEst_edges <- extractTetradEdges(dag_graph)
        
        bayesEst$edges <- bayesEst_edges
        bayesEst$dag <- dag
        bayesEst$bayesPm <- pm
        bayesEst$bayesIm <- im
    }

    return(bayesEst)
}
