fas.mixed <- function(df, numCategoriesToDiscretize = 4, depth = 3, significance = 0.05, sepsetsReturnEmptyIfNotFixed = FALSE, verbose = FALSE, java.parameters = NULL,
    priorKnowledge = NULL){
    
    params <- list(NULL)
    
    if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params <- c(java.parameters = java.parameters)
    }
    
    # Data Frame to Independence Test
    tetradData <- loadMixedData(df, numCategoriesToDiscretize)
    indTest <- .jnew("edu/cmu/tetrad/search/IndTestConditionalGaussianLRT",
            tetradData, significance)
    
    indTest <- .jcast(indTest, "edu/cmu/tetrad/search/IndependenceTest")
    
    fas <- list()
    class(fas) <- "fas.mixed"
    
    fas$datasets <- deparse(substitute(df))
    
    cat("Datasets:\n")
    cat(deparse(substitute(df)),"\n\n")
    
    # Initiate FAS Mixed
    fas_instance <- .jnew("edu/cmu/tetrad/search/Fas", indTest)
    .jcall(fas_instance, "V", "setDepth", as.integer(depth))
    .jcall(fas_instance, "V", "setSepsetsReturnEmptyIfNotFixed", sepsetsReturnEmptyIfNotFixed)
    .jcall(fas_instance, "V", "setVerbose", verbose)
    
    if(!is.null(priorKnowledge)){
        .jcall(fas_instance, "V", "setKnowledge", priorKnowledge)
    }
    
    params <- c(params, depth = as.integer(depth))
    params <- c(params, significance = significance)
    params <- c(params, sepsetsReturnEmptyIfNotFixed = as.logical(sepsetsReturnEmptyIfNotFixed))
    params <- c(params, verbose = as.logical(verbose))
    
    if(!is.null(priorKnowledge)){
        params <- c(params, prior = priorKnowledge)
    }
    fas$parameters <- params
    
    cat("Graph Parameters:\n")
    cat("depth = ", as.integer(depth),"\n")
    cat("significance = ", as.numeric(significance),"\n")
    cat("sepsetsReturnEmptyIfNotFixed = ", sepsetsReturnEmptyIfNotFixed,"\n")
    cat("verbose = ", verbose,"\n")
    
    # Search
    tetrad_graph <- .jcall(fas_instance, "Ledu/cmu/tetrad/graph/Graph;", "search", check=FALSE)
    
    if(!is.null(e <- .jgetEx())){
        .jclear()
        fas$nodes <- colnames(df)
        fas$edges <- NULL
        # print("Java exception was raised")
        # print(e)
    }else{
        V <- extractTetradNodes(tetrad_graph)
        
        fas$nodes <- V
        
        # extract edges
        fas_edges <- extractTetradEdges(tetrad_graph)
        
        fas$edges <- fas_edges
    }
    
    return(fas)
}
