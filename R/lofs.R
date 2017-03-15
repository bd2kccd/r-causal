lofs <- function(dfs, initGraph, lofs.rule = NULL, edgeCorrected = FALSE, alpha = 1.0, 
    epsilon = 1.0, zeta = 0.0, java.parameters = NULL, priorKnowledge = NULL){
    
    params <- list(NULL)
    
    if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params <- c(java.parameters = java.parameters)
    }

    # Data Frame to Tetrad Dataset
    tetradDataList <- .jnew("java/util/ArrayList")
    for(i in 1:length(dfs)){
        tetradData <- loadContinuousData(dfs[i])
        tettradDataList.add(tetradData)
    }
    tetradDataList <- .jcast(tettradDataList, "java/util/List")

    lofs <- list()
    class(lofs) <- "lofs"

    lofs$datasets <- deparse(substitute(dfs))

    cat("Datasets:\n")
    cat(deparse(substitute(dfs)),"\n\n")

    # Lofs Rule
    if(is.null(lofs.rule)){
        lofs.rule <- .jfield("edu.cmu.tetrad.search.Lofs2$Rule",,"EB")
    }
    
    # Initiate lofs
    lofs_instance <- .jnew("edu/cmu/tetrad/search/Lofs2", initGraph, tetradDataList)
    .jcall(lofs_instance, "V", "setRule", lofs.rule)
    .jcall(lofs_instance, "V", "setEdgeCorrected", edgeCorrected)
    .jcall(lofs_instance, "V", "setAlpha", alpha)
    .jcall(lofs_instance, "V", "setEpsilon", epsilon)
    .jcall(lofs_instance, "V", "setZeta", zeta)

    if(!is.null(priorKnowledge)){
        .jcall(lofs_instance, "V", "setKnowledge", priorKnowledge)
    }

    params <- c(params, lofs.rule = lofs.rule)
    params <- c(params, edgeCorrected = edgeCorrected)
    params <- c(params, alpha = alpha)
    params <- c(params, epsilon = epsilon)
    params <- c(params, zeta = zeta)

    if(!is.null(priorKnowledge)){
        params <- c(params, prior = priorKnowledge)
    }
    lofs$parameters <- params

    cat("Graph Parameters:\n")
    cat("LOFS Rule = ", lofs.rule,"\n")
    cat("edgeCorrected = ", edgeCorrected,"\n")
    cat("alpha = ", alpha,"\n")
    cat("epsilon = ", epsilon,"\n")
    cat("zeta = ", zeta,"\n")

    # Search
    tetrad_graph <- .jcall(lofs_instance, "Ledu/cmu/tetrad/graph/Graph;", 
        "orient")

    V <- extractTetradNodes(tetrad_graph)

    lofs$nodes <- V

    # extract edges
    lofs_edges <- extractTetradEdges(tetrad_graph)

    lofs$edges <- lofs_edges

    # convert output of lofs into an R object (graphNEL)
    lofs_graphNEL = tetradPattern2graphNEL(resultGraph = tetrad_graph,
        verbose = verbose)

    lofs$graphNEL <- lofs_graphNEL

    return(lofs)
}
