#int[] inputs, int[] outputs, boolean useGES, DataSet data, int[] trueInputs,
#                    double alphaPC, double  alphaSober, double gesDiscount


fges <- function(inputs, outputs, useGES=TRUE, data, trueInputs, alphaPC=.05, alphaSober=.05, gesDiscount=2,
    verbose = FALSE, java.parameters = NULL, priorKnowledge = NULL){
    
    params <- list(NULL)
    
    if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params <- c(java.parameters = java.parameters)
    }

    # Data Frame to Tetrad Dataset
    score <- dataFrame2TetradSemBicScore(data)

    dm <- list()
    class(dm) <- "DMSearch"

    dm$datasets <- deparse(substitute(df))

    cat("Datasets:\n")
    cat(deparse(substitute(df)),"\n\n")

    # Initiate DMSearch
    dm_instance <- .jnew("edu/cmu/tetrad/search/DMSearch", inputs, outputs, useGES, data, trueInputs, alphaPC, alphaSober, gesDiscount)
    ## .jcall(fges_instance, "V", "setMaxDegree", as.integer(maxDegree))
    ## .jcall(fges_instance, "V", "setNumPatternsToStore", as.integer(0))
    ## .jcall(fges_instance, "V", "setFaithfulnessAssumed", faithfulnessAssumed)
    ## .jcall(fges_instance, "V", "setParallelism", as.integer(numOfThreads))
    ## .jcall(fges_instance, "V", "setVerbose", verbose)

    ## if(!is.null(priorKnowledge)){
    ##     .jcall(fges_instance, "V", "setKnowledge", priorKnowledge)
    ## }


    params <- c(params, alphaPC)
    params <- c(params, alphaSober)
    params <- c(params, gesDiscount)
    params <- c(params, verbose)
    
    
    ## params <- c(params, penaltydiscount = as.double(penaltydiscount))
    ## params <- c(params, maxDegree = as.integer(maxDegree))
    ## params <- c(params, faithfulnessAssumed = as.logical(faithfulnessAssumed))
    ## params <- c(params, numOfThreads = as.integer(numOfThreads))
    ## params <- c(params, verbose = as.logical(verbose))

    ## if(!is.null(priorKnowledge)){
    ##     params <- c(params, prior = priorKnowledge)
    ## }

    dm$parameters <- params

    cat("Graph Parameters:\n")
    cat("penalty discount = ", penaltydiscount,"\n")
    cat("maxDegree = ", as.integer(maxDegree),"\n")
    cat("faithfulnessAssumed = ", faithfulnessAssumed,"\n")
    cat("numOfThreads = ", as.integer(numOfThreads),"\n")
    cat("verbose = ", verbose,"\n")

    # Search
    tetrad_graph <- .jcall(fges_instance, "Ledu/cmu/tetrad/graph/Graph;", 
        "search")

    V <- extractTetradNodes(tetrad_graph)

    dm$nodes <- V

    # extract edges
    dm_edges <- extractTetradEdges(tetrad_graph)

    dm$edges <- dm_edges

    # convert output of FGES into an R object (graphNEL)
    dm_graphNEL = tetradPattern2graphNEL(resultGraph = tetrad_graph,
        verbose = verbose)

    dm$graphNEL <- dm_graphNEL

    return(dm)
}
