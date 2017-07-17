bootstrap.rfci <- function(df, depth = 3, maxPathLength = -1,
    significance = 0.05, completeRuleSetUsed = FALSE, faithfulnessAssumed = TRUE,
    numBootstrapSamples = 10, ensembleMethod = 'Highest', verbose = FALSE,
    java.parameters = NULL, priorKnowledge = NULL){
    
    params <- list(NULL)
    
    if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params <- c(java.parameters = java.parameters)
    }

    # Data Frame to Tetrad Dataset
    data <- loadContinuousData(df)

    fges <- list()
    class(fges) <- "bootstrap.rfci"

    fges$datasets <- deparse(substitute(df))

    cat("Datasets:\n")
    cat(deparse(substitute(df)),"\n\n")

    # Parameters
    parameters_instance <- .jnew("edu/cmu/tetrad/util/Parameters")
    .jcall(parameters_instance, "V", "set", "depth", as.integer(depth))
    .jcall(parameters_instance, "V", "set", "maxPathLength", as.integer(maxPathLength))
    .jcall(parameters_instance, "V", "set", "alpha", significance)
    .jcall(parameters_instance, "V", "set", "completeRuleSetUsed", completeRuleSetUsed)
    .jcall(parameters_instance, "V", "set", "faithfulnessAssumed", faithfulnessAssumed)
    .jcall(parameters_instance, "V", "set", "numPatternsToStore", as.integer(0))
    .jcall(parameters_instance, "V", "set", "verbose", verbose)

    # Initiate Bootstrapping RFCI
    algoName <- .jfield("edu/pitt/dbmi/algo/bootstrap/BootstrapAlgName",, "RFCI")
    fges_instance <- .jnew("edu/pitt/dbmi/algo/bootstrap/BootstrapTest", data, algoName)
    .jcall(fges_instance, "V", "setNumBootstrapSamples", as.integer(numBootstrapSamples))
    .jcall(fges_instance, "V", "setVerbose", verbose)
    .jcall(fges_instance, "V", "setParameters", parameters_instance)
    .jcall(fges_instance, "V", "setEdgeEnsemble", ensembleMethod)
    .jcall(fges_instance, "V", "setParallelMode", FALSE)

    if(!is.null(priorKnowledge)){
        .jcall(fges_instance, "V", "setKnowledge", priorKnowledge)
    }

    params <- c(params, depth = as.integer(depth))
    params <- c(params, maxPathLength = as.integer(maxPathLength))
    params <- c(params, significance = as.double(significance))
    params <- c(params, completeRuleSetUsed = as.logical(completeRuleSetUsed))
    params <- c(params, faithfulnessAssumed = as.logical(faithfulnessAssumed))
    params <- c(params, numBootstrapSamples = as.integer(numBootstrapSamples))
    params <- c(params, ensembleMethod = ensembleMethod)
    params <- c(params, verbose = as.logical(verbose))

    if(!is.null(priorKnowledge)){
        params <- c(params, prior = priorKnowledge)
    }
    fges$parameters <- params

    cat("Graph Parameters:\n")
    cat("depth = ", as.integer(depth),"\n")
    cat("maxPathLength = ", as.integer(maxPathLength),"\n")
    cat("significance = ", significance,"\n")
    cat("completeRuleSetUsed = ", completeRuleSetUsed,"\n")
    cat("faithfulnessAssumed = ", faithfulnessAssumed,"\n")
    cat("numBootstrapSamples = ", as.integer(numBootstrapSamples),"\n")
    cat("ensembleMethod = ", ensembleMethod,"\n")
    cat("verbose = ", verbose,"\n")

    # Search
    tetrad_graph <- .jcall(fges_instance, "Ledu/cmu/tetrad/graph/Graph;", 
        "search", check=FALSE)

    if(!is.null(e <- .jgetEx())){
        .jclear()
        fges$nodes <- colnames(df)
        fges$edges <- NULL
        # print("Java exception was raised")
        # print(e)
    }else{
        V <- extractTetradNodes(tetrad_graph)
        
        fges$nodes <- V
        
        # extract edges
        fges_edges <- extractTetradEdges(tetrad_graph)
        
        fges$edges <- fges_edges
    }
    
    return(fges)
}
