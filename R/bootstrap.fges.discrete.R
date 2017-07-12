bootstrap.fges.discrete <- function(df, structurePrior = 1.0, samplePrior = 1.0, maxDegree = 3,
    faithfulnessAssumed = TRUE, numBootstrapSamples = 10, ensembleMethod = 'Highest',
    verbose = FALSE, java.parameters = NULL, priorKnowledge = NULL){
    
    params <- list(NULL)
    
    if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params <- c(java.parameters = java.parameters)
    }

    # Data Frame to Tetrad Dataset
    data <- loadDiscreteData(df)

    fges <- list()
    class(fges) <- "bootstrap.fges.discrete"

    fges$datasets <- deparse(substitute(df))

    cat("Datasets:\n")
    cat(deparse(substitute(df)),"\n\n")

    # Parameters
    parameters_instance <- .jnew("edu/cmu/tetrad/util/Parameters")
    .jcall(parameters_instance, "V", "set", "structurePrior", structurePrior)
    .jcall(parameters_instance, "V", "set", "samplePrior", samplePrior)
    .jcall(parameters_instance, "V", "set", "maxDegree", as.integer(maxDegree))
    .jcall(parameters_instance, "V", "set", "faithfulnessAssumed", faithfulnessAssumed)
    .jcall(parameters_instance, "V", "set", "numPatternsToStore", as.integer(0))
    .jcall(parameters_instance, "V", "set", "verbose", verbose)

    # Initiate Bootstrapping FGES Discrete
    algoName <- .jfield("edu/pitt/dbmi/algo/bootstrap/BootstrapAlgName",, "FGES")
    fges_instance <- .jnew("edu/pitt/dbmi/algo/bootstrap/BootstrapTest", data, algoName)
    .jcall(fges_instance, "V", "setNumBootstrapSamples", as.integer(numBootstrapSamples))
    .jcall(fges_instance, "V", "setVerbose", verbose)
    .jcall(fges_instance, "V", "setParameters", parameters_instance)
    .jcall(fges_instance, "V", "setEdgeEnsemble", ensembleMethod)
    .jcall(fges_instance, "V", "setParallelMode", FALSE)

    if(!is.null(priorKnowledge)){
        .jcall(fges_instance, "V", "setKnowledge", priorKnowledge)
    }

    params <- c(params, structurePrior = as.double(structurePrior))
    params <- c(params, samplePrior = as.double(samplePrior))
    params <- c(params, maxDegree = as.integer(maxDegree))
    params <- c(params, faithfulnessAssumed = as.logical(faithfulnessAssumed))
    params <- c(params, numBootstrapSamples = as.integer(numBootstrapSamples))
    params <- c(params, ensembleMethod = ensembleMethod)
    params <- c(params, verbose = as.logical(verbose))

    if(!is.null(priorKnowledge)){
        params <- c(params, prior = priorKnowledge)
    }
    fges$parameters <- params

    cat("Graph Parameters:\n")
    cat("structurePrior = ", structurePrior,"\n")
    cat("samplePrior = ", samplePrior,"\n")
    cat("maxDegree = ", as.integer(maxDegree),"\n")
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
