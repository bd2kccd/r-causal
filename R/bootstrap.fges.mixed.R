bootstrap.fges.mixed <- function(df, numCategoriesToDiscretize = 4, penaltydiscount = 4.0,
    structurePrior = 1.0, maxDegree = 3, faithfulnessAssumed = TRUE, numBootstrapSamples = 10,
    ensembleMethod = 'Highest', verbose = FALSE, java.parameters = NULL, priorKnowledge = NULL){
    
    params <- list(NULL)
    
    if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params <- c(java.parameters = java.parameters)
    }

    # Data Frame to Tetrad Dataset
    data <- loadMixedData(df, numCategoriesToDiscretize)

    fges <- list()
    class(fges) <- "bootstrap.fges.mixed"

    fges$datasets <- deparse(substitute(df))

    cat("Datasets:\n")
    cat(deparse(substitute(df)),"\n\n")

    # Parameters
    parameters_instance <- .jnew("edu/cmu/tetrad/util/Parameters")
    
    obj_penaltydiscount <- .jnew("java/lang/Double", penaltydiscount)
    parameter_instance <- .jcast(obj_penaltydiscount, "java/lang/Object")
    .jcall(parameters_instance, "V", "set", "penaltyDiscount", parameter_instance)
    
    obj_structurePrior <- .jnew("java/lang/Double", structurePrior)
    parameter_instance <- .jcast(obj_structurePrior, "java/lang/Object")
    .jcall(parameters_instance, "V", "set", "structurePrior", obj_structurePrior)
    
    obj_maxDegree <- .jnew("java/lang/Integer", as.integer(maxDegree))
    parameter_instance <- .jcast(obj_maxDegree, "java/lang/Object")
    .jcall(parameters_instance, "V", "set", "maxDegree", parameter_instance)
    
    obj_faithfulnessAssumed <- .jnew("java/lang/Boolean", faithfulnessAssumed)
    parameter_instance <- .jcast(obj_faithfulnessAssumed, "java/lang/Object")
    .jcall(parameters_instance, "V", "set", "faithfulnessAssumed", parameter_instance)
    
    numPatternsToStore <- .jnew("java/lang/Integer", as.integer(0))
    parameter_instance <- .jcast(numPatternsToStore, "java/lang/Object")
    .jcall(parameters_instance, "V", "set", "numPatternsToStore", parameter_instance)
    
    obj_verbose <- .jnew("java/lang/Boolean", verbose)
    parameter_instance <- .jcast(obj_verbose, "java/lang/Object")
    .jcall(parameters_instance, "V", "set", "verbose", parameter_instance)

    # Initiate Bootstrapping FGES Mixed
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

    params <- c(params, numCategoriesToDiscretize = as.integer(numCategoriesToDiscretize))
    params <- c(params, penaltyDiscount = as.double(penaltydiscount))
    params <- c(params, structurePrior = as.double(structurePrior))
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
    cat("numCategoriesToDiscretize = ", as.integer(numCategoriesToDiscretize),"\n")
    cat("penalty discount = ", penaltydiscount,"\n")
    cat("structurePrior = ", structurePrior,"\n")
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
