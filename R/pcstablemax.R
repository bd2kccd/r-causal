pcstablemax <- function(df, dataType = 0, numCategoriesToDiscretize = 4, depth = 3, maxPathLength = 3,
	useHeuristic = TRUE, alpha = 0.05,
    verbose = FALSE, java.parameters = NULL, priorKnowledge = NULL){
    
    params <- list(NULL)
    
    if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params <- c(java.parameters = java.parameters)
    }

    # Data Frame to Independence Test
    indTest <- NULL
    if(dataType == 0){
    	tetradData <- loadContinuousData(df)
    	if(numBootstrap < 1){
	    	indTest <- .jnew("edu/cmu/tetrad/search/IndTestFisherZ", tetradData, alpha)
    	}else{
    		indTest <- .jnew("edu/cmu/tetrad/algcomparison/independence/FisherZ")
    	}
    }else if(dataType == 1){
    	tetradData <- loadDiscreteData(df)
    	if(numBootstrap < 1){
    		indTest <- .jnew("edu/cmu/tetrad/search/IndTestChiSquare", tetradData, alpha)
    	}else{
    		indTest <- .jnew("edu/cmu/tetrad/algcomparison/independence/ChiSquare")
    	}
    }else{
    	tetradData <- loadMixedData(df, numCategoriesToDiscretize)
    	if(numBootstrap < 1){
    		indTest <- .jnew("edu/cmu/tetrad/search/IndTestConditionalGaussianLRT",
        	    tetradData, alpha)
    	}else{
    		indTest <- .jnew("edu/cmu/tetrad/algcomparison/independence/ConditionalGaussianLRT")
    	}
    }

    pcmax <- list()
    class(pcmax) <- "pcstablemax"

    pcmax$datasets <- deparse(substitute(df))

    cat("Datasets:\n")
    cat(deparse(substitute(df)),"\n\n")

	pcmax_instance <- NULL

	if(numBootstrap < 1){
		indTest <- .jcast(indTest, "edu/cmu/tetrad/search/IndependenceTest")
		
    	# Initiate PcMax
    	pcmax_instance <- .jnew("edu/cmu/tetrad/search/PcStableMax", indTest)
    	.jcall(pcmax_instance, "V", "setDepth", as.integer(depth))
    	.jcall(pcmax_instance, "V", "setMaxPathLength", as.integer(maxPathLength))
    	.jcall(pcmax_instance, "V", "setUseHeuristic", useHeuristic)
    }else{
    	indTest <- .jcast(indTest, "edu/cmu/tetrad/algcomparison/independence/IndependenceWrapper")
		
		algorithm <- .jnew("edu/cmu/tetrad/algcomparison/algorithm/oracle/pattern/PcStableMax", indTest)
		algorithm <- .jcast(algorithm, "edu/cmu/tetrad/algcomparison/algorithm/Algorithm")
		
		# Parameters
    	parameters_instance <- .jnew("edu/cmu/tetrad/util/Parameters")
    
    	obj_depth <- .jnew("java/lang/Integer", as.integer(depth))
    	parameter_instance <- .jcast(obj_depth, "java/lang/Object")
    	parameters_instance$set("depth", parameter_instance)
    
    	obj_maxPathLength <- .jnew("java/lang/Integer", as.integer(maxPathLength))
    	parameter_instance <- .jcast(obj_maxPathLength, "java/lang/Object")
    	parameters_instance$set("maxPathLength", parameter_instance)

		obj_useHeuristic <- .jnew("java/lang/Boolean", useHeuristic)
    	parameter_instance <- .jcast(obj_useHeuristic, "java/lang/Object")
    	parameters_instance$set("useHeuristic", parameter_instance)

    
    	obj_alpha <- .jnew("java/lang/Double", alpha)
    	parameter_instance <- .jcast(obj_alpha, "java/lang/Object")
    	parameters_instance$set("alpha", parameter_instance)
    	
    	obj_verbose <- .jnew("java/lang/Boolean", verbose)
    	parameter_instance <- .jcast(obj_verbose, "java/lang/Object")
    	parameters_instance$set("verbose", parameter_instance)
	
		# Initiate Bootstrapping PcMax
		pcmax_instance <- .jnew("edu/pitt/dbmi/algo/bootstrap/GeneralBootstrapTest", tetradData, algorithm, numBootstrap)
		edgeEnsemble <- .jfield("edu/pitt/dbmi/algo/bootstrap/BootstrapEdgeEnsemble", name=ensembleMethod)
		pcmax_instance$setEdgeEnsemble(edgeEnsemble)
		pcmax_instance$setParameters(parameters_instance)
    }
    
    pcmax_instance$setVerbose(verbose)

    if(!is.null(priorKnowledge)){
        .jcall(pcmax_instance, "V", "setKnowledge", priorKnowledge)
    }

	params <- c(params, dataType = as.integer(dataType))
    params <- c(params, depth = as.integer(depth))
    params <- c(params, maxPathLength = as.integer(maxPathLength))
    params <- c(params, useHeuristic = as.logical(useHeuristic))
    params <- c(params, alpha = alpha)
    if(numBootstrap > 0){
	    params <- c(params, numBootstrap = as.integer(numBootstrap))
    	params <- c(params, ensembleMethod = ensembleMethod)
    }
    params <- c(params, verbose = as.logical(verbose))

    if(!is.null(priorKnowledge)){
        params <- c(params, prior = priorKnowledge)
    }
    pcmax$parameters <- params

    cat("Graph Parameters:\n")
    cat("dataType = ", as.integer(dataType), "\n")
    cat("depth = ", as.integer(depth),"\n")
    cat("maxPathLength = ", as.integer(maxPathLength), "\n")
    cat("useHeuristic = ", useHeuristic, "\n")
    cat("alpha = ", alpha, "\n")
    if(numBootstrap > 0){
	    cat("numBootstrap = ", as.integer(numBootstrap),"\n")
    	cat("ensembleMethod = ", ensembleMethod,"\n")
    }
    cat("verbose = ", verbose, "\n")

    # Search
    tetrad_graph <- .jcall(pcmax_instance, "Ledu/cmu/tetrad/graph/Graph;", 
        "search", check=FALSE)

    if(!is.null(e <- .jgetEx())){
        .jclear()
        pcmax$nodes <- NULL
        pcmax$edges <- NULL
        print("Java exception was raised")
        print(e)
    }else{
        V <- extractTetradNodes(tetrad_graph)
        
        pcmax$nodes <- V
        
        # extract edges
        pcmax_edges <- extractTetradEdges(tetrad_graph)
        
        pcmax$edges <- pcmax_edges
    }
    
    return(pcmax)
}
