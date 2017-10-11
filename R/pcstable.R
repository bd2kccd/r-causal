pcstable <- function(df, dataType = 0, numCategoriesToDiscretize = 4, depth = 3, alpha = 0.05, 
	numBootstrap = -1, ensembleMethod = 'Highest', verbose = FALSE, 
	java.parameters = NULL, priorKnowledge = NULL){
    
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

    pcstable <- list()
    class(pcstable) <- "pcstable"

    pcstable$datasets <- deparse(substitute(df))

    cat("Datasets:\n")
    cat(deparse(substitute(df)),"\n\n")

	pcstable_instance <- NULL

	if(numBootstrap < 1){
		indTest <- .jcast(indTest, "edu/cmu/tetrad/search/IndependenceTest")

    	# Initiate PC-Stable
    	pcstable_instance <- .jnew("edu/cmu/tetrad/search/PcStable", indTest)
    	.jcall(pcstable_instance, "V", "setDepth", as.integer(depth))
    }else{
    	indTest <- .jcast(indTest, "edu/cmu/tetrad/algcomparison/independence/IndependenceWrapper")
		
		algorithm <- .jnew("edu/cmu/tetrad/algcomparison/algorithm/oracle/pattern/PcStable", indTest)
		algorithm <- .jcast(algorithm, "edu/cmu/tetrad/algcomparison/algorithm/Algorithm")
		
		# Parameters
    	parameters_instance <- .jnew("edu/cmu/tetrad/util/Parameters")
    
    	obj_depth <- .jnew("java/lang/Integer", as.integer(depth))
    	parameter_instance <- .jcast(obj_depth, "java/lang/Object")
    	parameters_instance$set("depth", parameter_instance)
    
    	obj_alpha <- .jnew("java/lang/Double", alpha)
    	parameter_instance <- .jcast(obj_alpha, "java/lang/Object")
    	parameters_instance$set("alpha", parameter_instance)
    	
    	obj_verbose <- .jnew("java/lang/Boolean", verbose)
    	parameter_instance <- .jcast(obj_verbose, "java/lang/Object")
    	parameters_instance$set("verbose", parameter_instance)
	
		# Initiate Bootstrapping PC
		pcstable_instance <- .jnew("edu/pitt/dbmi/algo/bootstrap/GeneralBootstrapTest", tetradData, algorithm, numBootstrap)
		edgeEnsemble <- .jfield("edu/pitt/dbmi/algo/bootstrap/BootstrapEdgeEnsemble", name=ensembleMethod)
		pcstable_instance$setEdgeEnsemble(edgeEnsemble)
		pcstable_instance$setParameters(parameters_instance)
    }
    
    pcstable_instance$setVerbose(verbose)

    if(!is.null(priorKnowledge)){
        .jcall(pcstable_instance, "V", "setKnowledge", priorKnowledge)
    }

	params <- c(params, dataType = as.integer(dataType))
    params <- c(params, depth = as.integer(depth))
    params <- c(params, alpha = alpha)
    if(numBootstrap > 0){
	    params <- c(params, numBootstrap = as.integer(numBootstrap))
    	params <- c(params, ensembleMethod = ensembleMethod)
    }
    params <- c(params, verbose = as.logical(verbose))

    if(!is.null(priorKnowledge)){
        params <- c(params, prior = priorKnowledge)
    }
    pcstable$parameters <- params

    cat("Graph Parameters:\n")
    cat("dataType = ", as.integer(dataType), "\n")
    cat("depth = ", as.integer(depth),"\n")
    cat("alpha = ", alpha, "\n")
    if(numBootstrap > 0){
	    cat("numBootstrap = ", as.integer(numBootstrap),"\n")
    	cat("ensembleMethod = ", ensembleMethod,"\n")
    }
    cat("verbose = ", verbose, "\n")

    # Search
    tetrad_graph <- .jcall(pcstable_instance, "Ledu/cmu/tetrad/graph/Graph;", 
        "search", check=FALSE)

    if(!is.null(e <- .jgetEx())){
        .jclear()
        pcstable$nodes <- NULL
        pcstable$edges <- NULL
        print("Java exception was raised")
        print(e)
    }else{
        V <- extractTetradNodes(tetrad_graph)
        
        pcstable$nodes <- V
        
        # extract edges
        pcstable_edges <- extractTetradEdges(tetrad_graph)
        
        pcstable$edges <- pcstable_edges
    }
    
    return(pcstable)
}
