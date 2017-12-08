ccd <- function(df, dataType = 0, numCategoriesToDiscretize = 4, depth = 3, alpha = 0.05,
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
    
    ccd <- list()
    class(ccd) <- "ccd"

    ccd$datasets <- deparse(substitute(df))

    cat("Datasets:\n")
    cat(deparse(substitute(df)),"\n\n")

	ccd_instance <- NULL

    if(numBootstrap < 1){
		indTest <- .jcast(indTest, "edu/cmu/tetrad/search/IndependenceTest")
		
	    # Initiate CCD
    	ccd_instance <- .jnew("edu/cmu/tetrad/search/Ccd", indTest)
    	.jcall(ccd_instance, "V", "setDepth", as.integer(depth))
    }else{
		indTest <- .jcast(indTest, "edu/cmu/tetrad/algcomparison/independence/IndependenceWrapper")
		
		algorithm <- .jnew("edu/cmu/tetrad/algcomparison/algorithm/oracle/pag/Ccd", indTest)
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
	
		# Initiate Bootstrapping CCD
		ccd_instance <- .jnew("edu/pitt/dbmi/algo/bootstrap/GeneralBootstrapTest", tetradData, algorithm, numBootstrap)
		edgeEnsemble <- .jfield("edu/pitt/dbmi/algo/bootstrap/BootstrapEdgeEnsemble", name=ensembleMethod)
		ccd_instance$setEdgeEnsemble(edgeEnsemble)
		ccd_instance$setParameters(parameters_instance)    
    }

	ccd_instance$setVerbose(verbose)

    if(!is.null(priorKnowledge)){
        .jcall(ccd_instance, "V", "setKnowledge", priorKnowledge)
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
    ccd$parameters <- params

    cat("Graph Parameters:\n")
    cat("dataType = ", as.integer(dataType),"\n")
    cat("depth = ", as.integer(depth),"\n")
    cat("alpha = ", as.numeric(alpha),"\n")
    if(numBootstrap > 0){
	    cat("numBootstrap = ", as.integer(numBootstrap),"\n")
    	cat("ensembleMethod = ", ensembleMethod,"\n")
    }
    cat("verbose = ", verbose,"\n")

    # Search
    tetrad_graph <- .jcall(ccd_instance, "Ledu/cmu/tetrad/graph/Graph;", 
        "search", check=FALSE)

    if(!is.null(e <- .jgetEx())){
        .jclear()
        ccd$nodes <- NULL
        ccd$edges <- NULL
        print("Java exception was raised")
        print(e)
    }else{
    	ccd$graph <- tetrad_graph
        
        V <- extractTetradNodes(tetrad_graph)
        
        ccd$nodes <- V
        
        # extract edges
        ccd_edges <- extractTetradEdges(tetrad_graph)
        
        ccd$edges <- ccd_edges
    }

    return(ccd)
}
