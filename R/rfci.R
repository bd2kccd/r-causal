rfci <- function(df, depth = 3, alpha = 0.05, completeRuleSetUsed = FALSE, 
	numBootstrap = -1, ensembleMethod = 'Highest', verbose = FALSE, java.parameters = NULL, 
    priorKnowledge = NULL){
    
    params <- list(NULL)
    
    if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params <- c(java.parameters = java.parameters)
    }

    rfci <- list()
    class(rfci) <- "rfci"

    rfci$datasets <- deparse(substitute(df))

    cat("Datasets:\n")
    cat(deparse(substitute(df)),"\n\n")

	rfci_instance <- NULL

	tetradData <- loadContinuousData(df)

	if(numBootstrap < 1){
	    # Data Frame to Independence Test
    	indTest <- .jnew("edu/cmu/tetrad/search/IndTestFisherZ", tetradData, alpha)
    
			indTest <- .jcast(indTest, "edu/cmu/tetrad/search/IndependenceTest")

	    # Initiate RFCI
	    rfci_instance <- .jnew("edu/cmu/tetrad/search/Rfci", indTest)
    	.jcall(rfci_instance, "V", "setDepth", as.integer(depth))
    	.jcall(rfci_instance, "V", "setCompleteRuleSetUsed", completeRuleSetUsed)
	}else{
		indTest <- .jnew("edu/cmu/tetrad/algcomparison/independence/FisherZ")
		indTest <- .jcast(indTest, "edu/cmu/tetrad/algcomparison/independence/IndependenceWrapper")
		
		algorithm <- .jnew("edu/cmu/tetrad/algcomparison/algorithm/oracle/pag/Rfci", indTest)
		algorithm <- .jcast(algorithm, "edu/cmu/tetrad/algcomparison/algorithm/Algorithm")
		
		# Parameters
    	parameters_instance <- .jnew("edu/cmu/tetrad/util/Parameters")
    
    	obj_depth <- .jnew("java/lang/Integer", as.integer(depth))
    	parameter_instance <- .jcast(obj_depth, "java/lang/Object")
    	parameters_instance$set("depth", parameter_instance)
    
    	obj_alpha <- .jnew("java/lang/Double", alpha)
    	parameter_instance <- .jcast(obj_alpha, "java/lang/Object")
    	parameters_instance$set("alpha", parameter_instance)
    	
    	obj_completeRuleSetUsed <- .jnew("java/lang/Boolean", completeRuleSetUsed)
    	parameter_instance <- .jcast(obj_completeRuleSetUsed, "java/lang/Object")
    	parameters_instance$set("completeRuleSetUsed", parameter_instance)
    
    	obj_verbose <- .jnew("java/lang/Boolean", verbose)
    	parameter_instance <- .jcast(obj_verbose, "java/lang/Object")
    	parameters_instance$set("verbose", parameter_instance)
	
		# Initiate Bootstrapping RFCI
		rfci_instance <- .jnew("edu/pitt/dbmi/algo/bootstrap/GeneralBootstrapTest", tetradData, algorithm, numBootstrap)
		edgeEnsemble <- .jfield("edu/pitt/dbmi/algo/bootstrap/BootstrapEdgeEnsemble", name=ensembleMethod)
		rfci_instance$setEdgeEnsemble(edgeEnsemble)
		rfci_instance$setParameters(parameters_instance)
	}
	
	rfci_instance$setVerbose(verbose)

    if(!is.null(priorKnowledge)){
        .jcall(rfci_instance, "V", "setKnowledge", priorKnowledge)
    }

    params <- c(params, depth = as.integer(depth))
    params <- c(params, alpha = alpha)
    params <- c(params, completeRuleSetUsed = as.logical(completeRuleSetUsed))
    if(numBootstrap > 0){
	    params <- c(params, numBootstrap = as.integer(numBootstrap))
    	params <- c(params, ensembleMethod = ensembleMethod)
    }
    params <- c(params, verbose = as.logical(verbose))

    if(!is.null(priorKnowledge)){
        params <- c(params, prior = priorKnowledge)
    }
    rfci$parameters <- params

    cat("Graph Parameters:\n")
    cat("depth = ", as.integer(depth),"\n")
    cat("alpha = ", as.numeric(alpha),"\n")
    cat("completeRuleSetUsed = ", completeRuleSetUsed, "\n")
    if(numBootstrap > 0){
	    cat("numBootstrap = ", as.integer(numBootstrap),"\n")
    	cat("ensembleMethod = ", ensembleMethod,"\n")
    }
    cat("verbose = ", verbose,"\n")

    # Search
    tetrad_graph <- .jcall(rfci_instance, "Ledu/cmu/tetrad/graph/Graph;", 
        "search", check=FALSE)

    if(!is.null(e <- .jgetEx())){
        .jclear()
        rfci$nodes <- NULL
        rfci$edges <- NULL
        print("Java exception was raised")
        print(e)
    }else{
        V <- extractTetradNodes(tetrad_graph)
        
        rfci$nodes <- V
        
        # extract edges
        rfci_edges <- extractTetradEdges(tetrad_graph)
        
        rfci$edges <- rfci_edges
    }

    return(rfci)
}
