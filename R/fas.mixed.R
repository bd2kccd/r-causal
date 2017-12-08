fas.mixed <- function(df, numCategoriesToDiscretize = 4, depth = 3, alpha = 0.05, 
	numBootstrap = -1, ensembleMethod = 'Highest', verbose = FALSE, java.parameters = NULL,
    priorKnowledge = NULL){
    
    params <- list(NULL)
    
    if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params <- c(java.parameters = java.parameters)
    }
    
    fas <- list()
    class(fas) <- "fas.mixed"
    
    fas$datasets <- deparse(substitute(df))
    
    cat("Datasets:\n")
    cat(deparse(substitute(df)),"\n\n")
  
    fas_instance <- NULL
    
    tetradData <- loadMixedData(df, numCategoriesToDiscretize)
    
	if(numBootstrap <= 0){
	    # Data Frame to Independence Test
    	indTest <- .jnew("edu/cmu/tetrad/search/IndTestConditionalGaussianLRT",
        	    tetradData, alpha)
    
    	indTest <- .jcast(indTest, "edu/cmu/tetrad/search/IndependenceTest")
    
		# Initiate FAS Mixed
    	fas_instance <- .jnew("edu/cmu/tetrad/search/Fas", indTest)
    	.jcall(fas_instance, "V", "setDepth", as.integer(depth))
	}else{
		indTest <- .jnew("edu/cmu/tetrad/algcomparison/independence/ConditionalGaussianLRT")
		indTest <- .jcast(indTest, "edu/cmu/tetrad/algcomparison/independence/IndependenceWrapper")
		
		algorithm <- .jnew("edu/cmu/tetrad/algcomparison/algorithm/oracle/pattern/FAS", indTest)
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
	
		# Initiate Bootstrapping FAS
		fas_instance <- .jnew("edu/pitt/dbmi/algo/bootstrap/GeneralBootstrapTest", tetradData, algorithm, numBootstrap)
		edgeEnsemble <- .jfield("edu/pitt/dbmi/algo/bootstrap/BootstrapEdgeEnsemble", name=ensembleMethod)
		fas_instance$setEdgeEnsemble(edgeEnsemble)
		fas_instance$setParameters(parameters_instance)
	}
	
	fas_instance$setVerbose(verbose)
	    
    if(!is.null(priorKnowledge)){
        .jcall(fas_instance, "V", "setKnowledge", priorKnowledge)
    }
    
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
    fas$parameters <- params
    
    cat("Graph Parameters:\n")
    cat("depth = ", as.integer(depth),"\n")
    cat("alpha = ", as.numeric(alpha),"\n")
    if(numBootstrap > 0){
	    cat("numBootstrap = ", as.integer(numBootstrap),"\n")
    	cat("ensembleMethod = ", ensembleMethod,"\n")
    }
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
    	fas$graph <- tetrad_graph
    	
        V <- extractTetradNodes(tetrad_graph)
        
        fas$nodes <- V
        
        # extract edges
        fas_edges <- extractTetradEdges(tetrad_graph)
        
        fas$edges <- fas_edges
    }
    
    return(fas)
}
