## Arguments: df=Dataset
## TestType = Either TETRAD_WISHART or TETRAD_DELTA
## fofcAlgorithm = Either GAP or SAG
## alpha = alpha value to be used in test.
## java.parameters = additional settings for java.
## Returns: latent variable clusters (as an edge list).
fofc <- function(df, TestType = "TETRAD_WISHART", fofcAlgorithm = "GAP", 
    alpha = .01, numBootstrap = -1, ensembleMethod = 'Highest', java.parameters = NULL){
     
    params <- list()
    params$java.parameters <- list()

    if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params <- c(java.parameters = java.parameters)
    }

    ## Converting to Tetrad dataset.
    df<-loadContinuousData(df)
    
	fofc <- list()
    class(fofc) <- "fofc"

    fofc$datasets <- deparse(substitute(data))

    params$TestType <- TestType
    params$fofcAlgorithm <- fofcAlgorithm
    params$alpha <- alpha

    fofc$parameters <- params

	fofc_instance <- NULL

	if(numBootstrap < 1){
	    fofcAlgorithmPath <- "edu/cmu/tetrad/search/FindOneFactorClusters"
    	TestTypePath <- "edu/cmu/tetrad/search/TestType"

    	## Instantiate fofc object.
    	fofc_instance <- .jnew("edu/cmu/tetrad/search/FindOneFactorClusters",
        	                   df, .jfield(TestTypePath,name=TestType),
            	               .jfield(paste(fofcAlgorithmPath, "$Algorithm", sep=""), name=fofcAlgorithm),
                	           as.double(alpha))
	
	}else{
		algorithm <- .jnew("edu/cmu/tetrad/algcomparison/algorithm/cluster/Fofc")
		algorithm <- .jcast(algorithm, "edu/cmu/tetrad/algcomparison/algorithm/Algorithm")
		
		# Parameters
    	parameters_instance <- .jnew("edu/cmu/tetrad/util/Parameters")
    	
    	useWishart = TRUE
    	if(TestType != 'TETRAD_WISHART'){
    		useWishart = FALSE;
    	}
    	obj_useWishart <- .jnew("java/lang/Boolean", useWishart)
    	parameter_instance <- .jcast(obj_useWishart, "java/lang/Object")
    	parameters_instance$set("useWishart", parameter_instance)
    
    	useGap = TRUE
    	if(TestType != 'GAP'){
    		useGap = FALSE;
    	}
    	obj_useGap <- .jnew("java/lang/Boolean", useGap)
    	parameter_instance <- .jcast(obj_useGap, "java/lang/Object")
    	parameters_instance$set("useGap", parameter_instance)

    	obj_alpha <- .jnew("java/lang/Double", alpha)
    	parameter_instance <- .jcast(obj_alpha, "java/lang/Object")
    	parameters_instance$set("alpha", parameter_instance)
    	
    	# Initiate Bootstrapping FOFC
		fofc_instance <- .jnew("edu/pitt/dbmi/algo/bootstrap/GeneralBootstrapTest", df, algorithm, numBootstrap)
		edgeEnsemble <- .jfield("edu/pitt/dbmi/algo/bootstrap/BootstrapEdgeEnsemble", name=ensembleMethod)
		fofc_instance$setEdgeEnsemble(edgeEnsemble)
		fofc_instance$setParameters(parameters_instance)
	}

    ## Search
    fofc_graph <- .jcall(fofc_instance, "Ledu/cmu/tetrad/graph/Graph;", "search")

    if(!is.null(e <- .jgetEx())){
        .jclear()
        fofc$nodes <- colnames(df)
        fofc$edges <- NULL
        print("Java exception was raised")
        print(e)
    }else{
        V <- extractTetradNodes(fofc_graph)

        ## Get nodes.
        fofc$nodes <- V
        
        ## extract edges
        fofc_edges <- extractTetradEdges(fofc_graph)
        
        fofc$edges <- fofc_edges
    }
    
    return(fofc)
}
