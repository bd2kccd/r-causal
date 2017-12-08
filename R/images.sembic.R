images.sembic <- function(dfs, penaltyDiscount = 4.0, maxDegree = 3,
    faithfulnessAssumed = TRUE, numBootstrap = -1, ensembleMethod = 'Highest', 
    verbose = FALSE, java.parameters = NULL, priorKnowledge = NULL){
    
    params <- list()
    
    if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params <- c(java.parameters = java.parameters)
    }

    images <- list()
    class(images) <- "images.sembic"

	images_instance <- NULL
    
	if(numBootstrap < 1){
	    # Data Frame to Tetrad Dataset
    	score <- dataFrames2TetradSemBicScoreImages(dfs,penaltyDiscount)
    
    	# Initiate ImaGES SemBic
    	images_instance <- .jnew("edu/cmu/tetrad/search/Fges", score)
    	.jcall(images_instance, "V", "setMaxDegree", as.integer(maxDegree))
    	.jcall(images_instance, "V", "setNumPatternsToStore", as.integer(0))
    	.jcall(images_instance, "V", "setFaithfulnessAssumed", faithfulnessAssumed)
	}else{
		datasets <- .jnew("java/util/ArrayList")
	    for(i in 1:length(dfs)){
    	    df <- dfs[[i]]
        	boxData <- loadContinuousData(df)
        	datasets$add(boxData)
    	}
		
		algorithm <- .jnew("edu/cmu/tetrad/algcomparison/algorithm/multi/ImagesBDeu")
		algorithm <- .jcast(algorithm, "edu/cmu/tetrad/algcomparison/algorithm/Algorithm")
	
	    # Parameters
    	parameters_instance <- .jnew("edu/cmu/tetrad/util/Parameters")
    
    	obj_penaltyDiscount <- .jnew("java/lang/Double", penaltyDiscount)
    	parameter_instance <- .jcast(obj_penaltyDiscount, "java/lang/Object")
    	parameters_instance$set("penaltyDiscount", parameter_instance)
    	
    	obj_maxDegree <- .jnew("java/lang/Integer", as.integer(maxDegree))
    	parameter_instance <- .jcast(obj_maxDegree, "java/lang/Object")
    	parameters_instance$set("maxDegree", parameter_instance)
    
    	obj_faithfulnessAssumed <- .jnew("java/lang/Boolean", faithfulnessAssumed)
    	parameter_instance <- .jcast(obj_faithfulnessAssumed, "java/lang/Object")
    	parameters_instance$set("faithfulnessAssumed", parameter_instance)
    
    	obj_verbose <- .jnew("java/lang/Boolean", verbose)
    	parameter_instance <- .jcast(obj_verbose, "java/lang/Object")
    	parameters_instance$set("verbose", parameter_instance)
    	
    	# Initiate Bootstrapping ImaGES SemBic
		images_instance <- .jnew("edu/pitt/dbmi/algo/bootstrap/GeneralBootstrapTest", datasets, algorithm, numBootstrap)
		edgeEnsemble <- .jfield("edu/pitt/dbmi/algo/bootstrap/BootstrapEdgeEnsemble", name=ensembleMethod)
		images_instance$setEdgeEnsemble(edgeEnsemble)
		images_instance$setParameters(parameters_instance)
	}

    .jcall(images_instance, "V", "setVerbose", verbose)

    if(!is.null(priorKnowledge)){
        .jcall(images_instance, "V", "setKnowledge", priorKnowledge)
    }

    params <- c(params, penaltyDiscount = penaltyDiscount)
    params <- c(params, maxDegree = as.integer(maxDegree))
    params <- c(params, faithfulnessAssumed = as.logical(faithfulnessAssumed))
    if(numBootstrap > 0){
	    params <- c(params, numBootstrap = as.integer(numBootstrap))
    	params <- c(params, ensembleMethod = ensembleMethod)
    }
    params <- c(params, verbose = as.logical(verbose))

    if(!is.null(priorKnowledge)){
        params <- c(params, prior = priorKnowledge)
    }
    images$parameters <- params

    cat("Graph Parameters:\n")
    cat("penaltyDiscount = ", penaltyDiscount,"\n")
    cat("maxDegree = ", as.integer(maxDegree),"\n")
    cat("faithfulnessAssumed = ", faithfulnessAssumed,"\n")
    if(numBootstrap > 0){
	    cat("numBootstrap = ", as.integer(numBootstrap),"\n")
    	cat("ensembleMethod = ", ensembleMethod,"\n")
    }
    cat("verbose = ", verbose,"\n")

    # Search
    tetrad_graph <- .jcall(images_instance, "Ledu/cmu/tetrad/graph/Graph;",
        "search", check=FALSE)

    if(!is.null(e <- .jgetEx())){
        .jclear()
        images$nodes <- colnames(df)
        images$edges <- NULL
        # print("Java exception was raised")
        # print(e)
    }else{
    	images$graph <- tetrad_graph
    	
        V <- extractTetradNodes(tetrad_graph)
        
        images$nodes <- V
        
        # extract edges
        images_edges <- extractTetradEdges(tetrad_graph)
        
        images$edges <- images_edges
    }
    
    return(images)
}
