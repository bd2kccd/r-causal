fges.mixed <- function(df, numCategoriesToDiscretize = 4, penaltyDiscount = 4,
    structurePrior = 1.0, maxDegree = 3, faithfulnessAssumed = TRUE, numBootstrap = -1, 
    ensembleMethod = 'Highest', verbose = FALSE, java.parameters = NULL, priorKnowledge = NULL){
    
    params <- list()
    
    if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params <- c(java.parameters = java.parameters)
    }

    fges <- list()
    class(fges) <- "fges.mixed"

    fges$datasets <- deparse(substitute(df))

    cat("Datasets:\n")
    cat(deparse(substitute(df)),"\n\n")

	fges_instance <- NULL
    
	if(numBootstrap < 1){
	    # Data Frame to Tetrad Dataset
    	score <- dataFrame2TetradConditionalGaussianScore(df,
        	        numCategoriesToDiscretize, penaltyDiscount, structurePrior)

    	# Initiate FGES Mixed
    	fges_instance <- .jnew("edu/cmu/tetrad/search/Fges", score)
    	.jcall(fges_instance, "V", "setMaxDegree", as.integer(maxDegree))
    	.jcall(fges_instance, "V", "setNumPatternsToStore", as.integer(0))
    	.jcall(fges_instance, "V", "setFaithfulnessAssumed", faithfulnessAssumed)
	}else{
		tetradData <- loadMixedData(df, numCategoriesToDiscretize)
		
		score <- .jnew("edu/cmu/tetrad/algcomparison/score/ConditionalGaussianBicScore")
		score <- .jcast(score, "edu/cmu/tetrad/algcomparison/score/ScoreWrapper")
		
		algorithm <- .jnew("edu/cmu/tetrad/algcomparison/algorithm/oracle/pattern/Fges", score)
		algorithm <- .jcast(algorithm, "edu/cmu/tetrad/algcomparison/algorithm/Algorithm")
	
	    # Parameters
    	parameters_instance <- .jnew("edu/cmu/tetrad/util/Parameters")
    
    	obj_penaltyDiscount <- .jnew("java/lang/Double", penaltyDiscount)
    	parameter_instance <- .jcast(obj_penaltyDiscount, "java/lang/Object")
    	parameters_instance$set("penaltyDiscount", parameter_instance)
    	
    	obj_structurePrior <- .jnew("java/lang/Double", structurePrior)
    	parameter_instance <- .jcast(obj_structurePrior, "java/lang/Object")
    	parameters_instance$set("structurePrior", parameter_instance)
    	
    	obj_maxDegree <- .jnew("java/lang/Integer", as.integer(maxDegree))
    	parameter_instance <- .jcast(obj_maxDegree, "java/lang/Object")
    	parameters_instance$set("maxDegree", parameter_instance)
    
    	obj_faithfulnessAssumed <- .jnew("java/lang/Boolean", faithfulnessAssumed)
    	parameter_instance <- .jcast(obj_faithfulnessAssumed, "java/lang/Object")
    	parameters_instance$set("faithfulnessAssumed", parameter_instance)
    
    	obj_verbose <- .jnew("java/lang/Boolean", verbose)
    	parameter_instance <- .jcast(obj_verbose, "java/lang/Object")
    	parameters_instance$set("verbose", parameter_instance)
    	
    	# Initiate Bootstrapping FGES
		fges_instance <- .jnew("edu/pitt/dbmi/algo/bootstrap/GeneralBootstrapTest", tetradData, algorithm, numBootstrap)
		edgeEnsemble <- .jfield("edu/pitt/dbmi/algo/bootstrap/BootstrapEdgeEnsemble", name=ensembleMethod)
		fges_instance$setEdgeEnsemble(edgeEnsemble)
		fges_instance$setParameters(parameters_instance)		
	}
	
	fges_instance$setVerbose(verbose)
	
    if(!is.null(priorKnowledge)){
        .jcall(fges_instance, "V", "setKnowledge", priorKnowledge)
    }

    params <- c(params, penaltyDiscount = as.double(penaltyDiscount))
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
    fges$parameters <- params

    cat("Graph Parameters:\n")
    cat("numCategoriesToDiscretize = ", as.integer(numCategoriesToDiscretize),"\n")
    cat("penaltyDiscount = ", penaltyDiscount,"\n")
    cat("structurePrior = ", structurePrior,"\n")
    cat("maxDegree = ", as.integer(maxDegree),"\n")
    cat("faithfulnessAssumed = ", faithfulnessAssumed,"\n")
    if(numBootstrap > 0){
	    cat("numBootstrap = ", as.integer(numBootstrap),"\n")
    	cat("ensembleMethod = ", ensembleMethod,"\n")
    }
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
    	fges$graph <- tetrad_graph
    	
        V <- extractTetradNodes(tetrad_graph)
        
        fges$nodes <- V
        
        # extract edges
        fges_edges <- extractTetradEdges(tetrad_graph)
        
        fges$edges <- fges_edges
    }

    return(fges)
}
