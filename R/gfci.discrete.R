gfci.discrete <- function(df, structurePrior = 1.0, samplePrior = 1.0, 
    maxDegree = 3, maxPathLength = -1, alpha = 0.05, completeRuleSetUsed = FALSE, 
    faithfulnessAssumed = TRUE, numBootstrap = -1, ensembleMethod = 'Highest', 
    verbose = FALSE, java.parameters = NULL, 
    priorKnowledge = NULL){
    
    params <- list(NULL)
    
    if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params <- c(java.parameters = java.parameters)
    }

    gfci <- list()
    class(gfci) <- "gfci.discrete"

    gfci$datasets <- deparse(substitute(df))

    cat("Datasets:\n")
    cat(deparse(substitute(df)),"\n\n")

	gfci_instance <- NULL
	
	if(numBootstrap < 1){
	    # Data Frame to Independence Test
    	tetradData <- loadDiscreteData(df)
		indTest <- .jnew("edu/cmu/tetrad/search/IndTestChiSquare", tetradData, alpha)
    
		indTest <- .jcast(indTest, "edu/cmu/tetrad/search/IndependenceTest")
	
    	# Data Frame to Tetrad Dataset
    	score <- dataFrame2TetradBDeuScore(df, structurePrior, samplePrior)
    
	    # Initiate GFCI Discrete
    	gfci_instance <- .jnew("edu/cmu/tetrad/search/GFci", indTest, score)
    	.jcall(gfci_instance, "V", "setMaxDegree", as.integer(maxDegree))
    	.jcall(gfci_instance, "V", "setMaxPathLength", as.integer(maxPathLength))
    	.jcall(gfci_instance, "V", "setCompleteRuleSetUsed", completeRuleSetUsed)
    	.jcall(gfci_instance, "V", "setFaithfulnessAssumed", faithfulnessAssumed)
	}else{
		tetradData <- loadDiscreteData(df)
		indTest <- .jnew("edu/cmu/tetrad/algcomparison/independence/ChiSquare")
		indTest <- .jcast(indTest, "edu/cmu/tetrad/algcomparison/independence/IndependenceWrapper")
		
		score <- .jnew("edu/cmu/tetrad/algcomparison/score/BdeuScore")
		score <- .jcast(score, "edu/cmu/tetrad/algcomparison/score/ScoreWrapper")
		
		algorithm <- .jnew("edu/cmu/tetrad/algcomparison/algorithm/oracle/pag/Gfci", indTest, score)
		algorithm <- .jcast(algorithm, "edu/cmu/tetrad/algcomparison/algorithm/Algorithm")
		
		# Parameters
    	parameters_instance <- .jnew("edu/cmu/tetrad/util/Parameters")
    
    	obj_structurePrior <- .jnew("java/lang/Double", structurePrior)
    	parameter_instance <- .jcast(obj_structurePrior, "java/lang/Object")
    	parameters_instance$set("structurePrior", parameter_instance)
    	
    	obj_samplePrior <- .jnew("java/lang/Double", samplePrior)
    	parameter_instance <- .jcast(obj_samplePrior, "java/lang/Object")
    	parameters_instance$set("samplePrior", parameter_instance)
    	
    	obj_maxDegree <- .jnew("java/lang/Integer", as.integer(maxDegree))
    	parameter_instance <- .jcast(obj_maxDegree, "java/lang/Object")
    	# .jcall(parameters_instance, "V", "set", "maxDegree", parameter_instance)
    	parameters_instance$set("maxDegree", parameter_instance)
    
    	obj_maxPathLength <- .jnew("java/lang/Integer", as.integer(maxPathLength))
    	parameter_instance <- .jcast(obj_maxPathLength, "java/lang/Object")
    	parameters_instance$set("maxPathLength", parameter_instance)
    	
    	obj_completeRuleSetUsed <- .jnew("java/lang/Boolean", completeRuleSetUsed)
    	parameter_instance <- .jcast(obj_completeRuleSetUsed, "java/lang/Object")
    	parameters_instance$set("completeRuleSetUsed", parameter_instance)
    
    	obj_faithfulnessAssumed <- .jnew("java/lang/Boolean", faithfulnessAssumed)
    	parameter_instance <- .jcast(obj_faithfulnessAssumed, "java/lang/Object")
    	parameters_instance$set("faithfulnessAssumed", parameter_instance)
    
    	obj_alpha <- .jnew("java/lang/Double", alpha)
    	parameter_instance <- .jcast(obj_alpha, "java/lang/Object")
    	parameters_instance$set("alpha", parameter_instance)
    	
    	obj_verbose <- .jnew("java/lang/Boolean", verbose)
    	parameter_instance <- .jcast(obj_verbose, "java/lang/Object")
    	parameters_instance$set("verbose", parameter_instance)
	
		# Initiate Bootstrapping GFCI
		gfci_instance <- .jnew("edu/pitt/dbmi/algo/bootstrap/GeneralBootstrapTest", tetradData, algorithm, numBootstrap)
		edgeEnsemble <- .jfield("edu/pitt/dbmi/algo/bootstrap/BootstrapEdgeEnsemble", name=ensembleMethod)
		gfci_instance$setEdgeEnsemble(edgeEnsemble)
		gfci_instance$setParameters(parameters_instance)
	}

	gfci_instance$setVerbose(verbose)

    if(!is.null(priorKnowledge)){
        .jcall(gfci_instance, "V", "setKnowledge", priorKnowledge)
    }

    params <- c(params, structurePrior = as.double(structurePrior))
    params <- c(params, samplePrior = as.double(samplePrior))
    params <- c(params, maxDegree = as.integer(maxDegree))
    params <- c(params, maxPathLength = as.integer(maxPathLength))
    params <- c(params, completeRuleSetUsed = as.logical(completeRuleSetUsed))
    params <- c(params, faithfulnessAssumed = as.logical(faithfulnessAssumed))
    if(numBootstrap > 0){
	    params <- c(params, numBootstrap = as.integer(numBootstrap))
    	params <- c(params, ensembleMethod = ensembleMethod)
    }
    params <- c(params, verbose = as.logical(verbose))

    if(!is.null(priorKnowledge)){
        params <- c(params, prior = priorKnowledge)
    }
    gfci$parameters <- params

    cat("Graph Parameters:\n")
    cat("structurePrior = ", structurePrior,"\n")
    cat("samplePrior = ", samplePrior,"\n")
    cat("maxDegree = ", as.integer(maxDegree),"\n")
    cat("maxPathLength = ", as.integer(maxPathLength),"\n")
    cat("alpha = ", as.numeric(alpha),"\n")
    cat("completeRuleSetUsed = ", completeRuleSetUsed,"\n")
    cat("faithfulnessAssumed = ", faithfulnessAssumed,"\n")
    if(numBootstrap > 0){
	    cat("numBootstrap = ", as.integer(numBootstrap),"\n")
    	cat("ensembleMethod = ", ensembleMethod,"\n")
    }
    cat("verbose = ", verbose,"\n")
    
    # Search
    tetrad_graph <- .jcall(gfci_instance, "Ledu/cmu/tetrad/graph/Graph;", "search", check=FALSE)

    if(!is.null(e <- .jgetEx())){
        .jclear()
        gfci$nodes <- colnames(df)
        gfci$edges <- NULL
        # print("Java exception was raised")
        # print(e)
    }else{
        V <- extractTetradNodes(tetrad_graph)
        
        gfci$nodes <- V
        
        # extract edges
        gfci_edges <- extractTetradEdges(tetrad_graph)
        
        gfci$edges <- gfci_edges
    }

    return(gfci)
}
