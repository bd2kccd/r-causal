tetradrunner <- function(algoId, dfs,testId = NULL, scoreId = NULL, priorKnowledge = NULL, 
	dataType = 0, numCategoriesToDiscretize = 4,java.parameters = NULL,...) {
  
  arguments <- list(...)
  params <- list()
  # result
  tetradrunner <- list()
  
  if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params <- c(java.parameters = java.parameters)
  }
  
  # algoId
  # Initiate Algorithm Annotations
  algoAnno_instance <- .jnew("edu/cmu/tetrad/annotation/AlgorithmAnnotations")
  algoClasses <- algoAnno_instance$getInstance()$getAnnotatedClasses()
  
  algoClass <- NULL
  
  algos <- algoClasses$toArray()
  for(i in 1:algos$length){
  		algo <- algos[[i]]
  		if(algo$getAnnotation()$command() == algoId){
  			algoClass <- algo
  			break
  		}
  }
  
  if(is.null(algo_instance)){
  		return(tetradrunner)
  }

  # testId
  testClass <- NULL
  
  if(!is.null(testId)){
	  	testAnno_instance <- .jnew("edu/cmu/tetrad/annotation/TestOfIndependenceAnnotations")
  		testClasses <- testAnno_instance$getInstance()$getAnnotatedClasses()
  		tests <- testClasses$toArray()
  		
		for(i in 1:tests$length){
	  		test <- tests[[i]]
  			if(test$getAnnotation()$command() == testId){
  				testClass <- test
  				break
  			}
  		}  
  }
  
  # scoreId
  scoreClass <- NULL
  
  if(!is.null(scoreId)){
	  	scoreAnno_instance <- .jnew("edu/cmu/tetrad/annotation/ScoreAnnotations")
  		scoreClasses <- scoreAnno_instance$getInstance()$getAnnotatedClasses()
  		scores <- scoreClasses$toArray()
  		
		for(i in 1:scores$length){
	  		score <- scores[[i]]
  			if(score$getAnnotation()$command() == scoreId){
  				scoreClass <- score
  				break
  			}
  		}  
  }
  
  dataset <- NULL
  
  if(!is.list(dfs)){

  		if(dataType == 0){
  				dataset <- loadContinuousData(df)
  		}else if(dataType == 1){
  				dataset <- loadDiscreteData(df)
  		}else{
  				dataset <- loadMixedData(df, numCategoriesToDiscretize)
  		}
 	    
  }else{
  		
  		dataset <- .jnew("java/util/ArrayList")
	    for(i in 1:length(dfs)){
    	    df <- dfs[[i]]
    	    
    	    if(dataType == 0){
	  				tetradData <- loadContinuousData(df)
  			}else if(dataType == 1){
  					tetradData <- loadDiscreteData(df)
  			}else{
  					tetradData <- loadMixedData(df, numCategoriesToDiscretize)
  			}
    	    
        	dataset$add(tetradData)
    	}
  }
  
  algoFactory_instance <- .jnew("edu/cmu/tetrad/algcomparison/algorithm/AlgorithmFactory")
  algo_instance <- algoFactory_instance$create(algoClass, testClass, scoreClass)
  
  if(!is.null(priorKnowledge)){
        algo_instance$setKnowledge(priorKnowledge)
  }
  
  # Parameters
  paramDescs_instance <- .jnew("edu/cmu/tetrad/util/ParamDescriptions")
  paramDescs_instance <- paramDescs_instance$getInstance()
  
  parameters_instance <- .jnew("edu/cmu/tetrad/util/Parameters")
  for(arg in names(arguments)){
    	if(!is.null(paramDescs_instance$get(arg))){
    		
    		value <- arguments[arg]
    		parameter_instance <- NULL
    		obj_value <- NULL
    		
    		if(is.boolean(value)){
    				obj_value <- .jnew("java/lang/Boolean", value)
    		}else if(value%%1 == 0){
    				obj_value <- .jnew("java/lang/Integer", value)
    		}else{
    				obj_value <- .jnew("java/lang/Double", value)
    		}
	    	
    		parameter_instance <- .jcast(obj_value, "java/lang/Object")
	    	parameters_instance$set(arg, parameter_instance)
	    	
    	}
    # print(arg) # argument's name
    # print(arguments[arg]) # argument's value
  }
  
  # Search
  tetrad_graph <- .jcall(algo_instance, "Ledu/cmu/tetrad/graph/Graph;", 
        "search", dataset, parameters_instance, check=FALSE)

    if(!is.null(e <- .jgetEx())){
        .jclear()
        tetradrunner$nodes <- colnames(df)
        tetradrunner$edges <- NULL
        # print("Java exception was raised")
        # print(e)
    }else{
    	tetradrunner$graph <- tetrad_graph
    	
        V <- extractTetradNodes(tetrad_graph)
        
        tetradrunner$nodes <- V
        
        # extract edges
        tetradrunner_edges <- extractTetradEdges(tetrad_graph)
        
        tetradrunner$edges <- tetradrunners_edges
    }
    
    return(tetradrunner)
}
