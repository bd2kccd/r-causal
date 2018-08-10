tetradrunner <- function(algoId, dataType, df = NULL, dfs = NULL, testId = NULL, scoreId = NULL, 
	priorKnowledge = NULL, numCategoriesToDiscretize = 4,java.parameters = NULL,...) {
  
  	arguments <- list(...)
  	
  	params <- list()
  	# result
  	tetradrunner <- list()
  
  	if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params <- c(java.parameters = java.parameters)
  	}
  
  	algoAnno_instance <- .jcall("edu/cmu/tetrad/annotation/AlgorithmAnnotations",
							"Ledu/cmu/tetrad/annotation/AlgorithmAnnotations;",
							"getInstance")
  	algoClasses <- algoAnno_instance$getAnnotatedClasses()
  
  	algoClass <- .jnull("java/lang/Class")
  	algoAnno <- NULL
  	
  	algoClasses <- algoClasses$toArray()
  	for(i in 1:algoClasses$length){
  		algo <- algoClasses[[i]]		
  		cmd <- algo$getAnnotation()$command()
  		 		
  		if(cmd == algoId){
  			algoClass <- algo$getClazz()
  			algoAnno <- algo$getAnnotation()
  			break
  		}
	}
	
	if(is.null(algoAnno)){
		cat(algoId,' is not found!\n')
		return
	}
	
	tetradProperties <- .jcall("edu/cmu/tetrad/util/TetradProperties",
							"Ledu/cmu/tetrad/util/TetradProperties;",
							"getInstance")
  
  	# testId
  	testClass <- .jnull("java/lang/Class")
	if(!is.null(testId) || algoAnno_instance$requireIndependenceTest(algoClass)){
		testAnno_instance <- .jcall("edu/cmu/tetrad/annotation/TestOfIndependenceAnnotations",
								"Ledu/cmu/tetrad/annotation/TestOfIndependenceAnnotations;",
								"getInstance")
  		testClasses <- testAnno_instance$getAnnotatedClasses()
  		testClasses <- testClasses$toArray()
  		
  		defaultTestClassName <- NULL
  		
  		# Default dataType
		continuous <- 'datatype.continuous.test.default'
		discrete <- 'datatype.discrete.test.default'
		mixed <- 'datatype.mixed.test.default'
	
		if(dataType == 'continuous'){
			defaultTestClassName <- tetradProperties$getValue(continuous)
		}else if(dataType == 'discrete'){
			defaultTestClassName <- tetradProperties$getValue(discrete)
		}else{
			defaultTestClassName <- tetradProperties$getValue(mixed)
		}	
		
		for(i in 1:testClasses$length){
			test <- testClasses[[i]]
	  		cmd <- test$getAnnotation()$command()
	  		tClass <- test$getClazz()
	  		name <- tClass$getName()
		  	
		  	if(name == defaultTestClassName){
		  		testClass <- tClass
		  	}
		  	
		  	if(!is.null(testId) && cmd == testId){
		  		testClass <- tClass
		  		break
		  	}
		}	
	}	
	
	# scoreId
	scoreClass <- .jnull("java/lang/Class")
	if(!is.null(scoreId) || algoAnno_instance$requireScore(algoClass)){
		scoreAnno_instance <- .jcall("edu/cmu/tetrad/annotation/ScoreAnnotations",
								"Ledu/cmu/tetrad/annotation/ScoreAnnotations;",
								"getInstance")
  		scoreClasses <- scoreAnno_instance$getAnnotatedClasses()
  		scoreClasses <- scoreClasses$toArray()
  		
  		defaultScoreClassName <- NULL
  		
  		# Default dataType
		continuous <- 'datatype.continuous.score.default'
		discrete <- 'datatype.discrete.score.default'
		mixed <- 'datatype.mixed.score.default'
	
		if(dataType == 'continuous'){
			defaultScoreClassName <- tetradProperties$getValue(continuous)
		}else if(dataType == 'discrete'){
			defaultScoreClassName <- tetradProperties$getValue(discrete)
		}else{
			defaultScoreClassName <- tetradProperties$getValue(mixed)
		}	
		
		for(i in 1:scoreClasses$length){
			score <- scoreClasses[[i]]
	  		cmd <- score$getAnnotation()$command()
	  		sClass <- score$getClazz()
	  		name <- sClass$getName()
	  		
	  		if(name == defaultScoreClassName){
		  		scoreClass <- sClass
		  	}
		  	
	  		if(!is.null(scoreId) && cmd == scoreId){
  				scoreClass <- sClass
  				break
  			}
		}	
	}

  	# dataset
  	tetradData <- NULL
  	if(!is.null(df)){

  		if(dataType == 'continuous'){
  				tetradData <- loadContinuousData(df)
  		}else if(dataType == 'discrete'){
  				tetradData <- loadDiscreteData(df)
  		}else{
  				tetradData <- loadMixedData(df, numCategoriesToDiscretize)
  		}
  		
  		tetradData <- .jcast(tetradData, 'edu/cmu/tetrad/data/DataModel')
 	    
  	}else if(!is.null(dfs)){
  		
  		tetradData <- .jnew("java/util/ArrayList")
	    for(i in 1:length(dfs)){
    	    df <- dfs[[i]]
    	    
    	    if(dataType == 'continuous'){
	  				df <- loadContinuousData(df)
  			}else if(dataType == 'discrete'){
  					df <- loadDiscreteData(df)
  			}else{
  					df <- loadMixedData(df, numCategoriesToDiscretize)
  			}
    	    
    	    df <- .jcast(df, 'edu/cmu/tetrad/data/DataModel')
    	    
        	tetradData$add(df)
    	}
    	
    	tetradData <- .jcast(tetradData, "java/util/List")
  	}else{
  		cat("Dataset is required!")
  		return
  	}
  
  	algo_instance <- .jcall("edu/cmu/tetrad/algcomparison/algorithm/AlgorithmFactory",
  							"Ledu/cmu/tetrad/algcomparison/algorithm/Algorithm;",
  							"create",algoClass, testClass, scoreClass)
  
  	if(!is.null(priorKnowledge)){
    	algo_instance$setKnowledge(priorKnowledge)
  	}
  
  # Parameters
  paramDescs_instance <- .jcall("edu/cmu/tetrad/util/ParamDescriptions",
  								"Ledu/cmu/tetrad/util/ParamDescriptions;",
  								"getInstance")
  
  parameters_instance <- .jnew("edu/cmu/tetrad/util/Parameters")
  for(arg in names(arguments)){
    	if(!is.null(paramDescs_instance$get(arg))){
    		
    		value <- arguments[[arg]]
    		parameter_instance <- NULL
    		obj_value <- NULL
    		
    		if(!is.character(value)){
    			if(is.logical(value)){
    					obj_value <- .jnew("java/lang/Boolean", value)
    			}else if(value%%1 == 0){
    					obj_value <- .jnew("java/lang/Integer", as.integer(value))
	    		}else{
	    				obj_value <- .jnew("java/lang/Double", value)
	    		}
	    	
	    		parameter_instance <- .jcast(obj_value, "java/lang/Object")
		    	parameters_instance$set(arg, parameter_instance)
    		}
    		
	    	
    	}
    # print(arg) # argument's name
    # print(arguments[arg]) # argument's value
  }
  
  # Search
  tetrad_graph <- .jcall(algo_instance, "Ledu/cmu/tetrad/graph/Graph;", 
        "search", tetradData, parameters_instance, check=FALSE)

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
        
        tetradrunner$edges <- tetradrunner_edges        
    }
    
    return(tetradrunner)
}

tetradrunner.tetradGraphToDot <- function(tetrad_graph){
	graph_dot <- .jcall("edu/cmu/tetrad/graph/GraphUtils",
							"S","graphToDot",tetrad_graph)
	return(graph_dot)
}

tetradrunner.listAlgorithms <- function(){
	algoAnno_instance <- .jcall("edu/cmu/tetrad/annotation/AlgorithmAnnotations",
							"Ledu/cmu/tetrad/annotation/AlgorithmAnnotations;",
							"getInstance")
  	algoClasses <- algoAnno_instance$getAnnotatedClasses()
  
  	algoClasses <- algoClasses$toArray()
  	for(i in 1:algoClasses$length){
  		algo <- algoClasses[[i]]		
  		algoType <- algo$getAnnotation()$algoType()$toString()
  		if(algoType != 'orient_pairwise'){
	  		cmd <- algo$getAnnotation()$command()
  			cat(cmd,"\n")
  		}
  	}
}

tetradrunner.listIndTests <- function(){
	testAnno_instance <- .jcall("edu/cmu/tetrad/annotation/TestOfIndependenceAnnotations",
							"Ledu/cmu/tetrad/annotation/TestOfIndependenceAnnotations;",
							"getInstance")
  	testClasses <- testAnno_instance$getAnnotatedClasses()
  	testClasses <- testClasses$toArray()
  		
	for(i in 1:testClasses$length){
		test <- testClasses[[i]]
	  	cmd <- test$getAnnotation()$command()
	  	cat(cmd,"\n")
	}
}

tetradrunner.listScores <- function(){
	scoreAnno_instance <- .jcall("edu/cmu/tetrad/annotation/ScoreAnnotations",
							"Ledu/cmu/tetrad/annotation/ScoreAnnotations;",
							"getInstance")
  	scoreClasses <- scoreAnno_instance$getAnnotatedClasses()
  	scoreClasses <- scoreClasses$toArray()
  		
	for(i in 1:scoreClasses$length){
		score <- scoreClasses[[i]]
	  	cmd <- score$getAnnotation()$command()
	  	cat(cmd,"\n")
	}
}

tetradrunner.getAlgorithmDescription <- function(algoId){
	algoAnno_instance <- .jcall("edu/cmu/tetrad/annotation/AlgorithmAnnotations",
							"Ledu/cmu/tetrad/annotation/AlgorithmAnnotations;",
							"getInstance")
  	algoClasses <- algoAnno_instance$getAnnotatedClasses()
  
  	algoClass <- NULL
  	algoAnno <- NULL
  	
  	algoClasses <- algoClasses$toArray()
  	for(i in 1:algoClasses$length){
  		algo <- algoClasses[[i]]		
  		cmd <- algo$getAnnotation()$command()
  		
  		
  		if(cmd == algoId){
  			algoClass <- algo$getClazz()
  			algoAnno <- algo$getAnnotation()
  			break
  		}
	}
	
	algoDesc_instance <- .jcall("edu/cmu/tetrad/util/AlgorithmDescriptions",
							"Ledu/cmu/tetrad/util/AlgorithmDescriptions;",
							"getInstance")
	
	cat(algoAnno$name(), " : ", algoDesc_instance$get(algoId))
	
	if(algoAnno_instance$requireIndependenceTest(algoClass)){
		cat("\nIt requires the independence test.")
	}
	if(algoAnno_instance$requireScore(algoClass)){
		cat("\nIt requires the score.")
	}
	if(algoAnno_instance$acceptKnowledge(algoClass)){
		cat("\nIt accepts the prior knowledge.")
	}
}

tetradrunner.getAlgorithmParameters <- function(algoId, testId = NULL, scoreId = NULL){
	algoAnno_instance <- .jcall("edu/cmu/tetrad/annotation/AlgorithmAnnotations",
							"Ledu/cmu/tetrad/annotation/AlgorithmAnnotations;",
							"getInstance")
  	algoClasses <- algoAnno_instance$getAnnotatedClasses()
  
  	algoClass <- .jnull("java/lang/Class")
  	
  	algoClasses <- algoClasses$toArray()
  	for(i in 1:algoClasses$length){
  		algo <- algoClasses[[i]]		
  		cmd <- algo$getAnnotation()$command()
  		 		
  		if(cmd == algoId){
  			algoClass <- algo$getClazz()
  			break
  		}
	}
	
	# testId
	testClass <- .jnull("java/lang/Class")
	
	if(!is.null(testId)){
		testAnno_instance <- .jcall("edu/cmu/tetrad/annotation/TestOfIndependenceAnnotations",
								"Ledu/cmu/tetrad/annotation/TestOfIndependenceAnnotations;",
								"getInstance")
  		testClasses <- testAnno_instance$getAnnotatedClasses()
  		testClasses <- testClasses$toArray()
  		
		for(i in 1:testClasses$length){
			test <- testClasses[[i]]
	  		cmd <- test$getAnnotation()$command()
		  	
		  	if(cmd == testId){
		  		testClass <- test$getClazz()
		  		break
		  	}
		}	
	}

	# scoreId
	scoreClass <- .jnull("java/lang/Class")
	
	if(!is.null(scoreId)){
		scoreAnno_instance <- .jcall("edu/cmu/tetrad/annotation/ScoreAnnotations",
								"Ledu/cmu/tetrad/annotation/ScoreAnnotations;",
								"getInstance")
  		scoreClasses <- scoreAnno_instance$getAnnotatedClasses()
  		scoreClasses <- scoreClasses$toArray()
  		
		for(i in 1:scoreClasses$length){
			score <- scoreClasses[[i]]
		  	cmd <- score$getAnnotation()$command()
	  	
		  	if(cmd == scoreId){
	  			scoreClass <- score$getClazz()
	  			break
	  		}
		}
	}
	
	algo_instance <- .jcall("edu/cmu/tetrad/algcomparison/algorithm/AlgorithmFactory",
  							"Ledu/cmu/tetrad/algcomparison/algorithm/Algorithm;",
  							"create",algoClass, testClass, scoreClass)
	
	algoParams <- algo_instance$getParameters()
		
	paramDescs_instance <- .jcall("edu/cmu/tetrad/util/ParamDescriptions",
  								"Ledu/cmu/tetrad/util/ParamDescriptions;",
  								"getInstance")
	for(i in 0:(algoParams$size()-1)){
		algoParam <- algoParams$get(i)
		paramDesc <- paramDescs_instance$get(algoParam)
		defaultValue <- paramDesc$getDefaultValue()
		desc <- paramDesc$getDescription()
		
		cat(algoParam,": ",desc," [default:",defaultValue,"]","\n")
	}
	
}