randomDag <- function(seed = NULL, numNodes = 10, numEdges = 10, java.parameters = NULL){
    
    params <- list(NULL)
    
    if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params <- c(java.parameters = java.parameters)
    }

    if(!is.null(seed)){
    	random_util <- .jcall("edu/cmu/tetrad/util/RandomUtil", 
    		"Ledu/cmu/tetrad/util/RandomUtil;", "getInstance")
    	.jcall(random_util, "V", "setSeed", .jlong(seed))
    	params <- c(seed = as.integer(seed))
    }

    randomDag <- list()
    class(randomDag) <- "randomDag"

    dag = NULL
    initEdges = -1
    while(initEdges < numEdges){
    	graph <- .jcall(
    				"edu/cmu/tetrad/graph/GraphUtils", 
    				"Ledu/cmu/tetrad/graph/Graph;", 
    				"randomGraph", 
    				as.integer(numNodes),as.integer(0),as.integer(numEdges),
    				as.integer(30),as.integer(15),as.integer(15),FALSE)
    	dag <- .jnew("edu/cmu/tetrad/graph/Dag", graph)
    	initEdges <- .jcall(dag, "I", "getNumEdges")
    }

    params <- c(params, numNodes = as.integer(numNodes))
    params <- c(params, numEdges = as.integer(numEdges))

    randomDag$parameters <- params

    cat("Graph Parameters:\n")
    cat("seed = ", seed,"\n")
    cat("numNodes = ", as.integer(numNodes),"\n")
    cat("numEdges = ", as.integer(numEdges),"\n")

	randomDag$dag <- dag

    V <- extractTetradNodes(dag)

    randomDag$nodes <- V

    # extract edges
   	randomDag_edges <- extractTetradEdges(dag)

    randomDag$edges <- randomDag_edges
    randomDag$dag <- dag

	# convert output of randomDag into an R object (graphNEL)
    randomDag_graphNEL = tetradPattern2graphNEL(resultGraph = dag, verbose = TRUE)

    randomDag$graphNEL <- randomDag_graphNEL

    return(randomDag)
}