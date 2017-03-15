fgesmb <- function(df, nodes, penaltydiscount = 4.0, maxDegree = 3, 
    aggressivelyPreventCycles = FALSE, faithfulnessAssumed = TRUE, 
    numOfThreads = 2, verbose = FALSE, java.parameters = NULL, priorKnowledge = NULL){
    
    params <- list(NULL)
    
    if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params <- c(java.parameters = java.parameters)
    }

    # Data Frame to Tetrad Dataset
    score <- dataFrame2TetradSemBicScore(df,penaltydiscount)

    # Nodes array to Tetrad Nodes
    tetrad_nodes <- .jnew("java/util/ArrayList")
    for(i in 1:length(nodes)){
        node <- nodes[i]
        node <- .jnew("edu/cmu/tetrad/graph/GraphNode", node)
        node <- .jcast(node, "edu/cmu/tetrad/graph/Node")
        tetrad_nodes$add(node)
    }
    tetrad_nodes <- .jcast(tetrad_nodes, "java/util/List")

    fgesmb <- list()
    class(fgesmb) <- "fgesmb"

    fgesmb$datasets <- deparse(substitute(df))

    cat("Datasets:\n")
    cat(deparse(substitute(df)),"\n\n")

    # Initiate fgesmb
    fgesmb_instance <- .jnew("edu/cmu/tetrad/search/FgesMb2", score)
    .jcall(fgesmb_instance, "V", "setMaxIndegree", as.integer(maxDegree))
    .jcall(fgesmb_instance, "V", "setAggressivelyPreventCycles", aggressivelyPreventCycles)
    .jcall(fgesmb_instance, "V", "setNumPatternsToStore", as.integer(0))
    .jcall(fgesmb_instance, "V", "setFaithfulnessAssumed", faithfulnessAssumed)
    .jcall(fgesmb_instance, "V", "setParallelism", as.integer(numOfThreads))
    .jcall(fgesmb_instance, "V", "setVerbose", verbose)

    if(!is.null(priorKnowledge)){
        .jcall(fgesmb_instance, "V", "setKnowledge", priorKnowledge)
    }

    params <- c(params, penaltydiscount = as.double(penaltydiscount))
    params <- c(params, maxDegree = as.integer(maxDegree))
    params <- c(params, aggressivelyPreventCycles = aggressivelyPreventCycles)
    params <- c(params, faithfulnessAssumed = as.logical(faithfulnessAssumed))
    params <- c(params, numOfThreads = as.integer(numOfThreads))
    params <- c(params, verbose = as.logical(verbose))

    if(!is.null(priorKnowledge)){
        params <- c(params, prior = priorKnowledge)
    }
    fgesmb$parameters <- params

    cat("Graph Parameters:\n")
    cat("penalty discount = ", penaltydiscount,"\n")
    cat("maxDegree = ", as.integer(maxDegree),"\n")
    cat("aggressivelyPreventCycles = ", aggressivelyPreventCycles,"\n")
    cat("faithfulnessAssumed = ", faithfulnessAssumed,"\n")
    cat("numOfThreads = ", as.integer(numOfThreads),"\n")
    cat("verbose = ", verbose,"\n")

    # Search
    tetrad_graph <- .jcall(fgesmb_instance, "Ledu/cmu/tetrad/graph/Graph;", 
        "search", tetrad_nodes)

    V <- extractTetradNodes(tetrad_graph)

    fgesmb$nodes <- V

    # extract edges
    fgesmb_edges <- extractTetradEdges(tetrad_graph)

    fgesmb$edges <- fgesmb_edges

    # convert output of FgesMb into an R object (graphNEL)
    fgesmb_graphNEL = tetradPattern2graphNEL(resultGraph = tetrad_graph,
        verbose = verbose)

    fgesmb$graphNEL <- fgesmb_graphNEL

    return(fgesmb)
}
