tsgfci <- function(df, penaltydiscount = 2.0, maxDegree = 3, maxPathLength = -1, significance = 0.05,
    completeRuleSetUsed = FALSE, faithfulnessAssumed = TRUE, verbose = FALSE, java.parameters = NULL, 
    priorKnowledge = NULL){
    
    params <- list(NULL)
    
    if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params <- c(java.parameters = java.parameters)
    }

    # Data Frame to Independence Test
    tetradData <- loadContinuousData(df)
    indTest <- .jnew("edu/cmu/tetrad/search/IndTestFisherZ", tetradData, significance)
    
	indTest <- .jcast(indTest, "edu/cmu/tetrad/search/IndependenceTest")

    # Data Frame to Tetrad Dataset
    score <- dataFrame2TetradSemBicScore(df,penaltydiscount)

    tsgfci <- list()
    class(tsgfci) <- "tsgfci"

    tsgfci$datasets <- deparse(substitute(df))

    cat("Datasets:\n")
    cat(deparse(substitute(df)),"\n\n")

    # Initiate TsGFci
    tsgfci_instance <- .jnew("edu/cmu/tetrad/search/TsGFci", indTest, score)
    .jcall(tsgfci_instance, "V", "setMaxIndegree", as.integer(maxDegree))
    .jcall(tsgfci_instance, "V", "setMaxPathLength", as.integer(maxPathLength))
    .jcall(tsgfci_instance, "V", "setCompleteRuleSetUsed", completeRuleSetUsed)
    .jcall(tsgfci_instance, "V", "setFaithfulnessAssumed", faithfulnessAssumed)
    .jcall(tsgfci_instance, "V", "setVerbose", verbose)

    if(!is.null(priorKnowledge)){
        .jcall(tsgfci_instance, "V", "setKnowledge", priorKnowledge)
    }

	params <- c(params, penaltydiscount = as.double(penaltydiscount))
    params <- c(params, maxDegree = as.integer(maxDegree))
    params <- c(params, maxPathLength = as.integer(maxPathLength))
    params <- c(params, significance = significance)
    params <- c(params, completeRuleSetUsed = as.logical(completeRuleSetUsed))
    params <- c(params, faithfulnessAssumed = as.logical(faithfulnessAssumed))
    params <- c(params, verbose = as.logical(verbose))

    if(!is.null(priorKnowledge)){
        params <- c(params, prior = priorKnowledge)
    }
    tsgfci$parameters <- params

    cat("Graph Parameters:\n")
    cat("penaltydiscount = ", penaltydiscount,"\n")
    cat("maxDegree = ", as.integer(maxDegree),"\n")
    cat("maxPathLength = ", as.integer(maxPathLength),"\n")
    cat("significance = ", as.numeric(significance),"\n")
    cat("completeRuleSetUsed = ", completeRuleSetUsed,"\n")
    cat("faithfulnessAssumed = ", faithfulnessAssumed,"\n")
    cat("verbose = ", verbose,"\n")
    
    # Search
    tetrad_graph <- .jcall(tsgfci_instance, "Ledu/cmu/tetrad/graph/Graph;", "search")

    V <- extractTetradNodes(tetrad_graph)

    tsgfci$nodes <- V

    # extract edges
    tsgfci_edges <- extractTetradEdges(tetrad_graph)

    tsgfci$edges <- tsgfci_edges

    return(tsgfci)
}