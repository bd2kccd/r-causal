gfci.discrete <- function(df, structurePrior = 1.0, samplePrior = 1.0, 
    depth = -1, maxPathLength = -1, completeRuleSetUsed = FALSE, 
    faithfulness = TRUE, verbose = FALSE, java.parameters = NULL, 
    priorKnowledge = NULL){
    
    params <- list(NULL)
    
    if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params <- c(java.parameters = java.parameters)
    }

    # Data Frame to Tetrad Dataset
    score <- dataFrame2TetradBDeuScore(df, structurePrior, samplePrior)
    
    gfci <- list()
    class(gfci) <- "gfci.discrete"

    gfci$datasets <- deparse(substitute(df))

    cat("Datasets:\n")
    cat(deparse(substitute(df)),"\n\n")

    # Initiate GFCI Discrete
    gfci_instance <- .jnew("edu/cmu/tetrad/search/Gfci", score)
    .jcall(gfci_instance, "V", "setMaxIndegree", as.integer(depth))
    .jcall(gfci_instance, "V", "setMaxPathLength", as.integer(maxPathLength))
    .jcall(gfci_instance, "V", "setCompleteRuleSetUsed", completeRuleSetUsed)
    .jcall(gfci_instance, "V", "setFaithfulnessAssumed", faithfulness)
    .jcall(gfci_instance, "V", "setVerbose", verbose)

    if(!is.null(priorKnowledge)){
        .jcall(gfci_instance, "V", "setKnowledge", priorKnowledge)
    }

    params <- c(params, structurePrior = as.double(structurePrior))
    params <- c(params, samplePrior = as.double(samplePrior))
    params <- c(params, depth = as.integer(depth))
    params <- c(params, maxPathLength = as.integer(maxPathLength))
    params <- c(params, completeRuleSetUsed = as.logical(completeRuleSetUsed))
    params <- c(params, faithfulness = as.logical(faithfulness))
    params <- c(params, verbose = as.logical(verbose))

    if(!is.null(priorKnowledge)){
        params <- c(params, prior = priorKnowledge)
    }
    gfci$parameters <- params

    cat("Graph Parameters:\n")
    cat("structurePrior = ", structurePrior,"\n")
    cat("samplePrior = ", samplePrior,"\n")
    cat("depth = ", as.integer(depth),"\n")
    cat("maxPathLength = ", as.integer(maxPathLength),"\n")
    cat("significance = ", significance,"\n")
    cat("completeRuleSetUsed = ", completeRuleSetUsed,"\n")
    cat("faithfulness = ", faithfulness,"\n")
    cat("verbose = ", verbose,"\n")
    
    # Search
    tetrad_graph <- .jcall(gfci_instance, "Ledu/cmu/tetrad/graph/Graph;", 
        "search")

    V <- extractTetradNodes(tetrad_graph)

    gfci$nodes <- V

    # extract edges
    gfci_edges <- extractTetradEdges(tetrad_graph)

    gfci$edges <- gfci_edges

    return(gfci)
}