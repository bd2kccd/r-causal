imagesccd <- function(df, penaltydiscount = 2.0, depth = 3, maxPathLength = 3, significance = 0.05,
    applyOrientAwayFromCollider = FALSE, useHeuristic = TRUE, useOrientTowardDConnections = TRUE,
    orientVisibleFeedbackLoops = TRUE, doColliderOrientations = TRUE, verbose = FALSE, 
    java.parameters = NULL, priorKnowledge = NULL){
    
    params <- list(NULL)
    
    if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params <- c(java.parameters = java.parameters)
    }

    # Data Frame to Tetrad Dataset
    score <- dataFrames2TetradSemBicScoreImages(dfs,penaltydiscount)

    indTest <- .jnew("edu/cmu/tetrad/search/IndTestScore", score, significance)
	indTest <- .jcast(indTest, "edu/cmu/tetrad/search/IndependenceTest")

    imagesccd <- list()
    class(imagesccd) <- "imagesccd"

    imagesccd$datasets <- deparse(substitute(df))

    cat("Datasets:\n")
    cat(deparse(substitute(df)),"\n\n")

    # Initiate CcdMax
    imagesccd_instance <- .jnew("edu/cmu/tetrad/search/CcdMax", indTest)
    .jcall(imagesccd_instance, "V", "setDepth", as.integer(depth))
    .jcall(imagesccd_instance, "V", "setMaxPathLength", as.integer(maxPathLength))
    .jcall(imagesccd_instance, "V", "setApplyOrientAwayFromCollider", applyOrientAwayFromCollider)
    .jcall(imagesccd_instance, "V", "setUseHeuristic", useHeuristic)
    .jcall(imagesccd_instance, "V", "setUseOrientTowardDConnections", useOrientTowardDConnections)
    .jcall(imagesccd_instance, "V", "setOrientVisibleFeedbackLoops", orientVisibleFeedbackLoops)
    .jcall(imagesccd_instance, "V", "setDoColliderOrientations", doColliderOrientations)
    .jcall(imagesccd_instance, "V", "setVerbose", verbose)

    if(!is.null(priorKnowledge)){
        .jcall(imagesccd_instance, "V", "setKnowledge", priorKnowledge)
    }

    params <- c(params, continuous = as.logical(continuous))
    params <- c(params, depth = as.integer(depth))
    params <- c(params, maxPathLength = as.integer(maxPathLength))
    params <- c(params, applyOrientAwayFromCollider = as.logical(applyOrientAwayFromCollider))
    params <- c(params, significance = significance)
    params <- c(params, useHeuristic = as.logical(useHeuristic))
    params <- c(params, useOrientTowardDConnections = as.logical(useOrientTowardDConnections))
    params <- c(params, orientVisibleFeedbackLoops = as.logical(orientVisibleFeedbackLoops))
    params <- c(params, doColliderOrientations = as.logical(doColliderOrientations))
    params <- c(params, verbose = as.logical(verbose))

    if(!is.null(priorKnowledge)){
        params <- c(params, prior = priorKnowledge)
    }
    imagesccd$parameters <- params

    cat("Graph Parameters:\n")
    cat("continuous = ", continuous,"\n")
    cat("depth = ", as.integer(depth),"\n")
    cat("maxPathLength = ", as.integer(maxPathLength),"\n")
    cat("applyOrientAwayFromCollider = ", applyOrientAwayFromCollider,"\n")
    cat("significance = ", as.numeric(significance),"\n")
    cat("useHeuristic = ", useHeuristic,"\n")
    cat("useOrientTowardDConnections = ", useOrientTowardDConnections,"\n")
    cat("orientVisibleFeedbackLoops = ", orientVisibleFeedbackLoops,"\n")
    cat("doColliderOrientations = ", doColliderOrientations,"\n")
    cat("verbose = ", verbose,"\n")

    # Search
    tetrad_graph <- .jcall(imagesccd_instance, "Ledu/cmu/tetrad/graph/Graph;", 
        "search")

    V <- extractTetradNodes(tetrad_graph)

    imagesccd$nodes <- V

    # extract edges
    imagesccd_edges <- extractTetradEdges(tetrad_graph)

    imagesccd$edges <- imagesccd_edges

    return(imagesccd)
}