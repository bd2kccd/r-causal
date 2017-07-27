images.sembic <- function(dfs, penaltydiscount = 4.0, maxDegree = 3,
    faithfulnessAssumed = TRUE, verbose = FALSE, java.parameters = NULL,
    priorKnowledge = NULL){
    
    params <- list()
    
    if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params <- c(java.parameters = java.parameters)
    }

    images <- list()
    class(images) <- "images.sembic"

    cat("Datasets:\n")
    datasets <- c()
    for(i in 1:length(dfs)){
        df <- dfs[[i]]
        datasets[i] <- deparse(substitute(df))
        cat(deparse(substitute(df)),"\n")
    }
    cat("\n")

    images$datasets <- datasets

    # Data Frame to Tetrad Dataset
    score <- dataFrames2TetradSemBicScoreImages(dfs,penaltydiscount)
    
    # Initiate ImaGES SemBic
    images_instance <- .jnew("edu/cmu/tetrad/search/Fges", score)
    .jcall(images_instance, "V", "setMaxDegree", as.integer(maxDegree))
    .jcall(images_instance, "V", "setNumPatternsToStore", as.integer(0))
    .jcall(images_instance, "V", "setFaithfulnessAssumed", faithfulnessAssumed)
    .jcall(images_instance, "V", "setVerbose", verbose)

    if(!is.null(priorKnowledge)){
        .jcall(images_instance, "V", "setKnowledge", priorKnowledge)
    }

    params <- c(params, penaltydiscount = as.double(penaltydiscount))
    params <- c(params, maxDegree = as.integer(maxDegree))
    params <- c(params, faithfulnessAssumed = as.logical(faithfulnessAssumed))
    params <- c(params, verbose = as.logical(verbose))

    if(!is.null(priorKnowledge)){
        params <- c(params, prior = priorKnowledge)
    }
    images$parameters <- params

    cat("Graph Parameters:\n")
    cat("penalty discount = ", penaltydiscount,"\n")
    cat("maxDegree = ", as.integer(maxDegree),"\n")
    cat("faithfulnessAssumed = ", faithfulnessAssumed,"\n")
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
        V <- extractTetradNodes(tetrad_graph)
        
        images$nodes <- V
        
        # extract edges
        images_edges <- extractTetradEdges(tetrad_graph)
        
        images$edges <- images_edges
    }
    
    return(images)
}
