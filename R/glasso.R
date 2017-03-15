glasso <- function(df, maxit = 10000, ia = FALSE, is = FALSE, itr = FALSE, ipen = FALSE, thr = .0001, 
    java.parameters = NULL){
    
    params <- list(NULL)
    
    if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params <- c(java.parameters = java.parameters)
    }

    tetradData <- loadContinuousData(df)
    tetradData <- dataset2DoubleMatrix2D(tetradData)
    
    glasso <- list()
    class(glasso) <- "glasso"

    glasso$datasets <- deparse(substitute(df))

    cat("Datasets:\n")
    cat(deparse(substitute(df)),"\n\n")

    # Initiate glasso
    glasso_instance <- .jnew("edu/cmu/tetrad/search/Glasso", tetradData)
    .jcall(glasso_instance, "V", "setMaxit", as.integer(maxit))
    .jcall(glasso_instance, "V", "setIa", ia)
    .jcall(glasso_instance, "V", "setIs", is)
    .jcall(glasso_instance, "V", "setItr", itr)
    .jcall(glasso_instance, "V", "setIpen", ipen)
    .jcall(glasso_instance, "V", "setThr", thr)
    .jcall(glasso_instance, "V", "setRhoAllEqual", 1.0)

    params <- c(params, maxit = as.integer(maxit))
    params <- c(params, ia = ia)
    params <- c(params, is = is)
    params <- c(params, itr = itr)
    params <- c(params, ipen = ipen)
    params <- c(params, thr = thr)

    glasso$parameters <- params

    cat("Graph Parameters:\n")
    cat("maxit = ", maxit,"\n")
    cat("ia = ", ia,"\n")
    cat("is = ", is,"\n")
    cat("itr = ", itr,"\n")
    cat("ipen = ", ipen,"\n")
    cat("thr = ", thr,"\n")

    # Search
    result <- .jcall(glasso_instance, "Ledu/cmu/tetrad/search/Glasso/Result;", 
        "search")
    result <- result$getWwi()
    result.array <- result$toArray()
    variables <- tetradData$getVariables()
    resultGraph <- .jnew("edu/cmu/tetrad/graph/EdgeListGraph", variables)
    size <- as.integer(variables$size())
    for (i in 1:size){
        for (j in (i+1):size){
            resultGraph$addUndirectedEdge(variables$get(as.integer(i)),variables$get(as.integer(j)))
        }
    }

    V <- extractTetradNodes(resultGraph)

    glasso$nodes <- V

    # extract edges
    glasso_edges <- extractTetradEdges(resultGraph)

    glasso$edges <- glasso_edges

    return(glasso)
}