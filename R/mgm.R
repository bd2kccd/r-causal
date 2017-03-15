mgm <- function(df, lambda = c(0,0,0), java.parameters = NULL){
    
    params <- list(NULL)
    
    if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params <- c(java.parameters = java.parameters)
    }

    tetradData <- loadContinuousData(df)
    lambda <- .jarray(lambda)
    
    mgm <- list()
    class(mgm) <- "mgm"

    mgm$datasets <- deparse(substitute(df))

    cat("Datasets:\n")
    cat(deparse(substitute(df)),"\n\n")

    # Initiate mgm
    mgm_instance <- .jnew("edu/pitt/csb/mgm/MGM", tetradData, lambda)

    params <- c(params, lambda = lambda)

    mgm$parameters <- params

    cat("Graph Parameters:\n")
    cat("lambda = ", lambda,"\n")

    # Search
    tetrad_graph <- .jcall(mgm_instance, "Ledu/cmu/tetrad/graph/Graph;", 
        "search")

    V <- extractTetradNodes(tetrad_graph)

    mgm$nodes <- V

    # extract edges
    mgm_edges <- extractTetradEdges(tetrad_graph)

    mgm$edges <- mgm_edges

    return(mgm)
}