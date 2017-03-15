lingam <- function(df, continuous = TRUE, pruneFactor = 1.0, java.parameters = NULL){
    
    params <- list(NULL)
    
    if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params <- c(java.parameters = java.parameters)
    }

    tetradData = NULL
    if(continuous){
    	tetradData <- loadContinuousData(df)
    }else{
    	tetradData <- loadDiscreteData(df)
    }
    
    lingam <- list()
    class(lingam) <- "lingam"

    lingam$datasets <- deparse(substitute(df))

    cat("Datasets:\n")
    cat(deparse(substitute(df)),"\n\n")

    # Initiate Lingam
    lingam_instance <- .jnew("edu/cmu/tetrad/search/Lingam")
    .jcall(lingam_instance, "V", "setPruneFactor", pruneFactor)

    params <- c(params, continuous = as.logical(continuous))
    params <- c(params, pruneFactor = pruneFactor)

    lingam$parameters <- params

    cat("Graph Parameters:\n")
    cat("continuous = ", continuous,"\n")
    cat("pruneFactor = ", pruneFactor,"\n")

    # Search
    tetrad_graph <- .jcall(lingam_instance, "Ledu/cmu/tetrad/graph/Graph;", 
        "search", tetradData)

    V <- extractTetradNodes(tetrad_graph)

    lingam$nodes <- V

    # extract edges
    lingam_edges <- extractTetradEdges(tetrad_graph)

    lingam$edges <- lingam_edges

    return(lingam)
}