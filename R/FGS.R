FGS <- function(df, penaltydiscount = 4.0, depth = 3.0, faithfulness = TRUE, verbose = FALSE){
        
    # Data Frame to Tetrad Dataset
    tetradData <- dataFrame2TetradDataset(df)
    
    cat("Datasets:\n")
    cat(deparse(substitute(df)),"\n\n")
    
    # Initiate FGS
    fgs_instance <- .jnew("edu/cmu/tetrad/search/Fgs", tetradData)
    .jcall(fgs_instance, "V", "setPenaltyDiscount", penaltydiscount)
    .jcall(fgs_instance, "V", "setDepth", as.integer(depth))
    .jcall(fgs_instance, "V", "setNumPatternsToStore", as.integer(0))
    .jcall(fgs_instance, "V", "setFaithfulnessAssumed", faithfulness)
    .jcall(fgs_instance, "V", "setVerbose", verbose)
    
    cat("Graph Parameters:\n")
    cat("penalty discount = ", penaltydiscount,"\n")
    cat("depth = ", as.integer(depth),"\n")
    cat("faithfulness = ", faithfulness,"\n\n")
    
    # Search
    tetrad_graph <- .jcall(fgs_instance, "Ledu/cmu/tetrad/graph/Graph;", "search")
    
    # convert output of FGS into an R object (graphNEL)
    fgs_graphNEL = tetradPattern2graphNEL(tetrad_graph,verbose)
    
    return(fgs_graphNEL)
}