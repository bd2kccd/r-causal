

                                        
## Arguments:
## inputs = indicies of input variables.
## outputs=indicies of output variables.
## useGES = true if algorithm should use GES to determine input-output adjacencies, otherwise uses PC.
## data=the dataset containing variables and observations to run the search on.
## trueInputs=indicies of input variables that should be included in the search (can be used to run search over a subset of input variables).
## alphaPC=alpha value used by PC algorithm.
## alphaSober=alpha value used when preforming Sober's step.
## gesDiscount=Penalty used by the FGES algorithm.
## Returns an endogenous latent variable graph (graphNEl format).


dm <- function(inputs, outputs, useGES=TRUE, data, trueInputs, alphaPC=.05, alphaSober=.05, gesDiscount=10,
    verbose = FALSE, minDiscount=4, java.parameters = NULL){
    
    params <- list()
    params$java.parameters <- list()
    
    if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params$java.parameters <- java.parameters
    }

    dm <- list()
    class(dm) <- "DMSearch"

    dm$datasets <- deparse(substitute(df))

    ## Initiate DMSearch
    dm_instance <- .jnew("edu/cmu/tetrad/search/DMSearch")

    if(useGES==FALSE){

    ## Use PC for adj. search.
    .jcall(dm_instance, "V", "setInputs", .jarray(as.integer(inputs)))
    .jcall(dm_instance, "V", "setOutputs", .jarray(as.integer(outputs)))
    .jcall(dm_instance, "V", "setTrueInputs", .jarray(as.integer(trueInputs)))
    .jcall(dm_instance, "V", "setData", data)
    .jcall(dm_instance, "V", "setVerbose", verbose)
    .jcall(dm_instance, "V", "setAlphaPC", as.double(alphaPC))
    .jcall(dm_instance, "V", "setAlphaSober", as.double(alphaSober))
    }
    else{
    ## Use FGES for adj. search.
    .jcall(dm_instance, "V", "setInputs", .jarray(as.integer(inputs)))
    .jcall(dm_instance, "V", "setOutputs", .jarray(as.integer(outputs)))
    .jcall(dm_instance, "V", "setTrueInputs", .jarray(as.integer(trueInputs)))
    .jcall(dm_instance, "V", "setData", data)
    .jcall(dm_instance, "V", "setVerbose", verbose)
    .jcall(dm_instance, "V", "setAlphaSober", as.double(alphaSober))
    .jcall(dm_instance, "V", "setDiscount", as.double(gesDiscount))
    .jcall(dm_instance, "V", "setMinDiscount", as.integer(minDiscount))
    }
## TODO: Get alternative constructor working (might not work as isn't in compiled jar used for rcausal right now.
## dm_instance <-.jnew("edu/cmu/tetrad/search/DMSearch", .jarray(as.integer(inputs)), .jarray(as.integer(outputs)), useGES, data, .jarray(as.integer(trueInputs)), as.double(alphaPC), as.double(alphaSober), as.double(gesDiscount), verbose, as.integer(minDiscount))

    params$useGES <- useGES
    params$alphaPC <- alphaPC
    params$alphaSober <-alphaSober
    params$gesDiscount <- gesDiscount
    params$minDiscount <- minDiscount
    params$verbose <- verbose

    dm$parameters <- params

    ## Search
    dm_graph <- .jcall(dm_instance, "Ledu/cmu/tetrad/graph/Graph;", 
        "search")


    if(!is.null(e <- .jgetEx())){
        .jclear()
        dm$nodes <- colnames(df)
        dm$edges <- NULL
        print("Java exception was raised")
        print(e)
    }else{
        V <- extractTetradNodes(dm_graph)

        ## Get nodes.
        dm$nodes <- V
        
        ## extract edges
        dm_edges <- extractTetradEdges(dm_graph)
        
        dm$edges <- dm_edges
    }

    return(dm)
}


dm(inputs=c(0,1), outputs=c(2,3), data=loadContinuousData(data.frame(X0=rnorm(100), X1=rnorm(100), X2=rnorm(100), X3=rnorm(100))), trueInputs=c(0,1))






