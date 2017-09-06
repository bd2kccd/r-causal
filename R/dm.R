

                                        
## Arguments:
## inputs = indices of input variables.
## outputs=indices of output variables.
## useGES = true if algorithm should use GES to determine input-output adjacencies, otherwise uses PC.
## data=the dataset containing variables and observations to run the search on.
## trueInputs=indices of input variables that should be included in the search (can be used to run search over a subset of input variables).
## alphaPC=alpha value used by PC algorithm.
## alphaSober=alpha value used when preforming Sober's step.
## gesDiscount=Penalty used by the FGES algorithm.
## Returns an endogenous latent variable graph (as an edge list).


dm <- function(inputs, outputs, useGES=TRUE, data, trueInputs, alphaPC=.05, alphaSober=.05, gesDiscount=10,
    verbose = FALSE, minDiscount=4, java.parameters = NULL){
    
    params <- list()
    params$java.parameters <- list()
    
    if(!is.null(java.parameters)){
        options(java.parameters = java.parameters)
        params$java.parameters <- java.parameters
    }

	orig.names <- names(data)
	new.names <-  paste("X", 0:(ncol(data)-1), sep="")

	## Need to rename variables as algorithm requires all variables be labeled: X0, X1, etc...
	names(data) <-new.names

	data <- loadContinuousData(data)
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

        ## Restoring original names to nodes.
        for(i in 1:length(new.names)){
            for(j in 1:length(orig.names)){
                if(i==j){
                    V <- gsub(V,
                              pattern=paste("^", new.names[j], "$", collapse="", sep=""),
                              replace=paste(orig.names[i], " ", collapse="", sep=""))
                }				
            }
        }      


        ## Get nodes.
        dm$nodes <- V
        
        ## extract edges
        dm_edges <- extractTetradEdges(dm_graph)

        ## Restoring original names to edges.
        for(i in 1:length(new.names)){
            for(j in 1:length(orig.names)){
                if(i==j){
                    dm_edges <- gsub(dm_edges,
                                     pattern=paste("^", new.names[j], " ", collapse="", sep=""),
                                     replace=paste(orig.names[i], " ", collapse="", sep=""))
                    dm_edges <- gsub(dm_edges,
                                     pattern=paste(" ", new.names[j], collapse="", sep=""),
                                     replace=paste(" ", orig.names[i], collapse="", sep=""))
                }				
            }
        }      
        dm$edges <- dm_edges
    }

    return(dm)
}

### Example.
##set.seed(123)
##a=rnorm(1000); b=rnorm(1000); c=rnorm(1000)+a+b; d=rnorm(1000)+a+b
##temp <- data.frame(a, b, c, d)

##dm(data=temp, inputs=c(0,1), outputs=c(2,3), useGES=TRUE, trueInputs=c(0,1))
##Should produce following output:

##$nodes
##[1] "L0" "a " "b " "c " "d "

##$edges
##[1] "a --> L0" "b --> L0" "L0 --> c" "L0 --> d"




