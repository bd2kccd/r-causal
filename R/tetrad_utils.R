###############################################################################
ugraphToTetradGraph <- function(ugmat, node_list){
  numNodes <- ncol(ugmat)
  varnames <- strsplit(gsub("\\[|\\]", "", 
                node_list$toString()), 
                split=", ")[[1]]
  edgelist <- c()
  for (i in 2:numNodes){
    for (j in 1:(i-1)){
      if (ugmat[j,i]==1) edgelist <- c(edgelist, 
                                        paste(varnames[j], 
                                        "---", 
                                        varnames[i]))
    }
  }
  
  varstring <- paste(varnames, collapse=" ")
  edgestring <- paste(1:length(edgelist),". ", 
                edgelist, "\n",sep="", collapse="")
  graphstring <- paste("\nGraph Nodes:\n", varstring, 
                        " \n\nGraph Edges: \n", 
                        edgestring, "\n", sep="")
  
  graphfilename <- "impossibly_long_graph_file_name_temporary.txt"
  if ("!"(file.exists(graphfilename))){
    write(graphstring, graphfilename)
    graphfile <- .jnew("java/io/File", graphfilename)
    newug_tetrad <- .jcall("edu/cmu/tetrad/graph/GraphUtils", 
                           "Ledu/cmu/tetrad/graph/Graph;", 
                           "loadGraphTxt", graphfile)
    newug_tetrad <- .jcast(newug_tetrad, "edu/cmu/tetrad/graph/Graph", 
                            check=TRUE)
    rm(graphfile)
    file.remove(graphfilename)
    return(newug_tetrad)
  } else {
    print("Whoops, don't want to overwrite existing file!")
    stop()
  }
}

########################################################
# converter: R dataframe into Tetrad BDeuScore wrapping a BoxDataSet
# requires dataframe with named columns
# Dataset is discrete.
# requires rJava, assumes the JVM is running from the
# latest Tetrad jar.
dataFrame2TetradBDeuScore <- function(df,structurePrior = 1.0, 
    samplePrior = 1.0){
    boxData <- loadDiscreteData(df)
    score <- .jnew("edu/cmu/tetrad/search/BDeuScore", boxData)
    score$setStructurePrior(as.double(structurePrior))
    score$setSamplePrior(as.double(samplePrior))
    score <- .jcast(score, "edu/cmu/tetrad/search/Score")
    return(score)
}

########################################################
# converter: a list of R dataframes into Tetrad SemBicScoreImages
# requires a list of dataframes with named columns
# Dataset is continuous
# requires rJava, assumes the JVM is running from the
# latest Tetrad jar.
dataFrames2TetradSemBicScoreImages <- function(dfs,penaltydiscount = 4.0){
    datasets <- .jnew("java/util/ArrayList")
    for(i in 1:length(dfs)){
        df <- dfs[[i]]
        boxData <- loadContinuousData(df)
        dataModel <- .jcast(boxData, "edu/cmu/tetrad/data/DataModel")
        datasets$add(dataModel)
        
    }
    datasets <- .jcast(datasets, "java/util/List")
    score <- .jnew("edu/cmu/tetrad/search/SemBicScoreImages", datasets)
    score$setPenaltyDiscount(penaltydiscount)
    score <- .jcast(score, "edu/cmu/tetrad/search/Score")
    return(score)
}

########################################################
# converter: R dataframe into Tetrad SemBicScore
# requires dataframe with named columns
# Dataset is continuous
# requires rJava, assumes the JVM is running from the
# latest Tetrad jar.
dataFrame2TetradSemBicScore <- function(df,penaltydiscount = 4.0){
    boxData <- loadContinuousData(df)
    covMat <- .jnew("edu/cmu/tetrad/data/CovarianceMatrixOnTheFly", boxData)
    covMat <- .jcast(covMat, "edu/cmu/tetrad/data/ICovarianceMatrix")
    score <- .jnew("edu/cmu/tetrad/search/SemBicScore", covMat)
	score$setPenaltyDiscount(penaltydiscount)
    score <- .jcast(score, "edu/cmu/tetrad/search/Score")
    return(score)
}

########################################################
# converter: R dataframe into Tetrad ConditionalGaussianScore
# requires dataframe with named columns
# Dataset is mixed data
# requires rJava, assumes the JVM is running from the
# latest Tetrad jar.
dataFrame2TetradConditionalGaussianScore <- function(df,
    numCategoriesToDiscretize = 4, penaltydiscount = 4, structurePrior = 1.0){
    boxData <- loadMixedData(df, numCategoriesToDiscretize)
    score <- .jnew("edu/cmu/tetrad/search/ConditionalGaussianScore",
                boxData, structurePrior, TRUE)
    score$setPenaltyDiscount(penaltydiscount)
    score <- .jcast(score, "edu/cmu/tetrad/search/Score")
    return(score)
}

########################################################
# converter: R covariance matrix into Tetrad covariance matrix
rCovMatrix2TetradCovMatrix <- function(covmat, node_list, sample_size){
  mat <- .jarray(covmat, dispatch=TRUE)
  tetmat <- .jnew("edu/cmu/tetrad/util/TetradMatrix", mat)
  tetcovmat <- .jnew("edu/cmu/tetrad/data/CovarianceMatrix", node_list, 
                        tetmat, as.integer(sample_size))
  tetcovmat <- .jcast(tetcovmat, "edu/cmu/tetrad/data/ICovarianceMatrix", 
                      check=TRUE)
  return(tetcovmat)
}

########################################################
# converter: Tetrad edge type into graphNEL edge list
# requires list of nodes and a set of edges
# extract nodes: 
# tetradPattern2graphNEL <- function(resultGraph,
#    verbose = FALSE){
#
#	V <- extractTetradNodes(resultGraph)

#    if(verbose){
#        cat("\nGraph Nodes:\n")
#        for(i in 1:length(V)){
#            cat(V[i]," ")
#        }
#        cat("\n\n")
#    }
    
	# extract edges
#	fgs_edges <- extractTetradEdges(resultGraph)
#	edgemat <- str_split_fixed(fgs_edges,  pattern=" ", n=3)

#    if(verbose){
#        cat("Graph Edges:\n")
#        if(length(fgs_edges) > 0){
#            for(i in 1:length(fgs_edges)){
#                cat(fgs_edges[i],"\n")
#            }
#        }
#    }

	# find undirected edge indices
#	undir <- which(edgemat[,2]=="---")
	
	# for each undirected edge, create a new edge with the two variables 
	# in reversed order. Also, remove the edge column, but name the columns
#	edgemat <- rbind(edgemat[,c(1,3)], edgemat[undir,c(3,1)])
#	colnames(edgemat) <- c("Parent", "Child")
	
	# create edge list for graphNEL format
#	edgel <- list(NULL)
#	for (i in 1:length(V)){
#		edgel[[i]] <- edgemat[which(edgemat[,1]==V[i]),2]
#	}
#	names(edgel) <- V
	
#	outputgraph <- graphNEL(nodes=V, edgeL=edgel, edgemode="directed")
#	return(outputgraph)
#}

############################################################
# extract nodes from Tetrad graph result
extractTetradNodes <- function(resultGraph){
    nods <- resultGraph$getNodes()
	V <- sapply(as.list(nods), with, toString())
    return(V)
}

############################################################
# extract edges from Tetrad graph result
extractTetradEdges <- function(resultGraph){
    eds <- resultGraph$getEdges()
    fgs_edges <- c()
    if(!is.null(eds)){
	   fgs_edges <- sapply(as.list(eds), .jrcall, "toString")
    }
    return(fgs_edges)
}

############################################################
# Create an IKnowledge object from lists of prior knowledge
priorKnowledge <- function(forbiddirect = NULL, requiredirect = NULL, 
    addtemporal = NULL){
    prior <- .jnew("edu/cmu/tetrad/data/Knowledge2")
    
    # forbidden directed edges
    if(!is.null(forbiddirect)){
        cat("Forbidden Directed Edges: ", length(forbiddirect), "\n")
        for(i in 1:length(forbiddirect)){
            forbid <- forbiddirect[[i]]
            from <- forbid[1]
            to <- forbid[2]
            prior$setForbidden(from, to)
            cat("Between ", from, " And ", to, "\n")
        }
    }
    
    # required directed edges
    if(!is.null(requiredirect)){
        cat("Required Directed Edges: ", length(requiredirect), "\n")
        for(i in 1:length(requiredirect)){
            require <- requiredirect[[i]]
            from <- require[1]
            to <- require[2]
            prior$setRequired(from, to)
            cat("From ", from, " To ", to, "\n")
        }
    }
    
    # add temporal nodes' tiers
    if(!is.null(addtemporal)){
        cat("Temporal Tiers: ", length(addtemporal), "\n")
        for(i in 1:length(addtemporal)){
            tier = as.integer(i-1)
            cat("Tier: ", tier)
            temporal <- addtemporal[[i]]
            tempClass <- class(temporal)
            if(identical(all.equal(tempClass, "forbiddenWithin"), TRUE)){
                prior$setTierForbiddenWithin(tier, TRUE)
                cat("forbiddenWithin")
            }
            cat("\n")
            for(j in 1:length(temporal)){
                node <- temporal[j]
                node <- gsub(" ", ".", node)
                cat("temporal node: ", node, "\n")
                # name <- .jnew("java/lang/String", node)
                prior$addToTier(tier, node)
                # .jcall(prior, "V", "addToTier", tier, name)
            }
        }
    }
    
    prior <- .jcast(prior, "edu/cmu/tetrad/data/IKnowledge", 
                            check=TRUE)
    
    return(prior)
}

############################################################
# Create an IKnowledge object from the knowledge file
priorKnowledgeFromFile <- function(knowlegeFile){
    file <- .jnew("java/io/File", knowlegeFile)
    reader <- .jnew("edu/cmu/tetrad/data/DataReader")
    prior <- .jcall(reader, "Ledu/cmu/tetrad/data/IKnowledge;", "parseKnowledge", file)
    return(prior)
}

############################################################
loadContinuousData <- function(df){
	node_names <- colnames(df)
	node_list <- .jnew("java/util/ArrayList")
	for (i in 1:length(node_names)){
		nodname <- .jnew("java/lang/String", node_names[i])
		nodi <- .jnew("edu/cmu/tetrad/data/ContinuousVariable", nodname)
		node_list$add(nodi)
	}
	node_list <- .jcast(node_list, "java/util/List")
	mt <- as.matrix(df)
	mat <- .jarray(mt, dispatch=TRUE)
    
    data <- .jnew("edu/cmu/tetrad/data/DoubleDataBox", mat)
    data <- .jcast(data, "edu/cmu/tetrad/data/DataBox")
    boxData <- .jnew("edu/cmu/tetrad/data/BoxDataSet", 
                            data, node_list)
    boxData <- .jcast(boxData, "edu/cmu/tetrad/data/DataSet")
	return(boxData)
}

############################################################
loadDiscreteData <- function(df){
	node_names <- colnames(df)
	node_list <- .jnew("java/util/ArrayList")
	for (i in 1:length(node_names)){
		nodname <- .jnew("java/lang/String", node_names[i])
        cat("node_names: ", node_names[i],"\n")
        cate <- unique(df[[node_names[i]]])
        cate <- sort(cate)
        cat("value: ")
        print(cate)
        cat("\n")
        cate_list <- .jnew("java/util/ArrayList")
        for(j in 1:length(cate)){
            cate_list$add(as.character(cate[j]))
        }
        cate_list <- .jcast(cate_list, "java/util/List")
		nodi <- .jnew("edu/cmu/tetrad/data/DiscreteVariable", 
                    nodname, cate_list)
		node_list$add(nodi)
        
        # Substitute a new categorial value
        cate <- data.frame(cate)
        new_col <- sapply(df[,i],function(x,cate) 
                    as.integer(which(cate[,1] == x)),cate=cate)
        new_col = as.integer(new_col - 1)
        df[,i] <- (data.frame(new_col))[,1]
	}
	node_list <- .jcast(node_list, "java/util/List")
	mt <- as.matrix(df)
	mat <- .jarray(t(mt), dispatch=TRUE)
	data <- .jnew("edu/cmu/tetrad/data/VerticalIntDataBox", mat)
    data <- .jcast(data, "edu/cmu/tetrad/data/DataBox")
    boxData <- .jnew("edu/cmu/tetrad/data/BoxDataSet", 
                            data, node_list)
    boxData <- .jcast(boxData, "edu/cmu/tetrad/data/DataSet")
	return(boxData)
}

############################################################
loadMixedData <- function(df, numCategoriesToDiscretize = 4){
    node_names <- colnames(df)
    cont_list <- c()
    disc_list <- c()
    node_list <- .jnew("java/util/ArrayList")
    for (i in 1:length(node_names)){
        nodname <- .jnew("java/lang/String", node_names[i])
        cate <- unique(df[[node_names[i]]])
        if(length(cate) > numCategoriesToDiscretize){
            # Continuous variable
            nodi <- .jnew("edu/cmu/tetrad/data/ContinuousVariable", nodname)
            node_list$add(nodi)
            
            cont_list <- c(cont_list, i)
        }else{
            # Discrete variable
            cate <- sort(cate)
            cate_list <- .jnew("java/util/ArrayList")
            for(j in 1:length(cate)){
                cate_list$add(as.character(cate[j]))
            }
            cate_list <- .jcast(cate_list, "java/util/List")
            nodi <- .jnew("edu/cmu/tetrad/data/DiscreteVariable",
            nodname, cate_list)
            node_list$add(nodi)
            
            # Substitute a new categorial value
            cate <- data.frame(cate)
            new_col <- sapply(df[,i],function(x,cate)
            as.integer(which(cate[,1] == x)),cate=cate)
            new_col = as.integer(new_col - 1)
            df[,i] <- (data.frame(new_col))[,1]

            disc_list <- c(disc_list, i)
        }
    }
    
    node_list <- .jcast(node_list, "java/util/List")
    mixedDataBox <- .jnew("edu/cmu/tetrad/data/MixedDataBox", node_list,as.integer(nrow(df)))
    
    for(row in 1:nrow(df)){
        # print(paste("row:",row,sep=" "))
        if(length(cont_list) > 0){
            for(j in 1:length(cont_list)){
                col <- cont_list[j]
                # print(paste("col:",col,sep=" "))
                value <- as.character(df[row,col])
                #print(value)
                value <- .jnew("java/lang/Double", value)
                value <- .jcast(value, "java/lang/Number")
                mixedDataBox$set(as.integer(row-1),as.integer(col-1),value)
            }
        }
        if(length(disc_list) > 0){
            for(j in 1:length(disc_list)){
                col <- disc_list[j]
                # print(paste("col:",col,sep=" "))
                value <- as.character(df[row,col])
                # print(value)
                value <- .jnew("java/lang/Integer", value)
                value <- .jcast(value, "java/lang/Number")
                mixedDataBox$set(as.integer(row-1),as.integer(col-1),value)
            }
        }
    }
    
    data <- .jcast(mixedDataBox, "edu/cmu/tetrad/data/DataBox")
    boxData <- .jnew("edu/cmu/tetrad/data/BoxDataSet",
                data, node_list)
    boxData <- .jcast(boxData, "edu/cmu/tetrad/data/DataSet")
    return(boxData)
}
