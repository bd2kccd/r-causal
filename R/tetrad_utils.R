################################################################################
ugraphToTetradGraph <- function(ugmat, node_list){
  numNodes <- ncol(ugmat)
  varnames <- strsplit(gsub("\\[|\\]", "", node_list$toString()), 
                                split=", ")[[1]]
  edgelist <- c()
  for (i in 2:numNodes){
    for (j in 1:(i-1)){
      if (ugmat[j,i]==1) edgelist <- c(edgelist, paste(varnames[j], "---", 
                                                       varnames[i]))
    }
  }
  
  varstring <- paste(varnames, collapse=" ")
  edgestring <- paste(1:length(edgelist),". ", edgelist, "\n",sep="", collapse="")
  graphstring <- paste("\nGraph Nodes:\n", varstring, " \n\nGraph Edges: \n", 
                       edgestring, "\n", sep="")
  
  graphfilename <- "impossibly_long_graph_file_name_temporary.txt"
  if ("!"(file.exists(graphfilename))){
    write(graphstring, graphfilename)
    graphfile <- .jnew("java/io/File", graphfilename)
    newug_tetrad <- .jcall("edu/cmu/tetrad/graph/GraphUtils", 
                           "Ledu/cmu/tetrad/graph/Graph;", 
                           "loadGraphTxt", graphfile)
    newug_tetrad <- .jcast(newug_tetrad, "edu/cmu/tetrad/graph/Graph", check=TRUE)
    rm(graphfile)
    file.remove(graphfilename)
    return(newug_tetrad)
  } else {
    print("Whoops, don't want to overwrite existing file!")
    stop()
  }
}

########################################################
# converter: R dataframe into Tetrad DataSet
# requires dataframe with named columns
# Assumes dataset is continuous - should make this a parameter
# requires rJava, assumes the JVM is running from the
# latest Tetrad jar.
dataFrame2TetradDataset <- function(df){
	node_names <- colnames(df)
	node_list <- .jnew("java/util/ArrayList")
	for (i in 1:length(node_names)){
		nodname <- .jnew("java/lang/String", node_names[i])
		nodi <- .jnew("edu/cmu/tetrad/graph/GraphNode", nodname)
		nodi <- .jcast(nodi, "edu/cmu/tetrad/graph/Node")
		node_list$add(nodi)
	}
	node_list <- .jcast(node_list, "java.util.List")
	mt <- as.matrix(df)
	mat <- .jarray(mt, dispatch=TRUE)
	tetradData <- .jnew("edu/cmu/tetrad/data/ColtDataSet", as.integer(nrow(df)), node_list)
	tetradData <- tetradData$makeContinuousData(node_list, mat)
    tetradData <- .jcall(tetradData, "Ledu/cmu/tetrad/data/DataSet;", "makeContinuousData", node_list, mat)
	tetradData <- .jcast(tetradData, "edu/cmu/tetrad/data/DataSet")
	return(tetradData)
}

########################################################
# converter: R covariance matrix into Tetrad covariance matrix
rCovMatrix2TetradCovMatrix <- function(covmat, node_list, sample_size){
  mat <- .jarray(covmat, dispatch=TRUE)
  tetmat <- .jnew("edu/cmu/tetrad/util/TetradMatrix", mat)
  tetcovmat <- .jnew("edu/cmu/tetrad/data/CovarianceMatrix", node_list, tetmat, as.integer(sample_size))
  tetcovmat <- .jcast(tetcovmat, "edu/cmu/tetrad/data/ICovarianceMatrix", 
                      check=TRUE)
  return(tetcovmat)
}

########################################################
# converter: Tetrad edge type into graphNEL edge list
# requires list of nodes and a set of edges

# extract nodes: 
tetradPattern2graphNEL <- function(resultGraph, verbose = FALSE){
	nods <- resultGraph$getNodes()
	V <- sapply(as.list(nods), with, toString())
	
    if(verbose){
        print("Graph Nodes:")
        for(i in 1:length(V)){
            print(V[i])
        }
    }
    
	# extract edges
	eds <- resultGraph$getEdges()
	fgs_edges <- sapply(as.list(eds), .jrcall, "toString")
	edgemat <- str_split_fixed(fgs_edges,  pattern=" ", n=3)

    if(verbose){
        print("Graph Edges:")
        for(i in 1:length(fgs_edges)){
            print(fgs_edges[i])
        }
        # print(fgs_edges)
    }

	# find undirected edge indices
	undir <- which(edgemat[,2]=="---")
	
	# for each undirected edge, create a new edge with the two variables 
	# in reversed order. Also, remove the edge column, but name the columns
	edgemat <- rbind(edgemat[,c(1,3)], edgemat[undir,c(3,1)])
	colnames(edgemat) <- c("Parent", "Child")
	
	# create edge list for graphNEL format
	edgel <- list()
	for (i in 1:length(V)){
		edgel[[i]] <- edgemat[which(edgemat[,1]==V[i]),2]
	}
	names(edgel) <- V
	
	outputgraph <- graphNEL(nodes=V, edgeL=edgel, edgemode="directed")
	return(outputgraph)
}

