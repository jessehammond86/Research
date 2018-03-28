################################################################################
##
## Function: GouldBrokerage
##
## Purpose: calculates Gould's brokerage score for each node in a given network.
##
##
## Inputs:
##  - input_mat: adjacency matrix used for network calculations. The input matrix
##    must be symmetric and binary.
##
## Process:
##  1. Intake adjacency matrix and calculate all indirect length-2 paths.
##  2. Drop isolates from data set - they are not useful in identifying
##      communities, and for some algorithms (spinglass) they cause errors.
##  3. Convert adjacency matrix to graph structure.
##  4. Identify all communities in the resulting graph.
##  5. Implement Gould's algorithm for identifying, for each node j, the
##      proportion of all length-2 paths connecting all other nodes (i,k)
##      that pass through node j.
##
## Returns:
##  A length-N vector where element i corresponds to the brokerage centrality
##  of node i in the network.
##
################################################################################

GouldBrokerage <- function(
  giant_component
  , direction = 'outgoing'
  , bridge = 'positive'
  , cl = cl
  )
  {
  
  n_nodes <- vcount(giant_component)
  
  ## Because we want positive bridges, delete all negative intra-community ties
  this_component <- delete.edges(
    giant_component
    , which(
      E(giant_component)$weight < 0 & 
        E(giant_component)$sharedcomm == 1
      )
    )

  ## If we want negative bridges, delete all positive inter-community ties;
  ## Otherwise, if we want positive bridges, delete all negative IC ties.
  if (bridge == 'negative'){
    this_component <- delete.edges(
      this_component
      , which(
        E(this_component)$weight > 0 &
          E(this_component)$sharedcomm == 0
      )
    )
  } else if (bridge == 'positive'){
    this_component <- delete.edges(
      this_component
      , which(
        E(this_component)$weight < 0 &
          E(this_component)$sharedcomm == 0
      )
    )
  }
  
  ## Set all weights to 1.0. This is unfortunate as it throws away valuable
  ##  information about tie weights, but is necessary as we are calculating
  ##  brokerage centrality based on number of paths rather than path weight.
  E(this_component)$weight <- 1
  
  
  ## Keep only EDGES that connect communities
  crosscomm_graph <- delete.edges(
    this_component
    , which(E(this_component)$sharedcomm == 1)
  )
  
  ## Keep only VERTICES that have cross-community edges
  crosscomm_graph <- delete.vertices(
    crosscomm_graph
    , which(degree(crosscomm_graph) == 0)
  )
  
  ## Extract vector of cross-community-edge-having vertex IDs
  crosscomm_nodes <- which(V(giant_component)$name %in% V(crosscomm_graph)$name)
  
  ## Retrieve each node's community membership
  node_membership <- V(giant_component)$membership
  
  ## Create a cross-community matrix: 1 if dyad (i,j) are in *different*
  ##  communities, and 0 otherwise
  dif_com_mat <-  matrix(1, nrow = n_nodes, ncol = n_nodes)
  for(i in 1:n_nodes){
    dif_com_mat[i, ] <- 1 - (node_membership %in% node_membership[i])
  }
  
  ##########
  ##
  ## Sub-function: brokerage
  ##
  ## Purpose: calculate brokerage score for a single node in the network.
  ##
  ## Inputs:
  ##  - node_idx: index of node for which brokerage score will be calculated.
  ##
  ## Process:
  ##  1. Get length-1 egonet for node i (all length-1 ties).
  ##  2. Calculate all 'valid' length-2 paths between all nodes in i's egonet.
  ##  3. Calculate the share of all paths between node i's egonet partners
  ##      that *do not* share a direct tie.
  ##
  ##
  ##########
  
  brokerage <- function(
    node_idx
    , .direction = direction){
    
    ## Get all 1-path neighbors of the focal node.
    ## Since the network is directed, differentiate between incoming and
    ##  outgoing ties when examining neighborhoods.
    if(.direction == 'outgoing'){
      
      paths2 <- ego(this_component, 1, node_idx, mode = 'out')[[1]][-1]
      
    } else if (.direction == 'incoming'){
      
      paths2 <- ego(this_component, 1, node_idx, mode = 'in')[[1]][-1]
      
    }
    
    
    ## Create an origin/destination matrix of all node pairs sharing 
    ##  a length-2 connection. 
    paths_idx <- as.matrix(expand.grid(paths2, paths2))
    paths_idx <- paths_idx[which(dif_com_mat[paths_idx] == 1), ]
    paths_idx <- matrix(paths_idx, ncol = 2)
    
    ##########
    ##
    ## Sub-function: brokerage_ik
    ##
    ## Purpose: calculate brokerage score for a single pair (i,k) that are both
    ##  direct neighbors of node i, and not direct neighbors of one another.
    ##
    ## Inputs:
    ##  - pair_idx: index of dyad (i,k) for which brokerage score
    ##    will be calculated.
    ##
    ## Process:
    ##  1. Calculate all shortest paths between nodes i and k.
    ##  2. Keep all paths of length 2.
    ##  3. Calculate the share of i-k paths that pass through node j.
    ##
    ##
    ##########
    brokerage_ik <- function(pair_idx){
      
      ## Initialize output object
      brokerage <- 0
      
      ## Calculate all shortest i-k paths
      ties_dist <- igraph::all_shortest_paths(
        this_component
        , from = paths_idx[pair_idx, 1]
        , to = paths_idx[pair_idx, 2]
        )$res
    
      ## Keep all paths of length 3 (i-j-k)
      all2paths <- ties_dist[lapply(ties_dist, 'length') == 3]
      
      ## If there exists one or more length-2 paths and no direct path between
      ##  nodes i and k, calculate the share of paths crossing node j.
      if(length(all2paths) > 0){
        n_all2paths <- length(all2paths)
        bikpik <- 1 / n_all2paths
        brokerage <- brokerage + bikpik
      }
      
      return(brokerage)
    }

    ## Calculate brokerage score for a given node.
    if(nrow(paths_idx) > 0){
      brokerage_out <- sum(sapply(1:nrow(paths_idx), brokerage_ik))
      return(brokerage_out)
    } else{
      return(0)
    }
  }
  
  ## Calculate brokerage scores for all i-k dyads that are direct partners
  ##  of node i.
  brokerage_scores <- parSapply(
    cl
    , crosscomm_nodes
    , brokerage
    )
  
  
  ## Create variable with appropriate # of observations, including nodes with
  ##  zero cross-community ties
  brokerage_scores_out <- rep(0, n_nodes)
  brokerage_scores_out[crosscomm_nodes] <- brokerage_scores
  
  ## Return network object
  return(brokerage_scores_out)
}