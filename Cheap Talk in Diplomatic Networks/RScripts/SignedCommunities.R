################################################################################
##
## Function: SignedCommunities.R
##
## Purpose: Calculates communities using Yang et al's (2007) algorithm for
##          partitioning signed/weighted networks, with a few minor changes
##          for improved efficiency.
##
## Functions:
##  - fec_sim: FEC function for identifying communities given a network
##      adjacency matrix.
##
## Inputs:
##  - in_matrix: adjacency matrix used for network calculations. The input
##      matrix can be undirected or directed, and weighted or unweighted.
##  - l: the number of steps for the random walk. Higher values of l will
##    lead to a smaller number of communities, while low values of l will force
##    a smaller number of communities as some set of nodes are likely to be
##    always out of reach of a given starting node.
##
## Process:
##  1. Intake adjacency matrix and extract only positive signed values. These
##      will be used in the initial community detection, and negative-signed
##      ties will be used to identify the cut points that minimize error.
##  2. Calculate P_ilt: the probability that a random walk starting from node
##      (i) will reach "sink node" (t) in (l) steps *or fewer*.
##  3. Sort pos-adjacency matrix in order of highest to lowest P_ilt by node.
##  4. Identify node at which to partition the sorted matrix by minimizing the
##      proportion of in-community negative ties and cross-community positive
##      ties for a given cut point.
##  5. Partition the graph at the optimal cut point.
##  6. Recursively repeat this process on the nodes in the first-identified
##      community until no cut points can be found that further minimize error.
##  7. Repeat steps (1-6) for all sink nodes (t) in the network.
##  8. Return the proportion of times that each dyad (i,j) appear in the same
##      community as identified for a given sink node, in the form of a 
##      weighted, undirected adjacency matrix.
##
## Returns:
##  A N x N matrix in which element (i,j) represents the proportion of times
##  that dyad is identified as being in the same community over all sink nodes
##  1, ..., n in the network.
##
## Citing this function: https://www.github.com/jrhammond
##
## Citing the original algorithm:
##  Yang, Bo, William Cheung, and Jiming Liu. 
##  "Community mining from signed social networks." 
##  IEEE transactions on knowledge and data engineering 19.10 (2007): 1333-1348.
##
################################################################################


##############################################################################
##
## Partitioning function: for a specified sink node, identify the optimum
##  number and population of communities that minimizes within-/cross-
##  community error.
##
## Arguments:
##  - in_matrix: adjacency matrix
##  - l: length of random walk
##  - sink_idx: index of sink node
##
## Returns:
##  - community_data: a list object with key statistics on the number and
##    population of communities identified by recursive partitioning.
##
##############################################################################

fec_function <- function(
  in_matrix
  , l = 10
  , sink_idx = 1
  , method = 'ties'
  ){
  
  ##############################################################################
  ##
  ## Sorting sub-function: for a given sink node and number of steps, identify
  ##  the probability that a random walk starting at any other node will reach
  ##  the sink at or before step l.
  ##
  ## Arguments:
  ##  - in_matrix: adjacency matrix
  ##  - l: length of random walk
  ##  - sink_idx: index of sink node
  ##
  ## Returns:
  ##  - sorted_matrix: sorted adjacency matrix
  ##
  ##############################################################################
  fc_function <- function(
    in_matrix
    , .l = l
    , .sink_idx = sink_idx
  ){
    
    n <- nrow(in_matrix)
    
    if(!is.null(rownames(in_matrix))){
      save_names <- rownames(in_matrix)
    } else {
      save_names <- 1:nrow(in_matrix)
    }
    rownames(in_matrix) <- 1:nrow(in_matrix)
    colnames(in_matrix) <- 1:nrow(in_matrix)
    
    ## Create Markov transition matrix: probability of moving from any node
    ##  to any other node in one step.
    transition_matrix <- in_matrix
    transition_matrix[transition_matrix < 0] <- 0
    transition_matrix <- drop0(transition_matrix)
    transition_rowsums <- rowSums(transition_matrix)
    transition_rowsums[transition_rowsums == 0] <- 1
    transition_matrix <- transition_matrix / transition_rowsums
    
    ## Set sink node as an 'absorbing state': ties can go into the sink node,
    ##  but do not come out. This is to avoid counting random walks that double
    ##  back aross the destination node.
    transition_matrix[.sink_idx, ] <- 0
    
    ## Create an output object: this will contain the probabilities of arriving
    ##  at a given sink node for all nodes in the network.
    sink_data <- Matrix(0, nrow = n, ncol = n)
    
    ## Cumulative probability of reaching sink node from any starting node
    ##  at step 1, ..., l
    for(i in 1:(.l)){
      sink_data <- sink_data + matpow(transition_matrix, k = i, genmulcmd = genmulcmd.vanilla, dup = dup.vanilla)$prod1
    }
    
    ## Only interested in the [.sink_idx]'th column of the matrix.
    sink_data <- sink_data[, .sink_idx]
    
    ## The sink node always has a probability of 1.0 of reaching itself.
    sink_data[.sink_idx] <- 1
    
    ## Sort node indexes by probability of reaching sink node.
    ## If nodes have character names, sort by probability and then by alphabet;
    ## otherwise, sort by node index.
    
    sinkprobs <- data.table(sort(sink_data[-.sink_idx], decreasing = T))
    sinkprobs[, name := as.integer(names(sort(sink_data[-.sink_idx], decreasing = T)))]
    setorder(sinkprobs, -V1, -name)
    sorted_sinkprobs <- c(1, sinkprobs$V1)
    sink_idx <- c(.sink_idx, sinkprobs$name)
    
    ## Sort adjacency matrix by probability in descending order.
    sorted_matrix <- in_matrix[
      sink_idx
      , sink_idx
      ]
    
    rownames(sorted_matrix) <- save_names[sink_idx]
    colnames(sorted_matrix) <- save_names[sink_idx]
    ## Return sorted matrix for next step of calculations.
    return(sorted_matrix)
  }
  
  
  ##########
  ## Lambda-function 1: for each square sub-matrix A[1:pos, 1:pos], identify
  ##  whether more positive ties are located in that sub-matrix versus the
  ##  sub-matrices found on the upper-right off-diagonal.
  ##########
  ecfun1 <- function(pos, .in_mat){
    n <- nrow(.in_mat)
    return(
      sum(
        rowSums(
          matrix(.in_mat[1:pos, 1:pos], nrow = pos)
        ) >=
          rowSums(
            matrix(.in_mat[1:pos, (pos+1):n], nrow = pos)
          )
      )
    )
  }
  
  
  ##########
  ## Lambda-function 2: for each square sub-matrix A[pos:n-1, pos:n-1],
  ##  identify whether more positive ties are located in that sub-matrix
  ##  versus the submatrices found in the lower-left off-diagonal.
  ##########
  ecfun2 <- function(pos, .in_mat){
    
    n <- nrow(.in_mat)
    
    return(
      sum(
        rowSums(
          matrix(.in_mat[(n-(pos-1)):n, (n-(pos-1)):n], nrow = pos)
        ) >=
          rowSums(
            matrix(.in_mat[(n-(pos-1)):n, 1:(n-pos)], nrow = pos)
          )
      )
    )
  }
  
  
  ##########
  ## Error-calculation function: for a given cutpoint/community, identify the
  ##  level of in-community negative ties and cross-community positive
  ##  ties, as a proportion of all ties in the network.
  ##########
  error_calc <- function(pos, .in_mat){
    
    n <- nrow(.in_mat)
    
    # if (.method == 'ties'){
    comm_c1 <- sum(.in_mat[1:pos, 1:pos] < 0) +
      sum(.in_mat[1:pos, (pos+1):n] > 0) +
      sum(.in_mat[(pos+1):n, 1:pos] > 0)
    
    comm_c2 <- sum(.in_mat[(pos+1):n, (pos+1):n] < 0) +
      sum(.in_mat[1:pos, (pos+1):n] > 0) +
      sum(.in_mat[(pos+1):n, 1:pos] > 0)
    
    return(list(
      'comm1_error' = comm_c1 / sum(abs(.in_mat))
      , 'comm2_error' = comm_c2 / sum(abs(.in_mat))
    )
    )
  }
  
  
  #########################################
  ##
  ## Community detection sub-function: for a given sorted matrix, identify the
  ##  cut-point (node at which to partition) that minimizes error.
  ##
  ## Arguments:
  ##  - sorted_matrix: sorted adjacency matrix
  ##  - error: optional value identifying the error value of a previous
  ##    partition. If this is a real number, this function will try to identify
  ##    a new cutpoint that returns a lower error rate.
  ##  - cut_idx: optional value identifying the cutpoint of a previous
  ##    partition. If this is a real number, only nodes (1:cut_idx) will be 
  ##    used in the partition analysis (re-partitioning an existing community).
  ##
  ## Returns: a list object with the following variables:
  ##  - error: the error rate of the given partition
  ##  - cut_idx: the index of the node chosen as a cutpoint
  ##  - membership: a vector of node indices identified as being within
  ##    the community created by partitioning at the cutpoint node
  ##
  #########################################
  ec_function <- function(
    in_submat = sorted_matrix
    , method = NULL
    , error1 = Inf
    , error2 = list()
    , master_matrix = sorted_matrix
    , comm_membership = data.table(
      'node' = rownames(sorted_matrix)
      , 'node_idx' = 1:nrow(sorted_matrix)
      , 'comm' = 1
      , 'comm_error' = NA_real_
    )
    , community_info = data.table(
      'start_idx' = integer()
      , 'cut_idx' = integer()
      , 'error_a' = numeric()
      , 'error_b' = numeric()
    )
    , comm_counter = 1
    , resplit = 0
  ){
    
    
    if(is.na(as.integer(rownames(master_matrix)[1]))){
      names_factor <- factor(rownames(master_matrix))
    } else {
      names_factor <- as.integer(rownames(master_matrix))
    }
    names_data <- data.table(as.integer(names_factor), names_factor)
    names_data$node_idx <- 1:nrow(names_data)
    
    if(!is.null(in_submat)){
      in_submat <- fc_function(in_submat, .sink_idx = 1, .l = 10)
      
      ## Get rid of isolates
      # kick_nodes <- rownames(in_submat)[rowSums(abs(in_submat)) == 0 & colSums(abs(in_submat)) == 0]
      # if(length(kick_nodes) > 0){
      #   in_submat <- in_submat[-which(rownames(in_submat) %in% kick_nodes), -which(rownames(in_submat) %in% kick_nodes)]
      # }
      
      sub_graph <- graph_from_adjacency_matrix(in_submat, mode = 'directed', diag = F, weighted = T)
      # Extract largest connected component
      # TODO: IMPLEMENT MULTI-COMPONENT HANDLING TO MAIN CLUSTER ALGORITHM
      cl <- clusters(sub_graph)
      keep_idx <- (which(cl$membership == which.max(cl$csize)))
      kick_nodes <- rownames(in_submat)[-keep_idx]
      sub_graph <- induced_subgraph(sub_graph, keep_idx)
      in_submat <- in_submat[
        which(cl$membership == which.max(cl$csize))
        , which(cl$membership == which.max(cl$csize))]
      comm_membership[node %in% kick_nodes, comm := comm + 1]
      
      if(length(in_submat) == 1){
        print("Only isolated nodes remain. Assigning each isolate its own community.")
        comm_membership[node %in% kick_nodes, comm := comm + 1:length(kick_nodes)]
        return(comm_membership)
      }
      
      
      ## Get dimensions of matrix (or sub-matrix) to partition
      n <- nrow(in_submat)
      
      ## Create lambda vectors.
      lambda1 <- sapply(1:(n-1), ecfun1, .in_mat = in_submat)
      lambda2 <- sapply(1:(n-1), ecfun2, .in_mat = in_submat)
      lambda2 <- lambda2[length(lambda2):1]
      
      ## Calculate S-value based on lambda vectors: higher values of S mean that
      ##  a given cutpoint i maximizes the number of positive ties located in both
      ##  diagonal sub-matrices relative to the number of positive ties located in
      ##  the off-diagonal matrices.
      S <- (lambda1 / (2:(n))) + (lambda2 / (n):2)
      S <- c(S, 2)
      
      ## Calculate error for all potential cutpoints (indices with max S value).
      comm_cuts <- which(S[-length(S)] %in% max(S[-length(S)]))
      comm_errors <- matrix(
        unlist(
          sapply(
            comm_cuts
            , error_calc
            , .in_mat = in_submat
          )
        )
        , nrow = 2)
      
      ## Get the highest-indexed cutpoint with the minimum error value.
      cut_idx <- comm_cuts[min(which(comm_errors[1,] %in% min(comm_errors[1,])))]
      
      if(!is.nan(comm_errors[1,1])){
        comm1_error <- comm_errors[1, max(which(comm_errors[1,] %in% max(comm_errors[1,])))]
        comm2_error <- comm_errors[2, max(which(comm_errors[1,] %in% max(comm_errors[1,])))]
      } else{
        print('Empty community cannot be split.')
        comm1_error <- Inf
        comm2_error <- error2[length(error2)]
      }
      
      if(is.null(error1[1][[1]])) {
        error1 <- 1
        comm2_error <- list()
      } 
      
      
      # print(paste('New community A error:', comm1_error))
      # print(paste('Old community A error:', error1))
      # print(paste('New community B error:', error2[length(error2)]))
    } else {
      print('Single-node community cannot be split.')
      comm1_error <- Inf
      comm2_error <- error2[length(error2)]
    }
    
    
    if(comm1_error < error1){
      print('Splitting community decreases error. Cut_idx decreases, attempting to split new sub-community.')
      
      this_error1 <- comm1_error
      this_error2 <- c(error2, comm2_error)
      
      this_comm_membership <- comm_membership
      this_comm1_members <- this_comm_membership[node %in% rownames(in_submat)[1:cut_idx], node]
      this_comm2_members <-  this_comm_membership[node %in% rownames(in_submat)[(cut_idx)+1:n], node]
      # this_comm_membership[node %in% this_comm1_members, comm_error := comm1_error]
      # this_comm_membership[node %in% this_comm2_members, comm_error := comm2_error[length(comm2_error)]]
      this_comm_membership[node %in% this_comm2_members, comm := comm + 1]
      this_comm_membership[!(node %in% this_comm1_members) & 
                             !(node %in% this_comm2_members) & 
                             comm >= (comm_counter+1)
                           , comm := comm + 1]
      setkeyv(this_comm_membership, c('comm', 'node_idx'))
      # print(this_comm_membership)
      
      comm_idxs <- which(names_data$names_factor %in% this_comm_membership[comm %in% comm_counter, node])
      
      # this_community_info <- rbind(community_info, list(min(comm_idxs), min(comm_idxs)+ length(comm_idxs), comm1_error, comm2_error))
      
      if(length(comm_idxs) > 1){
        new_submat <- master_matrix[comm_idxs, comm_idxs]
      } else {
        new_submat <- NULL
      }
      
      ec_function(
        in_submat = new_submat
        , error1 = this_error1
        , error2 = this_error2
        , master_matrix = master_matrix
        , comm_membership = this_comm_membership
        , community_info = this_community_info
        , comm_counter = comm_counter
        , resplit = 1
        , method = method
      )
      
    } else {
      print('Splitting community A does not decrease error. Cut_idx increases, attempting to split sub-community B.')
      this_counter <- comm_counter + 1
      comm_idxs <- which(names_data$names_factor %in% comm_membership[comm == this_counter, node])
      print(this_counter)
      
      # comm_membership[comm %in% this_counter, comm_error := error2[length(error2)]]
      
      if(length(comm_idxs) <= 1){
        # print('Maximum number of error-reducing partitions found.')
        return(comm_membership)
      } else{
        
        new_submat = master_matrix[comm_idxs, comm_idxs]
        
        # this_error1 <- error2[length(error2)]
        this_error1 <- error2[length(error2)]
        this_error2 <- error2[-length(error2)]
        # this_error1 <- 0.5
        # this_error2 <- error2[-length(error2)]
        
        
        ec_function(
          in_submat = new_submat
          , error1 = this_error1
          , error2 = this_error2
          , master_matrix = master_matrix
          , comm_membership = comm_membership
          , community_info = community_info
          , comm_counter = this_counter
          , resplit = 0
          , method = method
        )
      }
    }
  }
  
  
  #########################################
  ##
  ##  Run primary sub-functions on input data.
  ##
  #########################################
  sorted_matrix <- fc_function(in_matrix, .sink_idx = sink_idx, .l = l)
  comm_outdata <- ec_function(sorted_matrix, method = method)
  
  #########################################
  ##
  ##  Return partition information.
  ##
  #########################################
  return(comm_outdata)
}




##############################################################################
##
## Batch-partitioning function: applies recursive partitioning over the entire
##  network in which each node is treated as a sink node for a set random walk
##  distance l. 
##
## Arguments:
##  - in_matrix: adjacency matrix
##  - l: length of random walk
##
## Returns:
##  - community_matrix: a symmetric/undirected N x N matrix where each element 
##    (i,j) identifies the proportion of times, over all sink nodes 1, ..., n,
##    in which dyad (i,j) were partitioned into the same community.
##
##############################################################################
fec_sim <- function(in_matrix, l = 10){
  
  ## Get number of nodes in a network.
  n <- nrow(in_matrix)
  nodes <- 1:n
  
  if(is.null(rownames(in_matrix))){
    rownames(in_matrix) <- 1:n
    colnames(in_matrix) <- 1:n
  }
  
  fec_comm <- function(pos, in_matrix, l){
    print(pos)
    ## Identify partitions for a given sink node
    comm_membership <- fec_function(in_matrix, l = l, sink_idx = pos, method = method)
    ## Sort according to node index
    setkeyv(comm_membership, 'node')
    
    error_vector <- comm_membership[!duplicated(comm), comm_error]
    
    ## Create output matrix to hold community values for this network
    this_comm <- Matrix(0, nrow = n, ncol = n, sparse = T)
    rownames(this_comm) <- rownames(in_matrix)
    colnames(this_comm) <- rownames(in_matrix)
    
    ## If the community detected is larger than one node, populate community
    ##  matrix: element (i,j) = 1 if dyad (i,j) are in the same community
    for(comm_i in unique(comm_membership$comm)){
      comm_idx <- as.matrix(expand.grid(
        which(rownames(in_matrix) %in% comm_membership[comm == comm_i, node])
        , which(rownames(in_matrix) %in% comm_membership[comm == comm_i, node])
      ))
      
      # this_comm[comm_idx]<- 1 - error_vector[comm_i]
      this_comm[comm_idx]<- 1
    }
    
    ## Update community matrix with community data from this sink node.
    return(this_comm)
  }
  
  sink_matrices <- sapply(nodes, fec_comm, in_matrix = in_matrix, l = l)
  
  community_matrix <- Reduce('+', sink_matrices)
  
  ## Return community matrix normalized by the number of nodes in the network.
  return(community_matrix / n)
  
}
