###############################################################################
##
## Attempting to implement Yang et al's (2007) agent-based community detection
##  algorithm for signed networks.
##
###############################################################################
rm(list = ls())
pacman::p_load(expm, data.table, igraph)
set.seed(80085)
# Start with signed matrix
n <- 20
in_matrix <- matrix(sample(c(-1, 0,1), n^2, prob = c(.05, .85, .1), replace = T), nrow = n)
# Force symmetry (not doing this for now)
# in_matrix[lower.tri(in_matrix)] <- t(in_matrix)[lower.tri(in_matrix)]
# Set diagonal to 0 (no self-ties)
diag(in_matrix) <- 0
# ingraph <- graph_from_adjacency_matrix(in_matrix, mode = 'directed')
# plot(ingraph)


ingraph <- read.graph("/Users/jesse/Downloads/karate/karate.gml",format=c("gml"))
karate_matrix <- as_adj(ingraph, sparse = F)


guinea <- fread('/Users/jesse/Dropbox/NetworkBrokerage/newguinea.csv')
tribesmat <- matrix(0, nrow = 16, ncol = 16)
tribesmat[as.matrix(guinea[, list(nodea, nodeb)])] <- guinea[, weight]
tribesmat[lower.tri(tribesmat)] <- t(tribesmat)[lower.tri(tribesmat)]



# in_matrix <- matrix(0, nrow = 28, ncol = 28)
# 
# in_matrix[1, c(2, 12, 28)] <- 1
# in_matrix[1, c(27)] <- -1
# 
# in_matrix[2, c(3)] <- 1
# in_matrix[2, c(15, 27)] <- -1
# 
# in_matrix[3, c(19)] <- 1
# in_matrix[3, c(4)] <- -1
# 
# in_matrix[4, c(5, 16)] <- 1
# 
# in_matrix[5, c(6)] <- 1
# in_matrix[5, c(18)] <- -1
# 
# in_matrix[6, c(7)] <- 1
# in_matrix[6, c(20)] <- -1
# 
# in_matrix[7, c(22)] <- 1
# in_matrix[7, c(8)] <- -1
# 
# in_matrix[8, c(9,18)] <- 1
# in_matrix[8, c(20,21)] <- -1
# 
# in_matrix[9, c(26)] <- 1
# in_matrix[9, c(21, 23)] <- -1
# 
# in_matrix[10, c(11, 21, 28)] <- 1
# 
# in_matrix[11, c(12)] <- 1
# in_matrix[11, c(24)] <- -1
# 
# in_matrix[12, c(13,27)] <- -1
# 
# in_matrix[13, c(14, 25)] <- 1
# 
# in_matrix[14, c(15)] <- 1
# in_matrix[14, c(27)] <- -1
# 
# in_matrix[15, c(16)] <- 1
# 
# in_matrix[16, c(17)] <- -1
# 
# in_matrix[17, c(18, 27)] <- 1
# 
# in_matrix[18, c(20)] <- -1
# 
# in_matrix[19, c(20,28)] <- 1
# 
# in_matrix[20, c(21)] <- 1
# 
# in_matrix[21, c(22)] <- -1
# 
# in_matrix[22, c(23)] <- 1
# 
# in_matrix[23, c(24)] <- 1
# 
# in_matrix[24, c(25)] <- 1
# 
# in_matrix[25, c(26)] <- -1
# 
# in_matrix[26, c(27)] <- 1
# 
# 
# in_matrix[lower.tri(in_matrix)] <- t(in_matrix)[lower.tri(in_matrix)]


fec_function <- function(in_matrix, l = 10, sink_idx = 1){
  
  ## Small function: apply to rows/columns of adjacency matrix to retrieve only
  ##  positive ties (negative ties set to 0)
  get_posties <- function(x){max(c(0, x))}
  
  fc_function <- function(in_matrix, .l = l, .sink_idx = sink_idx){
    n <- nrow(in_matrix)
    transfer_matrix <- apply(in_matrix, c(1,2), get_posties) / 
      rowSums(apply(in_matrix, c(1,2), get_posties) )
    transfer_matrix[is.nan(transfer_matrix)] <- 0
    transfer_matrix[is.infinite(transfer_matrix)] <- 1
    
    
    ## Set sink_idx as node l: how likely are you to cross node $t$ in $l$ random steps
    ##  around the network, starting from node 1, ..., n?
    sink_matrix <- transfer_matrix
    sink_matrix[.sink_idx, ] <- 0
    
    sink_data <- matrix(0, nrow = n, ncol = 1)
    # outdata <- transfer_matrix
    for(i in 1:.l){
      sink_data <- sink_data + ((sink_matrix %^% i)[,.sink_idx])
    }
    sink_data[.sink_idx] <- 1
    
    names(sink_data) <- as.character(1:n)
    sorted_sinkprobs <- sink_data[as.integer(names(sort(sink_data, decreasing = F)))]
    sorted_sinkprobs <- sorted_sinkprobs[length(sorted_sinkprobs):1]
    sinkval_idx <- which(as.integer(names(sorted_sinkprobs)) == .sink_idx)
    sorted_sinkprobs <- c(sorted_sinkprobs[sinkval_idx], sorted_sinkprobs[-sinkval_idx])
    
    ##sort according to sink_idx distance
    rownames(in_matrix) <- as.character(1:n)
    colnames(in_matrix) <- as.character(1:n)
    
    sorted_matrix <- in_matrix[
      as.integer(names(sorted_sinkprobs))
      , as.integer(names(sorted_sinkprobs))
      ]
    
    return(sorted_matrix)
  }
  
  ec_function <- function(sorted_matrix, error = Inf, cut_idx = NULL){
    
    n <- nrow(sorted_matrix)
    
    if(!is.null(cut_idx)){
      sorted_matrix <- sorted_matrix[1:cut_idx, 1:cut_idx]
    } else if(is.null(cut_idx)){
      cut_idx <- n
    }
    
    n <- nrow(sorted_matrix)
    
    ecfun1 <- function(pos){
      return(
        sum(
          rowSums(
            matrix(sorted_matrix[1:pos, 1:pos], nrow = pos)
            ) >=
              rowSums(
                matrix(sorted_matrix[1:pos, (pos+1):n], nrow = pos)
                )
          )
      )
      }
    ecfun2 <- function(pos){
      return(
        sum(
          rowSums(
            matrix(sorted_matrix[(n-(pos-1)):n, (n-(pos-1)):n], nrow = pos)
            # matrix(sorted_matrix[pos:n, pos:n], nrow = (n-pos+1))
          ) >=
            rowSums(
              matrix(sorted_matrix[(n-(pos-1)):n, 1:(n-pos)], nrow = pos)
              # matrix(sorted_matrix[pos:n, 1:(pos-1)], nrow = (n-pos+1))
              )
          )
      )
      }
    
    lambda1 <- sapply(1:(n-1), ecfun1)
    lambda2 <- sapply(1:(n-1), ecfun2)
    lambda2 <- lambda2[length(lambda2):1]
    
    S <- (lambda1 / (1:(n-1))) + (lambda2 / (n-1):1)
    S <- c(S, 2)
    
    error_calc <- function(cut){
      comm_c <- sum(sorted_matrix[1:cut, 1:cut] < 0) +
        sum(sorted_matrix[1:cut, (cut+1):n] > 0) +
        sum(sorted_matrix[(cut+1):n, 1:cut] > 0)
      
      return(comm_c / sum(abs(sorted_matrix)))
      }
    
    comm_cuts <- which(S[-length(S)] %in% max(S[-length(S)]))
    comm_errors <- sapply(comm_cuts, error_calc)
    
    ## Get the largest index with the minimum error value.
    cut_idx <- comm_cuts[max(which(comm_errors %in% min(comm_errors)))]
    
    return(
      list(
        'error' = min(comm_errors)
        , 'cut_idx' = cut_idx
        , 'membership' = as.integer(rownames(sorted_matrix))[1:cut_idx]
        )
      )
  }
  
  ec_recursion <- function(
    comm_outdata
      , .sink_idx = sink_idx){
    
    .cut_idx <- comm_outdata$cut_idx[1]
    
    if(.cut_idx == 1){
      message(paste('Sink node', .sink_idx, 'defined as its own community: unable to recursively
            partition single-node community.'))
      names(comm_outdata$membership) <- .cut_idx
      
      return(
        list('stats' = comm_outdata
             , 'sorted_matrix' = sorted_matrix
        )
      )
    }
    
    comm_outdata$membership <- c(
      as.integer(rownames(sorted_matrix))
      , as.integer(rownames(sorted_matrix))[1:comm_outdata$cut_idx])
    
    error_improve <- T
    
    while(error_improve == T){
      
      try_newcomm <- ec_function(
        sorted_matrix
        , cut_idx = tail(comm_outdata$cut_idx, 1)
      )
      
      if(sum(abs(sorted_matrix[1:try_newcomm$cut_idx, 1:try_newcomm$cut_idx])) == 0){
        break
      }
      
      if(try_newcomm$error <= tail(comm_outdata$error, 1)){
        comm_outdata$error <- c(comm_outdata$error, try_newcomm$error)
        comm_outdata$cut_idx <- c(comm_outdata$cut_idx, try_newcomm$cut_idx)
        comm_outdata$membership <- c(comm_outdata$membership, try_newcomm$membership)
        error_improve <- T
      } else{
        error_improve <- F
      }
    }
    
    comm_outdata$membership <- (sort(table(comm_outdata$membership)))
    
    return(
      list('stats' = comm_outdata
           , 'sorted_matrix' = sorted_matrix
      )
    )
    
  }
  

  sorted_matrix <- fc_function(in_matrix, .sink_idx = sink_idx, .l = l)
  comm_outdata <- ec_function(sorted_matrix)
  
  community_data <- ec_recursion(comm_outdata)
  
  return(community_data)
}

fec_sim <- function(in_matrix){
  
  n <- nrow(in_matrix)
  same_comm <- matrix(0, nrow = n, ncol = n)
  
  for(i in 1:n){
    this_fec <- fec_function(in_matrix, l = 10, sink_idx = i)
    comm_membership <- data.table(
      'community' = this_fec$stats$membership
      , 'node' = as.integer(names(this_fec$stats$membership))
    )
    setkeyv(comm_membership, 'node')
    
    this_comm <- matrix(0, nrow = n, ncol = n)
    
    if(nrow(this_comm) > 1){
      for(comm in unique(comm_membership$community)){
        this_comm[as.matrix(expand.grid(
          comm_membership[community == comm, node]
          ,comm_membership[community == comm, node]
        ))] <- 1
      }
      same_comm <- same_comm + this_comm  
    }
  }
  
  return(same_comm / n)
  
}
fec_sim(in_matrix)


karate_sim <- fec_sim(karate_matrix)
testgraph <- graph_from_adjacency_matrix(karate_sim, mode = 'undirected', weighted = T, diag = F)

plot(testgraph
     , vertex.label = NA
     , edge.width = E(testgraph)$weight
     # , layout = layout.kamada.kawai
)




foo <- numeric()
for(pct in seq(0.05, 0.95, by = 0.05)){
  karate_sim2 <- karate_sim
  karate_sim2[karate_sim2 < pct] <- 0
  testgraph2 <- graph_from_adjacency_matrix(karate_sim2, mode = 'undirected', weighted = T, diag = F)
  foo <- c(foo, length(components(testgraph2)$csize))
}
plot(foo)




plot(testgraph2
     # , vertex.label = NA
     , edge.width = E(testgraph)$weight
     # , layout = layout.kamada.kawai
)

fec_sim(in_matrix)
fec_sim(karate_matrix)



# fec_function(in_matrix, l = 10, sink_idx = 6)
# fec_function(karate_matrix, l = 10, sink_idx = 1)
# fec_function(tribesmat, l = 10, sink_idx = 1)
# 
# 
## Start with signed matrix
n <- 20
in_matrix <- matrix(sample(c(-1, 0,1), n^2, prob = c(.05, .85, .1), replace = T), nrow = n)
## Force symmetry (not doing this for now)
# in_matrix[lower.tri(in_matrix)] <- t(in_matrix)[lower.tri(in_matrix)]
## Set diagonal to 0 (no self-ties)
diag(in_matrix) <- 0
# ingraph <- graph_from_adjacency_matrix(in_matrix, mode = 'directed')
# # plot(ingraph)
# 
# foo <- fec_function(in_matrix, l = 10, sink_idx = 1)
# print(foo)
