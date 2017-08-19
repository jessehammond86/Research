###############################################################################
#
# Calculating network-level imbalance based on triads,
#  and calculating edge-level measures of 'stress' based on
#  how much balance/imbalance would be created by flipping
#  the sign of any existing tie in a network.
#
# Author: Jesse Hammond
###############################################################################

## Load packages
pacman::p_load(igraph, data.table, dplyr)

##############################
##
## triadicBalance function
##
## Required input:
##  - K x K signed (-1, 0, 1) adjacency matrix
##
## Functionality:
##  - Balance calculation: identify 2- and/or 3-path balance on a signed network
##    via one of several methods. Balance metrics can be returned at the
##    network, node, or dyad level.
##  - Tension calculation: for each edge i, identify the change in
##    network balance that would result from flipping the sign of that edge.
##    Tension metrics can also be returned at the network, node, or dyad level.
##
## Arguments:
##  signed_mat (K x K matrix) : input data
##  tension (Logical T/F): calculate tension statistics?
##  type (triadic, all): constrain balance calculation to triads only?
##    NOTE: only works with distance = 2
##  baltype (cutoff, ratio, path-pair): choice of balance algorithm
##  out (network, dyad, node): level at which to calculate balance
##  .parallel (integer): number of cores to use in parallel
##  distance (2 or 3): distance over which to calculate path balance
##
##############################

triadicBalance <- function(
  signed_mat
  , tension = F
  , out = 'network'
  , .parallel = 0
  ){


  ##########
  ##
  ## Sub-function: calculate balance for network, node, and edge.
  ##
  ##########

  bal_calc <- function(.signed_mat = signed_mat
                       , .baltype = baltype
                       , .out = out
                       , .symmetric = symmetric){

    ## Get number of nodes in network
    nodes <- nrow(.signed_mat)

    ## Calculate the sign of all observed cycles. Since the output is a ratio,
    ## cycles can also be used for undirected graph structures.
    net_cycles <- t(
      matrix(triangles(
        graph_from_adjacency_matrix(.adj_mat, weighted = T, mode = 'undirected')
      ), nrow = 3)
    )
    net_cycles <- data.table(
      rbind(
        net_cycles[, c(1,2,3)]
        , net_cycles[, c(1,3,2)]
        , net_cycles[, c(2,1,3)]
        , net_cycles[, c(2,3,1)]
        , net_cycles[, c(3,2,1)]
        , net_cycles[, c(3,1,2)]
        )
      )
    net_cycles[, prod := apply(
        cbind(
          .signed_mat[as.matrix(net_cycles[, c(1,2)])]
          , .signed_mat[as.matrix(net_cycles[, c(2,3)])]
          , .signed_mat[as.matrix(net_cycles[, c(3,1)])]
        )
        , 1, 'prod'
      )
    ]


    ## Transform output to node or network level.

    if (.out == 'network'){
      out_data <- sum(net_cycles[, prod] == 1) / nrow(net_cycles)
    }
    else if (.out == 'node'){
      out_data <- net_cycles[, list(mean(prod == 1), .N), by = V1]
      if(.symmetric == T){
        out_data[, N := N/2]
      }
      setnames(out_data, c('node_id', 'balance', 'n_triads'))
      out_data <- merge(data.table(node_id = 1:nodes), out_data, all.x = T)
      out_data[is.na(n_triads), n_triads := 0]
      setkeyv(out_data, 'node_id')
    }

    return(out_data)

  }


  #########
  ##
  ## Subfunction: flip the sign of one tie and re-calculate balance
  ##
  #########

  tens_balcalc <- function(
    x
    , in_mat = signed_mat
    , .symmetric = symmetric
    , .change_idx = change_idx
    , .out = out
    ){

    in_mat[rbind(.change_idx[x, ])] <- in_mat[rbind(.change_idx[x, ])] * -1

    tens_stats <- bal_calc(
      .signed_mat = in_mat
      , .symmetric = symmetric
      , .out = .out
      )

    return(tens_stats)
  }


  ##########
  ##
  ## Calculate network balance and/or tension stats
  ##
  ##########

  ## Set up parallel cluster if desired
  if(.parallel > 0){
    cl <- makeCluster(.parallel, type = 'SOCK')
    clusterEvalQ(cl, library(data.table))
  }

  ## Format input
  nodes <- nrow(signed_mat)
  symmetric <- ifelse(isSymmetric(signed_mat), T, F)

  ## Calculate tension (if desired)
  if(tension == F){
    bal_stats <- bal_calc()

    return(bal_stats)
  }
  else if(tension == T){

    ## Create some output objects
    net_tension <- matrix(NA, nrow = nodes, ncol = nodes)

    ## Calculate network-level tension:
    ## what is the change in network balance resulting from
    ## flipping the sign of a single given tie?
    change_idx <- which(signed_mat != 0, arr.ind = T)
    .nties <- 1:nrow(change_idx)
    balance_stats <- sapply(bal_calc(.out = 'network'), mean, na.rm = T)

    if(.parallel == 0){
      tension_stats <- sapply(.nties, tens_balcalc, .out = 'network')
    }
    else {
      tension_stats <- t(parSapply(cl = cl, .nties, tens_balcalc))
    }

    tension_stats <- tension_stats - balance_stats

    ## Assign tension values to output
    net_tension[change_idx] <- tension_stats

    ## Shut down parallel operations if used
    if(.parallel > 0){
      stopCluster(cl)
    }

    return(net_tension)
  }
}
