###############################################################################
#
# Calculating network-level imbalance based on 2- and 3-paths,
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
## bal_23path function
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

bal_23path <- function(
  signed_mat
  , tension = F
  , type = 'triadic'
  , baltype = 'cutoff'
  , out = 'network'
  , .parallel = 0
  , distance = 3
  ){


  ##########
  ##
  ## Sub-function: calculate balance for network, node, and edge.
  ##
  ##########

  bal_calc <- function(.signed_mat = signed_mat
                       , .adj_mat = adj_mat
                       , .baltype = baltype
                       , .out = out
                       , .type = type
                       , .symmetric = symmetric
                       , .distance = distance){

    ## Get number of nodes in network
    nodes <- nrow(.signed_mat)


    ## Calculate paths and signs
    signed_path2 <- .signed_mat %*% .signed_mat
    adj_path2 <- .adj_mat %*% .adj_mat

    if (.distance == 3){
      signed_path3 <- signed_path2 %*% .signed_mat
      adj_path3 <- adj_path2 %*% .adj_mat
    }


    ## Calculate positive vs negative signed paths
    pos_path1 <- .adj_mat
    pos_path1[.signed_mat < 0] <- 0

    neg_path1 <- .adj_mat
    neg_path1[.signed_mat > 0] <- 0

    # neg_path2 <- adj_path2
    # neg_path2[signed_path2 < 0] <- neg_path2[signed_path2 < 0] + signed_path2[signed_path2 < 0]
    # pos_path2 <- adj_path2 - neg_path2

    pos_path2 <- (adj_path2 + signed_path2) / 2
    neg_path2 <- (adj_path2 - pos_path2)

    if(.type != 'triadic'){
      if (.distance == 3){
        pos_path3 <- (adj_path3 + signed_path3) / 2
        neg_path3 <- (adj_path3 - pos_path3)
      }

      if (.distance == 2){
        pos_mat <- (pos_path1 * pos_path2) + (pos_path1 * neg_path2)
        neg_mat <- (neg_path1 * pos_path2) + (neg_path1 * neg_path2)
        all_mat <- .adj_mat * adj_path2
        # pos_mat <- pos_path1 + pos_path2
        # neg_mat <- neg_path1 + neg_path2
        # all_mat <- .adj_mat + adj_path2
      } else if (.distance == 3){
        pos_mat <- pos_path1 + pos_path2 + pos_path3
        neg_mat <- neg_path1 + neg_path2 + neg_path3
        all_mat <- .adj_mat + adj_path2 + adj_path3
      }

    ## Generate output matrix object

    ## Cutoff: Imbalance is a binary measure identifying whether any given
    ## dyad has at least one positive AND one negative path connecting
    ## the two nodes.

    ## Ratio: Imbalance is calculated as the ratio of positive to negative
    ## signed paths between any two nodes.

    ## Path-pair: Imbalance is calculated by looking at the number of
    ## differently-signed pairs of paths, divided by the total number of
    ## path pairs between any two nodes.

    if (.baltype == 'cutoff'){
      out_mat <- Matrix(0, nrow = nodes, ncol = nodes, sparse = T)
      out_mat[which(all_mat > 0 & all_mat == neg_mat)] <- -1
      out_mat[which(all_mat > 0 & all_mat == pos_mat)] <- 1
      out_mat[which(all_mat != pos_mat & all_mat != neg_mat)] <- 7
    }
    else if (.baltype == 'ratio'){
      out_mat <- Matrix(-1, nrow = nodes, ncol = nodes, sparse = T)
      # out_mat[which(all_mat > 0 & all_mat == neg_mat)] <- 0
      # out_mat[which(all_mat > 0 & all_mat == pos_mat)] <- 1
      mix_idx <- which(
        all_mat > 0
        )
      out_mat[mix_idx] <- pos_mat[mix_idx] / all_mat[mix_idx]
    }
    else if (.baltype == 'path-pair'){
      mixed_paths <- (pos_mat * neg_mat)
      same_paths <- Matrix(0, nrow = nodes, ncol = nodes, sparse = T)
      same_paths[1:nodes, 1:nodes] <- apply(pos_mat, c(1,2), 'choose', 2) + apply(neg_mat, c(1,2), 'choose', 2)
      out_mat <- Matrix(0, nrow = nodes, ncol = nodes, sparse = T)
      out_mat[which(all_mat > 1)] <- same_paths[which(all_mat > 1)] /
        (mixed_paths[which(all_mat > 1)] + same_paths[which(all_mat > 1)])
      out_mat[which(all_mat == 1)] <- 1
    }

    ## Return network, dyad, or nodal stats; or raw path counts.
    if (.out == 'network'){
      if (.baltype == 'cutoff'){
        out_data <- mean(out_mat@x %in% c(-1, 1))
      }

      else if (.baltype %in% 'ratio'){
        out_data <- mean(abs(out_mat[which(out_mat > -1)]), na.rm = T)
      }

      else if (.baltype %in% 'path-pair'){
        out_data <- mean(out_mat@x[which(out_mat@x > 0)])
      }
    }
    else if (.out == 'node'){
      if (.baltype == 'cutoff'){
        out_data <- apply(
          out_mat, 1, function(x) mean(x[which(x != 0)] %in% c(-1,1))
          )
      }
      else if (.baltype %in% 'ratio'){
        out_data <- apply(
          out_mat, 1, function(x) mean(x[which(x > -1)])
        )
      }
      else if (.baltype %in% 'path-pair'){
        out_data <- apply(
          out_mat, 1, function(x) mean(x[which(x != 0)])
        )
      }
    }
    else if (.out == 'dyad'){
      out_data <- out_mat
    }
    else if (.out == 'raw'){
      out_data <- array(c(all_mat, pos_mat, neg_mat), dim = c(nodes, nodes, 3))
      dimnames(out_data)[[3]] <- c('total', 'positive', 'negative')
    }

  } else if (.type == 'triadic') {
      pos_mat <- (pos_path1 * pos_path2) + (pos_path1 * neg_path2)
      neg_mat <- (neg_path1 * pos_path2) + (neg_path1 * neg_path2)
      all_mat <- .adj_mat * adj_path2
      # pos_mat <- pos_path1 + pos_path2
      # neg_mat <- neg_path1 + neg_path2
      # all_mat <- .adj_mat + adj_path2

      if (.baltype == 'cutoff'){
        out_mat <- Matrix(0, nrow = nodes, ncol = nodes, sparse = T)
        out_mat[which(neg_mat > 0)] <- -1
        out_mat[which(pos_mat > 0 & neg_mat == 0)] <- 1
      }
      else if (.baltype == 'ratio'){
        out_mat <- Matrix(-1, nrow = nodes, ncol = nodes, sparse = T)
        # out_mat[which(all_mat > 0 & all_mat == neg_mat)] <- 0
        # out_mat[which(all_mat > 0 & all_mat == pos_mat)] <- 1
        mix_idx <- which(
          all_mat > 0
        )
        out_mat[mix_idx] <- pos_mat[mix_idx] / all_mat[mix_idx]
      }

      if (.out == 'network'){
        if (.baltype == 'cutoff'){
          out_data <- sum(out_mat@x == 1) / sum(out_mat@x %in% c(-1, 1))
        }

        else if (.baltype %in% 'ratio'){
          out_data <- mean(abs(out_mat[which(out_mat > -1)]), na.rm = T)
        }
      }
      else if (.out == 'node'){
        if (.baltype == 'cutoff'){
          out_data <- apply(
            out_mat, 1, function(x) sum(x > 0) / sum(x != 0)
          )
        }
        else if (.baltype %in% 'ratio'){
          out_data <- apply(
            out_mat, 1, function(x) mean(x[which(x > -1)])
          )
        }
      }
      else if (.out == 'dyad'){
        out_data <- out_mat
      }
      else if (.out == 'raw'){
        out_data <- array(c(all_mat, pos_mat, neg_mat), dim = c(nodes, nodes, 3))
        dimnames(out_data)[[3]] <- c('total', 'positive', 'negative')
      }
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
    , in_adj = adj_mat
    , .change_idx = change_idx
    , .out = out
    , .baltype = baltype
    ){

    in_mat[rbind(.change_idx[x, ])] <- in_mat[rbind(.change_idx[x, ])] * -1

    tens_stats <- bal_calc(
      .signed_mat = in_mat
      , .adj_mat = in_adj
      , .baltype = .baltype
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

  ## Check: is this an empty network?
  if(sum(signed_mat != 0) == 0){
    return(NA)
  }

  ## Format input
  nodes <- nrow(signed_mat)
  adj_mat <- signed_mat
  adj_mat[which(adj_mat != 0)] <- 1
  symmetric <- ifelse(isSymmetric(signed_mat), T,)

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

# foo <- Matrix(sample(c(-1,0,1), 25, replace = T), nrow = 5)
# foo <- matrix(rep(0, 16), nrow = 4)
# foo[1,2] <- 1
# foo[2,1] <- 1
# plot(as.network.matrix(foo))
# diag(foo) <- 0
# print(bal_23path(week5_mat1, out = 'node', type = 'triadic', baltype = 'ratio'))
# print(bal_23path(foo, out = 'dyad', baltype = 'cutoff'))
# print(bal_23path(foo, out = 'dyad', baltype = 'ratio'))

