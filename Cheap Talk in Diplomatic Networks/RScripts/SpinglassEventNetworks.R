###############################################################################
##
## Simulations, tests, and analytics for Yang's signed networks community
##  detection algorithm.
##
###############################################################################
rm(list = ls())
pacman::p_load(expm, data.table, igraph, Matrix, matpow, tsna, pvclust)
setwd('/Users/jesse/Dropbox/NetworkSignedComm')
source('/Users/jesse/Dropbox/NetworkBalance/network_balance.R')
source('SignedCommunities.R')
set.seed(80085)


###############################################################################
##
## Loading/creating network data structures
##
###############################################################################

##########
## Signed/directed network: simulated matrix from random draws
##########
n <- 20
test_matrix <- Matrix(sample(c(-1, 0,1), n^2, prob = c(.05, .85, .1), replace = T), nrow = n)
test_graph <- graph_from_adjacency_matrix(test_matrix, mode = 'directed', weighted = T, diag = F)
E(test_graph)$color <- ifelse(E(test_graph)$weight == 1, 'blue', 'red')
plot(test_graph)
# Set diagonal to 0 (no self-ties)
diag(test_matrix) <- 0
test_matrix <- drop0(test_matrix)

##########
## Signed/directed network: New Guinean tribes
##########
guinea <- fread('./newguinea.csv')
tribes_matrix <- Matrix(0, nrow = 16, ncol = 16, sparse = T)
tribes_matrix[as.matrix(guinea[, list(nodea, nodeb)])] <- guinea[, weight]
tribes_matrix[lower.tri(tribes_matrix)] <- t(tribes_matrix)[lower.tri(tribes_matrix)]
tribes_graph <- graph_from_adjacency_matrix(tribes_matrix, mode = 'undirected', weighted = T, diag = F)
E(tribes_graph)$color <- ifelse(E(tribes_graph)$weight > 0, 'blue', 'red')

##########
## Unsigned/undirected network/weighted: Football games
##########
football_graph <- read_graph('./football.gml', format = 'gml')
football_matrix <- as_adj(football_graph, sparse = T)


##########
## Unsigned/undirected network: karate association
##########
karate_graph <- read.graph("./karate.gml",format=c("gml"))
karate_matrix <- as_adj(karate_graph, sparse = T)


##########
## Signed/directed/weighted network: monthly international interactions
##########
# international_multiplex <- phoenixNet::phoenix_net(
#   start_date = 20010101
#   , end_date = 20150101
#   , level = 'pentaclass'
#   , phoenix_loc = '/Users/jesse/Dropbox/Minerva/phoenix'
#   , icews_loc = '/Users/jesse/Dropbox/Minerva/icews'
#   , update = T
#   , time_window = 'month'
#   , tie_type = 'count'
#   , sources = 'ICEWS'
#   )
# save(international_multiplex, file = '/Users/jesse/Dropbox/NetworkSignedComm/intl_events.Rdata')
load('/Users/jesse/Dropbox/NetworkSignedComm/intl_events.Rdata')


## Set up dates
global_start_end <- get.network.attribute(international_multiplex$dailynets[[1]],'net.obs.period')$observations[[1]]
global_start_date_int <- global_start_end[1]
global_end_date_int <- global_start_end[2]
global_start_date <- as.Date(as.character(global_start_date_int), format = '%Y%m%d')
global_end_date <- as.Date(as.character(global_end_date_int), format = '%Y%m%d')
dates <- as.integer(format(seq.Date(global_start_date, global_end_date, 'month'), '%Y%m%d'))
start_date <- dates[1]
end_date <- last(dates)

## Get size of global network
n_states <- network.size(international_multiplex$dailynets[[1]])

## Collapse to daily network
mcoop_full <- international_multiplex$dailynets$code2
mconf_full <- international_multiplex$dailynets$code4

monthly_interactions <- list()
## Loop through dates and generate daily(etc) data
for(i in 1:(length(dates)-1)){
  
  ###### State network characteristics
  
  ## Record analysis time
  ptm <- proc.time()

  ## Create matrices for state data at time T
  mcoop <- network.collapse(mcoop_full, at = dates[i])
  mcoop <- as.matrix.network(mcoop, attrname = 'N')
  mconf <- network.collapse(mconf_full, at = dates[i])
  mconf <- as.matrix.network(mconf, attrname = 'N')
  
  ## Create signed network matrix - Conflict breaks ties
  conf_idx <- which(mconf > 0 & mconf >= mcoop)
  coop_idx <- which(mcoop > 0 & mcoop > mconf)
  
  signed_mat <- matrix(0, nrow = n_states, ncol = n_states)
  rownames(signed_mat) <- rownames(mcoop)
  colnames(signed_mat) <- colnames(mcoop)
  
  
  signed_mat[coop_idx] <- mcoop[coop_idx]
  signed_mat[conf_idx] <- -mconf[conf_idx]
  
  monthly_interactions[[i]] <- signed_mat
}


# ##########
# ## Unsigned/undirected network: political blogs
# ##########
# blogs_graph <- read.graph("./polblogs.gml",format=c("gml"))
# blogs_matrix <- as_adj(blogs_graph, sparse = T)
# blogs_matrix[blogs_matrix > 1] <- 1
# 
# 
# ##########
# ## Signed/directed/weighted network: Slashdot (2008-11-06)
# ##########
# slashdot_graph <- read.graph('./slashdot081106.txt', format = 'ncol')
# slashdot_matrix <- as_adj(slashdot_graph, sparse = T)
# 
# system.time(matpow(slashdot_matrix, 2))


###############################################################################
##
## Community detection: initial tests on single sink nodes to test speed
##
###############################################################################

########################################
## Comm1_info: Identify and plot community membership for sink = 1
########################################

comm_info <- function(in_matrix, in_graph, walk = 10, sink = 5, method = 'ties'){
  comm <- fec_function(in_matrix, l = walk, sink_idx = sink, method = method)
  setkeyv(comm, c('node'))
  V(in_graph)$color <- comm[, comm]
  plot(in_graph, edge.arrow.size = 0.1, vertex.label.cex = 0.75)
  setkeyv(comm, c('comm', 'node'))
  return(comm)
}

## Random network (n = 20)
comm_info(test_matrix, test_graph, sink = 1)

## New Guinean tribes (n = 16)
comm_info(tribes_matrix, tribes_graph, method = 'ties')

## Football graph (n = 115)
comm_info(football_matrix, football_graph, walk = 60, sink = 20)

## International interactions graph (n = ?)
yearsdata <- Reduce('+', monthly_interactions[120:123])
# yearsdata[which(yearsdata < 1 & yearsdata > -1)] <- 0
events_graph <- graph_from_adjacency_matrix(yearsdata, mode = 'directed', diag = F)
# Extract largest connected component
# TODO: IMPLEMENT MULTI-COMPONENT HANDLING TO MAIN CLUSTER ALGORITHM
cl <- clusters(events_graph)
events_graph <- induced_subgraph(events_graph, which(cl$membership == which.max(cl$csize)))
yearsdata <- yearsdata[which(cl$membership == which.max(cl$csize)), which(cl$membership == which.max(cl$csize))]
comm_info(yearsdata, events_graph, walk = 20, sink = 1)


comm <- fec_function(yearsdata, l = 20, sink_idx = 1)
setkeyv(comm, c('node'))
V(events_graph)$color <- comm[, comm]
plot(events_graph, edge.arrow.size = 0.1, vertex.label.cex = 0.75
     , vertex.size = ifelse(V(events_graph)$color == 6, 4, 1)
     , layout = layout.kamada.kawai
     , vertex.color = ifelse(V(events_graph)$color == 6, 'red', 'blue'))


## Slashdot graph (n = 77,000)
# foo <- comm_info(slashdot_matrix, slashdot, walk = 10, sink = 1)

###############################################################################
##
## Community detection tests
##
###############################################################################

########################################
## comm_cutpoints: extract and visualize the number of communities identified
##  as a function of the dyadic cutpoint: the proportion of runs over which
##  dyad (i,j) were identified as being members of the same community.
##
## Arguments:
##  - in-matrix: signed adjacency matrix.
##  - walk: length of random walk to identify communities.
##
## Outputs:
##  - Plot showing number of communities identified by a given cutpoint between
##    0 and 1, by 0.05 increments.
##  - Matrix object where element (i,j) takes a value between 0 and 1. This
##    value represents the proportion of runs in which nodes i and j were
##    identified as being in the same community.
########################################

comm_cutpoints <- function(in_matrix, walk = 10){
  comm_sim <- fec_sim(in_matrix, l = walk)
  comms_seq <- numeric()
  for(pct in seq(0.05, 0.95, by = 0.05)){
    comm_sim2 <- comm_sim
    comm_sim2[comm_sim2 < pct] <- 0
    comm_graph <- graph_from_adjacency_matrix(comm_sim2, mode = 'undirected', weighted = T, diag = F)
    comms_seq <- c(comms_seq, length(components(comm_graph)$csize))
  }
  plot(comms_seq ~ seq(0.05, 0.95, by = 0.05))
  return(comm_sim)
}


########################################
## plot_comms: Plot the communities identified by comm_cutpoints function.
##  Nodes are clustered by community membership, and ties can be set to either
##  raw ties (positive/negative) or shared-community-membership ties.
##
## Arguments:
##  - comm_graph: either signed adjacency matrix or signed igraph object.
##  - comm_matrix: weighted community matrix generated by comm_cutpoints
##  - cutoff: cutoff for edge identification from comm_matrix. Ranges 0-1;
##    only ties with weight > cutoff are identified in the output graph.
##  - type: type of edges to plot. Values can be 'ties' or 'communities',
##    indicating whether actual edges (ties) or shared community membership
##    (communities) connect nodes.
##
## Outputs:
##  - Plot of communities with ties identified by 'type' variable.
########################################


plot_comms <- function(comm_graph, comm_matrix, cutoff = 0.9, type = 'ties', returnData = 'plot'){
  
  ## If fed a matrix (or Matrix) convert to igraph
  if (class(comm_graph) %in% c('dgCMatrix', 'matrix')){
    if(isSymmetric(comm_graph)){
      comm_graph <- graph_from_adjacency_matrix(comm_graph, mode = 'undirected', weighted = T, diag = F)
    } else {
      comm_graph <- graph_from_adjacency_matrix(comm_graph, mode = 'directed', weighted = T, diag = F)
    }
  } else if (class(comm_graph) %in% 'igraph'){
    if(is.null(E(comm_graph)$weight)){
      E(comm_graph)$weight <- 1
    }
  }
  
  comm_graph_full <- graph_from_adjacency_matrix(comm_matrix, mode = 'undirected', weighted = T, diag = F)
  comm_matrix[comm_matrix < cutoff] <- 0
  
  comm_graph_cutoff <- graph_from_adjacency_matrix(comm_matrix, mode = 'undirected', weighted = T, diag = F)
  V(comm_graph_cutoff)$membership <- components(comm_graph_cutoff)$membership
  V(comm_graph)$membership <- components(comm_graph_cutoff)$membership
  
  memberships <- components(comm_graph_cutoff)$membership
  if(!is.null(V(comm_graph)$name)){
    names(memberships) <- V(comm_graph)$name
  } else{
    names(memberships) <- 1:vcount(comm_graph_full)
  }
  
  comm_graph_cutoff <- Matrix(0, nrow = length(memberships), ncol = length(memberships))
  for(comm in unique(memberships)){
    comm_graph_cutoff[which(memberships %in% comm), which(memberships %in% comm)] <- 1
  }
  comm_graph_cutoff <- graph_from_adjacency_matrix(comm_graph_cutoff, mode = 'undirected', diag = F)
  comms_layout <- layout.fruchterman.reingold(comm_graph_cutoff)
  
  
  weight.community=function(row,membership,weight.within,weight.between){
    if(
      as.numeric(membership[which(names(membership)==row[1])]) == 
      as.numeric(membership[which(names(membership)==row[2])])){
        weight=weight.within
      }else{
        weight=weight.between
      }
      return(weight)
    }
  E(comm_graph)$sharedcomm <- apply(get.edgelist(comm_graph),1,weight.community,memberships,1,0)
  
  if(min(E(comm_graph)$weight) < 0){
    E(comm_graph)$color <- ifelse(E(comm_graph)$sharedcomm == 1,
      ifelse(E(comm_graph)$weight < 0
             , adjustcolor("red", alpha.f = .75)
             , adjustcolor("blue", alpha.f = .75))
      , ifelse(E(comm_graph)$weight < 0
       , adjustcolor("Pink", alpha.f = .25)
       , adjustcolor("SkyBlue2", alpha.f = .25))
    )
  } else {
    E(comm_graph)$color <- ifelse(
      E(comm_graph)$sharedcomm == 1
      , adjustcolor("grey40", alpha.f = .75)
      , adjustcolor("grey40", alpha.f = .15)
    )
  }
  
  if(type == 'ties'){
    plot(comm_graph
         , vertex.label.cex = 0.5
         , vertex.size = 4
         , vertex.color = V(comm_graph)$membership
         , edge.width = log(abs(E(comm_graph)$weight + 1))
         , edge.arrow.size = 0.1
         , layout = comms_layout
         )
  } else if(type == 'communities'){
    comm_graph <- delete_edges(comm_graph, which(E(comm_graph)$sharedcomm == 0))
    plot(comm_graph
         , vertex.label.cex = 0.5
         , vertex.size = 4
         , vertex.color = V(comm_graph)$membership
         , edge.width = log(E(comm_graph)$weight + 1) + 1
         , edge.arrow.size = 0.1
         , layout = comms_layout
         )
  }

  
  if(returnData == 'tiesgraph'){
    return(comm_graph)
  } else if (returnData == 'communitygraph') {
    return(comm_graph_cutoff)
  }
}


########################################
## Identify communities based on cutpoints for each network matrix.
########################################

##########
## Karate network
##########
karate_comm_mat <- comm_cutpoints(karate_matrix)
plot_comms(karate_graph, karate_comm_mat, 0.9, type = 'ties')
plot_comms(karate_matrix, karate_comm_mat, 0.75, type = 'communities')
karate_clust <- pvclust(as.matrix(karate_comm_mat)
                        , method.dist = 'correlation'
                        , method.hclust = 'complete'
                        , parallel = T)
plot(karate_clust)
pvrect(karate_clust, alpha = 0.95)

##########
## New Guinean network
##########
tribes_comm_mat <- comm_cutpoints(tribes_matrix)
plot_comms(tribes_matrix, tribes_comm_mat, 0.75, type = 'ties')
plot_comms(tribes_matrix, tribes_comm_mat, 0.75, type = 'communities')
tribes_clust <- pvclust(as.matrix(tribes_comm_mat)
                        , method.dist = 'correlation'
                        , method.hclust = 'complete'
                        , parallel = T)
plot(tribes_clust)
pvrect(tribes_clust, alpha = 0.95)


##########
## Random signed network
##########
random_comm_mat <- comm_cutpoints(test_matrix)
plot_comms(test_matrix, random_comm_mat, 0.5, type = 'ties')
plot_comms(test_matrix, random_comm_mat, 0.5, type = 'communities')
random_clust <- pvclust(as.matrix(random_comm_mat)
                        , method.dist = 'correlation'
                        , method.hclust = 'complete'
                        , parallel = T)
plot(random_clust)
pvrect(random_clust, alpha = 0.95)

##########
## Football network
##########
football_comm_mat <- comm_cutpoints(football_matrix, walk = 20)
plot_comms(football_graph, football_comm_mat, 0.9, type = 'ties')
plot_comms(football_matrix, football_comm_mat, 0.9, type = 'communities')
football_clust <- pvclust(as.matrix(football_comm_mat)
                        , method.dist = 'correlation'
                        , method.hclust = 'ward.D2'
                        , parallel = T)
plot(football_clust)
pvrect(football_clust, alpha = 0.99)

##########
## International network
##########

## International interactions graph (n = 255)
# yearsdata <- Reduce('+', monthly_interactions[1:12])

monthly_components <- list()
monthly_communities <- list()
pv_structures <- list()
pv_clusters <- list()

for(thismonth in 1:length(monthly_interactions)){
  monthdata <- monthly_interactions[[thismonth]]
  monthdata[which(monthdata == 1 | monthdata == -1)] <- 0
  events_graph <- graph_from_adjacency_matrix(monthdata, mode = 'directed', diag = F)
  # Extract largest connected component
  # TODO: IMPLEMENT MULTI-COMPONENT HANDLING TO MAIN CLUSTER ALGORITHM
  cl <- clusters(events_graph)
  events_graph <- induced_subgraph(events_graph, which(cl$membership == which.max(cl$csize)))
  monthdata <- monthdata[which(cl$membership == which.max(cl$csize)), which(cl$membership == which.max(cl$csize))]
  thismonth_comm_mat <- comm_cutpoints(monthdata, walk = 20)
  plot_comms(monthdata, thismonth_comm_mat, .7, type = 'ties', returnData = 'communities')
  
  comms_pv <- pvclust(
    as.matrix(thismonth_comm_mat)
    , method.dist="correlation"
    , method.hclust = 'average'
    , parallel = T)
  
  intl_clusters <- pvpick(comms_pv, alpha=0.95, pv="au", type="geq", max.only=TRUE)
  
  monthly_components[[thismonth]] <- monthdata
  monthly_communities[[thismonth]] <- thismonth_comm_mat
  pv_structures[[thismonth]] <- comms_pv
  pv_clusters[[thismonth]] <- intl_clusters
  
}

save(monthly_components, file = './monthly_components_ge1.Rdata')
save(monthly_communities, file = './monthly_communities_ge1.Rdata')
save(pv_structures, file = './pv_structures_ge1.Rdata')
save(pv_clusters, file = './pv_clusters_ge1.Rdata')


###########################################################################
##################################################
#########################
## Re-run the above code but now remove the requirement that a tie has to
## include >1 interactions between states in a given month.
#########################
##################################################
###########################################################################

monthly_components <- list()
monthly_communities <- list()
pv_structures <- list()
pv_clusters <- list()

for(thismonth in 1:length(monthly_interactions)){
  monthdata <- monthly_interactions[[thismonth]]
  # monthdata[which(monthdata == 1 | monthdata == -1)] <- 0
  events_graph <- graph_from_adjacency_matrix(monthdata, mode = 'directed', diag = F)
  # Extract largest connected component
  # TODO: IMPLEMENT MULTI-COMPONENT HANDLING TO MAIN CLUSTER ALGORITHM
  cl <- clusters(events_graph)
  events_graph <- induced_subgraph(events_graph, which(cl$membership == which.max(cl$csize)))
  monthdata <- monthdata[which(cl$membership == which.max(cl$csize)), which(cl$membership == which.max(cl$csize))]
  thismonth_comm_mat <- comm_cutpoints(monthdata, walk = 20)
  plot_comms(monthdata, thismonth_comm_mat, .7, type = 'ties', returnData = 'communities')
  
  comms_pv <- pvclust(
    as.matrix(thismonth_comm_mat)
    , method.dist="correlation"
    , method.hclust = 'average'
    , parallel = T)
  
  intl_clusters <- pvpick(comms_pv, alpha=0.95, pv="au", type="geq", max.only=TRUE)
  
  monthly_components[[thismonth]] <- monthdata
  monthly_communities[[thismonth]] <- thismonth_comm_mat
  pv_structures[[thismonth]] <- comms_pv
  pv_clusters[[thismonth]] <- intl_clusters
  
}

save(monthly_components, file = './monthly_components_ge0.Rdata')
save(monthly_communities, file = './monthly_communities_ge0.Rdata')
save(pv_structures, file = './pv_structures_ge0.Rdata')
save(pv_clusters, file = './pv_clusters_ge0.Rdata')



thismonth_graph <- graph_from_adjacency_matrix(
  monthly_components[[168]]
  , weighted = T
  , mode = 'directed'
  , diag = F
  )

foo <- cluster_spinglass(
  thismonth_graph
  , implementation = 'neg'
  , spins = 50
  )


memberships <- foo$membership
V(thismonth_graph)$membership <- memberships
comm_graph_cutoff <- matrix(0, nrow = length(memberships), ncol = length(memberships))
rownames(comm_graph_cutoff) <- V(thismonth_graph)$name
colnames(comm_graph_cutoff) <- V(thismonth_graph)$name
for(comm in unique(memberships)){
  comm_graph_cutoff[which(memberships %in% comm), which(memberships %in% comm)] <- 1
}
comm_graph_cutoff <- graph_from_adjacency_matrix(comm_graph_cutoff, mode = 'undirected', diag = F)
comms_layout <- layout.fruchterman.reingold(comm_graph_cutoff)
memberships <- components(comm_graph_cutoff)$membership

weight.community=function(row,membership,weight.within,weight.between){
  if(
    as.numeric(membership[which(names(membership)==row[1])]) == 
    as.numeric(membership[which(names(membership)==row[2])])){
    weight=weight.within
  }else{
    weight=weight.between
  }
  return(weight)
}
weight.community(get.edgelist(thismonth_graph), memberships, 1, 0)
E(thismonth_graph)$sharedcomm <- apply(get.edgelist(thismonth_graph),1,weight.community,memberships,1,0)

E(thismonth_graph)$color <- ifelse(E(thismonth_graph)$sharedcomm == 1,
                              ifelse(E(thismonth_graph)$weight < 0
                                     , adjustcolor("red", alpha.f = .75)
                                     , adjustcolor("blue", alpha.f = .75))
                              , ifelse(E(thismonth_graph)$weight < 0
                                       , adjustcolor("Pink", alpha.f = .25)
                                       , adjustcolor("SkyBlue2", alpha.f = .25))
)
 


plot(
  foo
  , thismonth_graph
  , layout = comms_layout
  , vertex.size = 2
  , edge.color = E(thismonth_graph)$color
  , vertex.color = memberships
  , vertex.label.cex = .5
  , edge.arrow.size = 0.2
  )





membership_mat <- data.table(
  'Var1' = integer()
  , 'Var2' = integer()
)

for(idx in 1:max(foo$membership)){
  this_vertexes <- which(V(thismonth_graph)$name %in% V(thismonth_graph)$name[V(thismonth_graph)$membership %in% idx])
  this_edges <- expand.grid(this_vertexes, this_vertexes)
  membership_mat <- rbind(membership_mat, this_edges)
}


for(idx in 1:max(foo$membership))){
  edges_list <- expand.grid(thismonth_graph$clusters[[clust]], thismonth_clust$clusters[[clust]])
  thismonth_graph
}

membership_mat <- membership_mat[Var1 > Var2]

thismonth_cut <- thismonth_graph
thismonth_cut <- delete_edges(thismonth_cut, 1:length(E(thismonth_cut)))
thismonth_cut <- add_edges(thismonth_cut, as.matrix(membership_mat)-1)
plot(thismonth_cut)



thismonth_graph <- delete_edges(thismonth_graph, 1:length(E(thismonth_graph)))

thismonth_clust <- pv_clusters[[thismonth]]
for(clust in 1:length(thismonth_clust$clusters)){
  edges_list <- expand.grid(thismonth_clust$clusters[[clust]], thismonth_clust$clusters[[clust]])
  thismonth_graph
}

comm_graph_full <- thismonth_graph


comm_graph_full <- graph_from_adjacency_matrix(comm_matrix, mode = 'undirected', weighted = T, diag = F)
comm_matrix[comm_matrix < cutoff] <- 0

comm_graph_cutoff <- graph_from_adjacency_matrix(comm_matrix, mode = 'undirected', weighted = T, diag = F)
V(comm_graph_cutoff)$membership <- components(comm_graph_cutoff)$membership
V(comm_graph)$membership <- components(comm_graph_cutoff)$membership

memberships <- components(comm_graph_cutoff)$membership
if(!is.null(V(comm_graph)$name)){
  names(memberships) <- V(comm_graph)$name
} else{
  names(memberships) <- 1:vcount(comm_graph_full)
}

comm_graph_cutoff <- Matrix(0, nrow = length(memberships), ncol = length(memberships))
for(comm in unique(memberships)){
  comm_graph_cutoff[which(memberships %in% comm), which(memberships %in% comm)] <- 1
}
comm_graph_cutoff <- graph_from_adjacency_matrix(comm_graph_cutoff, mode = 'undirected', diag = F)
comms_layout <- layout.fruchterman.reingold(comm_graph_cutoff)


weight.community=function(row,membership,weight.within,weight.between){
  if(
    as.numeric(membership[which(names(membership)==row[1])]) == 
    as.numeric(membership[which(names(membership)==row[2])])){
    weight=weight.within
  }else{
    weight=weight.between
  }
  return(weight)
}
E(comm_graph)$sharedcomm <- apply(get.edgelist(comm_graph),1,weight.community,memberships,1,0)

if(min(E(comm_graph)$weight) < 0){
  E(comm_graph)$color <- ifelse(E(comm_graph)$sharedcomm == 1,
                                ifelse(E(comm_graph)$weight < 0
                                       , adjustcolor("red", alpha.f = .75)
                                       , adjustcolor("blue", alpha.f = .75))
                                , ifelse(E(comm_graph)$weight < 0
                                         , adjustcolor("Pink", alpha.f = .25)
                                         , adjustcolor("SkyBlue2", alpha.f = .25))
  )
} else {
  E(comm_graph)$color <- ifelse(
    E(comm_graph)$sharedcomm == 1
    , adjustcolor("grey40", alpha.f = .75)
    , adjustcolor("grey40", alpha.f = .15)
  )
}

if(type == 'ties'){
  plot(comm_graph
       , vertex.label.cex = 0.5
       , vertex.size = 4
       , vertex.color = V(comm_graph)$membership
       , edge.width = log(abs(E(comm_graph)$weight + 1))
       , edge.arrow.size = 0.1
       , layout = comms_layout
  )
} else if(type == 'communities'){
  comm_graph <- delete_edges(comm_graph, which(E(comm_graph)$sharedcomm == 0))
  plot(comm_graph
       , vertex.label.cex = 0.5
       , vertex.size = 4
       , vertex.color = V(comm_graph)$membership
       , edge.width = log(E(comm_graph)$weight + 1) + 1
       , edge.arrow.size = 0.1
       , layout = comms_layout
  )
