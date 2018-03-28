###############################################################################
##
## ProcessICEWSData.R
##
## Depends on: ProcessICEWSNetworks.R
## 
## Purpose: Processes the pos/neg/signed arrays of cleaned and normalized data
##  produced by the ProcessICEWSNetworks.R script.
##
## Function: Intakes temporal network arrays and identifies network communities
##  based on positive and negative ties. Returns information about community
##  membership and network descriptive statistics.
##
## Output: A list object of length K where K is the number of time periods in
##  the data set. Each element contains a dyadic data set containing information
##  on whether a given pair of states share community membership, as well as
##  their similarity in their respective community networks.
##
## Output files: 
##  - monthly_community_data.Rdata: list containing membership and dyadic data
##    for monthly signed networks.
##  - yearly_community_data.Rdata: list containing membership and dyadic data
##    for yearly signed networks.
##
###############################################################################

rm(list=ls())

## Load a bunch of dependencies

if (!'pacman' %in% installed.packages()) install.packages('pacman')
if(!'phoenixNet' %in% installed.packages()) devtools::install_github('jrhammond/phoenixNet')
pacman::p_load(phoenixNet, igraph, parallel)

## Set working directory

os_detect <- Sys.info()['sysname']

if (os_detect == 'Darwin'){
  setwd('/Users/localadmin/Dropbox/Research/CheapTalk')
  # setwd('/Users/jesse/Dropbox/NetworkSignedComm')
  
} else if (os_detect == 'Linux'){
  setwd('/media/jesse/Files/Dropbox/CheapTalk')
  
} else if (os_detect == 'Windows'){
  setwd('C:/Users/Jesse/Dropbox/Research/CheapTalk')
}


###############################################################################
##
## Load in function to calculate and extract various types of network brokerage
##  centrality: the extent to which a node connects its community to other
##  communities in the network in incoming/outgoing, positive/negative ties.
##
###############################################################################

source('./Rscripts/GouldBrokerageSigned.R')


###############################################################################
##
## Load in data: temporal array networks
##
###############################################################################

## Load network data
# load('./Data/AnalysisData/monthly_arrays_raw.Rdata')
# load('./Data/AnalysisData/monthly_arrays_gold.Rdata')

load('./Data/AnalysisData/yearly_arrays_raw.Rdata')
load('./Data/AnalysisData/yearly_arrays_gold.Rdata')


###############################################################################
##
## Set up parallel-computing cluster for brokerage centrality estimatino
##
###############################################################################
parallel_nodes <- 6
cl <- makeCluster(getOption('cl.cores', parallel_nodes))
clusterEvalQ(cl, library(igraph))

###############################################################################
##
## Function: CommExtract
##
## Purpose: intakes an igraph object, identifies communities via the spinglass
##  algorithm, and returns information about node community membership.
##
## Arguments:
##  - input_graph: sociomatrix or igraph object
##  - show_plot (T/F): show a plot of the communities?
##
## Outputs:
##  - PLOT: visualization of community membership with nodes positioned by
##    community.
##  - out-graph: igraph object containing community information for the largest
##    connected component of the input graph.
##
###############################################################################

CommExtract <- function(
  input_graph
  , show_plot = F
  ){
  
  ## Extract list of state names
  states <- V(input_graph)$name
  
  ## Generate edgewise ID
  E(input_graph)$edgeID <- 1:length(E(input_graph))
  
  ## Identify giant component of input graph
  clusters_id <- clusters(input_graph)
  giant_component <- induced_subgraph(
    input_graph
    , which(clusters_id$membership %in% which.max(clusters_id$csize))
  )
  
  ## Identify clusters within giant component
  comm_membership <- cluster_spinglass(
    giant_component
    , implementation = 'neg'
    , spins = 20
    , gamma = 1
    , gamma.minus = 1
  )
  for(i in 1:length(comm_membership)){print(comm_membership[[i]])}
  
  ## Retrieve community membership
  memberships <- comm_membership$membership
  V(giant_component)$membership <- memberships
  
  
  ## Identify whether a given edge connects nodes within the same community.
  edge_pairs <- data.table(
    get.edgelist(giant_component, names = F)
  )
  
  E(giant_component)$sharedcomm <- 0
  E(giant_component)$sharedcomm[which(V(giant_component)$membership[edge_pairs$V1] == 
          V(giant_component)$membership[edge_pairs$V2])] <- 1
 
  
  ##########
  ##
  ## Calculate shared community status of ties, and assign edge parameters
  ##  based on cooperative/conflictual sign as well as inter/intra-community
  ##  status.
  ## Create temporary new graph where only ties within communities exist.
  ##  The purpose of this graph is to retrieve positions for plotting the
  ##  original network: fruchtermann-reingold layout for networks where only
  ##  ties connecting communities exist will cause nodes in the same community
  ##  to cluster together while communities overall stay separate.
  ##########
  
  comm_graph_cutoff <- matrix(0, nrow = length(memberships), ncol = length(memberships))
  rownames(comm_graph_cutoff) <- V(giant_component)$name
  colnames(comm_graph_cutoff) <- V(giant_component)$name
  for(comm in unique(memberships)){
    comm_graph_cutoff[which(memberships %in% comm), which(memberships %in% comm)] <- 1
  }
  comm_graph_cutoff <- graph_from_adjacency_matrix(comm_graph_cutoff, mode = 'undirected', diag = F)
  comms_layout <- layout.fruchterman.reingold(comm_graph_cutoff)
  
  comm_edgelist <- data.table(as_edgelist(comm_graph_cutoff))
  setnames(comm_edgelist, c('statea', 'stateb'))
  comm_edgelist[, sharedcomm := 1]
  

  
  ## Assign colors: red or blue for conflict/cooperation, more vivid for ties
  ##  within the same community, fainter for ties across communities
  E(giant_component)$color <- ifelse(
    E(giant_component)$sharedcomm == 1,
    ifelse(E(giant_component)$weight < 0
           , adjustcolor("red", alpha.f = .75)
           , adjustcolor("blue", alpha.f = .75))
    , ifelse(E(giant_component)$weight < 0
             , adjustcolor("red", alpha.f = .35)
             , adjustcolor("SkyBlue2", alpha.f = .25))
  )
  
  ## Plot if required.
  if (show_plot == T){
    plot(
      comm_membership
      , giant_component
      , layout = comms_layout
      , vertex.size = 2
      , edge.color = E(giant_component)$color
      , vertex.color = memberships
      , vertex.label.cex = .75
      , edge.arrow.size = 0.2
    )
  }
  
  
  
  
  ##########
  ##
  ## Extract positive/negative, incoming/outgoing brokerage centrality
  ##  for each vertex in the network.
  ##
  ##########
  
  # ## Outgoing/cooperative
  # V(giant_component)$brokerage_out_coop <- GouldBrokerage(
  #   giant_component
  #   , bridge = 'positive'
  #   , direction = 'outgoing'
  #   , cl = cl
  # )
  # 
  # ## Outgoing/conflictual
  # V(giant_component)$brokerage_out_conf <- GouldBrokerage(
  #   giant_component
  #   , bridge = 'negative'
  #   , direction = 'outgoing'
  #   , cl = cl
  # )
  # 
  # ## Incoming/cooperative
  # V(giant_component)$brokerage_in_coop <- GouldBrokerage(
  #   giant_component
  #   , bridge = 'positive'
  #   , direction = 'incoming'
  #   , cl = cl
  # )
  # 
  # ## Incoming/conflictual
  # V(giant_component)$brokerage_in_conf <- GouldBrokerage(
  #   giant_component
  #   , bridge = 'negative'
  #   , direction = 'incoming'
  #   , cl = cl
  # )
  # 
  ##########
  ##
  ## Merge information from GIANT COMPONENT with information from ORIGINAL
  ##  INPUT GRAPH (including isolated nodes).
  ##
  ##########

  ## Edge color (red = conflict, blue = cooperation)
  E(input_graph)$color <- 'grey'
  E(input_graph)$color[
    E(input_graph)$edgeID %in% E(giant_component)$edgeID
    ] <- E(giant_component)$color

  ## Node community membership
  V(input_graph)$membership <- NA
  V(input_graph)$membership[
    V(input_graph)$name %in% V(giant_component)$name
    ] <- V(giant_component)$membership
  # 
  # ## Node brokerage: outgoing/cooperative
  # V(input_graph)$brokerage_out_coop <- 0
  # V(input_graph)$brokerage_out_coop[
  #   V(input_graph)$name %in% V(giant_component)$name
  #   ] <- V(giant_component)$brokerage_out_coop
  # 
  # ## Node brokerage: outgoing/conflictual
  # V(input_graph)$brokerage_out_conf <- 0
  # V(input_graph)$brokerage_out_conf[
  #   V(input_graph)$name %in% V(giant_component)$name
  #   ] <- V(giant_component)$brokerage_out_conf
  # 
  # ## Node brokerage: incoming/cooperative
  # V(input_graph)$brokerage_in_coop <- 0
  # V(input_graph)$brokerage_in_coop[
  #   V(input_graph)$name %in% V(giant_component)$name
  #   ] <- V(giant_component)$brokerage_in_coop
  # 
  # ## Node brokerage: incoming/conflictual
  # V(input_graph)$brokerage_in_conf <- 0
  # V(input_graph)$brokerage_in_conf[
  #   V(input_graph)$name %in% V(giant_component)$name
  #   ] <- V(giant_component)$brokerage_in_conf
  # 
  
  ###########
  ##
  ## Get information about each node's community size and position within
  ##  its community.
  ##
  ###########
  
  V(input_graph)$comm_size <- 0
  V(input_graph)$comm_indeg <- 0
  V(input_graph)$comm_outdeg <- 0
  V(input_graph)$comm_between <- 0
  V(input_graph)$comm_embedded <- 0
  
  full_degree <- data.table(
    name = V(input_graph)$name
    , degree = degree(input_graph, mode = 'total', normalized = T)
  )
  
  for(this_comm in unique(memberships)){
    this_commnet <- induced_subgraph(
      input_graph
      , which(V(input_graph)$membership %in% this_comm)
      )
    
    ## Community size
    this_comm_size <- vcount(this_commnet)
    
    ## Betweenness (positive ties only)
    this_comm_between <- betweenness(
      delete_edges(this_commnet, which(E(this_commnet)$weight < 0))
      , directed = T
      , weights = E(delete_edges(this_commnet, which(E(this_commnet)$weight < 0)))$weight
      , normalized = T
      )

    ## In-degree
    this_comm_indeg <- degree(this_commnet, mode = 'in', normalized = T)
    
    ## Out-degree
    this_comm_outdeg <- degree(this_commnet, mode = 'out', normalized = T)
    
    ## Community embeddedness (community degree / total degree)
    this_degree <- full_degree[name %in% V(this_commnet)$name]
    this_comm_embedded <- degree(this_commnet, mode = 'total', normalized = T) * this_degree[, degree]
    
    ## Assign subnet values to overall graph variables
    V(input_graph)$comm_between[
      V(input_graph)$name %in% names(this_comm_between)] <- this_comm_between
    V(input_graph)$comm_between[is.nan(V(input_graph)$comm_between)] <- 0
    
    V(input_graph)$comm_indeg[
      V(input_graph)$name %in% names(this_comm_indeg)] <- this_comm_indeg
    
    V(input_graph)$comm_embedded[
      V(input_graph)$name %in% names(this_comm_embedded)] <- this_comm_embedded
    
    
    V(input_graph)$comm_outdeg[
      V(input_graph)$name %in% names(this_comm_outdeg)] <- this_comm_outdeg
    
    V(input_graph)$comm_size[
      V(input_graph)$name %in% V(this_commnet)$name] <- this_comm_size
    
  }
  
  
  ##########
  ##
  ## Generate dyadic output data table and merge with data
  ##
  ########## 
  
  ## Create full edge list
  full_edgelist <- data.table(
    expand.grid(
      V(input_graph)$name
      , V(input_graph)$name
    )
  )
  setnames(full_edgelist, c('statea', 'stateb'))
  
  ## Merge with edge data on weights
  edges_edgelist <- data.table(
    as_edgelist(input_graph)
    , E(input_graph)$weight
    )
  setnames(
    edges_edgelist
    , c('statea', 'stateb', 'weight')
    )
  full_edgelist <- merge(
    full_edgelist, edges_edgelist
    , by = c('statea', 'stateb')
    , all.x = T
    )
  full_edgelist[, direct_tie_pos := ifelse(
    weight > 0, 1, 0
    )
  ]
  full_edgelist[, direct_tie_neg := ifelse(
    weight < 0, 1, 0
    )
  ]
  
  ## Merge with edge data on shared community ties
  full_edgelist <- merge(
    full_edgelist, comm_edgelist
    , by = c('statea', 'stateb')
    , all.x = T
  )
  
  ## Fill NA entries
  full_edgelist[is.na(weight), weight := 0]
  full_edgelist[is.na(direct_tie_pos), direct_tie_pos := 0]
  full_edgelist[is.na(direct_tie_neg), direct_tie_neg := 0]
  full_edgelist[is.na(sharedcomm), sharedcomm := 0]
  
  setkeyv(full_edgelist, c('statea', 'stateb'))
  
  ## Add dyadic variables based on vertex values
  vertex_vars <- data.table(
    V(input_graph)$name
    , V(input_graph)$comm_size
    , V(input_graph)$comm_indeg
    , V(input_graph)$comm_outdeg
    , V(input_graph)$comm_between
    , V(input_graph)$comm_embedded
    # , V(input_graph)$brokerage_out_coop
    # , V(input_graph)$brokerage_out_conf
    # , V(input_graph)$brokerage_in_coop
    # , V(input_graph)$brokerage_in_conf
  )
  
  var_names <- c(
    'comm_size', 'comm_indeg', 'comm_outdeg', 'comm_between', 'comm_embedded'
    # , 'broker_out_coop', 'broker_out_conf', 'broker_in_coop', 'broker_in_conf'
  )
  
  setnames(
    vertex_vars
    , c('state', var_names)
    )
  
  ## Merge in vertex info about state A
  full_edgelist <- merge(
    full_edgelist
    , vertex_vars
    , by.x = 'statea'
    , by.y = 'state'
    , all.x = T
  )
  setnames(
    full_edgelist
    , var_names
    , paste0(var_names, '_a')
    )
  
  ## Merge in vertex info about state B
  full_edgelist <- merge(
    full_edgelist
    , vertex_vars
    , by.x = 'stateb'
    , by.y = 'state'
    , all.x = T
  )
  setnames(
    full_edgelist
    , var_names
    , paste0(var_names, '_b')
  )
  
  setcolorder(full_edgelist, c(2,1,3:ncol(full_edgelist)))
  setkeyv(full_edgelist, c('statea', 'stateb'))
  
  ## Return graph object with membership information and edge coloration.
  return(full_edgelist)
}


###############################################################################
##
## Apply community-extraction to temporal signed networks and store in output
##  list of data tables.
##
###############################################################################

########## Yearly data
yearly_commdata_gold <- list()
yearly_commdata_raw <- list()

for(this_time in 1:length(yearly_arrays_gold)){
  
  ## Goldstein scores
  this_graph_gold <- graph_from_adjacency_matrix(
    yearly_arrays_gold[[this_time]][,,3]
    , weighted = T
    , mode = 'directed'
    , diag = F)
  this_commdata_gold <- CommExtract(this_graph_gold)
  
  ## Raw counts
  this_graph_raw <- graph_from_adjacency_matrix(
    yearly_arrays_raw[[this_time]][,,3]
    , weighted = T
    , mode = 'directed'
    , diag = F)
  this_commdata_raw <- CommExtract(this_graph_raw, show_plot = T)
  
  yearly_commdata_gold[[this_time]] <- this_commdata_gold
  yearly_commdata_raw[[this_time]] <- this_commdata_raw
  
  print(this_time)
}


###############################################################################
##
## Shut down parallel cluster
##
###############################################################################

stopCluster(cl)


###############################################################################
##
## Save lists of edgelists to output files.
##
###############################################################################

save(yearly_commdata_gold, file = './Data/AnalysisData/yearly_commdata_gold.Rdata')
save(yearly_commdata_raw, file = './Data/AnalysisData/yearly_commdata_raw.Rdata')
