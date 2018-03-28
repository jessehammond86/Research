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
pacman::p_load(phoenixNet, igraph, parallel, nFactors)

## Set working directory

os_detect <- Sys.info()['sysname']

if (os_detect == 'Darwin'){
  setwd('/Users/localadmin/Dropbox/NetworkSignedComm')
  # setwd('/Users/jesse/Dropbox/NetworkSignedComm')
  
} else if (os_detect == 'Linux'){
  setwd('/media/jesse/Files/Dropbox/NetworkSignedComm')
  
} else if (os_detect == 'Windows'){
  setwd('C:/Users/Jesse/Dropbox/NetworkSignedComm')
}


###############################################################################
##
## Load in function to calculate and extract various types of network brokerage
##  centrality: the extent to which a node connects its community to other
##  communities in the network in incoming/outgoing, positive/negative ties.
##
###############################################################################

source('./GouldBrokerageSigned.R')


###############################################################################
##
## Load in data: temporal array networks
##
###############################################################################

## Load network data
# load('./Data/AnalysisData/monthly_arrays_raw.Rdata')
# load('./Data/AnalysisData/monthly_arrays_gold.Rdata')

load('./Data/AppendixData/yearly_arrays_raw.Rdata')
load('./Data/AppendixData/yearly_arrays_gold.Rdata')


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
  
  
  ## Fill NA entries
  full_edgelist[is.na(weight), weight := 0]
  full_edgelist[is.na(direct_tie_pos), direct_tie_pos := 0]
  full_edgelist[is.na(direct_tie_neg), direct_tie_neg := 0]
  
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

########## Monthly data
monthly_commdata_gold <- list()
monthly_commdata_raw <- list()

# for(this_time in 1:(length(monthly_arrays_gold)-1)){
#   
#   ## Goldstein scores
#   this_graph_gold <- graph_from_adjacency_matrix(
#     monthly_arrays_gold[[this_time]][,,3]
#     , weighted = T
#     , mode = 'directed'
#     , diag = F)
#   this_commdata_gold <- CommExtract(this_graph_gold)
#   
#   ## Raw counts
#   this_graph_raw <- graph_from_adjacency_matrix(
#     monthly_arrays_raw[[this_time]][,,3]
#     , weighted = T
#     , mode = 'directed'
#     , diag = F)
#   this_commdata_raw <- CommExtract(this_graph_raw)
#   
#   ## Add to list
#   monthly_commdata_gold[[this_time]] <- this_commdata_gold
#   monthly_commdata_raw[[this_time]] <- this_commdata_raw
#   
#   print(this_time)
#   
# }


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
  this_commdata_raw <- CommExtract(this_graph_raw)
  
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

# save(monthly_commdata_gold, file = './Data/AnalysisData/monthly_commdata_gold.Rdata')
# save(monthly_commdata_raw, file = './Data/AnalysisData/monthly_commdata_raw.Rdata')

save(yearly_commdata_gold, file = './Data/AppendixData/yearly_commdata_gold.Rdata')
save(yearly_commdata_raw, file = './Data/AppendixData/yearly_commdata_raw.Rdata')
