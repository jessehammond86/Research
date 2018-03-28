rm(list=ls())

## Load a bunch of dependencies

if (!'pacman' %in% installed.packages()) install.packages('pacman')
if(!'phoenixNet' %in% installed.packages()) devtools::install_github('jrhammond/phoenixNet')
if(!'phoxy' %in% installed.packages()) devtools::install_github('jrhammond/phoxy')
pacman::p_load(phoenixNet, multiplex, lubridate, phoxy
               , egonet, proxy, e1071, matrixcalc, expm, corrplot
               , blockmodeling, vars, TSA, forecast, zoo, tsna, scales
               , ggplot2, xergm, countrycode, parallel, doSNOW)


## Set working directory

os_detect <- Sys.info()['sysname']

if (os_detect == 'Darwin'){
  setwd('/Users/jesse/Dropbox/DomIntNetworks')
  phoenixnet_dir <- '/Users/jesse/Dropbox/Minerva'
  source('/Users/jesse/Dropbox/NetworkBalance/network_balance.R')
  source('/Users/jesse/Dropbox/DomIntNetworks/Scripts/00dom_int_netstats.R')
  
} else if (os_detect == 'Linux'){
  setwd('/media/jesse/Files/Dropbox/DomIntNetworks')
  phoenixnet_dir <- '/media/jesse/Files/Dropbox/Minerva'
  source('/media/jesse/Files/Dropbox/NetworkBalance/network_balance.R')
  source('/media/jesse/Files/Dropbox/DomIntNetworks/Scripts/00dom_int_netstats.R')
  
} else if (os_detect == 'Windows'){
  setwd('C:/Users/Jesse/Dropbox/DomIntNetworks')
  phoenixnet_dir <- 'C:/Users/Jesse/Dropbox/Minerva'
  source('C:/Users/Jesse/Dropbox/NetworkBalance/network_balance.R')
  source('C:/Users/Jesse/Dropbox/DomIntNetworks/Scripts/00dom_int_netstats.R')
}


##############################################################################
##
## Step 1: 5-year aggregations of interstate coop/conflict interactions
##
## Goal: generate a set of 4 2-layer networks, each covering 5 of the 20-year 
##  data period. Each network contains the sum of cooperation and conflict 
##  events between states for this period.
##
##############################################################################

#### Generate domestic/international stats by state

## Load network data

load('./Data/EventData/global_events.Rdata')

## Load states list

states <- fread('./Data/StatePop/global_pop_2010.csv')
states <- states[pop2010 > 500]
states <- states[['Country Code']]
states <- states[-c(43, 102, 131, 143, 150, 161)]

actors <- c(
  'BUS', 'COP', 'CRM', 'CVL', 'EDU', 'GOV'
  , 'JUD', 'MED', 'MIL', 'OPP', 'PTY', 'REB', 'SPY'
)


## Create list of state-actors
stateactors <- paste0(
  expand.grid(states, actors)$Var1
  , expand.grid(states, actors)$Var2
)


for(actioncode in c('code0', 'code1', 'code2', 'code3', 'code4')){
  global_events$dailynets[[actioncode]] <- 
    network::delete.vertices(
      global_events$dailynets[[actioncode]]
      , which(
        !network::get.vertex.attribute(
          global_events$dailynets[[actioncode]], 'vertex.names'
        ) %in% states
      )
    )
}

n <- length(states)

start_dates <- as.Date(
  c('1995-01-01', '2000-01-01', '2005-01-01', '2010-01-01')
)

end_dates <- as.Date(
  c('1999-12-31', '2004-12-31', '2009-12-31', '2014-12-31')
)


## Set up dates
time_window <- 'week'
global_start_end <- get.network.attribute(global_events$dailynets[[1]],'net.obs.period')$observations[[1]]
global_start_date_int <- global_start_end[1]
global_end_date_int <- global_start_end[2]
global_start_date <- as.Date(as.character(global_start_date_int), format = '%Y%m%d')
global_end_date <- as.Date(as.character(global_end_date_int), format = '%Y%m%d')
dates <- as.integer(format(seq.Date(global_start_date, global_end_date, time_window), '%Y%m%d'))
start_date <- dates[1]
end_date <- last(dates)


#### Aggregate weekly networks to year level


agg_networks <- function(
  date_set
){
  
  ## Generate output objects
  
  coop_net_out <- matrix(0, nrow = n, ncol = n)
  conf_net_out <- matrix(0, nrow = n, ncol = n)
  
  ## Sum weekly interactions
  for(i in date_set){
    coop_net <- network.collapse(
      global_events$dailynets$code2
      , at = dates[i])
    
    conf_net <- network.collapse(
      global_events$dailynets$code4
      , at = dates[i])
    
    
    coop_net_out <- coop_net_out + as.matrix.network(coop_net)
    conf_net_out <- conf_net_out + as.matrix.network(conf_net)
  }
  
  return(list(coop_net_out, conf_net_out))
}

nets_95_00 <- agg_networks(1:261)
nets_00_05 <- agg_networks(262:522)
nets_05_10 <- agg_networks(523:783)
nets_10_15 <- agg_networks(784:length(dates))


##############################################################################
##
## Step 2: 5-year aggregation of domestic dynamics
##
## Goal: generate a set of 4 5-year aggregate data sets. Each data set contains
##  one row per state, and indicates whether that state experienced a domestic
##  shock during that 5-year period.
##
##############################################################################


## Load analysis data

load('./Data/EventData/full_data.Rdata')


#######
##
## Generate some new variables
##
#######

## Inverting balance metrics to now be IMbalance metric (0 = balanced, 1 = imbalanced)

out_data[, net_balance_cut := 1 - net_balance_cut]
out_data[, net_balance_cut_lag1 := 1- net_balance_cut_lag1]

out_data[, global_balance_cut := 1 - global_balance_cut]
out_data[, global_balance_cut_lag1 := 1- global_balance_cut_lag1]

## Identifying isolated states

out_data[, isolate := 0L]
out_data[ego_coop_outdeg == 0 & ego_conf_outdeg == 0
         & ego_coop_indeg == 0 & ego_conf_indeg == 0, isolate := 1L]


## Create degree-3 polynomial for time

out_data[, date := as.integer(date)]
out_data[, date2 := date ^ 2]
out_data[, date3 := date ^ 3]


## Calculate NET egonet conflict

out_data[, net_ego_conflict := ego_conf_outdeg - ego_coop_outdeg]
out_data[, net_ego_conflict_lag1 := ego_conf_outdeg_lag1 - ego_coop_outdeg_lag1]


## Calculate NET domestic conflict density

out_data[, net_conflict_density := conf_density - coop_density]
out_data[, net_conflict_density_lag1 := conf_density_lag1 - coop_density_lag1]


## Create dummies for conflicts/wars

out_data[, civil_dummy := 1 * (civil_conflicts > 0)]
out_data[, inter_dummy := 1 * (interstate_conflicts > 0)]
out_data[, extra_dummy := 1 * (extrainter_conflicts > 0)]

out_data[, civilwar_dummy := 1 * (civil_wars > 0)]
out_data[, interwar_dummy := 1 * (interstate_wars > 0)]
out_data[, extrawar_dummy := 1 * (extrainter_wars > 0)]

cols <- c('global_balance_cut', 'global_balance_cut_lag1'
          , 'global_balance_pp', 'global_balance_pp_lag1')
for(col in cols){
  out_data[[col]][is.na(out_data[[col]])] <- 1.0
}


################################################################################
##
## Plot some plots
##
################################################################################


mod.edge2HPD <- function(
  edge_df = NULL
  , unique.rows = TRUE
  , axis.cols = NULL
  , type = "2D"
  , desc = NULL
  , edge.weight = NULL
  , edge.color = NULL
  , node.color = NULL
  , node.size = NULL
  , node.radius = NULL
  , node.axis = NULL) 
{
  #edge.weight - a list corresponding to edge weights (same order as in edge_df)
  #edge.color - a lis corresponding to edge colors (same order as in edge_df)
  #node.color - a data frame consisting of two columns: column 1 - node labels, column 2 - node color
  #node.size - a data frame consisting of two columns: column 1 - node labels, column 2 - node size
  #node.radius - a data frame consisting of two columns: column 1 - node labels, column 2 - node radius
  #node.axis - a data frame consisting of two columns: column 1 - node labels, column 2 - node axis
  
  if (is.null(edge_df)){
    stop("No edge data provided")
  }
  if (!is.data.frame(edge_df)){
    stop("edge_df is not a data frame")
  }
  if (unique.rows)
  {
    nr.old <- nrow(edge_df)
    edge_df <- unique(edge_df)
    
    if (nr.old > nrow(edge_df))
      cat("\n\t", nr.old - nrow(edge_df), "non-unique data-frame rows removed!\n\n")
  }
  
  # Get node labels
  lab1 <- as.character(unlist(edge_df[, 1]))
  lab2 <- as.character(unlist(edge_df[, 2]))
  
  
  # Get number of unique nodes
  nn <- length(unique(c(lab1, lab2)))
  
  # Define node ID
  id <- 1:nn
  # Define node label
  label <- unique(c(lab1, lab2))
  # Create a data frame for node attributes
  node.attributes <- data.frame(id, label)
  
  ####################################################
  # Node size definition
  if (!is.null(node.size))
  {
    if (is.numeric(node.size[, 2]) | is.integer(node.size[, 2]))
    {
      nSize <- c()
      
      for (i in 1:length(label))
      {
        indx <- which(as.character(node.size[,1]) == label[i])
        
        if (length(indx[1]) != 0)
          nSize = c(nSize, node.size[indx[1],2])
        else
        {
          msg <- paste("No size data provided for the node ", nodes$id[n], ". Value 1 will be assigned to this node!", sep = "")
          warning(msg)
          nSize = c(nSize, 1)
        }
      }
      
      node.attributes <- cbind(node.attributes, size = nSize)
      rm(i, nSize, indx)
    }#is.numeric
    else{
      stop("Node size is not numeric or integer.")  
    }
  }#is.null
  
  if (is.null(node.size))
  {
    warning("No data provided for the node size. All nodes will be assigned size 1!")
    node.attributes <- cbind(node.attributes, size = rep(1, nn))
  }
  
  ####################################################
  # Node color definition
  
  if (!is.null(node.color))
  {
    nCol <- c()
    
    for (i in 1:length(label))
    {
      indx <- which(as.character(node.color[,1]) == label[i])
      
      if (length(indx[1]) != 0)
        nCol = c(nCol, as.character(node.color[indx[1],2]))
      else
      {
        msg <- paste("No color data provided for the node ", nodes$id[n], ". Black color will be assigned to this node!", sep = "")
        warning(msg)
        nCol = c(nCol, "black")
      }
    }
    
    node.attributes <- cbind(node.attributes, color = nCol)
    rm(i, nCol, indx)
  }#is.null
  
  if (is.null(node.color))
  {
    warning("No data provided for the node color. All nodes will be colored black!")
    node.attributes <- cbind(node.attributes, color = as.character(rep("black", nn)))
  }
  
  ####################################################
  # Node radius definition
  
  if (!is.null(node.radius))
  {
    if (is.numeric(node.radius[, 2]) | is.integer(node.radius[, 2]))
    {
      nSize <- c()
      
      for (i in 1:length(label))
      {
        indx <- which(as.character(node.radius[,1]) == label[i])
        
        if (length(indx[1]) != 0)
          nSize = c(nSize, node.radius[indx[1],2])
        else
        {
          msg <- paste("No raidus data provided for the node ", nodes$id[n], ". Random values will be assigned!", sep = "")
          warning(msg)
          nSize = c(nSize,  sample(nn, 1))
        }
      }
      
      node.attributes <- cbind(node.attributes, radius = nSize)
      rm(i, nSize, indx)
    }#is.numeric
    else{
      stop("Node raidus is not integer.")  
    }
  }#is.null
  
  if (is.null(node.radius))
  {
    warning("No data provided for the node radius. All nodes will be assigned random radius values")
    node.attributes <- cbind(node.attributes, radius = sample(nn, nn))
  }
  
  ####################################################
  # Node axis definition
  
  if (!is.null(node.axis))
  {
    if (is.integer(node.axis[, 2]))
    {
      nSize <- c()
      
      for (i in 1:length(label))
      {
        indx <- which(as.character(node.axis[,1]) == label[i])
        
        if (length(indx[1]) != 0)
          nSize = c(nSize, node.axis[indx[1],2])
        else
        {
          msg <- paste("No axis data provided for the node ", nodes$id[n], ". This node will be assigned to axis 1!", sep = "")
          warning(msg)
          nSize = c(nSize,  1)
        }
      }
      
      node.attributes <- cbind(node.attributes, axis = nSize)
      rm(i, nSize, indx)
    }#is.integer
    else{
      stop("Node axis is not integer.")  
    }
  }#is.null
  
  if (is.null(node.axis))
  {
    warning("No data provided for the node axis. All nodes will be assigned to axis 1")
    node.attributes <- cbind(node.attributes, axis = rep(1, nn))
  }
  
  ######################################################
  
  # Create HPD object
  HPD <- list()
  
  # Define node attributes
  HPD$nodes$id <- as.integer(node.attributes$id)
  HPD$nodes$lab <- as.character(node.attributes$label)
  HPD$nodes$axis <- as.integer(node.attributes$axis)
  HPD$nodes$radius <- as.numeric(node.attributes$radius)
  HPD$nodes$size <- as.numeric(node.attributes$size)
  HPD$nodes$color <- as.character(node.attributes$color)
  
  ####################################################
  
  # Get number of edges
  ne <- nrow(edge_df)
  
  ####################################################
  # Edge weight definition
  
  if (!(is.null(edge.weight))) 
  {
    if (length(edge.weight) != nrow(edge_df))
      stop("Edge weights are not provided for all edges!") 
    
    if (is.numeric(edge.weight) | is.integer(edge.weight))
      edge_df <- cbind(edge_df, weight = edge.weight)
    else
      stop("Edge weight column is not numeric or integer.")  
  } 
  
  if (is.null(edge.weight))
  {
    warning("No edge weight provided Setting default edge weight to 1")
    edge_df <- cbind(edge_df, weight = rep(1, ne))
  }
  
  ####################################################
  # Edge color definition
  
  if (!(is.null(edge.color))) 
  {
    if (length(edge.color) != nrow(edge_df))
      stop("Edge colors are not provided for all edges!") 
    else 
      edge_df <- cbind(edge_df, color = as.character(edge.color))
  } 
  
  if (is.null(edge.color))
  {
    warning("No edge color provided. Setting default edge color to gray")
    edge_df <- cbind(edge_df, color = rep("gray", ne))
  }
  
  ####################################################
  # Set up edge list
  # Merge by default sorts things and changes the order of edges, so edge list has to stay paired
  edge.hlp <- merge(edge_df, node.attributes[, 1:2], by.x = 1, by.y = "label")
  edge <- merge(edge.hlp, node.attributes[1:2], by.x = 2, by.y = "label")
  
  HPD$edges$id1 <- as.integer(edge$id.x)
  HPD$edges$id2 <- as.integer(edge$id.y)
  
  HPD$edges$weight <- as.numeric(edge$weight)
  HPD$edges$color <- as.character(edge$color)
  
  HPD$nodes <- as.data.frame(HPD$nodes)
  HPD$edges <- as.data.frame(HPD$edges)
  
  # Add description
  if (is.null(desc)) {
    desc <- "No description provided"
  }
  HPD$desc <- desc
  
  # Define axis columns
  if (is.null(axis.cols)){
    axis.cols <- brewer.pal(length(unique(HPD$nodes$axis)), "Set1")
  }
  
  
  HPD$axis.cols <- axis.cols
  HPD$nodes$axis <- as.integer(HPD$nodes$axis)
  HPD$nodes$size <- as.numeric(HPD$nodes$size)
  HPD$nodes$color <- as.character(HPD$nodes$color)
  HPD$nodes$lab <- as.character(HPD$nodes$lab)
  HPD$nodes$radius <- as.numeric(HPD$nodes$radius)
  HPD$nodes$id <- as.integer(HPD$nodes$id)
  HPD$edges$id1 <- as.integer(HPD$edges$id1)
  HPD$edges$id2 <- as.integer(HPD$edges$id2)
  HPD$edges$weight <- as.numeric(HPD$edges$weight)
  HPD$edges$color <- as.character(HPD$edges$color)
  HPD$type <- type
  
  class(HPD) <- "HivePlotData"
  
  # Check HPD object
  chkHPD(HPD)
  return (HPD)
}

mod.mineHPD <- function(HPD, option = "", radData = NULL) 
{
  edges <- HPD$edges
  nodes <- HPD$nodes
  nn <- length(nodes$id)   
  
  ### ++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
  
  if (option == "axis <- source.man.sink") {
    
    # A change that allows this function to be used for undirected graphs
    # Now all nodes will be assigned to an axis
    
    done <- FALSE # a check to make sure all nodes get an axis
    
    for (n in 1:nn) {    
      id1 <- which(n ==edges$id1)
      id2 <- which(n ==edges$id2)
      
      if ((length(id1) == 0) & (length(id2) > 0 )) {
        nodes$axis[n] <- 2
        done <- TRUE
        next
      } # these are sinks, as they only receive an edge
      
      # note that set operations below drop duplicate values
      
      #Change 1 starts here
      if (length(id1) > 0)
      {
        if (length(id2) == 0)
        {
          nodes$axis[n] <- 1
          done <- TRUE
          next
        }        
        else
        {
          #Change 1 ends here
          common <- union(id1, id2)          
          source <- setdiff(id1, common)
          if (length(source) == 1) {
            nodes$axis[n] <- 1
            done <- TRUE
            next		
          } # these are sources
          
          if (length(common) >= 1) {
            nodes$axis[n] <- 3
            done <- TRUE
            next		
          } # these are managers
        }
      } 
      
      if (!done) {
        msg <- paste("node ", nodes$id[n], " was not assigned to an axis", sep = "")
        warning(msg)
      }  # alert the user there was a problem
      
    } # end of loop inspecting nodes
    
    nodes$axis <- as.integer(nodes$axis)
    
  }  ##### end of option == "axis <- source.man.sink
  
  ### ++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
  
  if (option == "rad <- random") {
    
    # This option assigns a random radius value to a node
    
    for (n in 1:nn)           
      nodes$radius[n] <- sample(1:nn, 1)
    
  }  ##### end of option == "rad <- random"
  
  ### ++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
  
  if (option == "rad <- userDefined") {
    
    # This option assigns a radius value to a node
    # based upon user specified values.
    
    if (is.null(radData)){
      stop("No edge data provided")
    }
    
    if (length(intersect(as.character(radData[,1]), as.character(nodes$lab))) == 0){
      stop("Provided data does not contain correct node labels")
    }          
    
    for (n in 1:nn)           
    {
      indexHlp <- which(as.character(radData[,1]) == nodes$lab[n])
      
      if (length(indexHlp) != 0)        
        nodes$radius[n] <- radData[indexHlp[1], 2]
      else
      {
        msg <- paste("No data provided for the node ", nodes$id[n], ". Value 1 will be assigned to this node!", sep = "")
        warning(msg)
        nodes$radius[n] <- 1
      }
    }
  }  ##### end of option == "rad <- userDefined"
  
  ### ++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
  
  if (option == "axis <- deg_one_two_more") 
  {
    
    # This option assigns a node to an axis
    # based upon whether its degree is 1, 2, or greater than two
    #     
    # degree 1 = axis 1, degree 2 = axis 2, degree >2 = axis3
    
    done <- FALSE # a check to make sure all nodes get an axis
    
    for (n in 1:nn) 
    {    
      id1 <- which(n ==edges$id1)
      id2 <- which(n ==edges$id2)         
      
      if ((length(id1) + length(id2)) == 1)
      {
        nodes$axis[n] <- 1
        done <- TRUE
        next
      } 
      
      if ((length(id1) + length(id2)) == 2)
      {
        nodes$axis[n] <- 2
        done <- TRUE
        next
      } 
      
      if ((length(id1) + length(id2)) > 2)
      {
        nodes$axis[n] <- 3
        done <- TRUE
        next
      }                 
      
      if (!done) {
        msg <- paste("node ", nodes$id[n], " was not assigned to an axis", sep = "")
        warning(msg)
      }  # alert the user there was a problem
      
    } # end of loop inspecting nodes
    
    nodes$axis <- as.integer(nodes$axis)
    
  }  ##### end of option == "axis <- deg_1_2_more
  
  ### ++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
  
  if (option == "axis <- deg_five_ten_more") 
  {
    
    # This option assigns a node to an axis
    # based upon whether its degree is <=5, 6-10, or greater than 10
    #     
    # degree <=5 = axis 1, degree between 6 and 10 = axis 2, degree >10 = axis32
    
    done <- FALSE # a check to make sure all nodes get an axis
    
    for (n in 1:nn) 
    {    
      id1 <- which(n ==edges$id1)
      id2 <- which(n ==edges$id2)         
      
      if ((length(id1) + length(id2)) <= 5)
      {
        nodes$axis[n] <- 1
        done <- TRUE
        next
      } 
      
      if (((length(id1) + length(id2)) > 5) & ((length(id1) + length(id2)) <= 10))
      {
        nodes$axis[n] <- 2
        done <- TRUE
        next
      } 
      
      if ((length(id1) + length(id2)) > 10)
      {
        nodes$axis[n] <- 3
        done <- TRUE
        next
      }                 
      
      if (!done) {
        msg <- paste("node ", nodes$id[n], " was not assigned to an axis", sep = "")
        warning(msg)
      }  # alert the user there was a problem
      
    } # end of loop inspecting nodes
    
    nodes$axis <- as.integer(nodes$axis)
    
  }  ##### end of option == "axis <- deg_five_ten_more"
  
  ### ++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
  
  if (option == "remove axis edge") {
    
    # This option removes edges which start and end on the same axis
    # It re-uses code from sumHPD
    
    # Create a list of edges to be drawn
    
    n1.lab <- n1.rad <- n2.lab <- n2.rad <- n1.ax <- n2.ax <- c()
    
    for (n in 1:(length(HPD$edges$id1))) {
      i1 <- which(HPD$edges$id1[n] == HPD$nodes$id)
      i2 <- which(HPD$edges$id2[n] == HPD$nodes$id)
      n1.lab <- c(n1.lab, HPD$nodes$lab[i1])
      n2.lab <- c(n2.lab, HPD$nodes$lab[i2])
      n1.rad <- c(n1.rad, HPD$nodes$radius[i1])
      n2.rad <- c(n2.rad, HPD$nodes$radius[i2])
      n1.ax <- c(n1.ax, HPD$nodes$axis[i1])
      n2.ax <- c(n2.ax, HPD$nodes$axis[i2])
    }
    
    fd <- data.frame(
      n1.id = HPD$edges$id1,
      n1.ax,
      n1.lab,
      n1.rad,
      n2.id = HPD$edges$id2,
      n2.ax,
      n2.lab,
      n2.rad,
      e.wt = HPD$edges$weight,
      e.col = HPD$edges$color)  	
    
    prob <- which(fd$n1.ax == fd$n2.ax)
    if (length(prob) == 0) cat("\n\t No edges were found that start and end on the same axis\n")
    if (length(prob) > 0) {
      edges <- edges[-prob,]
      cat("\n\t", length(prob), "edges that start and end on the same axis were removed\n")
    }
    
  }  ##### end of option == "remove axis edge"
  
  ### ++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
  
  if (option == "axis <- split") {
    
    # This option splits all axes into 2 new axes 
    # It can be used to address the "edge on the same axis" issue
    # This option may increase the number of nodes - a single node from the parent axis may appear on 2 "children" axes
    
    nodesNew <- nodes
    nodesOld <- nodes
    
    nAxes <- unique(nodes$axis)
    numAxes <- length(nAxes)
    
    #Renumerate axes
    for (i in numAxes:1)
      nodesOld[which(nodesOld$axis == nAxes[i]), "axis"] <- as.integer(2*nAxes[i] - 1)
    
    
    #Duplicate nodes 
    #Renumerate axes
    for (i in numAxes:1)
      nodesNew[which(nodesNew$axis == nAxes[i]), "axis"] <- as.integer(2*nAxes[i])
    
    #Re-numerate node ids
    nodesNew$id <- nodesNew$id + nn
    
    #Duplicated set of nodes with correct axis and node ids
    nodes <- rbind(nodesOld, nodesNew)
    rm(nodesOld, nodesNew)
    
    #Now create duplicated set of edges and re-numerate node ids for interactions
    edgesNew1 <- edges
    edgesNew1$id1 <- edgesNew1$id1 + nn
    edgesNew1$id2 <- edgesNew1$id2 + nn
    
    edgesNew2 <- edges
    edgesNew2$id1 <- edgesNew2$id1 + nn
    
    edgesNew3 <- edges
    edgesNew3$id2 <- edgesNew3$id2 + nn
    
    edges <- rbind(edges, edgesNew1, edgesNew2, edgesNew3)
    
    nodesAxis <- nodes[, c("id", "axis")]
    
    edgesHlp <- merge(edges, nodesAxis, by.x = "id1", by.y = "id")
    edges <- merge(edgesHlp, nodesAxis, by.x = "id2", by.y = "id")
    
    edgesOK <- edges[((edges$axis.x == 1) & (edges$axis.y == 2*numAxes)) | ((edges$axis.x == 2*numAxes) & (edges$axis.y == 1)), ]
    edgesHlp <- edgesOK
    
    if (numAxes > 1)
      for (i in 1:(numAxes - 1))
      {
        edgesOK <- edges[((edges$axis.x == 2*i) & (edges$axis.y == (2*i + 1))) | ((edges$axis.x == (2*i + 1)) & (edges$axis.y == 2*i)), ]
        edgesHlp <- rbind(edgesHlp, edgesOK)
      }
    
    for (i in 1:numAxes)
    {
      edgesOK <- edges[((edges$axis.x == (2*i - 1)) & (edges$axis.y == 2*i)) | ((edges$axis.x == 2*i) & (edges$axis.y == (2*i - 1))), ]
      edgesHlp <- rbind(edgesHlp, edgesOK)
    }
    
    edges <- edgesHlp[, 1:4]
    
    unique.ids <- unique(c(edges$id1, edges$id2))
    
    nodes <- nodes[nodes$id %in% unique.ids, ]  
    
    # Check if the new number of axes is 2 times larger than old one
    # if not, we need to adjust axis numbers
    nodesAxis.new <- sort(unique(nodes$axis))
    
    if(length(nodesAxis.new) != 2*numAxes)
      for (i in 1:length(nodesAxis.new))
        if (i != nodesAxis.new[i]){
          nodes[which(nodes$axis == nodesAxis.new[i]), "axis"] <- i
        }     
    
  }  ##### end of option == "axis <- split"
  
  ### ++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
  
  # Final assembly and checking...
  
  HPD$edges <- edges
  HPD$nodes <- nodes
  chkHPD(HPD)
  HPD
}



library("igraph")
library("plyr")
library("HiveR")
library("RColorBrewer")
library("grDevices")

pacman::p_load(igraph, plyr, HiveR, RColorBrewer, grDevices)

############################################################################################

out_data$date <- as.Date(out_data$date)

start_dates <- as.Date(
  c('1995-01-01', '2000-01-01', '2005-01-01', '2010-01-01')
)

end_dates <- as.Date(
  c('1999-12-31', '2004-12-31', '2009-12-31', '2014-12-31')
)


hivedata1 <- out_data[date >= start_dates[1] & date <= end_dates[1]
                      , list(
                        mean_netconflict = mean(net_conflict_density)
                        , sum_netconflict = sum(net_conflict_density)
                        , mean_balance = mean(net_balance_cut)
                        , coup = max(coupdummy)
                        , civil = max(civil_dummy)
                      )
                      , by = statename]



edges95 <- data.table(melt(nets_95_00[[1]]), melt(nets_95_00[[2]])$value)
setnames(edges95, c('nodei', 'nodej', 'coop', 'conf'))

edges95 <- edges95[!nodei == nodej]

edges95[, net_conf := conf - coop]

edges95 <- merge(edges95, hivedata1, by.x = 'nodei', by.y = 'statename')

nodes95 <- edges95[, list(sum(net_conf), max(coup), max(civil)) , by = nodei ]
setnames(nodes95, c('nodei', 'coop', 'coup', 'civil'))

# Create a graph. Use simplify to ensure that there are no duplicated edges or self loops

gD <- simplify(graph.adjacency(nets_95_00[[1]]))

## Generate weight based on number of interactions

gD <- igraph::set.edge.attribute(gD, 'weight', value = edges95[coop > 0 | conf > 0, net_conf])

# Calculate degree for all nodes
degOut <- degree(gD, v = V(gD), mode = "out")
gD <- set.vertex.attribute(gD, "degree", index = V(gD), value = degOut)

## Assign state-level variables
gD <- set.vertex.attribute(gD, 'civil', index = V(gD), value = nodes95$civil)
gD <- set.vertex.attribute(gD, 'coup', index = V(gD), value = nodes95$coup)
gD <- set.vertex.attribute(gD, 'meanconf', index = V(gD), value = nodes95$mean_netconflict)
gD <- set.vertex.attribute(gD, 'meanbal', index = V(gD), value = nodes95$mean_balance)



# Check the attributes
# summary(gD)

############################################################################################
#Determine node/edge color based on the properties

# Calculate node size
# We'll interpolate node size based on the node betweenness centrality, using the "approx" function
# And we will assign a node size for each node based on its betweenness centrality
approxVals <- approx(c(0.5, 1.5), n = length(unique(V(gD)$degree)))
nodes_size <- sapply(V(gD)$degree, function(x) approxVals$y[which(sort(unique(V(gD)$degree)) == x)])
rm(approxVals)

# Define node color
# We'll interpolate node colors based on the node degree using the "colorRampPalette" function from the "grDevices" library

# This function returns a function corresponding to a collor palete of "bias" number of elements
# F2 <- colorRampPalette(c("#CBD8FF", "#023FF7"), bias = length(unique(V(gD)$degree)), space = "rgb", interpolate = "linear")
F2 <- colorRampPalette(c("red", "white", "blue"), bias = length(unique(V(gD)$degree)), space = "rgb", interpolate = "linear")


# Now we'll create a color for each degree
colCodes <- F2(length(unique(V(gD)$degree)))
# And we will assign a color for each node based on its degree
nodes_col <- sapply(V(gD)$degree, function(x) colCodes[which(sort(unique(V(gD)$degree)) == x)])
rm(F2, colCodes)

# Assign visual attributes to edges using the same approach as we did for nodes
# F2 <- colorRampPalette(c("#CBD8FF", "#023FF7"), bias = length(unique(E(gD)$weight)), space = "rgb", interpolate = "linear")
F2 <- colorRampPalette(c("red", "white", "blue"), bias = length(unique(E(gD)$weight)), space = "rgb", interpolate = "linear")
colCodes <- F2(length(unique(E(gD)$weight)))
edges_col <- sapply(E(gD)$weight, function(x) colCodes[which(sort(unique(E(gD)$weight)) == x)])
rm(F2, colCodes)

############################################################################################
# Now the new (HiveR) part

# Now the same using adj2HPD() instead of edge2HPD()

# First, we'll create an adjacency matrix from our graph (gD)
gAdj <- get.adjacency(gD, type = "both", edges = FALSE, names = TRUE, sparse = FALSE)

# Then we'll create the hive object for it
hive1 <- adj2HPD(gAdj, type = "2D")

# Assign nodes to a radius based on their degree (number of edges they are touching)
hive2 <- mineHPD(hive1, option = "rad <- tot.edge.count")
# Assign nodes to a radius based on the user specified values (in our case betweenness centrality)
# hive2 <- mod.mineHPD(hive1, option = "rad <- userDefined", radData = data.frame(nds = V(gD)$name, bc = log(V(gD)$degree)))


# Assign nodes to axes based on their position in the edge list
hive3 <- mod.mineHPD(hive2, option = "axis <- source.man.sink")

# In some cases (for undirected graphs), some nodes will not be assigned to any axes
# In those cases, use the function from "mod.mineHPD.R" 
#source("mod.mineHPD.R")
#hive3 <- mod.mineHPD(hive2, option = "axis <- source.man.sink")

# Removing zero edges for better visualization 
hive4 <- mineHPD(hive3, option = "remove zero edge")

# Node/edge customization is the same as above


############################################################################################
# Let's do some node/edge customization

# First do nodes
nodes <- hive4$nodes

# Change the node color and size based on node degree and betweenness values
for (i in 1:nrow(nodes))
{
  nodes$color[i] <- nodes_col[which(nodes$lab[i] == V(gD)$name)]
  nodes$size[i] <- nodes_size[which(nodes$lab[i] == V(gD)$name)]
}

# Reassign these nodes to the hive(4) object
hive4$nodes <- nodes

saveplot <- hive4


# Now do the edges
edges <- hive4$edges


# Change the edge color and weight based on edge weight
for (i in 1:nrow(edges))
{
  index1 <- which(nodes$id == edges$id1[i])
  index2 <- which(nodes$id == edges$id2[i])

  edges$color[i] <- edges_col[which(E(gD)[as.character(nodes$lab[index1]) %->% as.character(nodes$lab[index2])] == E(gD))]
  
  edges$weight[i] <- .1+ edges95$coop[which(E(gD)[as.character(nodes$lab[index1]) %->% as.character(nodes$lab[index2])] == E(gD))]
}

# Reassign these edges to the hive(4) object
hive4$edges <- edges

nodes95[, radius := NULL]
nodes95[, radius := frank(coop, ties.method = 'average'), by = civil]
# setkeyv(nodes95, c('nodei', 'nodej'))
hive4 <- mod.mineHPD(hive4, option = "rad <- userDefined", radData = data.frame(nds = V(gD)$name, bc = nodes95$radius))

test <- hive4
test$nodes$axis[nodes95$civil == 1] <- 1L
test$nodes$axis[nodes95$civil == 0] <- 2L
test$axis.cols = c('red', 'blue')

# And plot it (Figure 5)
test$edges <- test$edges[test$edges$weight > 0.1, ]
plotHive(test, method = "abs", bkgnd = "white")



###############################################


#################################################
# Now lets expand the available options and add some new function(alitie)s
# Available in: "mod.mineHPD.R"



# Assign nodes to axes based on their degree
# Low degrees (1, 2, >2)
hive3 <- mod.mineHPD(hive2, option = "axis <- deg_one_two_more")
# Higer degrees (<=5, 6-10, >10)
hive3 <- mod.mineHPD(hive2, option = "axis <- deg_five_ten_more")

# Split axes - this function splits each of the 3 axes into 2 new axes (thus, resulting in 6 axes) 
# and removes edge on the same axis (but it introduces new (duplicated) nodes)
test2 <- mod.mineHPD(test, option = "axis <- split")

#################################################




